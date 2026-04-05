;;; popterm.el --- Posframe terminal toggler with smart backends  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Chetan Koneru <kchetan.hadoop@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (posframe "1.4.0"))
;; Keywords: terminals, convenience, tools
;; URL: https://github.com/CsBigDataHub/popterm.el

;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; popterm.el — a terminal toggler merging three established patterns:
;;
;;  * Centered child-frame popup  (seagle0128/init-shell.el)
;;  * Multi-backend, named instances  (justinlime/toggle-term.el)
;;  * Smart auto-cd, TRAMP-awareness, scope  (jixiuf/vterm-toggle.el)
;;
;; Display methods:
;;   posframe   — centered child frame (requires posframe.el)
;;   window     — bottom split window
;;   fullscreen — full-frame buffer switch
;;
;; Backends (auto-detected at runtime, no hard dependency):
;;   vterm / ghostel / eat / shell / eshell
;;
;; Quick start:
;;   (use-package popterm
;;     :bind (("C-`"  . popterm-toggle)
;;            ("C-~"  . popterm-toggle-cd)
;;            ([f9]   . popterm-window-toggle))
;;     :config
;;     (setq popterm-backend        'vterm
;;           popterm-display-method 'posframe
;;           popterm-scope          'project
;;           popterm-auto-cd        t))

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'comint)

;;; ── Customization ─────────────────────────────────────────────────────────────

(defgroup popterm nil
  "Posframe-based terminal toggler with smart backends."
  :group 'terminals
  :prefix "popterm-")

(defcustom popterm-backend 'vterm
  "Terminal backend to use.
One of `vterm', `ghostel', `eat', `shell', or `eshell'."
  :type '(choice (const :tag "vterm"   vterm)
                 (const :tag "ghostel" ghostel)
                 (const :tag "eat"     eat)
                 (const :tag "shell"   shell)
                 (const :tag "eshell"  eshell))
  :group 'popterm)

(defcustom popterm-display-method 'posframe
  "How to display the terminal.
`posframe'   — centered child frame (requires posframe.el).
`window'     — bottom split window.
`fullscreen' — full-frame buffer switch."
  :type '(choice (const :tag "Posframe child frame" posframe)
                 (const :tag "Window split"         window)
                 (const :tag "Fullscreen"            fullscreen))
  :group 'popterm)

(defcustom popterm-scope nil
  "Scope for reusing existing terminal buffers.
nil         — reuse any popterm buffer.
`project'   — only buffers belonging to the current project root.
`frame'     — only buffers not visible in another frame.
`dedicated' — one fixed buffer per backend (ignore other instances)."
  :type '(choice (const :tag "All buffers"  nil)
                 (const :tag "Project"      project)
                 (const :tag "Frame"        frame)
                 (const :tag "Dedicated"    dedicated))
  :group 'popterm)

(defcustom popterm-auto-cd nil
  "If non-nil, send a cd to the originating buffer directory on every toggle."
  :type 'boolean
  :group 'popterm)

(defcustom popterm-cd-auto-create-buffer t
  "Non-nil allows `popterm-toggle-cd' to create a buffer when none exists.
When nil, `popterm-toggle-cd' is a no-op if no terminal is found in scope."
  :type 'boolean
  :group 'popterm)

;; ── Posframe geometry ─────────────────────────────────────────────────────────

(defcustom popterm-posframe-width-ratio 0.62
  "Posframe width as a fraction of the parent frame width."
  :type 'float
  :group 'popterm)

(defcustom popterm-posframe-height-ratio 0.62
  "Posframe height as a fraction of the parent frame height."
  :type 'float
  :group 'popterm)

(defcustom popterm-posframe-min-width 100
  "Minimum posframe width in columns."
  :type 'integer
  :group 'popterm)

(defcustom popterm-posframe-border-width 3
  "Posframe internal border width in pixels."
  :type 'integer
  :group 'popterm)

(defcustom popterm-posframe-poshandler
  (when (fboundp 'posframe-poshandler-frame-center)
    #'posframe-poshandler-frame-center)
  "Position handler passed to `posframe-show'."
  :type 'function
  :group 'popterm)

(defcustom popterm-posframe-focus-delay 0.35
  "Seconds to inhibit the hidehandler after showing the posframe.
On Wayland/pgtk, focus-change events are asynchronous and can arrive
before `select-frame-set-input-focus' completes.  Without this guard,
pressing the toggle key twice in quick succession hides the posframe
immediately after showing it because the hidehandler fires during the
focus-transfer window.  Increase this value if the race still occurs
on a slow compositor."
  :type 'float
  :group 'popterm)

;; ── Window split geometry ─────────────────────────────────────────────────────

(defcustom popterm-window-height-ratio 0.30
  "Window-split height as a fraction of the frame height."
  :type 'float
  :group 'popterm)

(defcustom popterm-window-side 'below
  "Side for the window split: `below', `above', `left', or `right'."
  :type '(choice (const below) (const above) (const left) (const right))
  :group 'popterm)

;;; ── Internal state ────────────────────────────────────────────────────────────

(defvar popterm--frame nil "Active posframe child frame.")
(defvar popterm--frame-buffer nil "Buffer displayed in the active posframe.")
(defvar popterm--window nil "Active window-split window.")
(defvar popterm--source-buffer nil "Buffer active before the last toggle.")
(defvar popterm--active-display-method nil
  "Display method currently used by the active Popterm session.")
(defvar popterm--saved-mode-line-format nil
  "Original `mode-line-format' for the active posframe buffer.")
(defvar popterm--inhibit-hidehandler nil
  "Non-nil while the posframe is being shown, inhibiting the hidehandler.
Set to t by `popterm--posframe-show' and cleared after
`popterm-posframe-focus-delay' seconds via a one-shot timer.  This
prevents the Wayland/pgtk async focus event from immediately re-hiding
the posframe before `select-frame-set-input-focus' has completed.")

(defvar popterm--focus-timer nil
  "One-shot timer that clears `popterm--inhibit-hidehandler'.
Stored so that rapid successive toggles within `popterm-posframe-focus-delay'
cancel the previous timer before spawning a new one, preventing a ghost
timer from resetting the inhibit flag mid-flight on the next show cycle.")

(defvar popterm--theme-refresh-timer nil
  "Debounce timer for re-showing the active posframe after a theme change.")

(defvar popterm--theme-watch-active nil
  "Non-nil while Popterm is watching theme changes for an active posframe.")

(defvar popterm--focus-guard-active nil
  "Non-nil while the posframe focus guard is installed.
Prevents duplicate installations of the `after-focus-change-function'
advice.  The guard reclaims focus for the posframe whenever the user
performs an operation (e.g. `C-x b') that lands focus on the parent
frame while the posframe is still visible.")

(defvar popterm--display-buffer-guard-active nil
  "Non-nil while Popterm is preventing window redisplay of posframe buffers.
Used together with `display-buffer-alist' so external packages cannot
re-display the active popterm buffer in a regular window while the
posframe session is active.")

(defconst popterm--display-buffer-guard-entry
  '(popterm--display-buffer-guard-p
    (display-buffer-no-window)
    (allow-no-window . t))
  "`display-buffer-alist' entry that blocks window redisplay of popterm buffers.")

(defconst popterm--display-buffer-prefix "*popterm-"
  "Canonical buffer-name prefix used when Popterm creates terminal buffers.")

(defvar-local popterm--managed-buffer nil
  "Non-nil in buffers created and managed by Popterm.
Backends such as Ghostel can rename their buffers after startup, so
Popterm cannot rely solely on `buffer-name' to recognize its terminals.")

(defvar-local popterm--buffer-backend nil
  "Backend symbol associated with the current Popterm buffer.")

(defvar-local popterm--buffer-instance-name nil
  "Optional instance name associated with the current Popterm buffer.")

(defun popterm--buffer-backend-p (buffer backend)
  "Return non-nil when BUFFER belongs to BACKEND.
Prefers Popterm's buffer-local metadata and falls back to the backend's
major mode for buffers created before that metadata existed."
  (with-current-buffer buffer
    (or (eq popterm--buffer-backend backend)
        (and (null popterm--buffer-backend)
             (derived-mode-p (popterm--mode backend))))))

(defun popterm--buffer-instance-matches-p (buffer name backend)
  "Return non-nil when BUFFER matches instance NAME for BACKEND.
Named instances use Popterm's buffer-local metadata when available so
backend-driven renames (for example Ghostel titles) do not break lookup."
  (and (popterm--buffer-p buffer backend)
       (with-current-buffer buffer
         (if (local-variable-p 'popterm--buffer-instance-name buffer)
             (equal popterm--buffer-instance-name name)
           (if name
               (string= (buffer-name buffer) (popterm--buffer-name name backend))
             (or popterm--managed-buffer
                 (bound-and-true-p popterm-mode)))))))

(defun popterm--buffer-p (&optional buffer-or-name backend)
  "Return non-nil when BUFFER-OR-NAME is a Popterm buffer.
When BACKEND is non-nil, also require that the buffer belongs to that
backend.  Uses Popterm's buffer-local marker instead of depending only
on the `*popterm-*' name prefix so title-renaming backends such as
Ghostel remain discoverable."
  (when-let* ((buffer (cond
                       ((bufferp buffer-or-name) buffer-or-name)
                       ((stringp buffer-or-name) (get-buffer buffer-or-name))
                       (t nil))))
    (with-current-buffer buffer
      (and (or popterm--managed-buffer
               (bound-and-true-p popterm-mode)
               (string-prefix-p popterm--display-buffer-prefix
                                (buffer-name buffer)))
           (or (null backend)
               (popterm--buffer-backend-p buffer backend))))))

(defun popterm--preserve-buffer-during-posframe-delete (orig buffer-or-name)
  "Keep Popterm terminal buffers alive during external posframe cleanup.

Some configurations call `posframe-delete-all' after theme changes.
That helper deletes child frames and then kills every posframe buffer,
which is unsafe for Popterm because its posframe buffer is the live
terminal session itself.  For Popterm buffers, clear stale posframe
state but preserve the buffer and process.  Delegate all other buffers
to ORIG with BUFFER-OR-NAME unchanged."
  (let ((buffer (get-buffer buffer-or-name)))
    (if (not (popterm--buffer-p buffer))
        (funcall orig buffer-or-name)
      (with-current-buffer buffer
        (when (boundp 'posframe--frame)
          (setq posframe--frame nil)))
      (when (eq buffer popterm--frame-buffer)
        (popterm--cleanup-posframe-state))
      nil)))

(defvar popterm--posframe-kill-buffer-advice-installed nil
  "Non-nil once Popterm has advised `posframe--kill-buffer'.")

(defun popterm--install-posframe-kill-buffer-advice ()
  "Install Popterm's protection around `posframe--kill-buffer'."
  (when (and (not popterm--posframe-kill-buffer-advice-installed)
             (fboundp 'posframe--kill-buffer))
    (setq popterm--posframe-kill-buffer-advice-installed t)
    (advice-add 'posframe--kill-buffer :around
                #'popterm--preserve-buffer-during-posframe-delete)))

(popterm--install-posframe-kill-buffer-advice)

;;; ── Minor mode + vterm keymap passthrough ─────────────────────────────────────

(defvar popterm-term-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<next>")  #'popterm-next)
    (define-key map (kbd "C-<prior>") #'popterm-prev)
    (define-key map (kbd "C-q")       #'popterm-return)
    map)
  "Keymap active in `popterm-mode' terminal buffers.")

(define-minor-mode popterm-mode
  "Minor mode active in popterm terminal buffers.
Key bindings (see also `popterm-term-map'):

\\[popterm-next]   — cycle forward  through popterm buffers
\\[popterm-prev]   — cycle backward through popterm buffers
\\[popterm-return] — return to source buffer

For vterm, these keys are registered in `vterm-keymap-exceptions' so
vterm's character-mode input handler passes them to Emacs.

\\{popterm-term-map}"
  :init-value nil
  :keymap popterm-term-map)

;; vterm intercepts keys at the process-filter level before minor-mode keymaps.
;; Listing a key in vterm-keymap-exceptions returns it to Emacs instead.
(defconst popterm--vterm-passthrough-keys
  '("C-<next>" "C-<prior>" "C-q")
  "Keys added to `vterm-keymap-exceptions' so `popterm-mode' bindings fire.")

(defun popterm--vterm-setup ()
  "Add `popterm--vterm-passthrough-keys' to `vterm-keymap-exceptions'.
Called from `vterm-mode-hook' so that vterm is guaranteed to be loaded
before `vterm-keymap-exceptions' is referenced.  vterm makes
`vterm-keymap-exceptions' buffer-local, so updates here affect only the
current buffer.  `cl-pushnew' keeps the operation idempotent."
  (when (boundp 'vterm-keymap-exceptions)
    (dolist (key popterm--vterm-passthrough-keys)
      (cl-pushnew key vterm-keymap-exceptions :test #'equal))))


;;; ── Backend helpers ───────────────────────────────────────────────────────────

;; Declare external variables as dynamic to prevent the lexical-binding trap.
;; With lexical-binding: t, `let' only creates a dynamic binding when the
;; variable is already declared special (defvar/defcustom).  Because popterm
;; lazily detects backends without hard-requiring them, the compiler has not
;; seen these defcustoms.  A bare (defvar foo) with no value is the canonical
;; Emacs idiom for "treat this as special in this file" without loading the
;; package or setting a default — used widely in cl-lib and ELPA packages for
;; exactly this cross-package dynamic-variable reference pattern.
(defvar eat-buffer-name)
(defvar eshell-buffer-name)
(defvar ghostel-buffer-name)
(defvar ghostel--process)
(declare-function ghostel "ghostel" ())
;; vterm-buffer-name is handled via a direct argument (see vterm backend below)
;; so no defvar is needed for it.

(defsubst popterm--mode (backend)
  "Return the major-mode symbol for BACKEND."
  (pcase backend
    ('vterm   'vterm-mode)
    ('ghostel 'ghostel-mode)
    ('eat     'eat-mode)
    ('shell   'shell-mode)
    ('eshell  'eshell-mode)))

(defun popterm--buffer-name (&optional name backend)
  "Return canonical buffer name for BACKEND with optional instance NAME."
  (let ((tag (pcase (or backend popterm-backend)
               ('vterm   "vterm")
               ('ghostel "ghostel")
               ('eat     "eat")
               ('shell   "shell")
               ('eshell  "eshell"))))
    (if name
        (format "*popterm-%s[%s]*" tag name)
      (format "*popterm-%s*" tag))))

(defun popterm--ghostel-create (buffer-name)
  "Create a fresh Ghostel terminal buffer named BUFFER-NAME.

Ghostel's public `ghostel' command always switches to the new buffer and
uses an internal counter to derive the final name.  Popterm wraps that
entry point in `save-window-excursion' to preserve the user's layout, then
renames the resulting buffer to BUFFER-NAME so named instances keep the
same naming semantics as the other backends."
  (unless (require 'ghostel nil t)
    (user-error "popterm: Ghostel not installed"))
  (let ((buffer
         (save-window-excursion
           (save-current-buffer
             (let ((ghostel-buffer-name buffer-name))
               (ghostel)
               (current-buffer))))))
    (unless (buffer-live-p buffer)
      (error "Popterm: Ghostel failed to create a buffer"))
    (with-current-buffer buffer
      (unless (string= (buffer-name buffer) buffer-name)
        (rename-buffer buffer-name t)))
    (get-buffer buffer-name)))

(defun popterm--create (name backend)
  "Create a fresh terminal buffer for BACKEND named NAME.
Activates `popterm-mode' and returns the buffer.

`display-buffer-alist' is temporarily overridden to `display-buffer-no-window'
so backends that initialize themselves by displaying a buffer do not pop a
window or disturb the current layout during creation."
  (let* ((display-buffer-alist
          '((".*" (display-buffer-no-window) (allow-no-window . t))))
         (bname (popterm--buffer-name name backend))
         (buf
          (save-current-buffer
            (pcase backend
              ('vterm
               ;; vterm accepts a buffer-name string as its first argument,
               ;; bypassing vterm-buffer-name entirely — no defvar needed.
               (if (fboundp 'vterm)
                   (progn
                     (vterm bname)
                     (get-buffer bname))
                 (user-error "popterm: Vterm not installed")))
              ('ghostel
               (popterm--ghostel-create bname))
              ('eat
               ;; eat-buffer-name is declared special above via (defvar),
               ;; so this let creates a true dynamic binding.
               (if (fboundp 'eat)
                   (let ((eat-buffer-name bname))
                     (eat)
                     (or (get-buffer bname)
                         ;; Rename safety net: eat created *eat* — take it.
                         (when-let ((eb (get-buffer "*eat*")))
                           (with-current-buffer eb
                             (rename-buffer bname t))
                           (get-buffer bname))))
                 (user-error "popterm: Eat not installed")))
              ('shell
               (shell bname))
              ('eshell
               ;; eshell-buffer-name declared special above via (defvar).
               ;; (eshell) without t uses get-buffer-create — idempotent
               ;; and correct since popterm--create only runs when no buffer
               ;; exists yet (popterm--get-or-create guards before this).
               (if (fboundp 'eshell)
                   (let ((eshell-buffer-name bname))
                     (eshell)
                     (get-buffer bname))
                 (user-error "popterm: Eshell not available")))))))
    (unless (buffer-live-p buf)
      (error "Popterm: %s failed to create a buffer named %S — check that the backend is installed and functional"
             backend bname))
    (with-current-buffer buf
      (setq-local popterm--managed-buffer t
                  popterm--buffer-backend backend
                  popterm--buffer-instance-name name)
      (popterm-mode 1))
    buf))

;;; ── Scope / project helpers ───────────────────────────────────────────────────

(defun popterm--buffer-directory (&optional buffer)
  "Return BUFFER's directory as an absolute path string, or nil.
Prefers command `buffer-file-name' and falls back to `default-directory'.  This
helper is defensive for special buffers where `default-directory' may be
nil, such as `*Messages*'."
  (with-current-buffer (or buffer (current-buffer))
    (let ((dir (or (and buffer-file-name
                        (file-name-directory buffer-file-name))
                   default-directory)))
      (when (stringp dir)
        (expand-file-name dir)))))

(defun popterm--project-root ()
  "Return the current project root directory string, or nil.
Uses `project-root' (project.el >= 0.3.0).  If the current buffer has no
usable directory, or the project backend does not implement
`project-root', return nil gracefully."
  ;; `project-roots' was deprecated in project.el 0.3.0 (Emacs 28).
  ;; We require Emacs 28.1, so `project-root' is always available.
  (when-let* ((dir  (popterm--buffer-directory))
              (proj (and (fboundp 'project-current)
                         (ignore-errors (project-current nil dir)))))
    (when (fboundp 'project-root)
      (ignore-errors (project-root proj)))))

(defun popterm--not-in-other-frame (cur-frame buf)
  "Non-nil when BUF is not displayed in a frame other than CUR-FRAME."
  (let ((wins (get-buffer-window-list buf nil t)))
    (not (cl-some (lambda (win)
                    (not (eq (window-frame win) cur-frame)))
                  wins))))

(defun popterm--buffer-list (&optional backend)
  "Return all live popterm buffers for BACKEND, filtered by `popterm-scope'."
  (let* ((b     (or backend popterm-backend))
         (frame (selected-frame))
         (root  (popterm--project-root)))
    (seq-filter
     (lambda (buf)
       (and (buffer-live-p buf)
            (popterm--buffer-p buf b)
            (pcase popterm-scope
              ('nil        t)
              ('dedicated  (popterm--buffer-instance-matches-p buf nil b))
              ('frame      (popterm--not-in-other-frame frame buf))
              ('project    (and root
                                (when-let ((dir (popterm--buffer-directory buf)))
                                  (ignore-errors
                                    (file-in-directory-p dir root))))))))
     (buffer-list))))

(defun popterm--get-or-create (&optional name backend)
  "Return a suitable terminal buffer, creating one only when needed.
Optional NAME selects a named instance; BACKEND overrides `popterm-backend'.
Guards against a killed-but-not-yet-GC'd buffer by verifying
`buffer-live-p' on any exact NAME match before returning it."
  (let* ((b     (or backend popterm-backend))
         (bufs  (popterm--buffer-list b))
         (exact (and name
                     (seq-find (lambda (buf)
                                 (and (buffer-live-p buf)
                                      (popterm--buffer-instance-matches-p buf name b)))
                               (buffer-list)))))
    (cond
     (exact exact)
     (bufs  (car bufs))
     (t     (popterm--create name b)))))

;;; ── Auto-cd / TRAMP-aware ─────────────────────────────────────────────────────

(defun popterm-cd-string (source-buf)
  "Return a shell cd command string for the directory of SOURCE-BUF.
Strips the TRAMP remote prefix so the command is valid on the remote host.
Returns nil when SOURCE-BUF has no usable directory.  Side-effect-free;
part of the public API for custom integrations.

Uses `file-remote-p' for TRAMP-awareness without requiring TRAMP at load time.
For local paths (macOS, Linux, no TRAMP), `file-remote-p' returns nil and the
directory is used as-is."
  (when-let* ((dir (popterm--buffer-directory source-buf)))
    (let* ((remote (file-remote-p dir))
           (local  (if remote (file-remote-p dir 'localname) dir)))
      (when local
        (format "cd %s" (shell-quote-argument (directory-file-name local)))))))

(defun popterm--send-cd (term-buf source-buf)
  "Send a cd command into TERM-BUF based on SOURCE-BUF's directory.
Uses each backend's dedicated send + return API to avoid PTY newline
issues: raw \\n is unreliable over a PTY (zsh/fish expect \\r).
`vterm-send-return' and `eat-self-input' with symbol `return' are the
correct idioms for their respective backends."
  (when-let ((cmd (and (buffer-live-p source-buf)
                       (popterm-cd-string source-buf))))
    (with-current-buffer term-buf
      (cond
       ((and (derived-mode-p 'vterm-mode)
             (fboundp 'vterm-send-string)
             (fboundp 'vterm-send-return))
        (vterm-send-string cmd)
        (vterm-send-return))
       ((and (derived-mode-p 'ghostel-mode)
             (boundp 'ghostel--process)
             ghostel--process
             (process-live-p ghostel--process))
        (process-send-string ghostel--process (concat cmd "\r")))
       ((and (derived-mode-p 'eat-mode)
             (fboundp 'eat-term-send-string)
             (fboundp 'eat-self-input)
             (boundp 'eat-terminal)
             eat-terminal)
        (eat-term-send-string eat-terminal cmd)
        (eat-self-input 1 'return))
       ((derived-mode-p 'comint-mode)
        (goto-char (point-max))
        (insert cmd)
        (comint-send-input))
       ((derived-mode-p 'eshell-mode)
        (goto-char (point-max))
        (insert cmd)
        (with-no-warnings (eshell-send-input)))))))

;;; ── Posframe focus guard ──────────────────────────────────────────────────────

(defun popterm--display-buffer-guard-p (buffer-or-name _action)
  "Non-nil when BUFFER-OR-NAME should not be re-displayed in a normal window.
This guard is only active while a popterm posframe session is active.  It
blocks `display-buffer' from showing popterm buffers in the parent frame,
which prevents packages such as ECA or gptel from splitting a normal
window to display the active posframe buffer."
  (and popterm--display-buffer-guard-active
       (popterm--posframe-visible-p)
       (let ((buf (if (bufferp buffer-or-name)
                      buffer-or-name
                    (get-buffer buffer-or-name))))
         (popterm--buffer-p buf))))

(defun popterm--effective-display-method ()
  "Return the currently active Popterm display method."
  (or popterm--active-display-method popterm-display-method))

(defun popterm--install-display-buffer-guard ()
  "Prevent popterm buffers from being shown in regular windows."
  (unless popterm--display-buffer-guard-active
    (setq popterm--display-buffer-guard-active t)
    (add-to-list 'display-buffer-alist popterm--display-buffer-guard-entry)))

(defun popterm--remove-display-buffer-guard ()
  "Allow popterm buffers to be shown in regular windows again."
  (setq popterm--display-buffer-guard-active nil)
  (setq display-buffer-alist
        (delete popterm--display-buffer-guard-entry display-buffer-alist)))

(defun popterm--posframe-focus-guard ()
  "Reclaim focus for the popterm posframe when it is still visible.
Intended for `after-focus-change-function'.  When the posframe is visible
and the user has inadvertently landed focus on the parent frame (e.g. via
`C-x b', `other-window', or a package that calls `display-buffer'), this
function schedules focus to return to the posframe child frame.

The guard is *not* triggered when:
 • The minibuffer is active (user is mid-completion).
 • Another child frame has focus (e.g. vertico-posframe, corfu-posframe).
 • The inhibit flag is set (we are in the initial show/hide focus-delay).
 • The posframe frame is no longer live/visible."
  (when (and popterm--focus-guard-active
             (not popterm--inhibit-hidehandler)
             (popterm--posframe-visible-p)
             ;; Don't fight with minibuffer interactions.
             (not (active-minibuffer-window))
             ;; Only act when the *parent* frame has focus — not when
             ;; another child frame (vertico-posframe etc.) is focused.
             (not (frame-parameter (selected-frame) 'parent-frame))
             ;; The selected frame is the parent, not the popterm frame.
             (not (eq (selected-frame) popterm--frame)))
    (let ((frame popterm--frame))
      ;; Use a zero-delay timer so we don't fight with the command that
      ;; just transferred focus.  `run-at-time' with 0 runs after the
      ;; current command cycle completes.
      (run-at-time 0 nil
                   (lambda ()
                     (when (and popterm--focus-guard-active
                                (frame-live-p frame)
                                (frame-visible-p frame)
                                (not (active-minibuffer-window)))
                       (select-frame-set-input-focus frame)))))))

(defun popterm--install-focus-guard ()
  "Install the posframe focus guard on `after-focus-change-function'."
  (unless popterm--focus-guard-active
    (setq popterm--focus-guard-active t)
    (add-function :after after-focus-change-function
      #'popterm--posframe-focus-guard)))

(defun popterm--remove-focus-guard ()
  "Remove the posframe focus guard from `after-focus-change-function'."
  (when popterm--focus-guard-active
    (setq popterm--focus-guard-active nil)
    (remove-function after-focus-change-function
                     #'popterm--posframe-focus-guard)))

(defun popterm--cancel-focus-timer ()
  "Cancel the pending posframe focus timer, if any."
  (when (timerp popterm--focus-timer)
    (cancel-timer popterm--focus-timer)
    (setq popterm--focus-timer nil)))

(defun popterm--restore-posframe-buffer-state ()
  "Restore buffer-local state temporarily changed for posframe display."
  (when (buffer-live-p popterm--frame-buffer)
    (with-current-buffer popterm--frame-buffer
      (setq-local mode-line-format popterm--saved-mode-line-format)))
  (setq popterm--saved-mode-line-format nil))

(defun popterm--cleanup-posframe-state ()
  "Remove Popterm's posframe guards and clear its session state."
  (popterm--remove-focus-guard)
  (popterm--remove-display-buffer-guard)
  (popterm--remove-theme-watch)
  (popterm--cancel-focus-timer)
  (popterm--restore-posframe-buffer-state)
  (setq popterm--inhibit-hidehandler nil
        popterm--frame nil
        popterm--frame-buffer nil)
  (when (eq popterm--active-display-method 'posframe)
    (setq popterm--active-display-method nil)))

(defun popterm--refresh-posframe-after-theme-change ()
  "Refresh the active Popterm posframe after theme change settle.

This updates the live child frame's colors in place instead of
re-showing the terminal buffer.  Re-showing a process-backed buffer such
as `vterm' during theme transitions can trigger unwanted `kill-buffer'
prompts."
  (setq popterm--theme-refresh-timer nil)
  (unwind-protect
      (when (and (eq popterm--active-display-method 'posframe)
                 (buffer-live-p popterm--frame-buffer)
                 (frame-live-p popterm--frame))
        (modify-frame-parameters
         popterm--frame
         `((internal-border-width . ,popterm-posframe-border-width)
           (internal-border-color . ,(face-background 'region nil t))
           (background-color . ,(face-background 'default nil t))
           (foreground-color . ,(face-foreground 'default nil t))))
        (redraw-frame popterm--frame))
    (setq popterm--inhibit-hidehandler nil)))

(defun popterm--queue-theme-refresh (&rest _args)
  "Debounce Popterm posframe refresh during theme transitions."
  (when (and popterm--theme-watch-active
             (buffer-live-p popterm--frame-buffer))
    (setq popterm--inhibit-hidehandler t)
    (when (timerp popterm--theme-refresh-timer)
      (cancel-timer popterm--theme-refresh-timer))
    ;; Let theme face updates finish before re-showing the child frame.
    (setq popterm--theme-refresh-timer
          (run-with-timer 0.05 nil
                          #'popterm--refresh-posframe-after-theme-change))))

(defun popterm--install-theme-watch ()
  "Watch theme change while a Popterm posframe is active."
  (unless popterm--theme-watch-active
    (setq popterm--theme-watch-active t)
    (when (boundp 'enable-theme-functions)
      (add-hook 'enable-theme-functions #'popterm--queue-theme-refresh))
    (advice-add 'load-theme :after #'popterm--queue-theme-refresh)
    (advice-add 'disable-theme :after #'popterm--queue-theme-refresh)))

(defun popterm--remove-theme-watch ()
  "Stop watching theme change for Popterm posframes."
  (when popterm--theme-watch-active
    (setq popterm--theme-watch-active nil)
    (when (boundp 'enable-theme-functions)
      (remove-hook 'enable-theme-functions #'popterm--queue-theme-refresh))
    (advice-remove 'load-theme #'popterm--queue-theme-refresh)
    (advice-remove 'disable-theme #'popterm--queue-theme-refresh))
  (when (timerp popterm--theme-refresh-timer)
    (cancel-timer popterm--theme-refresh-timer)
    (setq popterm--theme-refresh-timer nil)))

;;; ── Posframe display ─────────────────────────────────────────────────────────

(defun popterm--posframe-hidehandler (_info)
  "Hidehandler for posframe's idle-timer daemon.
Called by `posframe-hidehandler-daemon-function' every 0.5s with INFO
plist.  Returns non-nil to request posframe hide the child frame.

Modelled after Centaur Emacs's `shell-pop-posframe-hidehandler', this
checks `selected-frame' against a whitelist of frames that should NOT
trigger a hide:

  1. The popterm child frame itself.
  2. Its parent frame (the root/workspace frame).
  3. Any other child frame (has a non-nil `parent-frame' parameter),
     such as vertico-posframe, corfu-posframe, company-posframe, etc.

Additionally, the minibuffer guard prevents hiding during standard
minibuffer completion regardless of completion style.

The `popterm--inhibit-hidehandler' guard prevents spurious hides during
the `popterm-posframe-focus-delay' window immediately after showing,
addressing the Wayland/pgtk async focus race.

Fixes posframe#155 and Centaur Emacs issue #482."
  (let* ((frame popterm--frame)
         (parent (and frame
                      (frame-live-p frame)
                      (frame-parent frame)))
         (should-hide
          (and (not popterm--inhibit-hidehandler)
               (frame-live-p frame)
               (frame-visible-p frame)
               (not (active-minibuffer-window))
               (not (frame-parameter (selected-frame) 'parent-frame))
               (not (memq (selected-frame) (list frame parent))))))
    ;; Only cleanup state if we're actually hiding the frame
    ;; This prevents the hidehandler from clearing state when it decides NOT to hide
    (when should-hide
      (popterm--cleanup-posframe-state))
    should-hide))


(defun popterm--posframe-show (buffer)
  "Show BUFFER in a centered posframe child frame."
  (require 'posframe)
  (let* ((w (max popterm-posframe-min-width
                 (round (* (frame-width) popterm-posframe-width-ratio))))
         (h (round (* (frame-height) popterm-posframe-height-ratio)))
         (frame nil))
    (setq popterm--active-display-method 'posframe
          popterm--frame-buffer buffer)
    (with-current-buffer buffer
      ;; Hide the mode line inside the popup for all backends.  Ghostel keeps
      ;; a normal `mode-line-format', while vterm/eat often appear modeless in
      ;; practice; suppressing it here keeps posframe presentation consistent.
      (setq popterm--saved-mode-line-format mode-line-format)
      (setq-local mode-line-format nil))
    (unwind-protect
        (setq frame
              (with-no-warnings
                (posframe-show
                 buffer
                 :poshandler (or popterm-posframe-poshandler
                                 #'posframe-poshandler-frame-center)
                 :hidehandler #'popterm--posframe-hidehandler
                 :left-fringe 8
                 :right-fringe 8
                 :width w
                 :height h
                 :min-width w
                 :min-height h
                 :internal-border-width popterm-posframe-border-width
                 :internal-border-color (face-background 'region nil t)
                 :background-color (face-background 'default nil t)
                 :foreground-color (face-foreground 'default nil t)
                 :override-parameters '((cursor-type . t))
                 :respect-mode-line nil
                 :accept-focus t)))
      (unless frame
        (popterm--restore-posframe-buffer-state)))
    (setq popterm--frame frame)
    ;; Inhibit the hidehandler during the focus-transfer window.
    ;; Cancel any in-flight timer first: rapid key-repeat within the delay
    ;; window would otherwise spawn multiple timers, and the earliest would
    ;; clear the inhibit flag mid-flight on the next show cycle.
    (setq popterm--inhibit-hidehandler t)
    (popterm--cancel-focus-timer)
    (setq popterm--focus-timer
          (run-with-timer popterm-posframe-focus-delay nil
                          (lambda ()
                            (setq popterm--inhibit-hidehandler nil)
                            (setq popterm--focus-timer nil))))
    (select-frame-set-input-focus popterm--frame)
    (with-current-buffer buffer
      (setq-local cursor-type 'box)
      (goto-char (point-max))
      (when (fboundp 'vterm-reset-cursor-point)
        (vterm-reset-cursor-point)))
    ;; Install guards so that operations landing focus on the parent
    ;; frame return to the posframe, and external `display-buffer' calls
    ;; cannot re-display popterm buffers in regular split windows.
    (popterm--install-display-buffer-guard)
    (popterm--install-focus-guard)
    (popterm--install-theme-watch)))

(defun popterm--posframe-hide ()
  "Hide the posframe via posframe's native API and restore parent focus.
`posframe-hide' takes a buffer (not a frame) and manages the child-frame
lifecycle correctly, avoiding orphaned frames."
  (let ((frame popterm--frame)
        (buffer popterm--frame-buffer))
    ;; If state variables are cleared but a child frame is still visible,
    ;; find it so we can hide it properly
    (unless (and frame buffer)
      (ignore
       (cl-some (lambda (f)
                  (when (and (frame-live-p f)
                             (frame-visible-p f)
                             (frame-parameter f 'parent-frame))
                    (let ((buf (window-buffer (frame-root-window f))))
                      (when (popterm--buffer-p buf)
                        (setq frame f
                              buffer buf)
                        t))))
                (frame-list))))
    ;; Remove guards FIRST so the parent-focus transfer below does not
    ;; re-focus the soon-to-be-hidden posframe or block later normal
    ;; display-buffer calls for popterm buffers.
    (popterm--cleanup-posframe-state)
    (let ((parent (and (frame-live-p frame)
                       (frame-parent frame))))
      (when (buffer-live-p buffer)
        (with-no-warnings
          (posframe-hide buffer)))
      (when parent
        (select-frame-set-input-focus parent)))))

(defun popterm--posframe-visible-p ()
  "Non-nil when the posframe is live and visible."
  (or (and (frame-live-p    popterm--frame)
           (frame-visible-p popterm--frame))
      ;; Fallback: check if any child frame is showing a popterm buffer
      ;; This handles cases where popterm--frame was cleared but the frame is still visible
      (cl-some (lambda (f)
                 (and (frame-live-p f)
                      (frame-visible-p f)
                      (frame-parameter f 'parent-frame)
                      (let ((buf (window-buffer (frame-root-window f))))
                        (popterm--buffer-p buf))))
               (frame-list))))

;;; ── Window display ────────────────────────────────────────────────────────────

(defun popterm--window-show (buffer)
  "Show BUFFER in a split window on `popterm-window-side'."
  (let* ((size (round (* (if (memq popterm-window-side '(below above))
                             (frame-height)
                           (frame-width))
                         popterm-window-height-ratio)))
         (win  (split-window (frame-root-window) (- size) popterm-window-side)))
    (setq popterm--active-display-method 'window)
    (set-window-buffer win buffer)
    (setq popterm--window win)
    (select-window win)
    (with-current-buffer buffer (goto-char (point-max)))))

(defun popterm--window-hide ()
  "Delete the split window and dereference it."
  (when (window-live-p popterm--window)
    (delete-window popterm--window))
  (setq popterm--window nil)
  (when (eq popterm--active-display-method 'window)
    (setq popterm--active-display-method nil)))

(defun popterm--window-visible-p ()
  "Non-nil when the split window is live."
  (window-live-p popterm--window))

;;; ── Safe buffer eject (fullscreen only) ──────────────────────────────────────

(defun popterm--eject-to-source ()
  "Switch to `popterm--source-buffer', or eject safely when it is dead.
Only called from `popterm--hide' while `current-buffer' is still the
terminal, so `bury-buffer' correctly targets the terminal buffer."
  (if (buffer-live-p popterm--source-buffer)
      (switch-to-buffer popterm--source-buffer)
    (bury-buffer)
    (previous-buffer)))

;;; ── Core toggle logic ─────────────────────────────────────────────────────────

(defun popterm--visible-p ()
  "Non-nil when any popterm display is currently active.
For fullscreen, `memq' checks the entire buffer list so that navigating
between named instances and then pressing the toggle key hides correctly."
  (let* ((method (popterm--effective-display-method))
         (visible
          (pcase method
            ('posframe   (popterm--posframe-visible-p))
            ('window     (popterm--window-visible-p))
            ('fullscreen (memq (current-buffer) (popterm--buffer-list))))))
    (unless visible
      (setq popterm--active-display-method nil))
    visible))

(defun popterm--hide ()
  "Hide the active popterm display and clean up cross-mode strays."
  (pcase (popterm--effective-display-method)
    ('posframe
     (popterm--posframe-hide)
     (popterm--window-hide))
    ('window
     (popterm--window-hide))
    ('fullscreen
     (popterm--window-hide)
     (popterm--eject-to-source)
     (setq popterm--active-display-method nil))))

(defun popterm--show (buffer)
  "Show BUFFER using the configured display method."
  (setq popterm--active-display-method popterm-display-method)
  (when-let ((w (get-buffer-window buffer)))
    (unless (eq popterm-display-method 'window)
      (delete-window w)))
  (pcase popterm-display-method
    ('posframe   (popterm--posframe-show buffer))
    ('window     (popterm--window-show   buffer))
    ('fullscreen (switch-to-buffer buffer)
                 (goto-char (point-max)))))

;;; ── Public commands ───────────────────────────────────────────────────────────

;;;###autoload
(defun popterm-toggle (&optional name backend)
  "Toggle the terminal popup.
Optional NAME selects a named instance; optional BACKEND overrides
`popterm-backend'.  Visibility is checked BEFORE any cleanup to prevent
the window-mode toggle from recreating rather than hiding the terminal."
  (interactive)
  (if (popterm--visible-p)
      (popterm--hide)
    (unless (eq popterm-display-method 'window)
      (popterm--window-hide))
    (setq popterm--source-buffer (current-buffer))
    (let ((buf (popterm--get-or-create name (or backend popterm-backend))))
      (popterm--show buf)
      (when popterm-auto-cd
        (popterm--send-cd buf popterm--source-buffer)))))

;;;###autoload
(defun popterm-toggle-cd (&optional name backend)
  "Like `popterm-toggle' but always cd to the source buffer's directory.
Optional NAME selects a named instance; BACKEND overrides `popterm-backend'.
Respects `popterm-cd-auto-create-buffer': a nil value prevents creating
a new buffer when none exists in scope."
  (interactive)
  (let ((popterm-auto-cd t))
    (if (or (popterm--visible-p)
            (popterm--buffer-list backend)
            popterm-cd-auto-create-buffer)
        (popterm-toggle name backend)
      (message "Popterm: No terminal in scope (set popterm-cd-auto-create-buffer to t to allow creation)"))))

;;;###autoload
(defun popterm-window-toggle ()
  "Toggle terminal in a window split, ignoring `popterm-display-method'."
  (interactive)
  (let ((popterm-display-method 'window))
    (popterm-toggle)))

;;;###autoload
(defun popterm-posframe-toggle ()
  "Toggle terminal in a posframe, ignoring `popterm-display-method'."
  (interactive)
  (let ((popterm-display-method 'posframe))
    (popterm-toggle)))

;;;###autoload
(defun popterm-toggle-named (name)
  "Toggle a named terminal instance, prompting for NAME."
  (interactive "sTerminal name: ")
  (popterm-toggle name popterm-backend))

;;; ── Backend convenience wrappers ─────────────────────────────────────────────

;;;###autoload
(defun popterm-vterm (&optional name)
  "Toggle a vterm popup for optional instance NAME.
Ignores `popterm-backend' setting."
  (interactive)
  (popterm-toggle name 'vterm))

;;;###autoload
(defun popterm-ghostel (&optional name)
  "Toggle a Ghostel popup for optional instance NAME.
Ignores `popterm-backend' setting."
  (interactive)
  (popterm-toggle name 'ghostel))

;;;###autoload
(defun popterm-eat (&optional name)
  "Toggle an eat popup for optional instance NAME.
Ignores `popterm-backend' setting."
  (interactive)
  (popterm-toggle name 'eat))

;;;###autoload
(defun popterm-shell (&optional name)
  "Toggle a shell popup for optional instance NAME.
Ignores `popterm-backend' setting."
  (interactive) (popterm-toggle name 'shell))

;;;###autoload
(defun popterm-eshell (&optional name)
  "Toggle an eshell popup for optional instance NAME.
Ignores `popterm-backend' setting."
  (interactive) (popterm-toggle name 'eshell))

;;; ── Buffer navigation ─────────────────────────────────────────────────────────

;;;###autoload
(defun popterm-next (&optional backend)
  "Cycle forward through popterm buffers for BACKEND, respecting `popterm-scope'."
  (interactive)
  (let* ((bufs (popterm--buffer-list backend))
         (cur  (current-buffer))
         (idx  (cl-position cur bufs :test #'eq))
         (n    (length bufs)))
    (when bufs
      (switch-to-buffer (nth (if idx (mod (1+ idx) n) 0) bufs)))))

;;;###autoload
(defun popterm-prev (&optional backend)
  "Cycle backward through popterm buffers for BACKEND, respecting `popterm-scope'."
  (interactive)
  (let* ((bufs (popterm--buffer-list backend))
         (cur  (current-buffer))
         (idx  (cl-position cur bufs :test #'eq))
         (n    (length bufs)))
    (when bufs
      (switch-to-buffer (nth (if idx (mod (1- idx) n) (1- n)) bufs)))))

;;;###autoload
(defun popterm-find ()
  "Select any popterm buffer via `completing-read'.
Searches all buffers regardless of `popterm-scope' — global navigator.
Use `popterm-next'/`popterm-prev' for scope-aware cycling."
  (interactive)
  (let ((bufs (seq-filter #'popterm--buffer-p (buffer-list))))
    (if bufs
        (switch-to-buffer
         (completing-read "Popterm: " (mapcar #'buffer-name bufs) nil t))
      (message "Popterm: No terminal buffers open"))))

;;; ── Return to source buffer ───────────────────────────────────────────────────

;;;###autoload
(defun popterm-return ()
  "Return to the buffer active before the last `popterm-toggle'.

For fullscreen: `popterm--hide' already called `popterm--eject-to-source'
while focus was on the terminal — `bury-buffer' safely targeted the terminal.

For posframe/window: `popterm--hide' has transferred focus back to the
parent frame before this code runs, so `current-buffer' is already the
user's code file.  Calling `bury-buffer' here would bury that file.
When the source is dead in these modes the popup is already closed and
the user is safely in their workspace; a message is sufficient."
  (interactive)
  (let ((method (popterm--effective-display-method)))
    (popterm--hide)
    (pcase method
      ((or 'posframe 'window)
       (if (buffer-live-p popterm--source-buffer)
           (if-let ((w (get-buffer-window popterm--source-buffer)))
               (select-window w)
             (switch-to-buffer popterm--source-buffer))
         (message "Popterm: Source buffer was killed"))))))

;;; ── Cleanup on buffer kill ────────────────────────────────────────────────────

(defun popterm--on-buffer-kill ()
  "Clean up frame/window ONLY when the actively displayed buffer is killed.
Prevents collateral damage when a background process kills a terminal
that is not currently shown."
  (when (popterm--buffer-p (current-buffer))
    (when (eq (current-buffer) popterm--frame-buffer)
      (popterm--posframe-hide))
    (when (and (window-live-p popterm--window)
               (eq (current-buffer) (window-buffer popterm--window)))
      (popterm--window-hide))))

;;; ── Global opt-in integration ───────────────────────────────────────────────

;;;###autoload
(define-minor-mode popterm-global-mode
  "Enable global Popterm integration.

When enabled, Popterm installs lightweight hooks needed for best behavior:

- Add `popterm--vterm-setup' to `vterm-mode-hook' so Popterm keys work in vterm.
- Add `popterm--on-buffer-kill' to `kill-buffer-hook' so Popterm cleans up its
  posframe/window if the active terminal buffer is killed.

Posframe auto-hide is handled by posframe's own idle-timer daemon via the
`:hidehandler' parameter passed to `posframe-show' — no global focus-change
hook is needed.

This mode is intentionally opt-in to avoid global side effects merely from
loading the library (a common MELPA review requirement)."
  :global t
  :group 'popterm
  (if popterm-global-mode
      (progn
        (add-hook 'vterm-mode-hook #'popterm--vterm-setup)
        (add-hook 'kill-buffer-hook #'popterm--on-buffer-kill))
    (remove-hook 'vterm-mode-hook #'popterm--vterm-setup)
    (remove-hook 'kill-buffer-hook #'popterm--on-buffer-kill)))

(provide 'popterm)
;;; popterm.el ends here
