;;; popterm.el --- Posframe terminal toggler with smart backends  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Chetan Koneru <kchetan.hadoop@gmail.com>
;; Version: 0.9.1
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
;;   vterm / eat / shell / eshell
;;
;; Quick start:
;;   (use-package popterm
;;     :bind ((\"C-`\"  . popterm-toggle)
;;            (\"C-~\"  . popterm-toggle-cd)
;;            ([f9]   . popterm-window-toggle))
;;     :config
;;     (setq popterm-backend        'vterm
;;           popterm-display-method 'posframe
;;           popterm-scope          'project
;;           popterm-auto-cd        t))

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tramp)

;;; ── Customization ─────────────────────────────────────────────────────────────

(defgroup popterm nil
  "Posframe-based terminal toggler with smart backends."
  :group 'terminals
  :prefix "popterm-")

(defcustom popterm-backend 'vterm
  "Terminal backend to use.
One of `vterm', `eat', `shell', or `eshell'."
  :type '(choice (const :tag "vterm"  vterm)
                 (const :tag "eat"    eat)
                 (const :tag "shell"  shell)
                 (const :tag "eshell" eshell))
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

(defvar popterm--frame         nil "Active posframe child frame.")
(defvar popterm--frame-buffer  nil "Buffer displayed in the active posframe.")
(defvar popterm--window        nil "Active window-split window.")
(defvar popterm--source-buffer nil "Buffer active before the last toggle.")
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

\[popterm-next]   — cycle forward  through popterm buffers
\[popterm-prev]   — cycle backward through popterm buffers
\[popterm-return] — return to source buffer

For vterm, these keys are registered in `vterm-keymap-exceptions' so
vterm's character-mode input handler passes them to Emacs.

\{popterm-term-map}"
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
before `vterm-keymap-exceptions' is referenced.  `cl-pushnew' is used
instead of `add-to-list' for idempotent per-buffer safety."
  (dolist (key popterm--vterm-passthrough-keys)
    (cl-pushnew key vterm-keymap-exceptions :test #'equal)))

(add-hook 'vterm-mode-hook #'popterm--vterm-setup)

;;; ── Backend helpers ───────────────────────────────────────────────────────────

(defsubst popterm--mode (backend)
  "Return the major-mode symbol for BACKEND."
  (pcase backend
    ('vterm  'vterm-mode)
    ('eat    'eat-mode)
    ('shell  'shell-mode)
    ('eshell 'eshell-mode)))

(defun popterm--buffer-name (&optional name backend)
  "Return canonical buffer name for BACKEND with optional instance NAME."
  (let ((tag (pcase (or backend popterm-backend)
               ('vterm  "vterm")
               ('eat    "eat")
               ('shell  "shell")
               ('eshell "eshell"))))
    (if name
        (format "*popterm-%s[%s]*" tag name)
      (format "*popterm-%s*" tag))))

(defun popterm--create (name backend)
  "Create a fresh terminal buffer for BACKEND named NAME.
Activates `popterm-mode' and returns the buffer.

`display-buffer-alist' is temporarily overridden to `display-buffer-no-window'
so vterm/eat initialize their PTY with correct frame dimensions without
popping a window or disturbing the current layout."
  (let* ((display-buffer-alist
          '((".*" (display-buffer-no-window) (allow-no-window . t))))
         (buf
          (save-current-buffer
            (pcase backend
              ('vterm
               (if (fboundp 'vterm)
                   (let ((vterm-buffer-name (popterm--buffer-name name backend)))
                     (vterm))
                 (user-error "popterm: Vterm not installed")))
              ('eat
               ;; Mirror the vterm pattern: bind eat-buffer-name so eat
               ;; creates the buffer under our name rather than *eat*.
               ;; Do NOT pre-create the buffer — eat must own its buffer
               ;; from the start so the PTY process is attached correctly.
               ;; Explicitly fetch the buffer after calling (eat) rather than
               ;; relying on its return value: interactive emulator entry
               ;; points may return a window object or nil, not a buffer.
               (if (fboundp 'eat)
                   (let ((eat-buffer-name (popterm--buffer-name name backend)))
                     (eat)
                     (get-buffer eat-buffer-name))
                 (user-error "popterm: Eat not installed")))
              ('shell
               (shell (popterm--buffer-name name backend)))
              ('eshell
               ;; eshell t creates a new buffer; fetch by name for the same
               ;; reason as eat — (eshell) return value is not guaranteed.
               (if (fboundp 'eshell)
                   (let ((eshell-buffer-name
                          (popterm--buffer-name name backend)))
                     (eshell t)
                     (get-buffer eshell-buffer-name))
                 (user-error "popterm: Eshell not available")))))))
    (with-current-buffer buf
      (popterm-mode 1))
    buf))

;;; ── Scope / project helpers ───────────────────────────────────────────────────

(defun popterm--project-root ()
  "Return the current project root directory string, or nil.
Uses `project-root' (project.el >= 0.3.0).  If the project backend
does not implement `project-root', returns nil gracefully."
  ;; `project-roots' was deprecated in project.el 0.3.0 (Emacs 28).
  ;; We require Emacs 28.1, so `project-root' is always available.
  (when-let* ((proj (and (fboundp 'project-current)
                         (project-current))))
    (when (fboundp 'project-root)
      (project-root proj))))

(defun popterm--not-in-other-frame (cur-frame buf)
  "Non-nil when BUF is not displayed in a frame other than CUR-FRAME."
  (let ((win (get-buffer-window buf t)))
    (or (null win) (eq (window-frame win) cur-frame))))

(defun popterm--buffer-list (&optional backend)
  "Return all live popterm buffers for BACKEND, filtered by `popterm-scope'."
  (let* ((b      (or backend popterm-backend))
         (mode   (popterm--mode b))
         (frame  (selected-frame))
         (root   (popterm--project-root))
         (prefix (format "*popterm-%s" (symbol-name b))))
    (seq-filter
     (lambda (buf)
       (and (buffer-live-p buf)
            (string-prefix-p prefix (buffer-name buf))
            (with-current-buffer buf (derived-mode-p mode))
            (pcase popterm-scope
              ('nil        t)
              ('dedicated  (string= (buffer-name buf)
                                    (popterm--buffer-name nil b)))
              ('frame      (popterm--not-in-other-frame frame buf))
              ('project    (and root
                                (with-current-buffer buf
                                  (string-prefix-p
                                   root
                                   (expand-file-name default-directory))))))))
     (buffer-list))))

(defun popterm--get-or-create (&optional name backend)
  "Return a suitable terminal buffer, creating one only when needed.
Optional NAME selects a named instance; BACKEND overrides `popterm-backend'.
Guards against a killed-but-not-yet-GC'd buffer by verifying
`buffer-live-p' on any exact NAME match before returning it."
  (let* ((b     (or backend popterm-backend))
         (bufs  (popterm--buffer-list b))
         (exact (let ((buf (and name
                                (get-buffer (popterm--buffer-name name b)))))
                  (and buf (buffer-live-p buf) buf))))
    (cond
     (exact exact)
     (bufs  (car bufs))
     (t     (popterm--create name b)))))

;;; ── Auto-cd / TRAMP-aware ─────────────────────────────────────────────────────

(defun popterm-cd-string (source-buf)
  "Return a shell cd command string for the directory of SOURCE-BUF.
Strips the TRAMP remote prefix so the command is valid on the remote host.
Side-effect-free; part of the public API for custom integrations."
  (with-current-buffer source-buf
    (let* ((dir    (or (and buffer-file-name
                            (file-name-directory buffer-file-name))
                       default-directory))
           (remote (file-remote-p dir))
           (local  (if remote (file-remote-p dir 'localname) dir)))
      (format "cd %s" (shell-quote-argument (directory-file-name local))))))

(defun popterm--send-cd (term-buf source-buf)
  "Send a cd command into TERM-BUF based on SOURCE-BUF's directory.
Uses each backend's dedicated send + return API to avoid PTY newline
issues: raw \\n is unreliable over a PTY (zsh/fish expect \\r).
`vterm-send-return' and `eat-self-input' with symbol `return' are the
correct idioms for their respective backends."
  (when (buffer-live-p source-buf)
    (let ((cmd (popterm-cd-string source-buf)))
      (with-current-buffer term-buf
        (cond
         ((and (derived-mode-p 'vterm-mode)
               (fboundp 'vterm-send-string)
               (fboundp 'vterm-send-return))
          (vterm-send-string cmd)
          (vterm-send-return))
         ((and (derived-mode-p 'eat-mode)
               (fboundp 'eat-term-send-string)
               (fboundp 'eat-self-input)
               (bound-and-true-p eat-terminal))
          (eat-term-send-string eat-terminal cmd)
          (eat-self-input 1 'return))
         ((derived-mode-p 'comint-mode)
          (goto-char (point-max))
          (insert cmd)
          (comint-send-input))
         ((derived-mode-p 'eshell-mode)
          (goto-char (point-max))
          (insert cmd)
          (eshell-send-input)))))))

;;; ── Posframe display ─────────────────────────────────────────────────────────

(defun popterm--posframe-focused-p ()
  "Return non-nil when the popterm posframe currently hold compositor focus.
On pgtk (Wayland), `frame-focus-state' is used to query the actual
compositor focus state rather than relying on `selected-frame', which
can lag behind the real state due to async Wayland event dispatch."
  (and (frame-live-p popterm--frame)
       (if (fboundp 'frame-focus-state)
           (frame-focus-state popterm--frame)
         (eq (selected-frame) popterm--frame))))

(defun popterm--after-focus-change ()
  "Hide the posframe when compositor focus leave the child frame.
Attached to `after-focus-change-function' (Emacs 27+), which fires on
real compositor-level focus events — unlike posframe's idle-timer
hidehandler, which polls `selected-frame' and is unreliable on
Wayland/pgtk where focus transfer is asynchronous.
The `popterm--inhibit-hidehandler' guard prevents spurious hides during
the `popterm-posframe-focus-delay' window immediately after a show."
  (when (and (not popterm--inhibit-hidehandler)
             (popterm--posframe-visible-p)
             (not (popterm--posframe-focused-p)))
    (popterm--posframe-hide)))

;; Register once at load time.  The function is a no-op when no posframe
;; is visible, so it is safe to leave permanently attached.
(add-function :after after-focus-change-function #'popterm--after-focus-change)

(defun popterm--posframe-show (buffer)
  "Show BUFFER in a centered posframe child frame."
  (require 'posframe)
  (let* ((w (max popterm-posframe-min-width
                 (round (* (frame-width)  popterm-posframe-width-ratio))))
         (h (round (* (frame-height) popterm-posframe-height-ratio))))
    (setq popterm--frame-buffer buffer)
    (setq popterm--frame
          (posframe-show
           buffer
           :poshandler            (or popterm-posframe-poshandler
                                      #'posframe-poshandler-frame-center)
           :left-fringe           8
           :right-fringe          8
           :width                 w
           :height                h
           :min-width             w
           :min-height            h
           :internal-border-width popterm-posframe-border-width
           :internal-border-color (face-background 'region  nil t)
           :background-color      (face-background 'default nil t)
           :foreground-color      (face-foreground 'default nil t)
           :override-parameters  '((cursor-type . t))
           :respect-mode-line     t
           :accept-focus          t))
    ;; Inhibit the hidehandler during the focus-transfer window.
    ;; Cancel any in-flight timer first: rapid key-repeat within the delay
    ;; window would otherwise spawn multiple timers, and the earliest would
    ;; clear the inhibit flag mid-flight on the next show cycle.
    (setq popterm--inhibit-hidehandler t)
    (when (timerp popterm--focus-timer)
      (cancel-timer popterm--focus-timer))
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
        (vterm-reset-cursor-point)))))

(defun popterm--posframe-hide ()
  "Hide the posframe via posframe's native API and restore parent focus.
`posframe-hide' takes a buffer (not a frame) and manages the child-frame
lifecycle correctly, avoiding orphaned frames."
  (let ((parent (and (frame-live-p popterm--frame)
                     (frame-parent popterm--frame))))
    (when (buffer-live-p popterm--frame-buffer)
      (posframe-hide popterm--frame-buffer))
    (when parent
      (select-frame-set-input-focus parent)))
  (setq popterm--frame        nil
        popterm--frame-buffer nil))

(defun popterm--posframe-visible-p ()
  "Non-nil when the posframe is live and visible."
  (and (frame-live-p    popterm--frame)
       (frame-visible-p popterm--frame)))

;;; ── Window display ────────────────────────────────────────────────────────────

(defun popterm--window-show (buffer)
  "Show BUFFER in a split window on `popterm-window-side'."
  (let* ((size (round (* (if (memq popterm-window-side '(below above))
                             (frame-height)
                           (frame-width))
                         popterm-window-height-ratio)))
         (win  (split-window (frame-root-window) (- size) popterm-window-side)))
    (set-window-buffer win buffer)
    (setq popterm--window win)
    (select-window win)
    (with-current-buffer buffer (goto-char (point-max)))))

(defun popterm--window-hide ()
  "Delete the split window and dereference it."
  (when (window-live-p popterm--window)
    (delete-window popterm--window))
  (setq popterm--window nil))

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
  (pcase popterm-display-method
    ('posframe   (popterm--posframe-visible-p))
    ('window     (popterm--window-visible-p))
    ('fullscreen (memq (current-buffer) (popterm--buffer-list)))))

(defun popterm--hide ()
  "Hide the active popterm display and clean up cross-mode strays."
  (pcase popterm-display-method
    ('posframe
     (popterm--posframe-hide)
     (popterm--window-hide))
    ('window
     (popterm--window-hide))
    ('fullscreen
     (popterm--window-hide)
     (popterm--eject-to-source))))

(defun popterm--show (buffer)
  "Show BUFFER using the configured display method."
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
(defun popterm-vterm  (&optional name)
  "Toggle a vterm popup for optional instance NAME, regardless of `popterm-backend'."
  (interactive) (popterm-toggle name 'vterm))

;;;###autoload
(defun popterm-eat (&optional name)
  "Toggle an eat popup for optional instance NAME, regardless of `popterm-backend'."
  (interactive) (popterm-toggle name 'eat))

;;;###autoload
(defun popterm-shell (&optional name)
  "Toggle a shell popup for optional instance NAME, regardless of `popterm-backend'."
  (interactive) (popterm-toggle name 'shell))

;;;###autoload
(defun popterm-eshell (&optional name)
  "Toggle an eshell popup for optional instance NAME, regardless of `popterm-backend'."
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
  (let ((bufs (seq-filter
               (lambda (b) (string-prefix-p "*popterm-" (buffer-name b)))
               (buffer-list))))
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
  (popterm--hide)
  (pcase popterm-display-method
    ((or 'posframe 'window)
     (if (buffer-live-p popterm--source-buffer)
         (if-let ((w (get-buffer-window popterm--source-buffer)))
             (select-window w)
           (switch-to-buffer popterm--source-buffer))
       (message "Popterm: Source buffer was killed")))))

;;; ── Cleanup on buffer kill ────────────────────────────────────────────────────

(defun popterm--on-buffer-kill ()
  "Clean up frame/window ONLY when the actively displayed buffer is killed.
Prevents collateral damage when a background process kills a terminal
that is not currently shown."
  (when (string-prefix-p "*popterm-" (buffer-name))
    (when (eq (current-buffer) popterm--frame-buffer)
      (popterm--posframe-hide))
    (when (and (window-live-p popterm--window)
               (eq (current-buffer) (window-buffer popterm--window)))
      (popterm--window-hide))))

(add-hook 'kill-buffer-hook #'popterm--on-buffer-kill)

(provide 'popterm)
;;; popterm.el ends here
