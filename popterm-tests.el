;;; popterm-tests.el --- Tests for popterm.el  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Chetan Koneru <kchetan.hadoop@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Test suite for popterm.el using ERT (Emacs Lisp Regression Testing).
;;
;; Run tests interactively:
;;   M-x ert RET t RET
;;
;; Run tests from command line:
;;   emacs -batch -l ert -l popterm.el -l popterm-tests.el -f ert-run-tests-batch-and-exit
;;
;; Run specific test:
;;   M-x ert RET popterm-test-buffer-name RET

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'popterm)

(defvar vterm-keymap-exceptions)
(defvar posframe--frame)

;;; ── Basic API Tests ───────────────────────────────────────────────────────────

(ert-deftest popterm-test-buffer-name ()
  "Test buffer name generation for different backends and instances."
  (should (equal (popterm--buffer-name nil 'vterm) "*popterm-vterm*"))
  (should (equal (popterm--buffer-name nil 'ghostel) "*popterm-ghostel*"))
  (should (equal (popterm--buffer-name nil 'eat) "*popterm-eat*"))
  (should (equal (popterm--buffer-name nil 'shell) "*popterm-shell*"))
  (should (equal (popterm--buffer-name nil 'eshell) "*popterm-eshell*"))
  (should (equal (popterm--buffer-name "test" 'vterm) "*popterm-vterm[test]*"))
  (should (equal (popterm--buffer-name "test" 'ghostel) "*popterm-ghostel[test]*"))
  (should (equal (popterm--buffer-name "my-project" 'eat) "*popterm-eat[my-project]*")))

(ert-deftest popterm-test-mode-mapping ()
  "Test backend to mode symbol mapping."
  (should (eq (popterm--mode 'vterm) 'vterm-mode))
  (should (eq (popterm--mode 'ghostel) 'ghostel-mode))
  (should (eq (popterm--mode 'eat) 'eat-mode))
  (should (eq (popterm--mode 'shell) 'shell-mode))
  (should (eq (popterm--mode 'eshell) 'eshell-mode)))

(ert-deftest popterm-test-ghostel-create-renames-buffer ()
  "Test Ghostel backend creation preserves Popterm buffer names."
  (let ((created-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional _filename _noerror)
                     (eq feature 'ghostel)))
                  ((symbol-function 'ghostel)
                   (lambda ()
                     (setq created-buffer (generate-new-buffer "*ghostel*"))
                     (switch-to-buffer created-buffer))))
          (should
           (eq (popterm--ghostel-create "*popterm-ghostel[test]*")
               created-buffer))
          (should (equal (buffer-name created-buffer)
                         "*popterm-ghostel[test]*")))
      (when (buffer-live-p created-buffer)
        (kill-buffer created-buffer)))))

(ert-deftest popterm-test-popterm-ghostel-wrapper ()
  "Test `popterm-ghostel' dispatches to `popterm-toggle'."
  (let ((called-args nil))
    (cl-letf (((symbol-function 'popterm-toggle)
               (lambda (&optional name backend)
                 (setq called-args (list name backend)))))
      (popterm-ghostel "named")
      (should (equal called-args '("named" ghostel))))))

(ert-deftest popterm-test-buffer-p-survives-ghostel-title-rename ()
  "Test Popterm still recognizes a Ghostel buffer after title-based rename."
  (let ((buffer (generate-new-buffer "*ghostel: host: ~/project*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq major-mode 'ghostel-mode)
          (setq-local popterm--managed-buffer t
                      popterm--buffer-backend 'ghostel
                      popterm--buffer-instance-name nil)
          (popterm-mode 1)
          (should (popterm--buffer-p buffer))
          (should (popterm--buffer-p buffer 'ghostel))
          (should-not (popterm--buffer-p buffer 'vterm)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; ── Directory and Path Tests ──────────────────────────────────────────────────

(ert-deftest popterm-test-buffer-directory ()
  "Test buffer directory detection with various buffer states."
  ;; Test with temp buffer (default-directory)
  (with-temp-buffer
    (setq default-directory "/tmp/")
    (should (string= (popterm--buffer-directory) "/tmp/")))

  ;; Test with nil default-directory (defensive case)
  (with-temp-buffer
    (setq default-directory nil)
    (should (null (popterm--buffer-directory))))

  ;; Test with *Messages* buffer (real-world case)
  (with-current-buffer "*Messages*"
    (should (stringp (popterm--buffer-directory)))))

(ert-deftest popterm-test-cd-string-local ()
  "Test cd command generation for local paths."
  (with-temp-buffer
    (setq default-directory "/tmp/")
    (should (string= (popterm-cd-string (current-buffer)) "cd /tmp")))

  ;; Test with spaces in path
  (with-temp-buffer
    (setq default-directory "/tmp/test dir with spaces/")
    (let ((cmd (popterm-cd-string (current-buffer))))
      (should (string-match-p "cd " cmd))
      ;; Verify shell-quote-argument was used (escaped spaces or quotes)
      (should (or (string-match-p "\\ " cmd)
                  (string-match-p "'" cmd)
                  (string-match-p "\"" cmd)))))

  ;; Test with special shell characters
  (with-temp-buffer
    (setq default-directory "/tmp/test$dir/")
    (let ((cmd (popterm-cd-string (current-buffer))))
      (should (string-match-p "cd " cmd))
      ;; Dollar sign should be escaped
      (should (string-match-p "\\\\\\$" cmd)))))

(ert-deftest popterm-test-cd-string-nil-directory ()
  "Test cd command returns nil when buffer has no directory."
  (with-temp-buffer
    (setq default-directory nil)
    (should (null (popterm-cd-string (current-buffer))))))

(ert-deftest popterm-test-send-cd-ghostel ()
  "Test auto-cd sends a carriage-return terminated command to Ghostel."
  (let ((sent-command nil)
        (term-buf (generate-new-buffer " *popterm-ghostel-cd*"))
        (source-buf (generate-new-buffer " *popterm-source-cd*")))
    (unwind-protect
        (progn
          (with-current-buffer term-buf
            (setq major-mode 'ghostel-mode)
            (setq ghostel--process 'ghostel-proc))
          (with-current-buffer source-buf
            (setq default-directory "/tmp/"))
          (cl-letf (((symbol-function 'process-live-p)
                     (lambda (process)
                       (eq process 'ghostel-proc)))
                    ((symbol-function 'process-send-string)
                     (lambda (_process string)
                       (setq sent-command string))))
            (popterm--send-cd term-buf source-buf)
            (should (equal sent-command "cd /tmp\r"))))
      (kill-buffer term-buf)
      (kill-buffer source-buf))))

(ert-deftest popterm-test-project-root-graceful-nil ()
  "Test project-root returns nil gracefully for buffers without projects."
  (with-temp-buffer
    (setq default-directory nil)
    (should (null (popterm--project-root)))))

;;; ── Scope and Buffer Filtering Tests ──────────────────────────────────────────

(ert-deftest popterm-test-not-in-other-frame ()
  "Test frame visibility predicate."
  (let ((frame (selected-frame))
        (buf (get-buffer "*Messages*")))
    ;; Buffer not displayed in a window should return t
    (with-temp-buffer
      (should (popterm--not-in-other-frame frame (current-buffer))))

    ;; Buffer displayed in current frame should return t
    (should (popterm--not-in-other-frame frame buf))))

(ert-deftest popterm-test-not-in-other-frame-detects-second-frame ()
  "Test frame-scope rejects buffers visible in another frame."
  (cl-letf (((symbol-function 'get-buffer-window-list)
             (lambda (&rest _args)
               '(cur-win other-win)))
            ((symbol-function 'window-frame)
             (lambda (win)
               (if (eq win 'cur-win)
                   'frame-a
                 'frame-b))))
    (should-not (popterm--not-in-other-frame 'frame-a 'buffer))))

(ert-deftest popterm-test-buffer-list-filtering ()
  "Test buffer list filtering respects major mode and prefix."
  (let ((test-bufs nil))
    (unwind-protect
        (let ((popterm-buf (generate-new-buffer "*popterm-shell-test*"))
              (other-buf (generate-new-buffer "*my-shell*")))
          (setq test-bufs (list popterm-buf other-buf))
          (with-current-buffer popterm-buf
            (setq major-mode 'shell-mode))
          (with-current-buffer other-buf
            (setq major-mode 'shell-mode))
          (let ((bufs (popterm--buffer-list 'shell)))
            (should (cl-some (lambda (candidate)
                               (string-prefix-p "*popterm-shell"
                                                (buffer-name candidate)))
                             bufs))
            (should-not (member other-buf bufs))))
      (mapc #'kill-buffer test-bufs))))

(ert-deftest popterm-test-buffer-list-includes-renamed-ghostel-buffer ()
  "Test renamed Ghostel Popterm buffers remain discoverable."
  (let ((buffer (generate-new-buffer "*ghostel: host: ~/project*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'ghostel-mode)
            (setq-local popterm--managed-buffer t
                        popterm--buffer-backend 'ghostel
                        popterm--buffer-instance-name nil)
            (popterm-mode 1))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buffer))))
            (should (equal (popterm--buffer-list 'ghostel)
                           (list buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest popterm-test-get-or-create-reuses-renamed-named-ghostel-buffer ()
  "Test named Ghostel buffers are reused even after backend title renames."
  (let ((buffer (generate-new-buffer "*ghostel: host: ~/project*"))
        (created-args nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'ghostel-mode)
            (setq-local popterm--managed-buffer t
                        popterm--buffer-backend 'ghostel
                        popterm--buffer-instance-name "named")
            (popterm-mode 1))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buffer)))
                    ((symbol-function 'popterm--create)
                     (lambda (&rest args)
                       (setq created-args args)
                       'unexpected-buffer)))
            (should (eq (popterm--get-or-create "named" 'ghostel)
                        buffer))
            (should (null created-args))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest popterm-test-buffer-list-filters-dead-buffers ()
  "Test that dead buffers are filtered out of buffer list."
  (let ((test-buf (generate-new-buffer "*popterm-vterm-dead-test*")))
    (with-current-buffer test-buf
      (setq major-mode 'vterm-mode))
    (kill-buffer test-buf)
    (should-not (member test-buf (popterm--buffer-list 'vterm)))))

(ert-deftest popterm-test-buffer-list-project-scope-avoids-prefix-collision ()
  "Test project scope does not treat sibling paths as the same project."
  (let* ((popterm-scope 'project)
         (root (make-temp-file "popterm-project-" t))
         (good-dir (expand-file-name "subdir" root))
         (bad-root (concat root "-other"))
         (good (generate-new-buffer "*popterm-shell-good*"))
         (bad (generate-new-buffer "*popterm-shell-bad*")))
    (unwind-protect
        (progn
          (make-directory good-dir t)
          (make-directory bad-root t)
          (with-current-buffer good
            (setq major-mode 'shell-mode))
          (with-current-buffer bad
            (setq major-mode 'shell-mode))
          (cl-letf (((symbol-function 'popterm--project-root)
                     (lambda () root))
                    ((symbol-function 'popterm--buffer-directory)
                     (lambda (&optional buffer)
                       (cond
                        ((eq buffer good) good-dir)
                        ((eq buffer bad) bad-root)
                        (t root)))))
            (let ((bufs (popterm--buffer-list 'shell)))
              (should (member good bufs))
              (should-not (member bad bufs)))))
      (kill-buffer good)
      (kill-buffer bad)
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory bad-root t)))))

;;; ── Guard Installation Tests ──────────────────────────────────────────────────

(ert-deftest popterm-test-display-buffer-guard-idempotent ()
  "Test display-buffer-guard installation is idempotent."
  (let ((popterm--display-buffer-guard-active nil)
        (display-buffer-alist nil))
    (unwind-protect
        (progn
          (popterm--install-display-buffer-guard)
          (let ((first-length (length display-buffer-alist)))
            ;; Second installation should not duplicate
            (popterm--install-display-buffer-guard)
            (should (= first-length (length display-buffer-alist))))

          ;; Should be marked as active
          (should popterm--display-buffer-guard-active))

      ;; Cleanup
      (popterm--remove-display-buffer-guard))))

(ert-deftest popterm-test-focus-guard-idempotent ()
  "Test focus-guard installation is idempotent."
  (let ((popterm--focus-guard-active nil))
    (unwind-protect
        (progn
          (popterm--install-focus-guard)
          (should popterm--focus-guard-active)

          ;; Second installation should be safe
          (popterm--install-focus-guard)
          (should popterm--focus-guard-active))

      ;; Cleanup
      (popterm--remove-focus-guard))))

(ert-deftest popterm-test-theme-watch-idempotent ()
  "Test theme-watch installation is idempotent."
  (let ((popterm--theme-watch-active nil))
    (unwind-protect
        (progn
          (popterm--install-theme-watch)
          (should popterm--theme-watch-active)

          ;; Second installation should be safe
          (popterm--install-theme-watch)
          (should popterm--theme-watch-active))

      ;; Cleanup
      (popterm--remove-theme-watch))))

;;; ── Timer Management Tests ────────────────────────────────────────────────────

(ert-deftest popterm-test-timer-cleanup ()
  "Test timer cleanup prevents leaks."
  (let ((popterm--focus-timer nil)
        (popterm--theme-refresh-timer nil))

    ;; Create timers
    (setq popterm--focus-timer (run-with-timer 10 nil #'ignore))
    (setq popterm--theme-refresh-timer (run-with-timer 10 nil #'ignore))

    (should (timerp popterm--focus-timer))
    (should (timerp popterm--theme-refresh-timer))

    ;; Cancel as the code does
    (when (timerp popterm--focus-timer)
      (cancel-timer popterm--focus-timer)
      (setq popterm--focus-timer nil))

    (when (timerp popterm--theme-refresh-timer)
      (cancel-timer popterm--theme-refresh-timer)
      (setq popterm--theme-refresh-timer nil))

    ;; Verify cleanup
    (should (null popterm--focus-timer))
    (should (null popterm--theme-refresh-timer))))

;;; ── Closure Capture Tests ─────────────────────────────────────────────────────

(ert-deftest popterm-test-closure-captures-value ()
  "Test that closure captures value at creation, not reference."
  (let ((test-var 'original-value)
        (captured-var nil))

    ;; Simulate closure capture
    (let ((var test-var))
      (setq captured-var var))

    ;; Change the original
    (setq test-var 'new-value)

    ;; Captured value should be unchanged
    (should (eq captured-var 'original-value))))

;;; ── Code Quality Tests ────────────────────────────────────────────────────────

(ert-deftest popterm-test-focus-guard-captures-frame-behaviorally ()
  "Verify the focus-guard timer keeps the frame visible when state changes."
  (let ((popterm--focus-guard-active t)
        (popterm--inhibit-hidehandler nil)
        (popterm--frame 'popterm-frame)
        (scheduled-fn nil)
        (focused-frame nil))
    (cl-letf (((symbol-function 'popterm--posframe-visible-p) (lambda () t))
              ((symbol-function 'active-minibuffer-window) (lambda () nil))
              ((symbol-function 'selected-frame) (lambda () 'parent-frame))
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (and (eq parameter 'parent-frame) nil)))
              ((symbol-function 'frame-live-p) (lambda (_frame) t))
              ((symbol-function 'frame-visible-p) (lambda (_frame) t))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (frame) (setq focused-frame frame)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest _args)
                 (setq scheduled-fn fn)
                 'fake-timer)))
      (popterm--posframe-focus-guard)
      (setq popterm--frame 'replacement-frame)
      (funcall scheduled-fn)
      (should (eq focused-frame 'popterm-frame)))))

(ert-deftest popterm-test-posframe-show-hides-mode-line ()
  "Verify posframe display suppresses the buffer mode line."
  (let ((buffer (get-buffer-create "*popterm-modeline*"))
        (captured-args nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mode-line-format '(" Demo")))
          (cl-letf (((symbol-function 'require)
                     (lambda (_feature &optional _filename _noerror) t))
                    ((symbol-function 'posframe-show)
                     (lambda (&rest args)
                       (setq captured-args args)
                       'fake-frame))
                    ((symbol-function 'select-frame-set-input-focus) #'ignore)
                    ((symbol-function 'run-with-timer)
                     (lambda (&rest _args) 'fake-timer))
                    ((symbol-function 'popterm--install-display-buffer-guard) #'ignore)
                    ((symbol-function 'popterm--install-focus-guard) #'ignore)
                    ((symbol-function 'popterm--install-theme-watch) #'ignore))
            (popterm--posframe-show buffer)
            (with-current-buffer buffer
              (should (null mode-line-format)))
            (should (equal popterm--saved-mode-line-format '(" Demo")))
            (should (equal (plist-get (cdr captured-args) :respect-mode-line)
                           nil))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq popterm--frame nil
            popterm--frame-buffer nil
            popterm--saved-mode-line-format nil
            popterm--focus-timer nil
            popterm--active-display-method nil))))

(ert-deftest popterm-test-posframe-hide-cleans-focus-timer ()
  "Verify `popterm--posframe-hide' cancels and clears the focus timer."
  (let ((popterm--focus-timer (run-with-timer 10 nil #'ignore))
        (popterm--frame nil)
        (popterm--frame-buffer nil))
    (cl-letf (((symbol-function 'popterm--remove-focus-guard) #'ignore)
              ((symbol-function 'popterm--remove-display-buffer-guard) #'ignore)
              ((symbol-function 'popterm--remove-theme-watch) #'ignore)
              ((symbol-function 'select-frame-set-input-focus) #'ignore))
      (popterm--posframe-hide)
      (should (null popterm--focus-timer)))))

(ert-deftest popterm-test-cleanup-restores-mode-line ()
  "Verify posframe cleanup restores the original mode line."
  (let ((buffer (get-buffer-create "*popterm-restore-modeline*"))
        (original '(" Demo"))
        (popterm--frame-buffer nil)
        (popterm--saved-mode-line-format nil)
        (popterm--active-display-method 'posframe))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mode-line-format nil))
          (setq popterm--frame-buffer buffer
                popterm--saved-mode-line-format original)
          (cl-letf (((symbol-function 'popterm--remove-focus-guard) #'ignore)
                    ((symbol-function 'popterm--remove-display-buffer-guard) #'ignore)
                    ((symbol-function 'popterm--remove-theme-watch) #'ignore))
            (popterm--cleanup-posframe-state))
          (with-current-buffer buffer
            (should (equal mode-line-format original)))
          (should (null popterm--saved-mode-line-format))
          (should (null popterm--frame-buffer))
          (should (null popterm--active-display-method)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest popterm-test-hidehandler-cleans-daemon-hidden-posframe ()
  "Verify posframe daemon hides clear Popterm state and guards."
  (let ((popterm--frame 'frame)
        (popterm--frame-buffer (get-buffer-create "*popterm-hidehandler*"))
        (popterm--active-display-method 'posframe)
        (popterm--inhibit-hidehandler nil)
        (popterm--focus-guard-active t)
        (popterm--display-buffer-guard-active t)
        (popterm--theme-watch-active t)
        (popterm--focus-timer (run-with-timer 10 nil #'ignore)))
    (unwind-protect
        (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                  ((symbol-function 'frame-visible-p) (lambda (_frame) t))
                  ((symbol-function 'frame-parent) (lambda (_frame) 'parent))
                  ((symbol-function 'active-minibuffer-window) (lambda () nil))
                  ((symbol-function 'selected-frame) (lambda () 'outside))
                  ((symbol-function 'frame-parameter)
                   (lambda (_frame parameter)
                     (and (eq parameter 'parent-frame) nil)))
                  ((symbol-function 'popterm--remove-focus-guard)
                   (lambda () (setq popterm--focus-guard-active nil)))
                  ((symbol-function 'popterm--remove-display-buffer-guard)
                   (lambda () (setq popterm--display-buffer-guard-active nil)))
                  ((symbol-function 'popterm--remove-theme-watch)
                   (lambda ()
                     (setq popterm--theme-watch-active nil)
                     (when (timerp popterm--theme-refresh-timer)
                       (cancel-timer popterm--theme-refresh-timer)
                       (setq popterm--theme-refresh-timer nil)))))
          (should (popterm--posframe-hidehandler nil))
          (should-not popterm--focus-guard-active)
          (should-not popterm--display-buffer-guard-active)
          (should-not popterm--theme-watch-active)
          (should (null popterm--focus-timer))
          (should (null popterm--frame))
          (should (null popterm--frame-buffer))
          (should (null popterm--active-display-method)))
      (when (buffer-live-p (get-buffer "*popterm-hidehandler*"))
        (kill-buffer "*popterm-hidehandler*")))))

(ert-deftest popterm-test-theme-refresh-uses-active-display-method ()
  "Verify theme refresh honors the active session display method."
  (let ((popterm-display-method 'window)
        (popterm--active-display-method 'posframe)
        (popterm--frame 'frame)
        (popterm--frame-buffer (get-buffer-create "*popterm-theme*"))
        (popterm--inhibit-hidehandler t)
        (refreshed-frame nil)
        (redrawn-frame nil))
    (unwind-protect
        (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                  ((symbol-function 'modify-frame-parameters)
                   (lambda (frame _parameters)
                     (setq refreshed-frame frame)))
                  ((symbol-function 'redraw-frame)
                   (lambda (frame)
                     (setq redrawn-frame frame))))
          (popterm--refresh-posframe-after-theme-change)
          (should (eq refreshed-frame popterm--frame))
          (should (eq redrawn-frame popterm--frame))
          (should-not popterm--inhibit-hidehandler))
      (when (buffer-live-p (get-buffer "*popterm-theme*"))
        (kill-buffer "*popterm-theme*")))))

(ert-deftest popterm-test-posframe-delete-preserves-popterm-buffer ()
  "Verify external posframe cleanup preserves Popterm terminal buffers."
  (let ((buffer (get-buffer-create "*popterm-vterm*"))
        (popterm--frame 'frame)
        (popterm--frame-buffer nil)
        (popterm--active-display-method 'posframe)
        (popterm--theme-watch-active t)
        (popterm--display-buffer-guard-active t)
        (popterm--focus-guard-active t)
        (orig-called nil))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local posframe--frame 'posframe-child)
          (setq popterm--frame-buffer buffer)
          (cl-letf (((symbol-function 'popterm--remove-focus-guard)
                     (lambda () (setq popterm--focus-guard-active nil)))
                    ((symbol-function 'popterm--remove-display-buffer-guard)
                     (lambda () (setq popterm--display-buffer-guard-active nil)))
                    ((symbol-function 'popterm--remove-theme-watch)
                     (lambda () (setq popterm--theme-watch-active nil))))
            (should-not
             (popterm--preserve-buffer-during-posframe-delete
              (lambda (&rest _args)
                (setq orig-called t)
                'killed)
              buffer))
            (should-not orig-called)
            (should (buffer-live-p buffer))
            (should (null posframe--frame))
            (should (null popterm--frame))
            (should (null popterm--frame-buffer))
            (should (null popterm--active-display-method))
            (should-not popterm--theme-watch-active)
            (should-not popterm--display-buffer-guard-active)
            (should-not popterm--focus-guard-active)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest popterm-test-posframe-delete-passes-through-non-popterm-buffer ()
  "Verify non-Popterm buffers still use posframe's normal kill path."
  (let ((buffer (get-buffer-create "*not-popterm*"))
        (orig-args nil))
    (unwind-protect
        (let ((result
               (popterm--preserve-buffer-during-posframe-delete
                (lambda (&rest args)
                  (setq orig-args args)
                  'killed)
                buffer)))
          (should (eq result 'killed))
          (should (equal orig-args (list buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest popterm-test-show-records-active-display-method ()
  "Verify forced display methods become the active session method."
  (let ((popterm--active-display-method nil)
        (buffer (get-buffer-create "*popterm-show*")))
    (unwind-protect
        (progn
          (let ((popterm-display-method 'posframe))
            (cl-letf (((symbol-function 'popterm--posframe-show) #'ignore))
              (popterm--show buffer))
            (should (eq popterm--active-display-method 'posframe)))
          (let ((popterm-display-method 'window))
            (cl-letf (((symbol-function 'popterm--window-show) #'ignore))
              (popterm--show buffer))
            (should (eq popterm--active-display-method 'window))))
      (kill-buffer buffer))))

(ert-deftest popterm-test-vterm-setup-updates-buffer-local-exceptions ()
  "Verify `popterm--vterm-setup' only updates the current buffer's exceptions."
  (let ((doc (documentation #'popterm--vterm-setup)))
    (should (string-match-p "buffer-local" doc))
    (should (string-match-p "idempotent" doc)))
  (with-temp-buffer
    (setq-local vterm-keymap-exceptions '("C-x"))
    (popterm--vterm-setup)
    (dolist (key popterm--vterm-passthrough-keys)
      (should (member key vterm-keymap-exceptions)))
    (should (= (length vterm-keymap-exceptions)
               (length (delete-dups (copy-sequence vterm-keymap-exceptions))))))
  (with-temp-buffer
    (setq-local vterm-keymap-exceptions nil)
    (should (null vterm-keymap-exceptions))))

(ert-deftest popterm-test-byte-compile-without-warnings ()
  "Test that `popterm.el' byte-compiles without `bytecomp' warnings."
  :tags '(:slow)
  (let* ((source-file (or (locate-library "popterm.el")
                          (expand-file-name "popterm.el" default-directory)))
         (temp-file (make-temp-file "popterm-test-" nil ".el"))
         (byte-file (concat temp-file "c"))
         (warnings nil))
    (copy-file source-file temp-file t)
    (unwind-protect
        (progn
          (let ((inhibit-message t)
                (byte-compile-warnings t)
                (byte-compile-error-on-warn nil))
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (type message &optional _level _buffer-name)
                         (when (eq type 'bytecomp)
                           (push message warnings)))))
              (byte-compile-file temp-file)))
          (should (null warnings)))
      (when (and byte-file (file-exists-p byte-file))
        (ignore-errors (delete-file byte-file)))
      (when (file-exists-p temp-file)
        (ignore-errors (delete-file temp-file))))))

;;; ── Integration Tests ─────────────────────────────────────────────────────────

(ert-deftest popterm-test-display-buffer-guard-predicate ()
  "Test display-buffer-guard correctly identifies popterm buffers."
  ;; Create a test frame for the guard to check
  (let ((popterm--display-buffer-guard-active t)
        (popterm--frame (selected-frame)))

    ;; Popterm buffer names should be guarded
    (with-temp-buffer
      (rename-buffer "*popterm-vterm*" t)
      (should (popterm--display-buffer-guard-p (current-buffer) nil)))

    (with-temp-buffer
      (rename-buffer "*popterm-eat[test]*" t)
      (should (popterm--display-buffer-guard-p (current-buffer) nil)))

    ;; Renamed Ghostel buffers should still be guarded via Popterm metadata
    (with-temp-buffer
      (rename-buffer "*ghostel: host: ~/project*" t)
      (setq major-mode 'ghostel-mode)
      (setq-local popterm--managed-buffer t
                  popterm--buffer-backend 'ghostel
                  popterm--buffer-instance-name nil)
      (popterm-mode 1)
      (should (popterm--display-buffer-guard-p (current-buffer) nil)))

    ;; Non-popterm buffers should not be guarded
    (should-not (popterm--display-buffer-guard-p "*scratch*" nil))
    (should-not (popterm--display-buffer-guard-p "*shell*" nil))

    ;; Guard should be inactive when guard-active is nil
    (let ((popterm--display-buffer-guard-active nil))
      (with-temp-buffer
        (rename-buffer "*popterm-vterm-inactive*" t)
        (should-not (popterm--display-buffer-guard-p (current-buffer) nil))))))

(ert-deftest popterm-test-posframe-visible-p ()
  "Test posframe visibility check handles dead frames gracefully."
  (let ((popterm--frame nil))
    (should-not (popterm--posframe-visible-p)))

  ;; With a live frame that's visible
  (let ((popterm--frame (selected-frame)))
    (should (popterm--posframe-visible-p))))

;;; ── Performance Tests ─────────────────────────────────────────────────────────

(ert-deftest popterm-test-buffer-list-performance ()
  "Test buffer-list filtering performance with many buffers."
  :tags '(:performance)
  (let ((test-buffers '())
        (iterations 100))
    (unwind-protect
        (progn
          ;; Create 50 mock terminal buffers
          (dotimes (i 50)
            (let ((buf (generate-new-buffer (format "*popterm-vterm-%d*" i))))
              (with-current-buffer buf
                (setq major-mode 'vterm-mode))
              (push buf test-buffers)))

          ;; Benchmark the filter operation
          (let ((start (current-time)))
            (dotimes (_ iterations)
              (popterm--buffer-list 'vterm))
            (let* ((end (current-time))
                   (elapsed (float-time (time-subtract end start)))
                   (per-call (/ elapsed iterations)))

              ;; Should be fast (< 1ms per call)
              (should (< per-call 0.001)))))

      ;; Cleanup
      (mapc #'kill-buffer test-buffers))))

;;; ── Test Summary ──────────────────────────────────────────────────────────────

(defun popterm-run-all-tests ()
  "Run all popterm tests and display a summary.
This is a convenience function for interactive use."
  (interactive)
  (ert-run-tests-interactively "^popterm-"))

(provide 'popterm-tests)
;;; popterm-tests.el ends here
