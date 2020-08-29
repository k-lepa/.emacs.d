;;; -*- lexical-binding: t -*-

;; Change behavior of prompts.
(fset 'yes-or-no-p 'y-or-n-p) 
(setq-default
 ;; Prevent Emacs from quitting when the last window is closed
 confirm-kill-emacs #'y-or-n-p
 ;; Don't use dialog boxes for mouse commands
 use-dialog-box nil)

;; If the line was already indented, then try to complete.
(setq tab-always-indent 'complete)

;; Echo unfinished commands after 200ms.
(setq-default echo-keystrokes 0.2)

;; Disable audio notifications.
(setq-default ring-bell-function #'ignore)

;; Disable the initial message and splash screen.
(setq-default initial-scratch-message ""
              inhibit-splash-screen t)

;; Set custom startup message.
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

;; Disable excess GUI widgets: the toolbar, the menubar and the scrollbar. 
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)

;; Disable syntax highlighting.
(global-font-lock-mode 0)

(let ((font-cell `(font . ,(format "%s-%s" "Inconsolata LGC" 12))))
  (add-to-list 'initial-frame-alist font-cell)
  (add-to-list 'default-frame-alist font-cell))

(when window-system
  (set-frame-size (selected-frame) 120 40))

