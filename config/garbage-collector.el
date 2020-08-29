;;; -*- lexical-binding: t -*-

;; Set the default garbage collection parameters.
(defvar my/gc-cons-threshold 32000000 "Preferred garbage collection threshold value.")
(defvar my/gc-cons-percentage 0.1 "Preferred garbage collection percentage value.")

(defun my/defer-garbage-collection ()
  "Set the garbage collection threshold to the highest possible for collection avoidance."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun my/restore-garbage-collection ()
  "Restore the garbage collection threshold parameters in a deferred fashion."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold my/gc-cons-threshold
                          gc-cons-percentage my/gc-cons-percentage))))

;; Defer garbage collection while Emacs is starting and restore the threshold when we're done.
(my/defer-garbage-collection)
(add-hook 'emacs-startup-hook #'my/restore-garbage-collection)

;; Raise and restore the garbage collection threshold for minibuffer commands.
(add-hook 'minibuffer-setup-hook #'my/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'my/restore-garbage-collection)

;; Collect all garbage whenever the focus changes to/from Emacs.
(add-function :after after-focus-change-function #'garbage-collect)
