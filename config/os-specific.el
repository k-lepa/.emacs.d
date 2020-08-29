;;; -*- lexical-binding: t -*-

(when (string-equal system-type "darwin")
  (setq exec-path (append exec-path '("/usr/local/bin")))

  ;; On macOS ls doesn't support the --dired.
  (setq dired-use-ls-dired nil))
