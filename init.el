;;; -*- lexical-binding: t -*-

;; No visual region selection.
(setq transient-mark-mode nil)
;; Disable the confirmation for narrow-to-region.
(put 'narrow-to-region 'disabled nil)

;; Update a buffer if a file changes on disk, check a file changes every second.
(global-auto-revert-mode)             
(setq-default auto-revert-interval 1)

;; Don't save customizations in this file.
(setq-default custom-file (concat user-emacs-directory "custom.el"))
;; (if (file-exists-p custom-file)
;;     (load custom-file))

;; Disable backups and auto-saving.
(setq-default make-backup-files nil
              auto-save-default nil)

;; Enable saving history.
(savehist-mode 1)
(setq-default savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Enable saving a cursor position in a file.
(save-place-mode 1)
(setq-default save-place-file "~/.emacs.d/saveplace")

;; Load remaining configuration from files in the config directory.
(defun my/cfg-load-config-files (files)
  "Load configuration from modular collection of files."
  (dolist (file files)
    (load (expand-file-name (concat user-emacs-directory "config/" file)))
    (message "Loaded config file: %s" file)))

(my/cfg-load-config-files '("garbage-collector" "ui-tweaks" "os-specific"))

(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; The use-package macro allows to isolate package configuration.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Install packages if they are not already installed.
(setq use-package-always-ensure t)

;; Set up packages only.
(my/cfg-load-config-files '("packages/shared"
	                    "packages/clojure"
			    "packages/java"))
