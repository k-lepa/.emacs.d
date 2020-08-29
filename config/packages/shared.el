;;; -*- lexical-binding: t -*-

;; The delight package is invoked with the :delight keyword for the use-package macro.
(use-package delight)

;; Use a theme that is inspired by ACME editor.
(use-package acme-theme :init (load-theme 'acme t))

;; Interactive interface for completion (ivy, counsel, and swiper).
(use-package counsel
  :delight (counsel-mode) (ivy-mode)

  :bind
  ("C-c r" . ivy-resume)
  ("C-s" . swiper-isearch)
  ("C-r" . swiper-isearch-backward)
  ("C-c i" . counsel-imenu)
  
  :custom
  (ivy-extra-directories nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (t      . ivy--regex-fuzzy)))

  :init
  (ivy-mode)
  (counsel-mode))

(use-package dired-collapse
  :hook
  (dired-mode . dired-collapse-mode))

;; which-key displays the key bindings following entered incomplete command in a popup.
(use-package which-key
  :delight

  :bind
  ("C-h M" . which-key-show-major-mode)
  ("C-h M-m" . which-key-show-minor-mode-keymap)
  
  :custom
  (which-key-show-early-on-C-h t "Allow C-h to trigger which-key before it is done automatically.")
  (which-key-idle-delay 10000 "Make sure which-key doesn't show normally.")
  (which-key-idle-secondary-delay 0.05)

  :init
  (which-key-mode))

;; smartparens deals with pairs in Emacs.
(use-package smartparens
  :delight
  :hook
  (emacs-lisp-mode . smartparens-strict-mode)

  :bind
  ("C-M-f" . sp-forward-sexp)
  ("C-M-b" . sp-backward-sexp)
  ("C-M-d" . sp-down-sexp)
  ("C-M-u" . sp-backward-up-sexp)
  ("C-M-<space>" . sp-mark-sexp)
  ("C-)" . sp-forward-slurp-sexp)
  ("C-{" . sp-backward-slurp-sexp)
  ("C-(" . sp-forward-barf-sexp)
  ("C-}" . sp-backward-barf-sexp)
  ("C-M-k" . sp-kill-sexp)
  ("M-<backspace>" . sp-splice-sexp-killing-backward)
  ("C-M-t" . sp-transpose-sexp)

  :config
  (require 'smartparens-config))

;; The mode keeps code always indented.
(use-package aggressive-indent
  :delight
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Auto-completion in source code.
(use-package company
  :delight
  :bind
  (:map company-mode-map)
  ([tab] . company-indent-or-complete-common)
  (:map company-active-map)
  ([tab] . company-complete-common-or-cycle)

  :custom
  (company-idle-delay 1 "Start completions automatically in 1s."))

;; Completely hide eldoc-mode.
(use-package emacs
  :delight
  (eldoc-mode)
  (abbrev-mode))

;; YASnippet automatically expands abbreviations into function templates.
(use-package yasnippet
  :delight (yas-minor-mode)
  :config
  (yas-global-mode)
  
  :custom
  (yas-snippet-dirs `(,(concat user-emacs-directory "snippets")) "YASnippets' locations"))

;;  On-the-fly syntax checking extension
(use-package flycheck
  :delight)

;; Project interaction.
(use-package projectile
  :delight
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy "Use ivy completion"))

;; Counsel projectile integration
(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; Language Server Protocol support.
(use-package lsp-mode
  :delight
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  
  :custom
  (lsp-completion-enable-additional-text-edit nil "Don't apply it when performing completion"))

;; Contains all the higher level UI modules of lsp-mode.
(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil "Disable lsp-ui-doc.")

  :custom-face
  (lsp-ui-sideline-code-action ((t (:foreground "orange1")))))

;; Debug Adapter Protocol support.
(use-package dap-mode
  :delight
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; Consistent coding styles.
(use-package editorconfig
  :delight
  :config
  (editorconfig-mode t))

(use-package treemacs
  :custom
  (treemacs-no-png-images t)
  (treemacs-width 150)
  (treemacs-show-cursor t))

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-theme "NoIcons" "Use own custom theme without icons.")

  :config
  (require 'treemacs)
  (require 'treemacs-themes)

  (treemacs-create-theme
   "NoIcons"
   :config
   (progn
     (treemacs-create-icon :icon "[ns] " :extensions (namespace))
     (treemacs-create-icon :icon "[txt] " :extensions (text))
     (treemacs-create-icon :icon "[meth] " :extensions (method))
     (treemacs-create-icon :icon "[fun] " :extensions (function))
     (treemacs-create-icon :icon "[ctor] " :extensions (constructor))
     (treemacs-create-icon :icon "[fld] " :extensions (field))
     (treemacs-create-icon :icon "[var] " :extensions (variable))
     (treemacs-create-icon :icon "[cls] " :extensions (class))
     (treemacs-create-icon :icon "[iface] " :extensions (interface))
     (treemacs-create-icon :icon "[mod] " :extensions (module))
     (treemacs-create-icon :icon "[prop] " :extensions (property))
     (treemacs-create-icon :icon "[unit] " :extensions (unit))
     (treemacs-create-icon :icon "[val] " :extensions (value))
     (treemacs-create-icon :icon "[enum] " :extensions (enum))
     (treemacs-create-icon :icon "[kwd] " :extensions (keyword))
     (treemacs-create-icon :icon "[snip]b" :extensions (snippet))
     (treemacs-create-icon :icon "[clr] " :extensions (color))
     (treemacs-create-icon :icon "[file] " :extensions (file))
     (treemacs-create-icon :icon "[ref] " :extensions (reference))
     (treemacs-create-icon :icon "[dir] " :extensions (folder))
     (treemacs-create-icon :icon "[emem] " :extensions (enum-member))
     (treemacs-create-icon :icon "[const] " :extensions (constant))
     (treemacs-create-icon :icon "[struct] " :extensions (struct))
     (treemacs-create-icon :icon "[evt] " :extensions (event))
     (treemacs-create-icon :icon "[op] " :extensions (operator))
     (treemacs-create-icon :icon "[tparam] " :extensions (type-parameter))
     (treemacs-create-icon :icon "[tmpl] " :extensions (template))))) 
