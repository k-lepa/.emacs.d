;;; -*- lexical-binding: t -*-

(use-package cider
  :hook 
  (cider-repl-mode . smartparens-strict-mode)
  (cider-repl-mode . company-mode)
  (cider-repl-mode . cider-company-enable-fuzzy-completion)

  (cider-mode . company-mode)
  (cider-mode . cider-company-enable-fuzzy-completion)
  (cider-mode . eldoc-mode) ; without it eldoc doesn't work in clojure-mode

  (before-save . cider-format-buffer)
         
  :bind 
  (:map cider-mode-map)
  ("C-M-\\" . cider-format-region)
  ("C-M-i" . company-complete-common)
  
  (:map cider-repl-mode-map)
  ("C-M-i" . company-complete-common)
  ("C-i" . nil)

  :custom-face
  (cider-fringe-good-face ((t (:foreground "textColor"))))
  (cider-result-overlay-face ((t nil)))
  (cider-stacktrace-error-class-face ((t nil)))
  (cider-stacktrace-error-message-face ((t nil)))
  (cider-stacktrace-ns-face ((t nil)))

  :custom
  (nrepl-hide-special-buffers t "Hide *nrepl-connection* and *nrepl-server* buffers.")
  (cider-special-mode-truncate-lines nil "Don't truncate lines in *cider-test-report* or *cider-doc*.")
  (cider-repl-display-help-banner nil "Disable big help banner in the beginning of repl.")
  (cider-repl-result-prefix ";; => " "Change the string used to prefix REPL result.")
  (cider-repl-use-clojure-font-lock nil "Disable syntax highlighting in REPL.")
  (cider-save-file-on-load t "Doesn't prompt on evaluation of a buffer.")
  
  :init
  (setq cider-jdk-src-paths '())
  (if-let ((java-home (getenv "JAVA_HOME")))
    (push (concat (file-name-as-directory java-home) "lib/src.zip") cider-jdk-src-paths))
  (if-let ((clj-src (getenv "CLOJURE_SOURCES")))
    (push clj-src cider-jdk-src-paths)))

(use-package clj-refactor
  :delight
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  
  :custom
  (cljr-warn-on-eval nil "Don't warn the user before running any op that requires ASTs."))

(use-package clojure-mode
  :hook
  (clojure-mode . smartparens-strict-mode)
  (clojure-mode . aggressive-indent-mode)
  (clojure-mode . clj-refactor-mode)
  
  :custom
  (clojure-toplevel-inside-comment-form t "Eval top level forms inside comment forms."))