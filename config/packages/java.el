;;; -*- lexical-binding: t -*-

;; Language Server Protocol for java.
(use-package lsp-java
  :hook
  (java-mode . lsp)
  (java-mode . flycheck-mode))

;; Debug Adapter Protocol for java.
(use-package dap-java
  :ensure nil)
