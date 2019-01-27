;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(require 'exec-path-from-shell)
(setq-default exec-path-from-shell-shell-name "/usr/bin/zsh")
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-initialize)

(map!
 :n "C-h" #'evil-window-left
 :n "C-j" #'evil-window-down
 :n "C-k" #'evil-window-up
 :n "C-l" #'evil-window-right
 :ni "C-]" #'+lookup/definition)

(after! smartparens
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :post-handlers '(:add ("||\n[i]" "RET")))))
