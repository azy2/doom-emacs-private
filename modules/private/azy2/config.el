;;; private/default/config.el -*- lexical-binding: t; -*-

(define-key key-translation-map (kbd "C-x") (kbd "C-u"))
(define-key key-translation-map (kbd "C-u") (kbd "C-x"))
(define-key key-translation-map (kbd "M-x") (kbd "M-u"))
(define-key key-translation-map (kbd "M-u") (kbd "M-x"))

(load! +bindings)

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 22)
      doom-theme 'doom-vibrant)

(require 'company)
(setq company-idle-delay 0
      company-minimum-prefix-length 2)

(def-package! cmake-ide
  :after (irony company flycheck rtags)
  :init
  (setq cmake-ide-build-pool-use-persistent-naming t)
  :config
  (require 'rtags)
  (cmake-ide-setup))

(def-package! evil-magit
  :after magit
  :config
  (define-key magit-mode-map (kbd doom-leader-key) nil)
  (add-hook! 'magit-mode-hook (evil-vimish-fold-mode -1))
  (add-hook! 'magit-mode-hook (evil-snipe-mode -1)))

(require 'flycheck)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.5)

(def-package! flycheck-pycheckers
  :after (flycheck)
  :config
  (setq flycheck-pycheckers-checkers '(mypy3 pep8 flake8 pylint))
  (add-hook! 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(after! smartparens
  ;; Automatically indent a block when pressing enter inside curly braces,
  ;; square brackets or parentheses
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (setq sp-autowrap-region t))

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("HACK"  . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

(setq show-trailing-whitespace t
      which-key-idle-delay 0.1)

(setq counsel-find-file-ignore-regexp nil)

(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq flycheck-pos-tip-timeout 60)

(add-hook 'doc-view-mode-hook #'auto-revert-mode)
