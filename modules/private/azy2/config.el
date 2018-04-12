;;; private/default/config.el -*- lexical-binding: t; -*-

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (setq-default exec-path-from-shell-shell-name "/usr/bin/zsh")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
  (exec-path-from-shell-initialize))

(define-key key-translation-map (kbd "C-x") (kbd "C-u"))
(define-key key-translation-map (kbd "C-u") (kbd "C-x"))
(define-key key-translation-map (kbd "M-x") (kbd "M-u"))
(define-key key-translation-map (kbd "M-u") (kbd "M-x"))

(load! +bindings)

(if (>= (display-pixel-width) 2560)
    (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 22))
  (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 16)))
(setq doom-city-lights-brighter-comments t
      doom-city-lights-comment-bg nil
      doom-city-lights-brighter-modeline t)
(setq doom-theme 'doom-city-lights)

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

;; (def-package! evil-magit
;;   :after magit
;;   :config
;;   (define-key magit-mode-map (kbd doom-leader-key) nil)
;;   (add-hook! 'magit-mode-hook (evil-snipe-mode -1)))

(require 'flycheck)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.5)

(def-package! flycheck-pycheckers
  :after (flycheck)
  :config
  (setq flycheck-pycheckers-checkers '(pep8 flake8 pylint))
  (setq flycheck-pycheckers-max-line-length 120)
  (setq flycheck-pycheckers-venv-root "~/.pyenv/versions")
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

(setq auto-window-vscroll nil)

;(after! helm
;  (set! :popup "^\\*helm" :ignore)
;  (setq helm-split-window-inside-p t
;        helm-echo-input-in-header-line t
;        helm-display-header-line t
;        helm-autoresize-min-height 30
;        helm-autoresize-max-height 0)
;  (helm-autoresize-mode t))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))

(add-hook! 'term-mode-hook 'persp-add-buffer)
(def-package! multi-term
  :config
  (setq multi-term-program "zsh")
  (setq-default term-bind-key-alist
                '(("C-c C-c" . term-interrupt-subjob)
                  ("C-c C-e" . term-send-esc)
                  ("C-y" . term-paste)
                  ("C-d" . term-send-eof)
                  ("M-w" . yank)
                  ("M-," . term-send-raw))))

;; (after! magit
;;   (defun magit-show-keybindings-section ()
;;     (magit-dispatch-popup)
;;     (let ((s (buffer-string)))
;;       (magit-popup-quit)
;;       (insert s)))

;;   (add-hook 'magit-status-sections-hook 'magit-show-keybindings-section :append))

(evil-set-initial-state 'pdf-view-mode 'normal)

(after! latex
  (defun azy2/compile-pdf ()
    (setq-local compilation-scroll-output t)
    (compile (concat "pdflatex " (file-name-nondirectory buffer-file-name))))
  (add-hook 'LaTeX-mode-hook
    '(lambda () (add-hook 'after-save-hook 'azy2/compile-pdf nil 'local))))

(after! rust-mode
  (setq rust-format-on-save t)
  (setenv "RUST_TARGET_PATH" "/home/ben/projects/plan8/")
  (setenv "RUSTFLAGS" "--sysroot /home/ben/.xargo"))


