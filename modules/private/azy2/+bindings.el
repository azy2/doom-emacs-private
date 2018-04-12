;;; config/default/+bindings.el -*- lexical-binding: t; -*-

(map! (:leader
        (:desc "code" :prefix "c"
          :desc "Toggle comment region" :nv ";" #'+azy2/comment-or-uncomment-line-or-region)
        (:desc "git" :prefix "g"
          :desc "Git status" :n "s" #'magit-status
          :desc "Git stage hunk" :n "S" #'git-gutter:stage-hunk)
        (:desc "code" :prefix "c"
          :desc "Build pdf" :n "p" (lambda! (save-buffer) (shell-command (concat "pdflatex " (file-name-nondirectory buffer-file-name))) (doom/escape)))
        :desc "Next theme" :n "T" #'+azy2/next-theme)
      (:map emacs-lisp-mode-map
        :ni "C-)" 'sp-forward-slurp-sexp
        :ni "C-(" 'sp-backward-slurp-sexp
        :ni "M-)" 'sp-forward-barf-sexp
        :ni "M-(" 'sp-backward-barf-sexp))
