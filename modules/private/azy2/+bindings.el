;;; config/default/+bindings.el -*- lexical-binding: t; -*-

(map! (:leader
        (:desc "code" :prefix "c"
          :desc "Toggle comment region" :nv ";" #'+azy2/comment-or-uncomment-line-or-region)))
