;;; private/azy2/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +azy2/comment-or-uncomment-line-or-region ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defvar +azy2/indentation-sensitive-modes '()
  "Modes that should not automatically indent when pasting")

(defvar +azy2/indentation-max-length 5000
  "The maximum length in characters for which to apply automatic indentation on paste.")

;;;###autoload
(defun +azy2/indent-paste-advise (original-function &rest args)
  "Automatically indent pasted code. See `+azy2/indentation-sensitive-modes'."
  (if (or (member major-mode +azy2/indentation-sensitive-modes))
      (apply original-function (cdr args))
    (let ((inhibit-message t)
          (transient-mark-mode nil))
      (evil-start-undo-step)
      ;; This would otherwise cause double highlighting
      (let ((evil-googles-mode nil))
        (apply original-function args))
      ;; Only indent when the region is not too large
      (when (<= (- (region-end) (region-beginning)) +azy2/indentation-max-length)
        (indent-region (region-beginning) (region-end) nil))
      ;; HACK: For some reason the `(evil-end-undo-step)' moves the point one
      ;;       unit to the left when in insert mode
      (when (evil-insert-state-p) (forward-char))
      (evil-end-undo-step))))
