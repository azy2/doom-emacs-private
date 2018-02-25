Install
-------
```bash
git clone git@github.com:azy2/doom-emacs-private.git ~/.doom.d
```

And use this `doom!` block in `~/.emacs.d/init.el`
```elisp
(require 'core (concat user-emacs-directory "core/core"))

(doom! :config
       private)
```
