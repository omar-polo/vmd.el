# vmd.el

vmd is an Emacs package to interact with [OpenBSD' vmd][vmd].


### vterm integration

To use vterm (or any other packages really) to handle a shell for the
console, `vmd-console-function` must point to a function that accepts
two parameters, name and cmd, and execute cmd (a list of strings) in
your preferred terminal.  By default, `ansi-term` (bundled with Emacs)
is used, so it's easy to switch to vterm:

```elisp
(defun my/vmd-vterm (name cmd)
  (let ((vterm-shell (mapconcat #'shell-quote-argument cmd " "))
        (vterm-buffer-name (concat "*" name "*")))
    (vterm)))
(setq vmd-console-function #'my/vmd-vterm)
```


[vmd]: http://man.openbsd.org/vmd
