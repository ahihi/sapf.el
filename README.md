# sapf.el

Emacs major mode for interacting with [sapf](https://github.com/lfnoise/sapf).

current features:

- start/stop the sapf process (`sapf-start`/`sapf-stop`)
- evaluate code (`sapf-run-multiple-lines`)

## setup

with straight.el:

```elisp
(use-package sapf
  :straight (sapf :type git :host github :repo "ahihi/sapf.el")
  :config
  ;; set sapf interpreter, in case it is not in your exec-path
  (setq sapf-interpreter "~/Downloads/sapf_v0.1.21/sapf")
  ;; pass arguments, such as the sample rate
  (setq sapf-interpreter-arguments '("-r" "44100"))
  ;; change the highlight function for evaluated code, if you like
  (setq sapf-highlight #'nav-flash-show)
  ;; auto-activate for .sapf files
  (add-to-list 'auto-mode-alist '("\\.sapf\\'" . sapf-mode))
  ;; define some key bindings
  (define-key sapf-mode-map (kbd "M-<return>") #'sapf-run-multiple-lines)
  (define-key sapf-mode-map (kbd "C-c C-s") #'sapf-start)
  (define-key sapf-mode-map (kbd "C-c C-q") #'sapf-stop))
```
