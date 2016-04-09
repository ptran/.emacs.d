;;;; Python emacs file

; Indentation preference
(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))

; Fix potential encoding issues
(setenv "LC_CTYPE" "UTF-8")

;; Sets the python interpreter to be ipython. To trick emacs into
;; thinking we're still running regular python, we run ipython in
;; classic mode.
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i --classic")
