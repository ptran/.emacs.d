;;;; C++ emacs file

;; Indentation preferences
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun my-c-indent-hook ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'inline-open '0))
(add-hook 'c-mode-common-hook 'my-c-indent-hook)

;; Display line numbers
(add-hook 'c-mode-common-hook (lambda () (linum-mode 1)))

;; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
