;; C++ emacs files

; Indentation preferences
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'inline-open '0)
)
(add-hook 'c-mode-common-hook 'my-indent-setup)

; ---------------------------------------------------------------------------

; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; ---------------------------------------------------------------------------

; Company
; credit: tuhdo.github.io/c-ide.html
(setq company-backends (delete 'company-semantic company-backends))
