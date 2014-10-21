;;;; C++ emacs file

; Indentation preferences
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'inline-open '0)
)
(add-hook 'c-mode-common-hook 'my-indent-setup)

; Comments auto fill
(add-hook 'c-mode-common-hook
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))

; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; gdb beast mode as default
(setq gdb-many-windows t
      gdb-show-main t
)

;;;; Company
; credit: tuhdo.github.io/c-ide.html
; ---------------------------------------------------------------------------
(setq company-backends (delete 'company-semantic company-backends))

; include paths to some important headers
(setq company-clang-arguments '("-I /usr/include"))
