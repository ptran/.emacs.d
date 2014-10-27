;;;; C++ emacs file

;; Indentation preferences
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun my-c-indent-hook ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'inline-open '0))
(add-hook 'c-mode-common-hook 'my-c-indent-hook)

;; Comments auto fill
(defun my-c-auto-fill-hook ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face) 'font-lock-comment-face)))))

(add-hook 'c-mode-common-hook 'my-c-auto-fill-hook)

;; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; gdb beast mode as default
(setq gdb-many-windows t
      gdb-show-main t
)

;; this allows clang use instead of CEDET semantic tools
(setq company-backends (delete 'company-semantic company-backends))

;; include paths to clang argument
(setq company-clang-arguments '("-I /usr/include"))
