;; lisp_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.2

(add-hook 'lisp-mode-hook #'company-mode)
(add-hook 'lisp-mode-hook #'eldoc-mode)
