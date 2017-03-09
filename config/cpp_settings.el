;; cpp_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.2

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

;; Irony-Mode
;; ---------------------------------------------------------------------------
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

(use-package irony
  :if (and (executable-find "clang") (executable-find "cmake"))
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-eldoc-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; flycheck-irony
;; ---------------------------------------------------------------------------
(use-package flycheck-irony
  :after flycheck
  :ensure t)

;; company-irony
;; ---------------------------------------------------------------------------
(use-package company-irony
  :after company
  :ensure t)

(use-package company-irony-c-headers
  :after company
  :ensure t)
