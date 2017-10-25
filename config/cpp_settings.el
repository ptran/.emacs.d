;; cpp_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d

;; Indentation preferences
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun my/c-indent-hook ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'inline-open '0))
(add-hook 'c-mode-common-hook #'my/c-indent-hook)

;; Display line numbers
(add-hook 'c-mode-common-hook #'linum-mode)

;; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Irony-Mode
;; ---------------------------------------------------------------------------
(defun my/irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

(use-package irony
  :if (and (executable-find "clang") (executable-find "cmake") (not (version< emacs-version "24.4")))
  :ensure t
  :config
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'my/irony-mode-hook)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :after company
  :ensure t
  :config
  (add-hook 'c-mode-hook #'company-mode)
  (add-hook 'c++-mode-hook #'company-mode)
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

;; irony-eldoc
(use-package irony-eldoc
  :after irony
  :no-require t
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; flycheck-irony
;; ---------------------------------------------------------------------------
(use-package flycheck-irony
  :after flycheck
  :no-require t
  :ensure t
  :config
  (add-hook 'c-mode-common-hook #'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
