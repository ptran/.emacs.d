;; python_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.2

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq-default python-indent 4)
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter "python"))
  (setq python-shell-interpreter-args "--simple-prompt -i"))

;; Display line number
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))

(use-package flycheck
  :after python
  :no-require t
  :config 
  (add-hook 'python-mode-hook 'flycheck-mode))

;; Anaconda-mode
;; ---------------------------------------------------------------------------
(use-package anaconda-mode
  :after python
  :ensure t
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after anaconda-mode
  :ensure t)

(use-package company
  :after company-anaconda
  :no-require t
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; ob-python
;; ---------------------------------------------------------------------------
(use-package ob-ipython
  :after python
  :ensure t)
