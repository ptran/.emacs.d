;;;; Python emacs file

(use-package python-mode
  :init
  (setq-default python-indent 4)
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "")))
(add-hook 'python-mode-hook 'run-python)

;; Anaconda-mode
;; ---------------------------------------------------------------------------
(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t)

(use-package company
  :no-require t
  :config
  (add-to-list 'company-backends 'company-anaconda))
