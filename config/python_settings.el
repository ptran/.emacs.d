;; python_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.3

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (if (executable-find "ipython")
      (setq-default python-shell-interpreter "ipython"
                    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                    python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
                    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                    python-shell-completion-setup-code
                    "from IPython.core.completerlib import module_completion"
                    python-shell-completion-string-code
                    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
  (setq-default python-indent 4
                python-shell-interpreter-args "--simple-prompt -i")
  (setq python-shell-prompt-detect-failure-warning nil))

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
  :no-require t
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; ob-ipython
;; ---------------------------------------------------------------------------
(use-package ob-ipython
  :if (executable-find "ipython")
  :ensure t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t))))

;; Emacs Ipython Notebook (EIN)
;; ---------------------------------------------------------------------------
(use-package ein
  :if (executable-find "ipython")
  :after (python anaconda-mode)
  :no-require t
  :ensure t
  :config
  ;; Hack below
  (setq ein:get-ipython-major-version 5))
