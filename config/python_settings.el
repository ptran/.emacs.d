;;;; Python emacs file

(use-package python-mode
  :init
  (setq-default python-indent 4)
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args ""
            python-shell-prompt-regexp "In \\[[0-9]+\\]: "
            python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
            python-shell-completion-setup-code
            "from IPython.core.completerlib import module_completion"
            python-shell-completion-module-string-code
            "';'.join(module_completion('''%s'''))\n"
            python-shell-completion-string-code
            "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    (when (executable-find "python")
      (setq python-shell-interpreter "python"
            python-shell-interpreter-args ""))))

;; Anaconda-mode
;; ---------------------------------------------------------------------------
(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'run-python))

(use-package company-anaconda
  :ensure t)

(use-package company
  :no-require t
  :config
  (add-to-list 'company-backends 'company-anaconda))
