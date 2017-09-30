;; python_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d

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
  :after (python anaconda-mode company)
  :no-require t
  :ensure t
  :config
  ;; Hack below
  (setq ein:get-ipython-major-version 5)
  ;; https://github.com/millejoh/emacs-ipython-notebook/issues/157
  (add-hook 'ein:notebook-mode-hook #'anaconda-mode)
  (defun user-ein-reply-callback (args content -metadata-not-used-)
    (let ((callback (plist-get args :callback))
          (candidates (plist-get content :matches)))
      (funcall callback candidates)))

  (defun user-company-ein-callback (callback)
    (ein:kernel-complete
     (ein:get-kernel)
     (thing-at-point 'line)
     (current-column)
     (list :complete_reply
           (cons #'user-ein-reply-callback (list :callback callback)))))

  (defun user-company-ein-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (case command
      (interactive (company-begin-backend 'user-company-ein-backend))
      (prefix (company-anaconda-prefix))
      (candidates (cons :async #'user-company-ein-callback))
      (location nil)
      (sorted t)))
  (add-to-list 'company-backends #'user-company-ein-backend))
