;; python_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (if (executable-find "ipython")
      (progn
        (setq python-shell-interpreter "ipython")
        (if (version< (replace-regexp-in-string "\n$" "" (shell-command-to-string "ipython --version")) "5")
            (setq python-shell-interpreter-args "-i")
          (setq python-shell-interpreter-args "--simple-prompt -i")))
    ;; Condition if ipython can't be found
      (setq python-shell-interpreter "python"
            python-shell-interpreter-args "-i"))
  (setq python-indent-offset 4))

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
  :if (and (executable-find "jupyter") (> emacs-major-version 24))
  :after (python anaconda-mode company)
  :no-require t
  :ensure t
  :config
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
  (add-to-list 'company-backends #'user-company-ein-backend t))
