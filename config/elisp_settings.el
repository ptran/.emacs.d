;; elisp_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran/.emacs.d

(defun my/emacs-lisp-mode-hook ()
  "My hook for emacs-lisp-model"
  ;; Use spaces, not tabs.
  (setq indent-tabs-mode nil)
  ;; Pretty-print eval'd expressions.
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'pp-eval-last-sexp))

;; From https://www.emacswiki.org/emacs/EmacsLispMode
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-hook)
