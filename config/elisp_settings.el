;; elisp_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; From https://www.emacswiki.org/emacs/EmacsLispMode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)
            ;; Pretty-print eval'd expressions.
            (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'pp-eval-last-sexp)
            ;; Recompile if .elc exists.
            (add-hook (make-local-variable 'after-save-hook)
                      (lambda () (byte-force-recompile default-directory)))))
