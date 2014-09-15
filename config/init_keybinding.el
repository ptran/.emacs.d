;;;; Prompt before closing emacs
; credit: http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html
; ---------------------------------------------------------------------------
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-x C-g") 'goto-line)

;;;; Company
; ---------------------------------------------------------------------------
; code completion keys
(require 'cc-mode)
(define-key c-mode-map (kbd "C-<tab>") 'company-complete)
(define-key c++-mode-map (kbd "C-<tab>") 'company-complete)

;;;; Helm
; credit: tuhdo.github.io/helm-intro.html
; ---------------------------------------------------------------------------
(global-set-key (kbd "M-x") 'helm-M-x)             ; Use M-x with helm
(global-set-key (kbd "M-y") 'helm-show-kill-ring)  ; kill-ring display
(global-set-key (kbd "C-x b") 'helm-mini)          ; change buffer
(global-set-key (kbd "C-x C-f") 'helm-find-files)  ; find-file
(global-set-key (kbd "C-c h o") 'helm-occur)       ; occur (Finds expression in current buffer)

