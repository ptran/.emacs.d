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
;; The default "C-x c" is close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(global-set-key (kbd "M-x") 'helm-M-x)             ; Use M-x with helm
(global-set-key (kbd "M-y") 'helm-show-kill-ring)  ; kill-ring display
(global-set-key (kbd "C-x b") 'helm-mini)          ; change buffer
(global-set-key (kbd "C-x C-f") 'helm-find-files)  ; find-file
(global-set-key (kbd "C-c h o") 'helm-occur)       ; occur (Finds expression in current buffer)

; gtag key bindings
(require 'helm-gtags)
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
