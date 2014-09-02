;; Philip Tran's .emacs file
;; 08-15-14

; Re-direct auto-save and back-up files (~ or #)
(setq backup-directory-alist
      `((".*" . ,"~/.saves")))
(setq auto-save-file-name-transforms
      `((".*", "~/.saves" t)))

; Load Zenburn theme
; credit (and pull): https://github.com/bbatsov/zenburn-emacs(.git)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

; Prompt before closing emacs
; credit: http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

; Change buffer switching system
; credit: http://stackoverflow.com/questions/7394289/how-can-i-more-easily-switch-between-buffers-in-emacs
(iswitchb-mode 1)
(setq iswitchb-buffer-ignore '("^ "
			       "*Completions*"
			       "*Shell Command Output*"
			       "*Messages*"
			       "Async Shell Command")
)

; Hippie-expand
; credit: http://github.com/redgaurdtoo/emacs.d/blob/master/init-hippie-expand.el
(global-set-key (kbd "M-/") 'hippie-expand)

; let hippie-expand support ctags
(defun tags-complete-tag (string predicate what)
  (save-excursion
    ; If we need to ask for the tag table, allow that.
    (if (eq what t)
	(all-completions string (tags-complete-table) predicate)
      (try-completion string (tags-completion-table) predicate)
    )
  )
)

      

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol)
)

; Proxy settings
; 
;

; ======================================================================
;                               C++
; ======================================================================

; Indentation preferences
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
)
(add-hook 'c-mode-common-hook 'my-indent-setup)

; Code completion
; ELPA install: cl-lib, company
(add-to-list 'load-path "~/.emacs.d/elpa/company-0.8.3")
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

; credit: tuhdo.github.io/c-ide.html
(setq company-backends (delete 'company-semantic company-backends))

(defun company-c-mode-hook()
  (define-key c-mode-map (kbd "<C-tab>") 'company-complete)
)

(defun company-c++-mode-hook()
  (define-key c++-mode-map (kbd "<C-tab>") 'company-complete)
)

(add-hook 'c-mode-hook 'company-c-mode-hook)
(add-hook 'c++-mode-hook 'company-c++-mode-hook)

; ----------------------------------------------------------------------
;                     company-mode settings
; ----------------------------------------------------------------------
; bigger popup window
(setq company-tooltip-limit 20)
; decrease delay before autocompletion popup shows
(setq company-idle-delay .3)
; remove blinking
(setq company-echo-delay 0)
; start autocompletion only after typing
(setq company-begin-commands '(self-insert-command))
; include paths to some important headers
(setq company-clang-arguments '("-I/usr/include"
				"-I/Users/ptran516/Dev/dlib"
				)
)
