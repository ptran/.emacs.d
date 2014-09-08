; Hide the menu and tool bars
(menu-bar-mode 0)
(tool-bar-mode 0)

; Stop the blinking cursor
(blink-cursor-mode 0)

; Stop the BS at start-up
(setq inhibit-startup-message t)

; Stop producing autosave(#) and backup(~) files
(setq auto-save-default nil)
(setq make-backup-files nil)

; Stop making sounds
(setq ring-bell-function (lambda()))

; Yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

; Clipboard enabled
(setq x-select-enable-clipboard t)

; Encoding setting
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
 
; Syntax highlighting
(global-font-lock-mode t)

; Require a newline at the end of files
(setq require-final-newline t)

; Change buffer listing to ibuffer
(defalias 'list-buffers 'ibuffer)

; Change buffer switching system
; credit: http://stackoverflow.com/questions/7394289/how-can-i-more-easily-switch-between-buffers-in-emacs
(iswitchb-mode 1)
(setq iswitchb-buffer-ignore '("^ "
			       "*Completions*"
			       "*Shell Command Output*"
			       "*Messages*"
			       "Async Shell Command")
)

; No tab indents
(setq-default indent-tabs-mode nil)

; Awesome scheme: Zenburn
; ---------------------------------------------------------------------------
; credit (and pull): https://github.com/bbatsov/zenburn-emacs(.git)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

; Dired Mode
; ---------------------------------------------------------------------------
; TODO

; Hippie-expand
; ---------------------------------------------------------------------------
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

; Highlight parentheses
; ---------------------------------------------------------------------------
; credit: http://www.emacswiki.org/emacs/HighlightParentheses
(require 'highlight-parentheses)
(require 'autopair)

(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append (if autopair-handle-action-fns
			       autopair-handle-action-fns
			     '(autopair-default-handle-action)
			     )
			   '( (lambda (action pair pos-before)
				(hl-paren-color-update))
			      )))))

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode t)

; Fill Column Indicator
; ---------------------------------------------------------------------------
(require 'fill-column-indicator)

(setq-default fill-column 80)
(setq fci-rule-color "white")
(setq fci-rule-width 5)

(add-hook 'c-mode-common-hook   (lambda () (fci-mode 1) ))
(add-hook 'emacs-lisp-mode-hook (lambda () (fci-mode 1) ))
(add-hook 'java-mode-hook       (lambda () (fci-mode 1) ))
(add-hook 'python-mode-hook     (lambda () (fci-mode 1) ))
(add-hook 'text-mode-hook       (lambda () (fci-mode 1) ))

; Undo-tree
; ---------------------------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode t)

; Company 
; (Auto-completion system)
; ---------------------------------------------------------------------------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

; bigger popup window
(setq company-tooltip-limit 20)
; decrease delay before autocompletion popup shows
(setq company-idle-delay .3)
; require three letters before popup
(setq company-minimum-prefix-length 3)
; remove blinking
(setq company-echo-delay 0)
; start autocompletion only after typing
(setq company-begin-commands '(self-insert-command))
; include paths to some important headers
(setq company-clang-arguments '("-I /usr/include"
				"-I /Users/ptran516/Dev/dlib"
				)
)

; Smex 
; (M-x enhancement)
; ---------------------------------------------------------------------------
(require 'smex)
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")

(global-set-key (kbd "M-x") 'smex)

; Popwin
; ---------------------------------------------------------------------------
(require 'popwin)
(popwin-mode 1)

; Helm
; ---------------------------------------------------------------------------
; TODO
