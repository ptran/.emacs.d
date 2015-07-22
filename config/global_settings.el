;; Stop that noob shit at startup
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Stop the blinking cursor
(blink-cursor-mode -1)

;; Keep autosave(#) and backup(~) files in specified folders
(setq backup-directory-alist `((".*" . , backup-dir)))
(setq auto-save-file-name-transforms `((".*" , auto-save-dir t)))
    
;; Stop making sounds
(setq ring-bell-function 'ignore)

;; Change options yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Clipboard enabled
(setq x-select-enable-clipboard t)

;; UTF-8 encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Display column and line numbers
(line-number-mode 1)
(column-number-mode 1)
 
;; Syntax highlighting
(global-font-lock-mode 1)

;; Require a newline at the end of files
(setq require-final-newline t)

;; Change buffer listing to ibuffer
(defalias 'list-buffers 'ibuffer)

;; No tab indents (use spaces instead)
(setq-default indent-tabs-mode nil)

;; Display function name on the first line
(semantic-mode 1)
(global-semantic-stickyfunc-mode 1)

;; Comment autofill
(defun comment-auto-fill-only-comments ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda () (not (eq (get-text-property (point) 'face) 'font-lock-comment-face)))))

;; Prompt before closing emacs
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; Nice-to-have keybindings
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "C-p") (lambda () (interactive) (next-line -5)))

;; ===========================================================================
;;                              CMAKE MODE
;; ===========================================================================
;; Add cmake to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; Change this to where cmake-mode.el is in your system
(autoload 'cmake-mode cmake-mode-dir t)

;; ===========================================================================
;;                          PACKAGE SPECIFICS
;; ===========================================================================
;; Emacs themes
(load-theme 'hc-zenburn t)

;; Hippie-expand
;; ---------------------------------------------------------------------------
;; let hippie-expand support ctags
(defun tags-complete-tag (string predicate what)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (if (eq what t)
	(all-completions string (tags-complete-table) predicate)
      (try-completion string (tags-completion-table) predicate))))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

;; Highlight parentheses
;; ---------------------------------------------------------------------------
(require 'highlight-parentheses)
(require 'autopair)

(defun autopair-add-on ()
  (setq autopair-handle-action-fns
        (append (if autopair-handle-action-fns autopair-handle-action-fns '(autopair-default-handle-action))
                '((lambda (action pair pos-before) (hl-paren-color-update))))))

(add-hook 'highlight-parentheses-mode-hook 'autopair-add-on)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode 1)

;; Fill Column Indicator
;; ---------------------------------------------------------------------------
(require 'fill-column-indicator)

(setq-default fill-column 80)
(setq-default fci-rule-color "gray")
(setq-default fci-rule-width 5)

(add-hook 'c-mode-common-hook   (lambda () (fci-mode 1) ))
(add-hook 'emacs-lisp-mode-hook (lambda () (fci-mode 1) ))
(add-hook 'java-mode-hook       (lambda () (fci-mode 1) ))
(add-hook 'python-mode-hook     (lambda () (fci-mode 1) ))
(add-hook 'text-mode-hook       (lambda () (fci-mode 1) ))

;; Undo-tree
;; ---------------------------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Company 
;; ---------------------------------------------------------------------------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-tooltip-limit 20
      company-idle-delay nil)

;; any edits to company-clang-arguments are presumably safe if the input is a
;; list
(put 'company-clang-arguments 'safe-local-variable #'listp)

;; Helm
;; ---------------------------------------------------------------------------
(add-to-list 'load-path (concat dot-d-dir "packages/helm"))

(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(setq helm-quick-update                     t 
      helm-buffers-fuzzy-matching           t 
      helm-move-to-line-cycle-in-source     t 
      helm-ff-search-library-in-sexp        t 
      helm-scroll-amount                    8 
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; man pages
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Setting up gtags
(require 'helm-gtags)

(setq helm-gtags-ignore-case t
      helm-gtags-auto-update nil
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-suggested-key-mapping t)

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) 
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(global-set-key (kbd "M-x") 'helm-M-x)            
(global-set-key (kbd "M-y") 'helm-show-kill-ring) 
(global-set-key (kbd "C-x b") 'helm-mini)         
(global-set-key (kbd "C-x C-f") 'helm-find-files) 
(global-set-key (kbd "C-c h o") 'helm-occur)      

;; gtags key bindings
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; Projectile
;; ---------------------------------------------------------------------------
(require 'projectile)
(projectile-global-mode 1)

;; use helm for all things projectile
(setq projectile-completion-system 'helm)

;; use other resources aside from elisp to index items
(setq projectile-indexing-method 'alien)

;; display  projects, and then files in a chosen project
(setq helm-projectile-sources-list '(helm-source-projectile-projects
                                     helm-source-projectile-files-list))
(setq projectile-switch-project-action 'helm-projectile)

;; Popwin
;; ---------------------------------------------------------------------------
(require 'popwin)
(popwin-mode 1)

;; Get popwin to display pages for certain functions
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
(push '("^\*magit .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*magit-.+\*$" :regexp t) popwin:special-display-config)
