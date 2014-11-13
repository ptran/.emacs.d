;; Stop that noob shit at startup
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Stop the blinking cursor
(blink-cursor-mode -1)

;; Stop producing autosave(#) and backup(~) files
(setq auto-save-default nil
      make-backup-files nil)

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

;; Add cmake to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; Change this to where cmake-mode.el is in your system
(autoload 'cmake-mode cmake-mode-dir t)

;; Emacs themes
(load-theme 'zenburn t)

;; Hippie-expand
;; ---------------------------------------------------------------------------
;; credit: http://github.com/redgaurdtoo/emacs.d/blob/master/init-hippie-expand.el

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
;; credit: http://www.emacswiki.org/emacs/HighlightParentheses
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

;;;; Fill Column Indicator
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

;;;; Undo-tree
;; ---------------------------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode t)

;;;; Company 
;; info: auto-completion system
;; ---------------------------------------------------------------------------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; bigger popup window
(setq company-tooltip-limit 20)
;; decrease delay before autocompletion popup shows
(setq company-idle-delay .3)
;; increase duration before timeout
(setq company-async-timeout 4)
;; require three letters before popup
(setq company-minimum-prefix-length 3)
;; remove blinking
(setq company-echo-delay 0)
;; start autocompletion only after typing
(setq company-begin-commands '(self-insert-command))
;; any edits to company-clang-arguments are presumably safe if the input is a
;; list
(put 'company-clang-arguments 'safe-local-variable #'listp)

;;;; Helm
;; info:   incremental completion and selection narrowing framework
;; credit: tuhdo.github.io/helm-intro.html
;; ---------------------------------------------------------------------------
(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; For man pages
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Setting up gtags
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-prefix-key (kbd "C-c g")
      helm-gtags-suggested-key-mapping t)

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

;;;; Projectile
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

;;;; Popwin
;; ---------------------------------------------------------------------------
(require 'popwin)
(popwin-mode 1)

;; Get popwin to display pages for certain functions
;; credit: https://gist.github.com/syl20bnr/5516054
;; ---------------------------------------------------------------------------
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

;; magit display<
(push '("^\*magit .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*magit-.+\*$" :regexp t) popwin:special-display-config)
