;; Stop that noob shit at startup
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Stop the blinking cursor
(blink-cursor-mode -1)

;; Keep backup(~) files in specified folder
(setq backup-directory-alist `((".*" . ,emacs-backup-dir)))

;; Set auto-save directory
(setq auto-save-file-transforms `((".*" ,emacs-auto-save-dir t)))

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

;; Autofill comments
(setq comment-auto-fill-only-comments t)

;; Nice-to-have keybindings
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-S-n") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (forward-line -5)))
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; ===========================================================================
;;                              CUDA MODE
;; ===========================================================================
;; Add cuda to the mode list.
(setq auto-mode-alist
      (append
       '(("\\.cu\\'" . cuda-mode))
       auto-mode-alist))

(autoload 'cuda-mode (concat dot-d-dir "packages/cuda-mode.el"))

;; ===========================================================================
;;                              CMAKE MODE
;; ===========================================================================
;; Add cmake to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(autoload 'cmake-mode cmake-mode-el t)

;; ===========================================================================
;;                          PACKAGE SPECIFICS
;; ===========================================================================
;; Emacs themes
(use-package hc-zenburn-theme
  :ensure t)

;; Hippie-expand
;; ---------------------------------------------------------------------------
(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill))
  :bind
  ("M-/" . hippie-expand))

;; Highlight parentheses
;; ---------------------------------------------------------------------------
(use-package highlight-parentheses
  :ensure t)

(use-package autopair
  :ensure t)

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
(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column 80)
  (setq-default fci-rule-color "gray")
  (setq-default fci-rule-width 5)
  (add-hook 'c-mode-common-hook   (lambda () (fci-mode 1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (fci-mode 1)))
  (add-hook 'java-mode-hook       (lambda () (fci-mode 1)))
  (add-hook 'python-mode-hook     (lambda () (fci-mode 1)))
  (add-hook 'text-mode-hook       (lambda () (fci-mode 1))))

;; Whitespace
;; ---------------------------------------------------------------------------
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq whitespace-line-column 100))

;; Undo-tree
;; ---------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :bind
  (("C-_" . undo)
   ("M-_" . redo))
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t))

;; Helm
;; ---------------------------------------------------------------------------
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (setq helm-candidate-number-limit 100
          helm-idle-delay 0.0
          helm-input-idle-delay 0.01
          helm-buffers-fuzzy-matching t
          helm-quick-update t
          helm-ff-skip-boring-files t)
  (helm-mode 1)
  :bind
  (("C-c h"   . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b"   . helm-buffers-list)
   ("M-y"     . helm-show-kill-ring)
   ("M-x"     . helm-M-x)
   ("C-x c o" . helm-occur)
   ("C-x c s" . helm-swoop))
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action))

(use-package helm-gtags
  :ensure t
  :defer t
  :init
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "C-c g"
        helm-gtags-suggested-key-mapping t)
  :bind
  (("C-c g a" . helm-gtags-tags-in-this-function)
   ("C-c g C-s" . helm-gtags-select)
   ("M-." . helm-gtags-dwim)
   ("M-," . helm-gtags-pop-stack)
   ("C-c <" . helm-gtags-previous-history)
   ("C-c >" . helm-gtags-next-history))
  :config
  (helm-gtags-mode 1)
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'java-mode-hook 'helm-gtags-mode))

(use-package helm-swoop
 :ensure t
 :defer t
 :bind
 (("C-S-s" . helm-swoop)
  ("M-i" . helm-swoop)
  ("M-I" . helm-swoop-back-to-last-point)
  ("C-c M-i" . helm-multi-swoop)
  ("C-x M-i" . helm-multi-swoop-all))
 :config
 (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
 (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))

;; Projectile
;; ---------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))

(use-package helm-projectile
  :ensure t
  :defer t
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile-find-file))

;; Magit
;; ---------------------------------------------------------------------------
(use-package magit
  :ensure t
  :bind
  (("C-c m" . magit-status)))

;; Popwin
;; ---------------------------------------------------------------------------
(use-package popwin
  :ensure t
  :config
  (setq popwin:special-display-config
        '(("*Help*" :height 0.4 :stick t)
          ("*Occur*" :position bottom :height 0.3)
          (magit-status-mode :position bottom :noselect t :height 0.3)
          ("*magit-commit*" :position bottom :noselect t :height 0.3 :stick nil)
          ("*magit-diff*" :position bottom :noselect t :height 0.3)
          ("*magit-edit-log*" :position bottom :noselect t :height 0.2)
          ("*magit-process*" :position bottom :noselect t :height 0.2)
          ("*Compile-Log" :height 20 :stick t)
          ("*Python*" :stick t)
          ("*eshell*" :height 0.3)))
  (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
  (add-hook 'helm-after-initialize-hook (lambda ()
                                          (popwin:display-buffer helm-buffer t)
                                          (popwin-mode -1)))
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1))))

;; Company
;; ---------------------------------------------------------------------------
(use-package company
  :ensure t
  :defer t
  :bind
  ("C-<tab>" . company-complete)
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5
        company-echo-delay 0
        company-show-numbers t)
  :config
  (global-company-mode 1))

;; Yasnippet
;; ---------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs (concat dot-d-dir "yasnippet-snippets"))
  :config
  (yas-global-mode 1))
