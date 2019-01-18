;; global_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran/.emacs.d

;;
(defvar my/emacs-backup-dir (concat my/dot-d-dir "backup") "Directory for backup files")
(defvar my/emacs-auto-save-dir (concat my/dot-d-dir "auto-save") "Directory for auto-save files")
(setq my/font-type "Source Code Pro:antialiasing=True:hinting=True")
(setq my/font-size 100)
(defvar my/markdown-command "/usr/bin/pandoc" "Program used for markdown generation")
;;

;; Stop that noob shit at startup
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Stop the blinking cursor
(blink-cursor-mode -1)

;; Keep backup(~) files in specified folder
(setq backup-directory-alist `((".*" . ,my/emacs-backup-dir)))

;; Set auto-save directory (make sure my/emacs-auto-save-dir ends with "/")
(unless (string-equal (substring my/emacs-auto-save-dir -1 nil) "/")
  (setq my/emacs-auto-save-dir (concat my/emacs-auto-save-dir "/")))
(setq auto-save-file-name-transforms `((".*" ,my/emacs-auto-save-dir t)))

;; Stop making sounds
(setq ring-bell-function 'ignore)

;; Change options yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Clipboard enabled
(setq x-select-enable-clipboard t)

;; Check if the font exists and set it
(defun font-exists-p (font)
  "Check if font exists"
  (if (null (x-list-fonts font))
      nil
    t))

;; Set font for stand-alone GUI emacs
(if (display-graphic-p)
    (if (font-exists-p my/font-type)
        (set-face-attribute 'default nil :font my/font-type)))
(set-face-attribute 'default nil :height my/font-size)

;; Set font for generated frames (daemon)
(defun my/set-frame-font (frame)
  "Sets frame font if font exists"
       (select-frame frame)
       (if (font-exists-p my/font-type)
           (set-frame-font my/font-type)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/set-frame-font))

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
      (save-buffers-kill-emacs)
    (message "Canceled exit")))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; Autofill comments
(setq comment-auto-fill-only-comments t)

;; Parentheses handling
(electric-pair-mode 1)
(show-paren-mode 1)

;; Spell check in text modes
(add-hook 'text-mode-hook #'turn-on-flyspell)

;; Nice-to-have keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode) ; auto-refresh ibuffer :B1:

(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; Prevent eshell from shifting to the bottom of the buffer after executing a command
;; https://emacs.stackexchange.com/questions/28819/eshell-goes-to-the-bottom-of-the-page-after-executing-a-command/28821
(defun my/eshell-mode-setup ()
            (remove-hook 'eshell-output-filter-functions
                         'eshell-postoutput-scroll-to-bottom))
(add-hook 'eshell-mode-hook 'my/eshell-mode-setup)

;; Indentation for org source code blocks
(setq org-src-tab-acts-natively t)

;; Store the custom settings in a separate file
(setq custom-file (concat my/dot-d-dir "custom.el"))
(load custom-file 'noerror)

;; ===========================================================================
;;                          PACKAGE SPECIFICS
;; ===========================================================================
;; Emacs themes
(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-aurora t))

;; highlight-parentheses
;; ---------------------
(use-package highlight-parentheses
  :ensure t
  :config
  (setq hl-paren-colors '("DarkSeaGreen1" "SeaGreen1" "SeaGreen3" "SeaGreen4"))
  (add-hook 'prog-mode-hook (lambda () (highlight-parentheses-mode t))))

;; smart-mode-line
;; ---------------
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  :config
  (sml/setup))

;; ibuffer-vc
;; ----------
(use-package ibuffer-vc
  :demand t
  :ensure t
  :config
  (defun my/ibuffer-vc-hook ()
    ;; ibuffer-vc-set-filter-groups-by-vc-root without buffer update for popwin
    (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook 'ibuffer-hook #'my/ibuffer-vc-hook))

;; hippie-expand
;; -------------
(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill))
  :bind
  (("M-/" . hippie-expand)))

;; fill Column Indicator
;; ---------------------
(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column 120)
  (setq-default fci-rule-color "gray")
  (setq-default fci-rule-width 2)
  (add-hook 'c-mode-hook #'fci-mode)
  (add-hook 'c++-mode-hook #'fci-mode)
  (add-hook 'python-mode-hook #'fci-mode))

;; whitespace
;; ----------
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (setq whitespace-line-column 120))

;; undo-tree
;; ---------
(use-package undo-tree
  :ensure t
  :demand t
  :bind
  (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;; ivy
;; ---
(use-package ivy
  :ensure t
  :demand t
  :diminish ivy-mode
  :init
  (setq ivy-height 15)
  (setq ivy-count-format "[%d/%d] ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

;; Mainly for ivy-wgrep-change-to-wgrep-mode
(use-package wgrep
  :ensure t)

;; projectile
;; ----------
(use-package projectile
  :after ivy
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (if (featurep 'ivy)
      (setq projectile-completion-system 'ivy))
  (setq projectile-indexing-method 'alien))

(use-package counsel-projectile
  :after projectile
  :ensure t)

;; magit
;; -----
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :ensure t)

;; company
;; -------
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5
        company-echo-delay 0
        company-show-numbers t)
  :config
  (global-company-mode 1))

;; yasnippet
;; ---------
(use-package yasnippet
  :ensure t
  :diminish yasnippet-mode
  :init
  (setq my/yas-snippet-dir (concat my/dot-d-dir "snippets"))
  (setq yas-snippet-dirs '(my/yas-snippet-dir))
  :config
  (yas-global-mode 1))

;; flycheck
;; --------
(use-package flycheck
  :if (not (version< emacs-version "24.3"))
  :ensure t
  :diminish flycheck-mode)

;; markdown-mode
;; -------------
(use-package markdown-mode
  :ensure t
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command my/markdown-command))

;; flymd
;; -----
(use-package flymd
  :if (executable-find "firefox")
  :after markdown-mode
  :ensure t
  :if (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
  :config
  (defun my/flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my/flymd-browser-function))

;; evil
;; ----
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  ;; Use Emacs state in these additional modes.
  (dolist (mode '(dired-mode eshell-mode grep-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Remove Insert mode from eshell
  (delete 'eshell-mode evil-insert-state-modes)

  ;; https://wikemacs.org/wiki/Evil
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  ;; Normal
  (define-key evil-normal-state-map "\C-w" 'evil-delete)
  (define-key evil-normal-state-map "\C-y" 'yank)
  ;; Insert
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-r" 'search-backward)
  ;; Visual
  (define-key evil-visual-state-map "\C-w" 'evil-delete)
  (define-key evil-visual-state-map "\C-y" 'yank))

(use-package evil-leader
  :after evil
  :ensure t
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","   'other-window
    "."   'mode-line-other-buffer
    ":"   'eval-expression
    "b"   'ivy-switch-buffer
    "f"   'counsel-find-file
    "k"   'kill-this-buffer
    "l"   'whitespace-mode
    "L"   'delete-trailing-whitespace
    "m"   'magit-status
    "o"   'delete-other-windows
    "pp"  'projectile-switch-project
    "pb"  'projectile-switch-to-buffer
    "pf"  'projectile-find-file
    "psg" 'projectile-grep
    "py"  'counsel-yank-pop
    "s"   'swiper
    "wa"  'windmove-left ;; switching through windows
    "ws"  'windmove-down
    "ww"  'windmove-up
    "wd"  'windmove-right
    "x"   'counsel-M-x)
  (global-evil-leader-mode))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-indent-textobject
  :after evil
  :ensure t)

(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package evil-magit
  :after evil magit
  :ensure t)
