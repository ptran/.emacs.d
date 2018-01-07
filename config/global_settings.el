;; global_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d

;; Check if configurables exist; if not, set them to default values
(unless (boundp 'emacs-backup-dir) (defconst emacs-backup-dir "/tmp" "Directory for backup files"))
(unless (boundp 'emacs-auto-save-dir) (defconst emacs-auto-save-dir "/tmp" "Directory for auto-save files"))
(unless (boundp 'my/markdown-coommand) (defconst my/markdown-command "/usr/bin/pandoc" "Program used for markdown generation"))
;;

;; Stop that noob shit at startup
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Stop the blinking cursor
(blink-cursor-mode -1)

;; Keep backup(~) files in specified folder
(setq backup-directory-alist `((".*" . ,emacs-backup-dir)))

;; Set auto-save directory
(setq auto-save-file-name-transforms `((".*" ,emacs-auto-save-dir t)))

;; Stop making sounds
(setq ring-bell-function 'ignore)

;; Change options yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Clipboard enabled
(setq x-select-enable-clipboard t)

;; Check if the font exists and set it
(defvar my/font-type "Source Code Pro:antialiasing=True:hinting=True")
(defun font-exists-p (font) "check if font exists"
       (if (null (x-list-fonts font))
           nil
         t))

;; Set font for stand-alone GUI emacs
(if (display-graphic-p)
    (if (font-exists-p my/font-type)
        (set-face-attribute 'default nil :font my/font-type)))

(unless (boundp 'my/font-size) (setq my/font-size 100))  ; check if my/font-size is defined; if not, set it to 100
(set-face-attribute 'default nil :height my/font-size)

;; Set font for generated frames (daemon)
(defun my/set-frame-font (frame) "sets frame font if font exists"
       (select-frame frame)
       (if (font-exists-p my/font-type)
           (set-frame-font my/font-type)))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/set-frame-font))

;; Set default frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 140))

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

;; Indentation for org source code blocks
(setq org-src-tab-acts-natively t)

;; Store the custom settings in a separate file
(setq custom-file (concat dot-d-dir "custom.el"))
(load custom-file 'noerror)

;; ===========================================================================
;;                              CUDA MODE
;; ===========================================================================
;; Add cuda to the mode list.
(setq auto-mode-alist
      (append
       '(("\\.cu\\'" . cuda-mode))
       '(("\\.cuh\\'" . cuda-mode))
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
(autoload 'cmake-mode (concat dot-d-dir "packages/cmake-mode.el"))

;; ===========================================================================
;;                          PACKAGE SPECIFICS
;; ===========================================================================
;; Emacs themes
(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-dark t))

;; smart-mode-line
;; ---------------
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  :config
  (sml/setup))

;; Ibuffer-vc
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

;; Hippie-expand
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

;; Fill Column Indicator
;; ---------------------
(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column 120)
  (setq-default fci-rule-color "gray")
  (setq-default fci-rule-width 2)
  (add-hook 'prog-mode-hook #'fci-mode))

;; Whitespace
;; ----------
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (setq whitespace-line-column 120))

;; Undo-tree
;; ---------
(use-package undo-tree
  :ensure t
  :demand t
  :bind
  (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;; Ivy
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
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1))

;; Mainly for ivy-wgrep-change-to-wgrep-mode
(use-package wgrep
  :ensure t)

;; Projectile
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

;; Magit
;; -----
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :ensure t)

;; Company
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

;; Yasnippet
;; ---------
(use-package yasnippet
  :ensure t
  :diminish yasnippet-mode
  :init
  (setq my/yas-snippet-dir (concat dot-d-dir "snippets"))
  (setq yas-snippet-dirs '(my/yas-snippet-dir))
  :config
  (yas-global-mode 1))

;; Flycheck
;; --------
(use-package flycheck
  :if (not (version< emacs-version "24.3"))
  :ensure t
  :diminish flycheck-mode)

;; Markdown-Mode
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
  :config
  (evil-mode 1)
  ;; Use Emacs state in these additional modes.
  (dolist (mode '(dired-mode eshell-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package evil-leader
  :after evil
  :ensure t
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","   'other-window
    "."   'mode-line-other-buffer
    ":"   'eval-expression
    "aa"  'align-regexp
    "b"   'ivy-switch-buffer
    "f"   'find-file
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
    "wh"  'windmove-left ;; Switching through windows
    "wj"  'windmove-down
    "wk"  'windmove-up
    "wl"  'windmove-right
    "x"   'counsel-M-x
    "y"   'yas-expand)
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
