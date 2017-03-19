 ;; global_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.2

;; =============== ;;
;;  Configuration  ;;
;; =============== ;;
(defconst emacs-backup-dir "~/.emacs.backup/" "directory backup files")
(defconst emacs-auto-save-dir "~/.emacs.autosave/" "directory auto-save files")
(defconst my/markdown-command "/usr/bin/pandoc" "program used for markdown generation")
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
(defvar my/font-type "Source Code Pro-10:antialiasing=True:hinting=True")
;; (defvar my/font-type "Inconsolata-12:antialiasing=True:hinting=True")
(defun font-exists-p (font) "check if font exists" (if (null (x-list-fonts font)) nil t))
(if (window-system)
    (if (font-exists-p my/font-type)
        (set-face-attribute 'default nil :font my/font-type)))

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

;; Nice-to-have keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode) ; auto-refresh ibuffer :B1:

(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-S-n") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (forward-line -5)))
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; Indentation for org source code blocks
(setq org-src-tab-acts-natively t)

;; Switching through windows
(use-package windmove
  :bind
  (("C-c w a" . windmove-left)
   ("C-c w w" . windmove-up)
   ("C-c w d" . windmove-right)
   ("C-c w s" . windmove-down)))

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
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; smart-mode-line
;; ---------------------------------------------------------------------------
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  :config
  (sml/setup))

;; Ibuffer-vc
;; ---------------------------------------------------------------------------
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

;; Origami
;; ---------------------------------------------------------------------------
(use-package origami
  :ensure t
  :bind
  (("C-c o u" . origami-open-node-recursively)
   ("C-c o U" . origami-open-all-nodes)
   ("C-c o f" . origami-close-node-recursively)
   ("C-c o F" . origami-close-all-nodes)
   ("C-c o T" . origami-toggle-all-nodes)
   ("C-c o s" . origami-show-only-node)
   ("C-c o C-_" . origami-undo)
   ("C-c o M-_" . origami-redo))
  :config
  (global-origami-mode))

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
  (("M-/" . hippie-expand)))

;; Fill Column Indicator
;; ---------------------------------------------------------------------------
(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column 80)
  (setq-default fci-rule-color "gray")
  (setq-default fci-rule-width 5)
  (add-hook 'prog-mode-hook #'fci-mode))

;; Whitespace
;; ---------------------------------------------------------------------------
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (setq whitespace-line-column 100))

;; Undo-tree
;; ---------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :demand t
  :bind
  (("C-_" . undo)
   ("M-_" . redo)
   ("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; Helm
;; ---------------------------------------------------------------------------
(use-package helm
  :if (not (version< emacs-version "24.3"))
  :ensure t
  :demand t
  :diminish helm-mode
  :init
  (setq helm-candidate-number-limit 100
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-buffers-fuzzy-matching t
        helm-quick-update t
        helm-ff-skip-boring-files t
        helm-split-window-in-side-p t) ;; fixes popwin-like behavior
  :bind
  (("C-c h"   . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-buffers-list)
   ("M-y"     . helm-show-kill-ring)
   ("M-x"     . helm-M-x)
   ("C-x c o" . helm-occur)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action))
  :config
  (require 'helm-config)
  (helm-mode 1))

(use-package helm-swoop
  :after helm
  :no-require t
  :ensure t
  :init
  (setq helm-swoop-split-window-function #'helm-default-display-buffer)
  :bind
  (("C-S-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   ("C-c M-p" . helm-multi-swoop-projectile)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)))

(use-package helm-descbinds
  :after helm
  :no-require t
  :ensure t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

;; Projectile
;; ---------------------------------------------------------------------------
(use-package projectile
  :after helm
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (if (featurep 'helm)
      (setq projectile-completion-system 'helm))
  (setq projectile-indexing-method 'alien))

(use-package helm-projectile
  :after projectile
  :ensure t
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile-find-file))

;; Magit
;; ---------------------------------------------------------------------------
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :ensure t
  :bind
  (("C-c m" . magit-status)))

;; Popwin
;; ---------------------------------------------------------------------------
; tuhdo's helm display variables and functions (for spacemacs)
(defvar helm-display-buffer-regexp `(,(rx bos "*" (* nonl) "helm" (* nonl) "*" eos)
                                     (display-buffer-in-side-window)
                                     (inhibit-same-window . t)
                                     (window-height . 0.4)))
(defvar tmp-display-buffer-alist nil)

(use-package popwin
  :if (not (version< emacs-version "22"))
  :ensure t
  :config
  (setq popwin:special-display-config
        '(("*Help*"            :height 0.4 :stick t)
          ("*Occur*"           :position bottom :height 0.3)
          (magit-status-mode   :position bottom :noselect t :height 0.35)
          ("*magit-commit*"    :position bottom :noselect t :height 0.35 :stick nil)
          ("*magit-diff*"      :position bottom :noselect t :height 0.35)
          ("*magit-edit-log*"  :position bottom :noselect t :height 0.35)
          ("*magit-process*"   :position bottom :noselect t :height 0.35)
          ("*grep*"            :position bottom :noselect t :height 0.5 :stick t :dedicated t)
          ("*Compile-Log"      :height 0.4 :stick t)
          ("*eshell*"          :height 0.4)))

  ; tuhdo's helm display functions (for spacemacs) continued
  (defun display-helm-at-bottom ()
    (let ((display-buffer-base-action '(nil)))
      (setq tmp-display-buffer-alist display-buffer-alist)
      (setq display-buffer-alist (list helm-display-buffer-regexp))
      (popwin-mode -1)))
  (defun restore-previous-display-config ()
    (popwin-mode 1)
    (setq display-buffer-alist tmp-display-buffer-alist)
    (setq tmp-display-buffer-alist nil))

  (add-hook 'helm-after-initialize-hook #'display-helm-at-bottom)
  (add-hook 'helm-cleanup-hook #'restore-previous-display-config)
  (popwin-mode 1))

;; Company
;; ---------------------------------------------------------------------------
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5
        company-echo-delay 0
        company-show-numbers t)
  (if (window-system)
      (bind-key "C-<tab>" 'company-complete)
    ; C-<tab> does not work in terminal mode
    (bind-key "C-c c" 'company-complete))
  :config
  (global-company-mode 1))

;; Yasnippet
;; ---------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :diminish yasnippet-mode
  :bind
  (("C-c y" . yas-expand))
  :init
  (setq yas-snippet-dirs (concat dot-d-dir "snippets"))
  :config
  (yas-global-mode 1))

;; Flycheck
;; ---------------------------------------------------------------------------
(use-package flycheck
  :if (not (version< emacs-version "24.3"))
  :ensure t
  :diminish flycheck-mode)

;; Google-this
;; ---------------------------------------------------------------------------
(use-package google-this
  :ensure t
  :config
  (google-this-mode 1)
  (global-set-key (kbd "C-c g") 'google-this-mode-submap))

;; Markdown-Mode
;; ---------------------------------------------------------------------------
(use-package markdown-mode
  :if (eq system-type 'gnu/linux)
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
;; ---------------------------------------------------------------------------
(use-package flymd
  :after markdown-mode
  :ensure t
  :if (eq system-type 'gnu/linux)
  :config
  (defun my/flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my/flymd-browser-function))
