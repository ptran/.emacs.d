;;; packages.el --- Core Infrastructure Packages -*- lexical-binding: t; -*-

;; --- Global Variable Declarations
(defvar my/emacs-auto-save-dir)

;; --- General.el
(use-package general
  :ensure t
  :demand t
  :config
  (general-def
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    "."   'mode-line-other-buffer
    ":"   'eval-expression
    "a"   'agent-shell
    "b"   'ivy-switch-buffer
    "f"   'counsel-find-file
    "g"   'gptel
    "G"   'gptel-menu
    "k"   'kill-this-buffer
    "l"   'whitespace-mode
    "L"   'delete-trailing-whitespace
    "m"   'magit-status
    "pp"  'projectile-switch-project
    "pb"  'projectile-switch-to-buffer
    "pf"  'projectile-find-file
    "pj"  'projectile-find-tag
    "psg" 'projectile-grep
    "py"  'counsel-yank-pop
    "s"   'swiper
    "wd"  'delete-window
    "wm"  'delete-other-windows
    "ws"  'split-window-below
    "wv"  'split-window-right
    "wh"  'windmove-left
    "wj"  'windmove-down
    "wk"  'windmove-up
    "wl"  'windmove-right
    "ww"  'other-window
    "x"   'counsel-M-x))

;; --- Undo Tree
(use-package undo-tree
  :ensure t
  :demand t
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :preface
  (defvar evil-undo-system)
  :init
  (setq evil-undo-system 'undo-tree)
  (setq undo-tree-history-directory-alist `(("." . ,my/emacs-auto-save-dir)))
  :config
  (global-undo-tree-mode 1))

;; --- Ivy / Counsel
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

(use-package wgrep
  :ensure t)

;; --- Projectile
(use-package projectile
  :after ivy
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien))

(use-package counsel-projectile
  :after projectile
  :ensure t)

;; --- Git
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :ensure t)

(use-package diff-hl
  :after magit
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; --- Evil
(use-package evil
  :ensure t
  :preface
  (defvar evil-motion-state-map)
  (defvar evil-normal-state-map)
  (defvar evil-visual-state-map)
  (defvar evil-insert-state-map)
  (defvar evil-emacs-state-modes)
  (defvar evil-insert-state-modes)
  (defun my/evil-emacs-state-modes-setup ()
    (dolist (mode '(dired-mode eshell-mode grep-mode))
      (add-to-list 'evil-emacs-state-modes mode)))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (my/evil-emacs-state-modes-setup)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Maps
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  
  ;; Custom Normal Bindings
  (general-define-key
   :states 'normal
   "C-c p p" 'projectile-switch-project
   "C-c p f" 'projectile-find-file
   "C-w"     'evil-delete
   "C-y"     'yank)

  ;; Custom Insert Bindings
  (general-define-key
   :states 'insert
   "C-c p p" 'projectile-switch-project
   "C-c p f" 'projectile-find-file
   "C-y"     'yank
   "C-e"     'end-of-line
   "C-w"     'evil-delete
   "C-r"     'search-backward)

  ;; Custom Visual Bindings
  (general-define-key
   :states 'visual
   "C-w"     'evil-delete
   "C-y"     'yank))

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

;; --- Completion & Checking
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5
        company-show-quick-access t)
  :config
  (global-company-mode 1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode)

(use-package hippie-exp
  :bind (("M-/" . hippie-expand))
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

;; --- macOS Environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; --- LLM Interaction
(use-package agent-shell
  :ensure t)

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gemini-flash-latest)
  (setq gptel-backend 
        (gptel-make-gemini "Gemini"
          :key (getenv "GEMINI_API_KEY")))
  (setq gptel-display-buffer-action '((display-buffer-reuse-window display-buffer-pop-up-window)))
  (defun gptel-quick-rewrite ()
    (interactive)
    (if (use-region-p)
        (gptel-rewrite (region-beginning) (region-end))
      (gptel-rewrite (line-beginning-position) (line-end-position)))))

(provide 'packages)
