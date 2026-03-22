;;; packages.el --- Core Infrastructure Packages -*- lexical-binding: t; -*-

;; Undo Tree
(use-package undo-tree
  :ensure t
  :demand t
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :init
  (setq evil-undo-system 'undo-tree)
  (setq undo-tree-history-directory-alist `(("." . ,my/emacs-auto-save-dir)))
  :config
  (global-undo-tree-mode 1))

;; Ivy / Counsel
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

;; Projectile
(use-package projectile
  :after ivy
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien))

(use-package counsel-projectile
  :after projectile
  :ensure t)

;; Git
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :ensure t)

(use-package diff-hl
  :after magit
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Evil-mode Leader
(use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "."   'mode-line-other-buffer
    ":"   'eval-expression
    "b"   'ivy-switch-buffer
    "a"   'counsel-projectile-find-file
    "f"   'counsel-find-file
    "g"   'gptel              ; "g" for gptel / Gemini
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
    "x"   'counsel-M-x)
  (global-evil-leader-mode))

;; Evil-mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  :config
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (evil-mode 1)
  (defun my/evil-emacs-state-modes-setup ()
    (dolist (mode '(dired-mode eshell-mode grep-mode))
      (add-to-list 'evil-emacs-state-modes mode)))
  (my/evil-emacs-state-modes-setup)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Maps
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  
  ;; Custom Normal Bindings
  (define-key evil-normal-state-map (kbd "C-c p p") 'projectile-switch-project)
  (define-key evil-normal-state-map (kbd "C-c p f") 'projectile-find-file)
  (define-key evil-normal-state-map "\C-w" 'evil-delete)
  (define-key evil-normal-state-map "\C-y" 'yank)

  ;; Custom Insert Bindings
  (define-key evil-insert-state-map (kbd "C-c p p") 'projectile-switch-project)
  (define-key evil-insert-state-map (kbd "C-c p f") 'projectile-find-file)
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-r" 'search-backward)

  ;; Custom Visual Bindings
  (define-key evil-visual-state-map "\C-w" 'evil-delete)
  (define-key evil-visual-state-map "\C-y" 'yank))

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

;; Completion & Checking
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5
        company-show-numbers t)
  :config
  (global-company-mode 1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode)

(use-package hippie-expand
  :bind (("M-/" . hippie-expand))
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

;; macOS Shell Environment (Crucial for API keys)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; AI Integration
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gemini-3.1-flash) ; Use the latest fast model
  (setq gptel-backend (gptel-make-gemini "Gemini"
                                         :key (getenv "GEMINI_API_KEY")
                                         :stream t))
  ;; Safeguards
  (setq-default gptel-max-tokens 2048) ; A bit more generous for 3.1
  (setq gptel-directives
        '((default . "You are a concise assistant. Provide short, direct answers. Avoid conversational filler.")
          (programmer . "You are an expert programmer. Provide code snippets and brief explanations. Use modern best practices."))))

(provide 'packages)
