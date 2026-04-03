;;; ui.el --- UI and Appearance Settings -*- lexical-binding: t; -*-

;; --- Fonts
(defvar my/font-size 130)
(defvar my/font-fallbacks 
  '("JetBrains Mono" "Source Code Pro" "Hack" "Menlo" "DejaVu Sans Mono" "Ubuntu Mono" "Monospace"))

(defun my/set-best-font (&optional frame)
  "Configure the best available font and apply OS-specific rendering optimizations."
  (when-let ((font (cl-find-if (lambda (f) (x-list-fonts f)) my/font-fallbacks)))
    (let ((font-spec (if (eq system-type 'gnu/linux) (concat font ":antialias=true:hinting=true") font)))
      (set-face-attribute 'default frame :font font-spec :height my/font-size)))
  
  ;; macOS Visual Polish
  (when (and (eq system-type 'darwin) (display-graphic-p))
    (set-frame-parameter frame 'ns-appearance 'dark)
    (set-frame-parameter frame 'ns-transparent-titlebar t)))

(if (display-graphic-p) (my/set-best-font))
(add-hook 'after-make-frame-functions #'my/set-best-font)

;; --- Clean up UI
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-font-lock-mode 1)

;; --- Themes & Mode Line
(defvar my/active-theme 'nord ; Set to 'catppuccin, 'kaolin, or 'nord
  "The current active theme.")

(use-package kaolin-themes
  :ensure t
  :if (eq my/active-theme 'kaolin)
  :config
  (load-theme 'kaolin-galaxy t))

(use-package catppuccin-theme
  :ensure t
  :if (eq my/active-theme 'catppuccin)
  :init
  (setq catppuccin-flavor 'mocha) ; 'latte, 'frappe, 'macchiato, or 'mocha
  :config
  (load-theme 'catppuccin t))

(use-package nord-theme
  :ensure t
  :if (eq my/active-theme 'nord)
  :config
  (load-theme 'nord t))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  :config
  (sml/setup))

;; --- Parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (setq highlight-parentheses-colors '("DarkSeaGreen1" "SeaGreen1" "SeaGreen3" "SeaGreen4"))
  (add-hook 'prog-mode-hook (lambda () (highlight-parentheses-mode t))))

;; --- Fringe indicators
(use-package display-fill-column-indicator
  :init
  (setq-default fill-column 120)
  :config
  (add-hook 'c-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'c++-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'python-mode-hook #'display-fill-column-indicator-mode))

(provide 'ui)
