;;; ui.el --- UI and Appearance Settings -*- lexical-binding: t; -*-

;; --- Fonts
(defun my/is-wsl-p ()
  "Return non-nil if running under WSL."
  (and (eq system-type 'gnu/linux)
       (getenv "WSL_DISTRO_NAME")))

(defvar my/font-size
  (cond ((my/is-wsl-p) 95) ; WSLg scaling is often 1.5x or 2x higher than native Linux
        (t 130)))

(defvar my/font-fallbacks
  '("JetBrains Mono" "Source Code Pro" "Hack" "Menlo" "DejaVu Sans Mono" "Ubuntu Mono" "Monospace"))

(defun my/set-best-font (&optional frame)
  "Configure the best available font and apply OS-specific rendering optimizations."
  (when (display-graphic-p frame)
    (when-let ((font (cl-find-if (lambda (f) (x-list-fonts f frame)) my/font-fallbacks)))
      (let ((font-spec (if (eq system-type 'gnu/linux) (concat font ":antialias=true:hinting=true") font)))
        (set-face-attribute 'default frame :font font-spec :height my/font-size)))

    ;; OS Specific Visual Polish
    (cond
     ((eq system-type 'darwin)
      (set-frame-parameter frame 'ns-appearance 'dark)
      (set-frame-parameter frame 'ns-transparent-titlebar t))
     ((my/is-wsl-p)
      (set-frame-parameter frame 'alpha-background 100)
      ;; Remove the ugly title bar (requires a window manager that respects it)
      (set-frame-parameter frame 'undecorated t)))))

(if (display-graphic-p) (my/set-best-font))
(add-hook 'after-make-frame-functions #'my/set-best-font)

;; --- GUI Tweaks & Cleanup
(setq inhibit-startup-message t
      use-dialog-box nil
      use-file-dialog nil
      x-gtk-use-system-tooltips nil)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-font-lock-mode 1)

;; --- Themes & Mode Line
(defvar my/active-theme 'nord ; 'catppuccin, 'kaolin, or 'nord
  "The current active theme.")

(use-package kaolin-themes :ensure t)
(use-package nord-theme :ensure t)
(use-package catppuccin-theme
  :ensure t
  :init (setq catppuccin-flavor 'macchiato))

;; Theme frame support (crucial for daemon/terminal colors)
(defun my/apply-theme (&optional frame)
  "Apply the active theme with correct settings for the current frame."
  (ignore frame)
  (let ((theme (pcase my/active-theme
                 ('kaolin 'kaolin-temple)
                 (t my/active-theme))))
    (load-theme theme t)))
(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f (my/apply-theme f))))
(unless (daemonp) (my/apply-theme))

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
