;;; ui.el --- UI and Appearance Settings -*- lexical-binding: t; -*-

;; Fonts
(setq my/font-type "Source Code Pro:antialiasing=True:hinting=True")
(setq my/font-size 130)

(defun font-exists-p (font)
  "Check if font exists"
  (if (null (x-list-fonts font)) nil t))

(if (display-graphic-p)
    (when (font-exists-p my/font-type)
      (set-face-attribute 'default nil :font my/font-type)))
(set-face-attribute 'default nil :height my/font-size)

;; Daemon font support
(defun my/set-frame-font (frame)
  (select-frame frame)
  (when (font-exists-p my/font-type)
    (set-frame-font my/font-type)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/set-frame-font))

;; Clean up UI
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-font-lock-mode 1)

;; Themes & Mode Line
(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-temple t))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  :config
  (sml/setup))

;; Parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (setq hl-paren-colors '("DarkSeaGreen1" "SeaGreen1" "SeaGreen3" "SeaGreen4"))
  (add-hook 'prog-mode-hook (lambda () (highlight-parentheses-mode t))))

;; Fringe indicators
(use-package display-fill-column-indicator
  :init
  (setq-default fill-column 120)
  :config
  (add-hook 'c-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'c++-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'python-mode-hook #'display-fill-column-indicator-mode))

(provide 'ui)
