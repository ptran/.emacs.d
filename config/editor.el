;;; editor.el --- Editor Behavior and Performance -*- lexical-binding: t; -*-

;; Performance
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq process-adaptive-read-buffering nil)

;; Encoding & Locales
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Mac Specific Settings
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (setq ns-function-modifier 'control))

;; Backups and Auto-saves
(setq backup-directory-alist `((".*" . ,my/emacs-backup-dir)))
(unless (string-equal (substring my/emacs-auto-save-dir -1 nil) "/")
  (setq my/emacs-auto-save-dir (concat my/emacs-auto-save-dir "/")))
(setq auto-save-file-name-transforms `((".*" ,my/emacs-auto-save-dir t)))

;; General Behavior
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil) ;; Use spaces, not tabs

;; Parentheses and pairs
(electric-pair-mode 1)
(show-paren-mode 1)

;; Spelling
(add-hook 'text-mode-hook #'turn-on-flyspell)

;; Keybindings (Core)
(defun ask-before-closing ()
  (interactive)
  (if (y-or-n-p "Are sure you want to exit Emacs? ")
      (save-buffers-kill-emacs)
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; Undo Tree
(use-package undo-tree
  :ensure t
  :demand t
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :init
  (setq undo-tree-history-directory-alist `(("." . ,my/emacs-auto-save-dir)))
  :config
  (global-undo-tree-mode 1))

;; Whitespace
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (setq whitespace-line-column 120))

(provide 'editor)
