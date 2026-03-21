;; init.el --- Philip Tran's Emacs Configuration -*- lexical-binding: t; -*-

;; Fix for evil-collection: must be set before any evil packages load
(setq evil-want-keybinding nil)

;; Set emacs directory
(defvar my/dot-d-dir (expand-file-name "~/.emacs.d/") ".emacs.d location")
(defvar my/emacs-backup-dir (concat my/dot-d-dir "backup") "Directory for backup files")
(defvar my/emacs-auto-save-dir (concat my/dot-d-dir "auto-save") "Directory for auto-save files")

;; Org Files
(defvar my/org-task-file "/home/ptran/Dropbox/Documents/Org/todo.org" "Org file for keeping track of tasks")
(defvar my/org-notes-file "/home/ptran/Dropbox/Documents/Org/notes.org" "Org file for taking notes")

;; Package management
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Add config to load-path
(add-to-list 'load-path (concat my/dot-d-dir "config"))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'cl-lib)

;; Auto-compile elisp for performance
(use-package auto-compile
  :ensure t
  :init (setq load-prefer-newer t)
  :config (auto-compile-on-load-mode))

;; Load core configuration categories
(require 'ui)
(require 'editor)
(require 'packages)
(require 'languages)

;; Store custom settings separately
(setq custom-file (concat my/dot-d-dir "custom.el"))
(load custom-file 'noerror)
