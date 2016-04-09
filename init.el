;; Define path variables for config
(defvar dot-d-dir           "~/.emacs.d/")
(defvar emacs-backup-dir    "~/.emacs.backup/")
(defvar emacs-auto-save-dir "~/.emacs.autosave/")

(if (eq system-type 'gnu-linux)
    (defvar cmake-mode-el "/usr/share/cmake-2.8/editors/emacs/cmake-mode.el")
  (if (eq system-type 'darwin)
      (defvar cmake-mode-el "")
    (defvar cmake-mode-el "")))

;; Package management
(require 'package)
(package-initialize)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents))

(unless (package-installed-p 'use_package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)

;; Compile elisp
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Load configurations
(load (concat dot-d-dir "config/global_settings.el"))
(load (concat dot-d-dir "config/cpp_settings.el"))
(load (concat dot-d-dir "config/lisp_settings.el"))
(load (concat dot-d-dir "config/python_settings.el"))

;; Add packages to the load
(add-to-list 'load-path (concat dot-d-dir "packages"))

;; If the operating system being used is Mac OS X, then meta == command
(if (eq system-type 'darwin)
    (progn
      (require 'redo+)
      (require 'mac-key-mode)
      (setq mac-command-modifier 'meta)))
