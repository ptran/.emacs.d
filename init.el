;; ptran516 .emacs file

;; Define path variables for config
(defvar dot-d-dir      "~/.emacs.d/")
(defvar cmake-mode-dir "/usr/local/Cellar/cmake/3.2.3/share/cmake/editors/emacs/cmake-mode.el")
(defvar backup-dir     "~/.emacs.backup/")
(defvar auto-save-dir  "~/.emacs.autosave/")

;; Package management with Pallet and Cask
(require 'cask "/usr/local/Cellar/cask/0.7.2/cask.el")
(cask-initialize)
(require 'pallet)

;; Load configurations
(load (concat dot-d-dir "config/aux_packages.el"))
(load (concat dot-d-dir "config/global_settings.el"))
(load (concat dot-d-dir "config/cpp_settings.el"))
(load (concat dot-d-dir "config/lisp_settings.el"))
(load (concat dot-d-dir "config/python_settings.el"))

;; If the operating system being used is Mac OS X, then meta == command
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'load-path (concat dot-d-dir "packages"))
      (require 'redo+)
      (require 'mac-key-mode)
      (setq mac-command-modifier 'meta))
  )
