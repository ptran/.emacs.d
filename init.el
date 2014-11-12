;; ptran516 .emacs file

;; Define path variables for config
(defvar dot-d-dir "~/.emacs.d/")
(defvar cmake-mode-dir "/usr/local/Cellar/cmake/3.0.2/share/cmake/editors/emacs/cmake-mode.el")

;; Package management
(require 'cask "/usr/local/Cellar/cask/0.7.2/cask.el")
(cask-initialize)
(require 'pallet)

;; Load packages
(load (concat dot-d-dir "config/init_load_package.el"))
;; Load misc settings
(load (concat dot-d-dir "config/misc_settings.el"))
;; Load keybindings
(load (concat dot-d-dir "config/init_keybinding.el"))
;; Load C++ settings
(load (concat dot-d-dir "config/emacs_cpp_init"))
;; Load lisp settings
(load (concat dot-d-dir "config/emacs_lisp_init"))
;; Load Python settings
(load (concat dot-d-dir "config/emacs_py_init"))

;; ===========================================================================
;;                          MAC OS X SPECIFIC
;; ===========================================================================
(add-to-list 'load-path (concat dot-d-dir "packages"))
(require 'redo+)
(require 'mac-key-mode)

(setq mac-command-modifier 'meta)
