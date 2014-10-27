;; ptran516 .emacs file

;; Set location of .emacs.d folder
(setq dot-d-folder "~/.emacs.d/")

;; Set location of cmake-mode.el file
(setq cmake-mode-loc "/usr/local/Cellar/cmake/3.0.2/share/cmake/editors/emacs/cmake-mode.el")

;; Load packages
(load (concat dot-d-folder "config/init_load_package.el"))
;; Load misc settings
(load (concat dot-d-folder "config/misc_settings.el"))
;; Load keybindings
(load (concat dot-d-folder "config/init_keybinding.el"))
;; Load C++ settings
(load (concat dot-d-folder "config/emacs_cpp_init"))
;; Load lisp settings
(load (concat dot-d-folder "config/emacs_lisp_init"))
;; Load Python settings
(load (concat dot-d-folder "config/emacs_py_init"))

;; ===========================================================================
;;                          MAC OS X SPECIFIC
;; ===========================================================================
(add-to-list 'load-path (concat dot-d-folder "packages"))
(require 'redo+)
(require 'mac-key-mode)

(setq mac-command-modifier 'meta)
