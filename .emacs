;; ptran516 .emacs file

; Load packages
(load "~/.emacs.d/config/init_load_package.el")
; Load keybindings
(load "~/.emacs.d/config/init_keybinding.el")
; Load misc settings
(load "~/.emacs.d/config/misc_settings.el")
; Load C++ settings
(load "~/.emacs.d/config/emacs_cpp_init")
; Load lisp settings
(load "~/.emacs.d/config/emacs_lisp_init")
; Load Python settings
(load "~/.emacs.d/config/emacs_py_init")

; ===========================================================================
;                          MAC OS X SPECIFIC
; ===========================================================================
(add-to-list 'load-path "~/.emacs.d/packages")
(require 'redo+)
(require 'mac-key-mode)

(setq mac-command-modifier 'meta)
