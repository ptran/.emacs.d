;; init.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;;
;;                     ░░░░░░░░░
;;                     ░░░░▄▀▀▀▀▀█▀▄▄▄▄░░░░
;;                     ░░▄▀▒▓▒▓▓▒▓▒▒▓▒▓▀▄░░
;;                     ▄▀▒▒▓▒▓▒▒▓▒▓▒▓▓▒▒▓█░
;;                     █▓▒▓▒▓▒▓▓▓░░░░░░▓▓█░
;;                     █▓▓▓▓▓▒▓▒░░░░░░░░▓█░
;;                     ▓▓▓▓▓▒░░░░░░░░░░░░█░
;;                     ▓▓▓▓░░░░▄▄▄▄░░░▄█▄▀░
;;                     ░▀▄▓░░▒▀▓▓▒▒░░█▓▒▒░░
;;                     ▀▄░░░░░░░░░░░░▀▄▒▒█░
;;                     ░▀░▀░░░░░▒▒▀▄▄▒▀▒▒█░
;;                     ░░▀░░░░░░▒▄▄▒▄▄▄▒▒█░
;;                     ░░░▀▄▄▒▒░░░░▀▀▒▒▄▀░░
;;                     ░░░░░▀█▄▒▒░░░░▒▄▀░░░
;;                     ░░░░░░░░▀▀█▄▄▄▄▀░░░

;; Set emacs directory
(defconst dot-d-dir "~/.emacs.d/" ".emacs.d location")

;; Load private settings (if available)
(defvar private-file (concat dot-d-dir "config/private_settings.el"))
(if (file-exists-p private-file) (load private-file))

;; Package management
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Add packages to the load
(add-to-list 'load-path (concat dot-d-dir "packages"))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'cl)

;; Compile elisp
(use-package auto-compile
  :ensure t
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode))

;; If the operating system being used is Mac OS X, then meta == command
(use-package mac-key-mode
  :if (eq system-type 'darwin)
  :config
  (setq mac-command-modifier 'meta))

;; Load configurations
(load (concat dot-d-dir "config/global_settings.el"))
(load (concat dot-d-dir "config/elisp_settings.el"))
(load (concat dot-d-dir "config/cpp_settings.el"))
(load (concat dot-d-dir "config/java_settings.el"))
(load (concat dot-d-dir "config/python_settings.el"))
(load (concat dot-d-dir "config/org_settings.el"))
(load (concat dot-d-dir "config/latex_settings.el"))
(load (concat dot-d-dir "config/web_settings.el"))
