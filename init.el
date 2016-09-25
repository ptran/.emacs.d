;; init.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1
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

;; Define path variables for config
(defconst dot-d-dir "~/.emacs.d/" ".emacs.d location")
(defconst emacs-backup-dir "~/.emacs.backup/" "directory backup files")
(defconst emacs-auto-save-dir "~/.emacs.autosave/" "directory auto-save files")
(defvar cmake-mode-el "/usr/local/share/cmake-3.5/editors/emacs/cmake-mode.el" "CMake el file")

;; Load private settings (if available)
(defvar private-file (concat dot-d-dir "config/private_settings.el"))
(if (file-exists-p private-file)
    (load private-file))

;; Check for network connectivity
(defvar my-online-p nil)
(unless (condition-case ;; [var eval handle-err]
            nil (delete-process (make-network-process
                                 :name "my-check-internet"
                                 :host "elpa.gnu.org"
                                 :service 80)) (error t))
  (setq my-online-p t))

;; Package management
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(when my-online-p
  (package-refresh-contents))
(package-initialize)

;; Add packages to the load
(add-to-list 'load-path (concat dot-d-dir "packages"))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Compile elisp
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Load configurations
(load (concat dot-d-dir "config/global_settings.el"))
(load (concat dot-d-dir "config/cpp_settings.el"))
(load (concat dot-d-dir "config/java_settings.el"))
(load (concat dot-d-dir "config/python_settings.el"))
(load (concat dot-d-dir "config/org_settings.el"))

;; If the operating system being used is Mac OS X, then meta == command
(use-package mac-key-mode
  :if (eq system-type 'darwin)
  :config
  (setq mac-command-modifier 'meta))
