;; Define path variables for config
(defconst dot-d-dir "~/.emacs.d/" ".emacs.d location")
(defconst emacs-backup-dir "~/.emacs.backup/" "directory backup files")
(defconst emacs-auto-save-dir "~/.emacs.autosave/" "directory auto-save files")

;; Load private settings (if available)
(defconst private-file (concat dot-d-dir "config/private_settings.el"))
(if (file-exists-p private-file)
    (load private-file)
  (defvar cmake-mode-el ""))

;; Check for network connectivity
(setq my-online-p nil)
(unless (condition-case ;; [var eval handle-err]
            nil (delete-process (make-network-process
                                 :name "my-check-internet"
                                 :host "elpa.gnu.org"
                                 :service 80)) (error t))
  (setq my-onlinep t))

(if (version< emacs-version "24.3")
    (error "This configuration requires emacs 24.3 or higher")
  (progn
    ;; Package management
    (require 'package)
    (unless (assoc-default "melpa" package-archives)
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

    (when my-onlinep
      (package-refresh-contents))
    (package-initialize)

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

    ;; Add packages to the load
    (add-to-list 'load-path (concat dot-d-dir "packages"))

    ;; Load configurations
    (load (concat dot-d-dir "config/global_settings.el"))
    (load (concat dot-d-dir "config/cpp_settings.el"))
    (load (concat dot-d-dir "config/lisp_settings.el"))
    (load (concat dot-d-dir "config/python_settings.el"))))

;; If the operating system being used is Mac OS X, then meta == command
(use-package redo+
  :if (eq system-type 'darwin))

(use-package mac-key-mode
  :if (eq system-type 'darwin)
  :config
  (setq mac-command-modifier 'meta))
