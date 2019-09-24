;; init.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran/.emacs.d

;; Set emacs directory
(defvar my/dot-d-dir (expand-file-name "~/.emacs.d/") ".emacs.d location")

;; Package management
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Add packages to the load
(add-to-list 'load-path (concat my/dot-d-dir "packages"))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'cl)

;; Compile elisp
(use-package auto-compile
  :ensure t
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode))

;; Load configurations
(load (concat my/dot-d-dir "config/global_settings.el"))
(load (concat my/dot-d-dir "config/elisp_settings.el"))
(load (concat my/dot-d-dir "config/cpp_settings.el"))
(load (concat my/dot-d-dir "config/python_settings.el"))
(load (concat my/dot-d-dir "config/org_settings.el"))
(load (concat my/dot-d-dir "config/latex_settings.el"))
(load (concat my/dot-d-dir "config/web_settings.el"))
