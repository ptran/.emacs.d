;; java_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran/.emacs.d

;; Check if configurables exist; if not, set them to default values
(unless (boundp 'my/eclim-eclipse-dir) (setq my/eclim-eclipse-dir "~/eclipse"))
(unless (boundp 'my/eclim-executable) (setq my/eclim-executable "~/eclipse/eclim"))
;;

;; eclim
;; ---------------------------------------------------------------------------
(use-package eclim
  :if (and (file-exists-p my/eclim-eclipse-dir) (executable-find my/eclim-executable))
  :ensure t
  :init
  (custom-set-variables
   '(eclim-eclipse-dirs `(,my/eclim-eclipse-dir))
   '(eclim-executable my/eclim-executable))
  :config
  (require 'eclimd)
  (add-hook 'java-mode-hook 'eclim-mode))

(use-package company-emacs-eclim
  :after eclim
  :ensure t
  :config
  (company-emacs-eclim-setup)
  (add-hook 'java-mode-hook 'company-mode))
