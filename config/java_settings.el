;; java_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.3

;; eclim
;; ---------------------------------------------------------------------------
(use-package eclim
  :ensure t
  :init
  (custom-set-variables
   '(eclim-eclipse-dirs '("~/eclipse"))
   '(eclim-executable "~/eclipse/eclim"))
  :config
  (require 'eclimd)
  (add-hook 'java-mode-hook 'eclim-mode))

(use-package company-emacs-eclim
  :ensure t
  :config
  (company-emacs-eclim-setup)
  (add-hook 'java-mode-hook 'company-mode))
