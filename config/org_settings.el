;; org_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.2

;; Templates defined in private_settings.el
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c r" . org-capture))
  :config
  (setq org-log-done t)
  (add-hook 'org-mode-hook #'org-indent-mode))

;; No prompt for execution
(setq org-confirm-babel-evaluate nil)

;; Display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
