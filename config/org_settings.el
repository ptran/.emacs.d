;; org_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.3

;; Templates defined in private_settings.el
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-log-done t)
  (add-hook 'org-mode-hook #'org-indent-mode))

;; Set TODO keywords
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "RECOMMENDATION" "|" "DONE" "CANCELED")))

;; No prompt for execution
(setq org-confirm-babel-evaluate nil)

;; Display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; Capture templates
(if (and (boundp 'my/org-task-file) (boundp 'my/org-notes-files))
    (setq org-capture-templates
          `(("t"
             "Task"
             entry
             (file my/org-task-file)
             "* TODO %?\nCAPTURED: %<%Y-%m-%d %H:%M>")
            ("T"
             "Detailed Task"
             entry
             (file my/org-task-file)
             "* TODO %^{Task}\nCAPTURED: %<%Y-%m-%d %H:%M>\n%?\n")
            ("r"
             "Recommendation"
             entry
             (file my/org-notes-file)
             "* RECOMMENDATION %? :%^{Type}:\nCAPTURED: %<%Y-%m-%d %H:%M>"))))
