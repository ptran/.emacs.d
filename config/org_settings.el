;; org_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.3

;; =============== ;;
;;  Configuration  ;;
;; =============== ;;
(defconst my/org-task-file "~/Dropbox/Documents/Org/todo.org")
(defconst my/org-notes-file "~/Dropbox/Documents/Org/notes.org")
;;

;; Templates defined in private_settings.el
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-agenda-files '("~/Dropbox/Documents/Org"))
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
  (setq org-log-done t)
  (add-hook 'org-mode-hook #'org-indent-mode)
  ;; org-babel settings
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ledger . t))))

;; Capture templates
(if (and (boundp 'my/org-task-file) (boundp 'my/org-notes-file))
    (setq org-capture-templates
          '(("t"
             "Task"
             entry
             (file+headline my/org-task-file "Tasks")
             "* TODO %?\nCAPTURED: %<%Y-%m-%d %H:%M>")
            ("T"
             "Detailed Task"
             entry
             (file+headline my/org-task-file "Tasks")
             "* TODO %^{Task}\nCAPTURED: %<%Y-%m-%d %H:%M>\n%?\n"))))

;; TODO Add ledger capture templates

;; Make org file bullets look nice
(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
