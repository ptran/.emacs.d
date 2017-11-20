;; org_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d

;; =============== ;;
;;  Configuration  ;;
;; =============== ;;
(defconst my/org-task-file "~/Dropbox/Documents/Org/todo.org" "Org file for keeping track of tasks")
(defconst my/org-notes-file "~/Dropbox/Documents/Org/notes.org" "Org file for taking notes")
(defconst my/appt-notification-app "~/Dropbox/bin/appt-notification" "Program to run for upcoming appointment reminders")
;;

;; Templates defined in private_settings.el
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-agenda-files `(,my/org-task-file))
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
;; TODO Add ledger capture templates
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

;; Make org file bullets look nice
(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Alert for scheduled org items
;; ---
;; SOURCE: https://emacs.stackexchange.com/questions/3844/good-methods-for-setting-up-alarms-audio-visual-triggered-by-org-mode-events/5821#5821
;; TODO: Make sure this works properly
(use-package appt
  :if (file-exists-p my/appt-notification-app)
  :after org
  :init
  (defun my/org-agenda-to-appt ()
    ;; Use appointment data from org-mode
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))
  (setq appt-message-warning-time 15) ; Show notification 15 minutes before event
  (setq appt-display-interval 5) ; Disable multiple reminders
  (setq appt-display-mode-line nil)
  :config
  (appt-activate t)
  ;; Update alarms when...
  ;; (1) ... Starting Emacs
  (my/org-agenda-to-appt)

  ;; (2) ... Everyday at 12:05am (useful in case you keep Emacs always on)
  (run-at-time "12:05am" (* 24 3600) 'my/org-agenda-to-appt)

  ;; (3) ... When todo.org is saved
  (add-hook 'after-save-hook
            '(lambda ()
               (if (string= (buffer-file-name) my/org-task-file)
                   (my/org-agenda-to-appt))))

  ;; Display appointments as a window manager notification
  (setq appt-disp-window-function 'my/appt-display)
  (setq appt-delete-window-function (lambda () t))
  (defun my/appt-display (min-to-app new-time msg)
    (if (atom min-to-app)
        (start-process "my/appt-notification-app" nil my/appt-notification-app min-to-app msg)
      (dolist (i (number-sequence 0 (1- (length min-to-app))))
        (start-process "my/appt-notification-app" nil my/appt-notification-app (nth i min-to-app) (nth i msg))))))
;; ---
