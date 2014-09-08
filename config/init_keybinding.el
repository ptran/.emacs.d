; Prompt before closing emacs
; credit: http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(global-set-key (kbd "C-c C-s") 'eshell)
(global-set-key (kbd "C-c s") 'shell)

(global-set-key (kbd "C-x C-g") 'goto-line)
