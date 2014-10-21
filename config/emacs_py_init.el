;;;; Python emacs file

; Indentation preference
(add-hook 'python-mode-hook '(lambda () 
 (setq python-indent 4)))

; fix potential encoding issues
(setenv "LC_CTYPE" "UTF-8")

;;;; python.el
; ---------------------------------------------------------------------------
(require 'python)

; ipython up in this bitch
(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--pylab"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;;;; Pymacs
; ---------------------------------------------------------------------------
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;;;; virtualenvwrapper
; ---------------------------------------------------------------------------
(require 'virtualenvwrapper)

(venv-initialize-interactive-shells) ;; interactive shell support
(venv-initialize-eshell) ;; eshell support

(setq venv-location "~/.virtualenvs/")
