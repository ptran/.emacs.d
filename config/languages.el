;;; languages.el --- Language Specific Configurations -*- lexical-binding: t; -*-

;; Emacs Lisp
(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :config
  (defun my/emacs-lisp-mode-hook ()
    (setq indent-tabs-mode nil)
    (display-line-numbers-mode 1)
    (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'pp-eval-last-sexp))
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-hook))

;; LSP Support (Eglot)
(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c f" . eglot-format))
  :config
  (add-to-list 'eglot-server-programs
               `((c++-mode c-mode) . ("/opt/homebrew/opt/llvm/bin/clangd" "--header-insertion=never"))))

;; C / C++ / CUDA
(use-package cc-mode
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode))
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :preface
  (defun my/c-mode-hook ()
    (setq c-basic-offset 2)
    (setq c-default-style "bsd")
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    (c-set-offset 'inlambda 0)
    (c-set-offset 'substatement-open 0))
  :config
  (add-hook 'c-mode-hook #'my/c-mode-hook)
  (add-hook 'c++-mode-hook #'my/c-mode-hook)
  (add-hook 'c-mode-hook #'display-line-numbers-mode)
  (add-hook 'c++-mode-hook #'display-line-numbers-mode)
  
  ;; Build Command
  (defun my/build-cmake-project (&optional cmake-args)
    "Build CMake project for current buffer."
    (interactive)
    (let* ((root (projectile-project-root))
           (build-dir (concat root "build"))
           (cmake-cmd (concat "cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON " cmake-args))
           (build-cmd (if (string-equal system-type "windows-nt")
                          "cmake --build . --config Release"
                        "make -j$(nproc)")))
      (unless (file-exists-p build-dir) (make-directory build-dir))
      (async-shell-command (concat "cd " build-dir "; " cmake-cmd "; " build-cmd) "*my/cmake-build*")))
  
  (define-key c++-mode-map (kbd "C-c m") #'my/build-cmake-project))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (add-hook 'cmake-mode-hook (lambda () (define-key cmake-mode-map (kbd "C-c m") #'my/build-cmake-project))))

(use-package cuda-mode :ensure t)

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . eglot-ensure)
  :init
  (setq python-indent-offset 4)
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "--simple-prompt -i")
    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i"))
  :config
  (add-hook 'python-mode-hook #'display-line-numbers-mode))

(use-package ein
  :ensure t
  :if (executable-find "jupyter"))

;; Web Mode
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.css\\'" "\\.js\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (setq web-mode-engines-alist '(("jinja" . "\\.html?\\'"))))

;; LaTeX
(use-package tex
  :ensure auctex
  :if (executable-find "tex")
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook (lambda () 
    (visual-line-mode 1)
    (flyspell-mode 1)
    (LaTeX-math-mode 1)
    (turn-on-reftex))))

(provide 'languages)
