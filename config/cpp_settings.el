;; cpp_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran/.emacs.d

(use-package cc-mode
  :preface
  (defun my/c-mode-hook ()
    "C and C++ coding style preferences"
    (setq c-basic-offset 2)
    (setq c-default-style "bsd")
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    (c-set-offset 'inlambda 0)
    (c-set-offset 'substatement-open 0))
  :config
  (add-hook 'c-mode-hook #'my/c-mode-hook)
  (add-hook 'c++-mode-hook #'my/c-mode-hook)
  ;; Display line numbers
  (add-hook 'c-mode-hook #'linum-mode)
  (add-hook 'c++-mode-hook #'linum-mode)
  ;; Open .h files in c++ mode
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

;; my/build-cmake-project
(defun my/build-cmake-project (&optional cmake-args)
  "Build CMake project for current buffer, expects project to be in a git repository"
  (interactive)
  (if (buffer-file-name)
      (progn
        (if (file-remote-p (buffer-file-name))
          (error "my/build-cmake-project error: Cannot build using a remote file"))
        (setq buffer-project-root (projectile-project-root buffer-file-name)))
    (error "my/build-cmake-project error: Current buffer is not a file"))
  ;; Find CMakeLists.txt at the top level directory
  (setq cmakelists-file (concat buffer-project-root "CMakeLists.txt"))
  (unless (file-exists-p cmakelists-file)
    (error "my/build-cmake-project error: %s" (concat buffer-project-root " does not have a CMakeLists.txt file")))
  ;; Make a build directory at project's top level directory
  (setq build-directory (concat buffer-project-root "build"))
  (unless (file-exists-p build-directory)
    (make-directory build-directory))
  ;; Execute cmake and build project
  (setq cmake-command (concat "cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON " cmake-args))
  (if (string-equal system-type "windows-nt")
      (setq build-command "cmake --build . --config Release")
    (setq build-command "make -j$(nproc)"))
  (async-shell-command (concat "cd " build-directory "; " cmake-command "; " build-command) "*my/cmake-build*"))

;; Bind build function to C-c m
(add-hook 'c++-mode-hook (lambda () (define-key c++-mode-map (kbd "C-c m") #'my/build-cmake-project)))

;; CMake Mode
;; ---------------------------------------------------------------------------
(use-package cmake-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist))
  (add-hook 'cmake-mode-hook (lambda () (define-key cmake-mode-map (kbd "C-c m") #'my/build-cmake-project))))

;; CUDA Mode
;; ---------------------------------------------------------------------------
(use-package cuda-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (append
         '(("\\.cu\\'" . cuda-mode))
         '(("\\.cuh\\'" . cuda-mode))
         auto-mode-alist)))

;; Irony-Mode
;; ---------------------------------------------------------------------------
(defun my/irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

(use-package irony
  :if (and (executable-find "clang") (executable-find "cmake") (not (version< emacs-version "24.4")))
  :ensure t
  :config
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'my/irony-mode-hook)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :after company
  :ensure t
  :config
  (add-hook 'c-mode-hook #'company-mode)
  (add-hook 'c++-mode-hook #'company-mode)
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

;; irony-eldoc
(use-package irony-eldoc
  :after irony
  :no-require t
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; flycheck-irony
;; ---------------------------------------------------------------------------
(use-package flycheck-irony
  :after flycheck
  :no-require t
  :ensure t
  :config
  (add-hook 'c-mode-hook #'flycheck-mode)
  (add-hook 'c++-mode-hook #'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
