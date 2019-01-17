;; cpp_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran/.emacs.d

;; Indentation preferences
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun my/c-indent-hook ()
  "My indentation preferences for c-mode-common"
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'inline-open '0))
(add-hook 'c-mode-hook #'my/c-indent-hook)
(add-hook 'c++-mode-hook #'my/c-indent-hook)

;; Display line numbers
(add-hook 'c-mode-hook #'linum-mode)
(add-hook 'c++-mode-hook #'linum-mode)

;; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(if (buffer-file-name)
    (file-name-directory buffer-file-name)
  (print "Current buffer is not a file"))

;; Custom cmake build function (requires projectile)
(defun my/build-cmake-project (&optional cmake-args)
  "Build CMake project for current buffer"
  (interactive)
  (if (buffer-file-name)
      (setq buffer-project-root (projectile-project-root buffer-file-name))
    (error "my/build-cmake-project error: Current buffer is not a file"))
  (setq cmakelists-file (concat buffer-project-root "CMakeLists.txt"))
  (unless (file-exists-p cmakelists-file)
    (error "my/build-cmake-project error: %s" (concat buffer-project-root " does not have a CMakeLists.txt file")))
  (setq build-directory (concat buffer-project-root "build"))
  (unless (file-exists-p build-directory)
    (make-directory build-directory))
  (setq cmake-command (concat "cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON " cmake-args))
  (if (string-equal system-type "windows-nt")
      (setq build-command "cmake --build . --config Release")
    (setq build-command "make -j$(nproc)"))
  (async-shell-command (concat "cd " build-directory "; " cmake-command "; " build-command) "*my/cmake-build*"))

;; Bind build function to C-c m
(add-hook 'c++-mode-hook (lambda () (define-key c++-mode-map (kbd "C-c m") #'my/build-cmake-project)))

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
