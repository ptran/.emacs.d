;; latex_settings.el
;;
;; Author:  Philip Tran
;; URL:     https://github.com/ptran516/.emacs.d
;; Version: 0.1.2

;; From http://stackoverflow.com/questions/2199678/how-to-call-latexmk-in-emacs-and-jump-to-next-error
(setq TeX-latexmk-command
      '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run Latexmk on file"))
(add-hook 'LaTeX-mode-hook (lambda () (push TeX-latexmk-command TeX-command-list)))

;; From http://cachestocaches.com/2015/8/getting-started-use-package/
(use-package tex
  :if (executable-find "tex")
  :no-require t
  :ensure auctex
  :config
  ;; From https://www.emacswiki.org/emacs/AUCTeX
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

;; latex-preview-pane
;; ---------------------------------------------------------------------------
(use-package latex-preview-pane
  :after tex
  :ensure t)
