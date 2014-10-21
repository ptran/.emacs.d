; ===========================================================================
;                            MISC SETTINGS
; ===========================================================================

;;;; Highlight parentheses
; ---------------------------------------------------------------------------
; clone (highlight-parenthesis): https://github.com/nschum/highlight-parentheses.el.git
(add-to-list 'load-path "~/.emacs.d/packages/highlight-parentheses")
; clone (autopair): https://github.com/capitaomorte/autopair.git
(add-to-list 'load-path "~/.emacs.d/packages/autopair")

;;;; Fill Column Indicator
; ---------------------------------------------------------------------------
; clone: https://github.com/alpaker/Fill-Column-Indicator.git
(add-to-list 'load-path "~/.emacs.d/packages/fill-column-indicator")

;;;; Undo-Tree
; ---------------------------------------------------------------------------
; clone: https://github.com/emacsmirror/undo-tree.git
(add-to-list 'load-path "~/.emacs.d/packages/undo-tree")

;;;; Popwin
; ---------------------------------------------------------------------------
; clone: https://github.com/m2ym/popwin-el.git
(add-to-list 'load-path "~/.emacs.d/packages/popwin")

;;;; Helm
; ---------------------------------------------------------------------------
; clone: https://github.com/emacs-helm/helm.git
(add-to-list 'load-path "~/.emacs.d/packages/helm")
; -gtags
; clone: https://github.com/syohex/emacs-helm-gtags.git
(add-to-list 'load-path "~/.emacs.d/packages/emacs-helm-gtags")

;;;; Company
; ---------------------------------------------------------------------------
; clone: https://github.com/company-mode/company-mode.git
(add-to-list 'load-path "~/.emacs.d/packages/company-mode")

; ===========================================================================
;                           PYTHON SPECIFIC PACKAGES
; ===========================================================================

;;;; Pymacs
; ---------------------------------------------------------------------------
; clone: https://github.com/pinard/Pymacs
(add-to-list 'load-path "~/.emacs.d/packages/pymacs")

;;;; dash (virtualenvwrapper dependency / Cask)
; ---------------------------------------------------------------------------
; clone: https://github.com/magnars/dash.el.git
(add-to-list 'load-path "~/.emacs.d/packages/dash")

;;;; s (virtualenvwrapper dependency)
; ---------------------------------------------------------------------------
; clone: https://github.com/magnars/s.el.git
(add-to-list 'load-path "~/.emacs.d/packages/s")

;;;; virtualenvwrapper
; ---------------------------------------------------------------------------
; clone: https://github.com/porterjamesj/virtualenvwrapper.el
(add-to-list 'load-path "~/.emacs.d/packages/virtualenvwrapper")
