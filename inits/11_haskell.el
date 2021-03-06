;;; 11_haskell.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:32:22 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-13 12:33:25 shigeya>

;;; Code:

;;; ** haskell-mode
(el-get-bundle 'haskell-mode)
(req-package haskell-mode
;;  :disabled
  :ensure t
  :loader el-get
  :commands
  (literate-haskell-mode haskell-mode)
  :mode
  (("\\.l[gh]s\\'" . literate-haskell-mode)
   ("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))   ;; 拡張子がhs
  :config
  (progn
    (custom-set-variables
     '(haskell-font-lock-symbols 'unicode) ;; display \ -> in unicode
     '(haskell-literate-default 'tex)
     '(haskell-stylish-on-save t)
     '(haskell-tags-on-save t)
     )
    ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
    (defun my-haskell-hook ()
      (setq mode-name " λ ")     ;; buffer-local
      (setq evil-auto-indent nil) ;; buffer-local
      (capitalized-words-mode)
      (diminish 'capitalized-words-mode "")
      (turn-on-eldoc-mode)
      (diminish 'eldoc-mode "")
      )
    (add-hook 'haskell-mode-hook 'my-haskell-hook)
    ;;;; for ghc-mod
    ;; see  http://www.mew.org/~kazu/proj/ghc-mod/en/install.html
    ;; if ghc-mod was installed by cabal, use ghc-mod.
    (when (require 'ghc "~/cabal/share/ghc-mod-/" 'noerror)
      (autoload 'ghc-init "ghc" nil t)
      (autoload 'ghc-debug "ghc" nil t)
      (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
      )
    (req-package shm
      :loader el-get
      :ensure shm
      :commands structured-haskell-mode
      :init (add-hook 'haskell-mode-hook 'structured-haskell-mode)
      )
    (req-package hi2
      :loader el-get
      :disabled
      :init
      (add-hook 'haskell-mode-hook 'turn-on-hi2)
      )
    )
  )
;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 11_haskell.el ends here
