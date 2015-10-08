;;; 11-haskell.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:32:22 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2014-05-26 09:35:58 senda>

;;; Code:

;;; ** haskell-mode
(use-package haskell-mode
  :disabled
  :ensure t
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
    (use-package shm
      :ensure shm
      :commands structured-haskell-mode
      :init (add-hook 'haskell-mode-hook 'structured-haskell-mode)
      )
    (use-package hi2
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
;;; 11-haskell.el ends here
