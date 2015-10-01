;;; linux-input.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:33:44 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-09-29 13:39:41 senda>

;;; Code:

;;; ** mozc

(require 'mozc nil t)
(setq default-input-method "japanese-mozc")
(setq mozc-candidate-style 'overlay)

;;; ** SKK

(use-package ddskk
  :disabled
;;  :ensure t
  :init
  (progn
    (bind-key "C-x C-j" 'skk-mode)
    (bind-key "C-x j"   'skk-mode)
    (bind-key "C-x t"   'skk-tutorial)
  ))

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; linux-input.el ends here

