;;; linux-init.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:35:42 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2014-05-11 16:36:09 shigeya>

;;; Code:

;;; ** linux cmigemo

;;
;; linux migemo setting
;;       http://qiita.com/catatsuy/items/c5fa34ead92d496b8a51
;;
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
;; for linux
  (setq migemo-command "cmigemo")
  (setq migemo-directory "/usr/share/cmigemo/utf-8/cmigemo-dict")
;;
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
)

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; linux-init.el ends here

