;;; 04_powerline.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya senda
;; Maintainer: shigeya senda
;; Created: Wed May 14 22:31:41 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-02 13:11:53 shigeya>

;;; Code:

;;; ** powerline
(el-get-bundle 'powerline)
(use-package powerline
  :disabled
  :ensure t
;;  :init
;;  (progn )
  :config
  (progn
    (powerline-default-theme)
    ;;(powerline-center-evil-theme)
    )
  :pin manual
  )

;; 参考
;; http://blechmusik.hatenablog.jp/entry/2013/12/13/020823
;; http://qiita.com/kenoss/items/f4d5a5f88af7a7477cfd


;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 04_powerline.el ends here
