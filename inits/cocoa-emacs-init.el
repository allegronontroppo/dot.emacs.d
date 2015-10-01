;;; cocoa-emacs-init.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:34:12 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2014-05-18 14:39:00 shigeya>

;;; Code:

;;; ** exec-path

;; mac環境で/usr/local/binなどにパスが通らない問題を解決する。
;; from http://qiita.com/catatsuy/items/3dda714f4c60c435bb25
(use-package exec-path-from-shell
  :ensure t
  :init
  (progn
    (exec-path-from-shell-initialize)
    ))

;;; ** ls

;; gls は brew install coreutils で入れられる。
(if (executable-find "gls")
    (setq insert-directory-program "gls") ;; diredの設定はgnu lsが前提
  (setq dired-listing-switches "-latFL"))

;;; ** emacsclient

;; /usr/bin/emacsclientは古すぎてうまくうごかない。
(if (executable-find "/usr/local/Cellar/emacs/24.3/bin/emacsclient")
    (custom-set-variables
     '(magit-emacsclient-executable
       "/usr/local/Cellar/emacs/24.3/bin/emacsclient")
     ))

;;; ** cmigemo

; cmigemo for osx (cmigemo was installed by 'brew install cmigemo')
;

(when (executable-find "cmigemo")
  (use-package migemo
    :init
    (progn
      (custom-set-variables
       '(migemo-command "/usr/local/bin/cmigemo")
       '(migemo-options '("-q" "--emacs"))
       '(migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
       '(migemo-user-dictionary nil)
       '(migemo-coding-system 'utf-8-unix)
       '(migemo-regex-dictionary nil)
       )
      (load-library "migemo")
      (migemo-init)
      )))

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; cocoa-emacs-init.el ends here


