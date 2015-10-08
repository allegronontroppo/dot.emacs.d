;;; 21_auto-complete.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:29:22 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-08 16:52:36 shigeya>

;;; Code:

;;; ** auto-complete

;;  日本語マニュアル：http://cx4a.org/software/auto-complete/manual.ja.html
(el-get-bundle 'auto-complete)
(req-package auto-complete
  :ensure t
  :loader el-get
  :config
  (progn
    (require 'auto-complete)
    (require 'auto-complete-config)
    (ac-config-default)

    ;; 辞書追加
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

    ;; 7.4. TAB で補完を完了する（ RET は使わない）
    ;; http://cx4a.org/software/auto-complete/manual.ja.html
    (define-key ac-completing-map "\t" 'ac-complete)
    (define-key ac-completing-map "\r" nil)

    ;; 7.8. 特定のモードで自動で auto-complete-mode を有効にする
    ;; http://cx4a.org/software/auto-complete/manual.ja.html

    ;;(global-auto-complete-mode t)

    (add-to-list 'ac-modes 'org-mode) ;; org-modeになったら、auto-complete をスタートさせる
    (add-to-list 'ac-modes 'text-mode)
    (add-to-list 'ac-modes 'ruby-mode)
    (add-to-list 'ac-modes 'pyhon-mode)
    ;;(add-to-list 'ac-modes 'ansys-mode)
    (add-to-list 'ac-modes 'markdown-mode)
    (add-to-list 'ac-modes 'emacs-lisp-mode)
    (add-to-list 'ac-modes 'lisp-interaction-mode)

    ;(setq ac-auto-start 3) ;; 3文字目から

    (custom-set-variables
     ;; C-n / C-p で選択
     '(ac-use-menu-map t)
     ;; 空気よんでくれるらしい。
     '(ac-dwim t)
     '(ac-auto-start 3)
     ;;'(ac-auto-start nil) ;; TABで開始
     '(ac-candidate-limit 32) ;; 最大候補個数  これを制限しとかないと重い！！
     )
    ;; yasnippetのbindingを指定するとエラーが出るので回避する方法。
    ;;(setf (symbol-function 'yas-active-keys)
    ;;      (lambda ()
    ;;        (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))
    ))

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 21-auto-complete.el ends here

