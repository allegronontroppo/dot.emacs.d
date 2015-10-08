;;; 22-yasnippet.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:15:27 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-08 17:20:46 shigeya>

;;; Code:

;;; * yasnippet 設定

;; auto-insertのように新規ファイルの作成でテンプレートを展開する。
;; そのときにyasnippetを使って展開するようにする。
;;
(defun yas-new-file-tpl ()
  "expand snippet 'new-file-tpl' when you open new file and
if you have a '~/.emacs.d/snipets/<major-mode>/new-file-tpl' file.
"
  (interactive)
  (when (and (buffer-file-name)
	     (not (file-exists-p (buffer-file-name)))
	     (= (point-max) 1))
    (when (file-exists-p (concat "~/.emacs.d/snippets/"
				 (symbol-name major-mode)
				 "/new-file-tpl"))
      (insert "new-file-tpl") ;; Adds the trigger text
      (yas-expand))           ;; Trigger snippet expansion
    ))

;; support functions
 
(defun @FILE-NAME@ () (file-name-nondirectory (buffer-file-name)))
(defun @DIRECTORY@ () (file-name-directory (buffer-file-name)))
(defun @DATE-STAMP@ ()(current-time-string))
(defun @YEAR@ ()      (substring (current-time-string) 20))
(defun @MONTH@ ()     (substring (current-time-string) 4 7))
(defun @DAY@ ()       (substring (current-time-string) 8 10))
(defun @DATE@ ()      (substring (current-time-string) 0 3))
(defun @HOUR@ ()      (substring (current-time-string) 11 13))
(defun @MINUTE@ ()    (substring (current-time-string) 14 16))
(defun @SECOND@ ()    (substring (current-time-string) 17 19))
(defun @USER@ ()      (user-full-name))
(defun @LOGIN@ ()     (user-login-name))


(req-package yasnippet-bundle
  :ensure yasnippet
  :bind
  (("C-+" . yas-expand-from-trigger-key)
   ("C-="     . hippie-expand)
   ;; globalで定義する。(yas-minor-mode-mapの方がいいのか？)
   ("M-g C-s" . yas-insert-snippet)       ;; 既存スニペットを挿入する
   ("M-g C-n" . yas-new-snippet)          ;; 新規スニペットを作成するバッファを用意する
   ("M-g C-v" . yas-visit-snippet-file))  ;; 既存スニペットを閲覧・編集する
  :init
  (progn
    (el-get-bundle 'yasnippet)
    (req-package yasnippet :ensure t :loader el-get)
    (custom-set-variables
     '(yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0"))
    (add-hook 'yas-minor-mode-hook 'yas-new-file-tpl)
    ;; ~/.emacs.d/にsnippetsというフォルダを作っておきましょう
    ;; (add-to-list yas-snippet-dirs "~/.emacs.d/snippets" ) ; デフォルト値
    ;; AndreaCrottiさんのをgitでとってきた。
    ;;  https://github.com/AndreaCrotti/yasnippet-snippets.git
    ;; (add-to-list yas-snippet-dirs "~/.emacs.d/git/yasnippet-snippets" 'APPEND)
    ;(yas-global-mode 1)
    ;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
    ;; (setqだとtermなどで干渉問題ありでした)
    ;; もちろんTAB以外でもOK 例えば "C-;"とか
    ;; (custom-set-variables '(yas-trigger-key "TAB"))
    ;;(bind-key "C-/" 'hippie-expand)
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    ;;
    (el-get-bundle 'header2)
    (req-package header2 :ensure t :loader el-get)
    ;; emacs-lisp新規ファイルsnippetで使用
    )
  :config
  (progn
    (yas-global-mode 1)
    )
  )
;;  ドキュメント
;; http://fukuyama.co/yasnippet
;; http://konbu13.hatenablog.com/entry/2014/01/12/113300
;; ** snippet例
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; ** snippetの書き方
;; http://fukuyama.co/yasnippet

;;; ** helm-c-yasnippet

(el-get-bundle 'helm-c-yasnippet)
(req-package helm-c-yasnippet
  :loader el-get
  :ensure t
  :bind
  (("C-c y" . helm-yas-complete)) ;; モードで定義されているsnippetを調べるのによい。
  :init
  (progn
    (custom-set-variables '(helm-yas-space-match-any-greedy t)) ;; [default: nil]
    ))

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 22-yasnippet.el ends here
