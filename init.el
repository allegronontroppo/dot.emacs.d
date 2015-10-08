;;; init.el --- emacs startup file  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: SENDA Shigeya
;; Maintainer: SENDA Shigeya
;; Created: Sun May 11 19:12:03 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-08 10:23:39 shigeya>

;;--------------------------------------------------------------------
;;　el-get + init-loader によるemacs初期化設定
;;
;; ・package.elやcaskを使わないで、el-getのみをつかってパッケージ管理する。
;; ・init-loaderで初期化ファイルを分割管理
;; ・use-packageでパッケージごとの初期化を分離
;; ・bind-keyでキーバインドを明確にする。
;;--------------------------------------------------------------------

;;; Code:

;; relocatable init directory setting 
;;   invoke 'emacs -q -l <path>/init.el' to test.
;;   use (locate-user-emacs "") to specify file or dir.
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq use-proxy t) ; t or nil
(when use-proxy
  ;; proxy
  (setq url-proxy-services '(("http"  . "proxy.ricoh.co.jp:8080")
			     ("https" . "proxy.ricoh.co.jp:8080")
			     ))
  ;; あと、gitのglobal設定で追加しておくこと。
  ;; cf.  http://sunday-programming.hatenablog.com/entry/2013/10/31/202918
  ;;
  ;; > git config --global http.proxy http://proxy.hogedomain.com:8080
  ;; > git config --global https.proxy http://proxy.hogedomain.com:8080
  ;; > git config --global url."https://".insteadOf git://
  ;; > git config --list  <-- これで確認
)

;;
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

;;
;; MELPA package.el
;;
;;(require 'package)
;;(add-to-list 'package-archives
;;	     '("melpa" . "http://melpa.org/packages/"))
;;(package-initialize)

;; el-get
;;
;;;; どうもel-get-install.elはうまくうごかないことがある。proxy環境だから？
;;;; site-lispの下に初期用にcloneしておくこと
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/el-get"))
;;(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

;(unless (require 'el-get nil 'noerror)
;  ;; use package.el to download el-get/el-get. (first time)
;  (require 'package)
;  (add-to-list 'package-archives
;	       '("melpa" . "http://melpa.org/packages/"))
;  (package-initialize)
;  (package-refresh-contents)
;  (package-install 'el-get) ;; el-get from melpa
;  (require 'el-get)
;  )
(require 'el-get)

(add-to-list 'el-get-recipe-path
	     (locate-user-emacs-file "/el-get-user/recipes"))
;;
;; normalize installed el-get recipe path
(let ((respdir (locate-user-emacs-file "site-lisp/el-get/recipes")))
  (if (file-directory-p respdir)
      (add-to-list 'el-get-recipe-path respdir)))
(let ((respdir (locate-user-emacs-file "el-get/el-get/recipes")))
  (if (file-directory-p respdir)
      (add-to-list 'el-get-recipe-path respdir)))

(setq el-get-sources
      '(
	(:name init-loader
	       :type http
	       :url "https://raw.github.com/gist/1021706/init-loader.el"
	       :description "[My Recipes] Split management init.el.")
	(:name mode-compile
	       :type http
	       :url "https://raw.githubusercontent.com/emacsmirror/mode-compile/master/mode-compile.el"
	       :description "Mode compile Emacs Lisp libraries.")
	))

;; cf https://github.com/uwabami/emacs
(setq el-get-verbose t)
(setq el-get-github-default-url-type 'https)

(el-get 'sync)

;; init-loader

(el-get-bundle 'init-loader)

;;(add-to-list 'load-path (locate-user-emacs-file "site-lisp/init-loader"))
(require 'init-loader)
;; you can debug init files by M-x init-loader-show-log.
(setq init-loader-show-log-after-init nil)
;(setq init-loader-byte-compile t) ;; byte-compile !!
(init-loader-load (locate-user-emacs-file "inits"))
;; inits配下の各初期設定ファイルが実行される。

;;---------------------------------------------------------------------

;;
;; customize settings
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; init.el ends here
