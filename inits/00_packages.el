;;; 00-package.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:17:31 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-08 10:21:16 shigeya>

;;; Code:

;;; * package

;;(setf (cdr(assoc "marmalade" package-archives))
;;      "https://marmalade-repo.org/packages/")

;;; ** pre-install-package

(require 'cl-lib)

(el-get-bundle 's)
(el-get-bundle 'f)
(el-get-bundle 'ht)

(require 's)
(require 'f)
(require 'ht)

;;; ** use-package
(el-get-bundle 'use-package)
(el-get-bundle 'bind-key)

(eval-when-compile (require 'use-package))
(require 'bind-key)

;; use (use-package XXX ...) to customize inits file after here.
;;
;; 使い方： https://github.com/jwiegley/use-package
;; :init   .emacsで出会ったときに実行される。
;; :config パッケージが実際にロードされるときに実行される。
;; :idle   idle intervalに実行。emacsが暇なときに実行する。
;; :if     条件付き  ex)  :if window-system
;; :disabled t いったんオフにしたいとき。
;; :load-path <path>  load-pathに追加。user-emacs-directory相対で指定。
;; :ensure 存在しなければpackageがその場でpackage-installされる。package名指定。
;;
;; 参考：  http://rubikitch.com/2014/09/09/use-package/

;;; ** el-get utils

;;; http://d.hatena.ne.jp/tarao/20150221/1424518030#tips-byte-compilation 
;;(el-get-bundle tarao/with-eval-after-load-feature-el)
;;(el-get-bundle 'with-eval-after-load-feature)

;; ex)
;; (el-get-bundle tarao/with-eval-after-load-feature-el)
;;
;; (el-get-bundle anything
;;   (global-set-key (kbd "C-x b") 'anything-for-files)
;;   (with-eval-after-load-feature 'anything
;;     (define-key anything-map (kbd "M-n") 'anything-next-source)
;;     (define-key anything-map (kbd "M-p") 'anything-previous-source)))


(el-get-bundle elpa:dash)
(require 'dash)

(el-get-bundle 'log4e)
(require 'log4e)

;;; ** req-package
(el-get-bundle 'req-package)
(require 'req-package)
(req-package-force el-get
 :init
 (progn
   (el-get 'sync)))

;(req-package monokai-theme
;  :config (print "nonokai theme is here and installed from el-get"))

;(el-get 'sync)


;; load auto-install and package.el
;;    cf. http://d.hatena.ne.jp/m-hiyama-memo/20150514/1431592229

;; auto-install
;;(el-get-bundle auto-install)  ;; ese manual install
;;     curl -O http://www.emacswiki.org/emacs/download/auto-install.el
(use-package auto-install
  :disabled ;;
  :config
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup)
  :pin manual
)

;; package.el
(use-package package
  :disabled
  :config
  ;; Add package-archives
  ;;   --- use default valie of 24.1 or later
  ;; Initialize
  (package-initialize)
  :pin manual
)

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 00-packages.el ends here
