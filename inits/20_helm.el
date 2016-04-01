;;; 20-helm.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:27:51 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-27 13:42:34 shigeya>

;;; Code:

;;; ** libraries
(el-get-bundle logito
  :type git
  :url "https://github.com/sigma/logito.git")

;;; ** helm
;; from http://koshigoeb.hateblo.jp/entry/2013/01/14/022746
(use-package helm-config
  :ensure helm
  :bind
  (("M-g M-b" . helm-buffers-list) ;; buffer-list
   ;;("M-x"   . helm-M-x)        ;; smexをつかう。
   ("M-g x"   . helm-M-x)        ;; call helm-M-x
   ("M-g M-i" . helm-imenu)      ;; call imenu by M-gi
   ("M-g M-l" . helm-recentf)
   ("M-g M-u" . helm-resume)     ;; same as C-x c b
   ("M-g M-f" . helm-find-files) ;; call anything by M-gf
   ("M-g M-m" . helm-mini)       ;; call helm-mini by M-gm
   ("M-g M-y" . helm-show-kill-ring) ;; kill-ring履歴一覧表示
   ("M-g C-g" . helm-git-find-files)
   ("M-g M-;" . helm-browse-project)
   ;; helm-ag
   ("M-g a g" . helm-ag)
   ("M-g a t" . helm-ag-this-file)
   ("M-g a p" . helm-ag-pop-stack)
   ("M-g a c" . helm-ag-clear-stack)
   ;; swoop
   ("M-'"     . helm-swoop)
   ("M-\""    . helm-swoop-back-to-last-point)
   ("C-c M-'" . helm-multi-swoop)
   ("C-x M-'" . helm-multi-swoop-all)
   )
  :config
  (progn
    (message "loading helm ...")
    (use-package gist
      :ensure t
      :config
      (use-package helm-gist :ensure t))
    (use-package helm-git
      :ensure t
      ;;:config
      ;;(global-set-key (kbd "C-x C-g") 'helm-git-find-files)
      )
    (use-package helm-ls-git :ensure t)
    ;;(use-package helm-themes :ensure t)  ;; カラーテーマは使わなくなった。

    (use-package helm-ag
      :ensure t
;      :bind
;      (("C-z a g" . helm-ag)
;       ("C-z a t" . helm-ag-this-file)
;       ("C-z a p" . helm-ag-pop-stack)
;       ("C-z a c" . helm-ag-clear-stack))
      :config
      (progn
; exec-path-from-shellで解決
;	(custom-set-variables
;	 ;; OSXのemacsでは/usr/local/binにパスが通っていない。
;	 '(helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup")
;	 )
	))

    (use-package helm-gtags
      ;; http://d.hatena.ne.jp/syohex/20120705/1341455747
      ;;(autoload 'gtags-mode "gtags" "" t)
      :ensure t
      :config
      (progn
	(eval-after-load "helm-gtags"
	  '(progn
	     (bind-key "M-g t t" 'helm-gtags-find-tag helm-gtags-mode-map)
	     (bind-key "M-g t r" 'helm-gtags-find-rtag helm-gtags-mode-map)
	     (bind-key "M-g t s" 'helm-gtags-find-symbol helm-gtags-mode-map)
	     (bind-key "M-g t p" 'helm-gtags-parse-file helm-gtags-mode-map)
	     (bind-key "M-g t <" 'helm-gtags-previous-history helm-gtags-mode-map)
	     (bind-key "M-g t >" 'helm-gtags-next-history helm-gtags-mode-map)
	     (bind-key "C-M-g"   'helm-gtags-pop-stack helm-gtags-mode-map)))
	(add-hook 'c-mode-hook 'helm-gtags-mode)
	(add-hook 'c++-mode-hook 'helm-gtags-mode)
	(add-hook 'java-mode-hook 'helm-gtags-mode)
	(add-hook 'asm-mode-hook 'helm-gtags-mode)
	;; customize
	(custom-set-variables
	 '(helm-gtags-path-style 'relative)
	 '(helm-gtags-ignore-case t)
	 '(helm-gtags-auto-update t))
	))

    ;; http://fukuyama.co/helm-swoop
    (use-package helm-swoop
      :ensure t
;      :bind
;      (("M-'"  . helm-swoop)
;       ("M-\"" . helm-swoop-back-to-last-point)
;       ("C-c M-'" . helm-multi-swoop)
;       ("C-x M-'" . helm-multi-swoop-all))
      :config
      (progn
	;; isearch実行中にhelm-swoopに移行
	(bind-key "M-'" 'helm-swoop-from-isearch isearch-mode-map)
	;; helm-swoop実行中にhelm-multi-swoop-allに移行
	(bind-key "M-'" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)

	;; Save buffer when helm-multi-swoop-edit complete
	(setq helm-multi-swoop-edit-save t)

	;; 値がtの場合はウィンドウ内に分割、nilなら別のウィンドウを使用
	(setq helm-swoop-split-with-multiple-windows nil)

	;; ウィンドウ分割方向 'split-window-vertically or 'split-window-horizontally
	(setq helm-swoop-split-direction 'split-window-vertically)

	;; nilなら一覧のテキストカラーを失う代わりに、起動スピードをほんの少し上げる
	(setq helm-swoop-speed-or-color t)
	))

    (use-package helm-descbinds
      ;; http://emacs-jp.github.io/packages/helm/helm-descbinds.html
      :ensure t
      :config (helm-descbinds-install)
      )
    (when (executable-find "cmigemo")
      (use-package helm-migemo
	:ensure t
	;;:bind   (("C-:" . helm-migemo))
	:init
	(progn
	  (setq helm-use-migemo t)
	  (define-key global-map [(control ?:)] 'helm-migemo)
	  ))
      ;;(require 'dired+) ;; helmから呼び出したdiredがdired+になるように。
      (message "loading helm ... done.")
      )
    )
  )

;;; ** smex
;;  M-x のidoバインディング拡張。TABで補完 C-s/C-rで候補を移動
(use-package smex
  :ensure smex
  :bind
  (
   ("M-x"     . smex)                            ;; call helm-M-x
   ("M-X"     . smex-major-mode-commands)        ;; call helm-M-x
   ("C-c M-x" . smex-update)
   ("C-c C-c M-x" . execute-extended-command)
   )
  :config
  (progn
    (smex-initialize)
    ))

;;(use-package helm-package
;;  :ensure t
;;  )

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 20-helm.el ends here

