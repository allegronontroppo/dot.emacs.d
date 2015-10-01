;;; 03-tabbar.el --- tabbar settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 14:55:05 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-09-28 17:35:42 senda>

;;; Code:

;;; ** tabbar

(use-package tabbar
  :ensure t
  :bind     ;; キーバインド設定
  (("<C-tab>" .   tabbar-forward-tab)
   ("<C-S-tab>" . tabbar-backward-tab))
  :config
  (progn
    ;; tabbar有効化   ;; ただし、初期化で設定しても画面に反映されない問題あり
    (tabbar-mode)

    ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
    (tabbar-mwheel-mode -1)
    
    ;; タブグループを使用（t：有効，nil：無効）
    (setq tabbar-buffer-groups-function nil)

    ;; ボタン非表示
    (dolist (btn '(tabbar-buffer-home-button
		   tabbar-scroll-left-button
		   tabbar-scroll-right-button))
      (set btn (cons (cons "" nil) (cons "" nil))))

    ;; タブ表示 一時バッファ一覧
    (defvar tabbar-displayed-buffers
      '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*"
	"*Faces*" "*Apropos*" "*Customize*" "*shell*" "*Help*")
      "*Regexps matches buffer names always included tabs.")

    ;; 作業バッファの一部を非表示
    (setq tabbar-buffer-list-function
	  (lambda ()
	    (let* ((hides (list ?\  ?\*))
		   (re (regexp-opt tabbar-displayed-buffers))
		   (cur-buf (current-buffer))
		   (tabs (delq
			  nil
			  (mapcar
			   (lambda (buf)
			     (let ((name (buffer-name buf)))
			       (when (or (string-match re name)
					 (not (memq (aref name 0) hides)))
				 buf)))
			   (buffer-list)))))
	      (if (memq cur-buf tabs)
		  tabs
		(cons cur-buf tabs)))))
    ;; font & face
    (let*
	((font (if (eq window-system 'w32) "ＭＳ Ｐゴシック" "Ricty"))
	 (bg (if (eq window-system 'w32) "SystemMenuBar" "DarkSlateGray3"))
	 )
      ;; タブ表示欄の見た目（フェイス）
      (set-face-attribute 'tabbar-default nil
			  :background bg)
      ;; 選択タブの見た目（フェイス）
      (set-face-attribute 'tabbar-selected nil
			  :foreground "red3"
			  :background bg
			  :box (list
				:line-width 1
				:color "gray80"
				:style 'released-button)
			  :overline "#F3F2EF"
			  :weight 'bold
			  :family font
			  )
      ;; 非選択タブの見た目（フェイス）
      (set-face-attribute 'tabbar-unselected nil
			  :foreground "black"
			  :background bg
			  :box (list
				:line-width 1
				:color "gray80"
				:style 'released-button)
			  :overline "#F3F2EF"
			  :family font
			  )
      )
    ;; タブ間隔の調整
    (set-face-attribute 'tabbar-separator nil
			:height 0.1)
    ))

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 03-tabbar.el ends here

