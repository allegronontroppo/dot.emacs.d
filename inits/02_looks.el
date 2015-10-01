;;; 02-looks.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: SENDA Shigeya
;; Maintainer: SENDA Shigeya
;; Created: <2014-05-09 00:57:50 shigeya>
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-01 11:06:29 shigeya>

;;; Code:

;;; * 表示系設定

;;; ** buffer

(custom-set-variables
 ;; バッファ画面外文字の切り詰め表示
 '(truncate-lines nil)
 ;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
 '(truncate-partial-width-windows t)
 )

;;; *** uniquify 
;; バッファ名をユニークにする
(use-package uniquify
  :init
  (progn
    ;; 同一バッファ名にディレクトリ付与
    (custom-set-variables
     '(uniquify-buffer-name-style 'forward)
     '(uniquify-buffer-name-style 'post-forward-angle-brackets)
     '(uniquify-ignore-buffers-re "*[^*]+*"))
    ))

;;; *** hiwin-mode
;;    アクティブなバッファと非アクティブなバッファを色づけする。
;;(if (file-exists-p "~/.emacs.d/site-lisp/hiwin.el")
;;    (require 'hiwin "~/.emacs.d/site-lisp/hiwin.el" t)
;;  (require 'hiwin nil t))

;;(when (functionp 'hiwin-activate)
;;  ;; hiwin-modeを有効化
;;  (hiwin-activate)
;;  ;; 非アクティブウィンドウの背景色を設定
;;  (custom-set-faces ;; user theme
;;   '(hiwin-face ((((class color)(background dark)) (:background "gray10"))
;;		 (((class color)(background light))(:background "bisque1"))
;;		 (t ())
;;		 )))
;;  )

;;; ** 行番号

;; バッファ中の行番号表示
(provide 'linum)
(global-linum-mode t)

;; 行番号のフォーマット
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
(set-face-attribute 'linum nil :height 0.8)
(custom-set-variables
 '(linum-format "%4d"))

;;; ** modeline

;; 行番号の表示
(line-number-mode t)

;; 列番号の表示
(column-number-mode t)

;; 時刻の表示
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)

;; cp932エンコード時の表示を「P」とする
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)


;;; ** cursor

;; カーソル点滅表示
(blink-cursor-mode 0)

(custom-set-variables
 ;; スクロール時のカーソル位置の維持
 '(scroll-preserve-screen-position t)

 ;; スクロール行数（一行ごとのスクロール）
 '(vertical-centering-font-regexp ".*")
 '(scroll-conservatively 35)
 '(scroll-margin 0)
 '(scroll-step 1)

 ;; 画面スクロール時の重複行数
 '(next-screen-context-lines 1)
 )

;;; ** scroll

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up-around)
  (interactive)
  (let* ( (start_num (+ 1 (count-lines (point-min) (point))) ) )
    (goto-char (point-max))
    (let* ( (end_num (+ 1 (count-lines (point-min) (point))) ) )
      (forward-line start_num )
      (let* ( (limit_num (- (- end_num start_num) (window-height)) ))
	(if (< (- (- end_num start_num) (window-height)) 0)
	    (goto-char (point-max))
	  ad-do-it)) )) )
(ad-activate 'scroll-up)

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down-around)
  (interactive)
  (let* ( (start_num (+ 1 (count-lines (point-min) (point)))) )
    (if (< start_num (window-height))
	(goto-char (point-min))
      ad-do-it) ))
(ad-activate 'scroll-down)

;;; ** anzu
;;    http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(use-package anzu
  :ensure t
  :bind
  (("M-g r"   . anzu-query-replace)
   ("M-g M-r" . anzu-query-replace-regexp))
  :init
  (progn
    (custom-set-variables
     '(anzu-mode-lighter "")        ; モードラインに表示されるマイナーモード名
     '(anzu-deactivate-region t)    ; 置換コマンド実行時にハイライトを無効
     '(anzu-search-threshold 1000)  ; 1000以上マッチするならそれ以上数えない
     (if (executable-find "cmigemo")
	 '(anzu-use-migemo t)       ; migemoを置換時使用
       '(anzu-use-migemo nil))
     '(anzu-minimum-input-length 3) ; 3文字以上じゃないとanzuにしない。
     ))
  :config
  (progn
    (global-anzu-mode +1)
    ))
;; anzu-query-replaceが query-replaceのanzu版,
;; anzu-query-replace-regexpが query-replace-regexpの anzu版となっています.
;; anzu-query-replace-at-cursor-thingです.
;; これは特定の範囲内(デフォルトでは関数内)の文字列を置換するコマンドで
;; デフォルトの被置換文字列はカーソル下の文字列(厳密にはシンボル)です. 
;; リファクタリングでとても重宝します.

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 02-looks.el ends here

