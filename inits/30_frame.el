;;; 30_frame.el --- frame settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 15:20:05 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2016-02-03 12:15:47 shigeya>

;;; Code:

;;; * frame.el

;;; ** フレームタイトルの設定
(setq frame-title-format "%b (%Z)")


;;; ** frame size toggle (max <-> normal)
;;
;; フレームサイズをトグルで切り替え
;; http://www.bookshelf.jp/soft/meadow_30.html#SEC416
;; 現在のフレームサイズを調べる方法
;; *scratch*バッファを開き， (frame-width), (frame-height) と書き， C-j する
;;
;; 引用元からの変更：
;;      既にサポートされていないw32関数を削除 (windows非依存)
;;      set-frame-sizeで大きさを変更
(defvar my-frame-max-flag nil "frame state for my-toggle-frame-size.")
(defvar my-last-frame-conf nil "last frame configuration.")

(defun my-toggle-frame-size ()
  (interactive)
  (if my-frame-max-flag
      (progn
        (setq my-frame-max-flag nil)
	(set-frame-configuration my-last-frame-conf)
	(message "not MAX.") (sit-for 2)
	)
      (setq my-frame-max-flag t)
      (setq my-last-frame-conf (current-frame-configuration))
      (set-frame-position (selected-frame) 0 0)
      ;; フレーム最大化時に (frame-width) (frame-height) で得た値
      (set-frame-size (selected-frame) 171 60)
      (message "to MAX.") (sit-for 2)
      ))

;; ediff 立ち上げ時に、画面を最大化
(add-hook 'ediff-before-setup-hook 'my-toggle-frame-size)
;; ediff 終了時に、画面を元に戻す
(add-hook 'ediff-quit-hook 'my-toggle-frame-size)

(global-set-key [f9] 'my-toggle-frame-size)      ; f9 に割り当て

;; ** theme設定

;; lightテーマかdarkテーマかで設定し直すこと。
;;
;; 各種色の設定は custom-set-facesで行い、light/darkの設定を行う。
;;  http://kei10in.hatenablog.jp/entry/20101101/1288617632
;;
;;(defvar **use-light-theme** nil "light theme or not")
(defvar **use-light-theme** t "light theme or not")
(cond 
 (**use-light-theme**
  (el-get-bundle 'moe-theme)
  (req-package moe-theme
    :loader el-get
    :ensure t)
  ;;(load-theme 'humane t)
  (load-theme 'moe-light t)
  ;;      (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  ;;      (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))
  (setq moe-theme-mode-line-color 'orange)
  ;;(when (functionp 'powerline-moe-theme)
  ;;  (powerline-moe-theme)
  ;;  )
  )
 (t
  ;;(load-theme 'wheatgrass)
  ;;(load-theme 'manoj-dark)
  ;;(el-get-bundle subatomic256
  ;;  :type git
  ;;  :url "https://github.com/d11wtq/subatomic256"
  ;;  )
  ;;(use-package subatomic256-theme :ensure t)
  ;;(load-theme 'subatomic256 t)
  ;;(use-package solarized-theme :ensure t)
  ;;(load-theme 'solarized-dark t)
  ;;(use-package color-theme-sanityinc-tomorrow :ensure t)
  ;;(load-theme 'sanityinc-tomorrow-night t)
  ;;(use-package color-theme-sanityinc-solarized :ensure t)
  ;;(load-theme 'sanityinc-solarized-dark t)
  ;;(use-package zenburn-theme :ensure t)
  ;;(load-theme 'zenburn t)
  ;;(use-package moe-theme :ensure t)
  ;;(load-theme 'moe-dark t)
  ;;(use-package zonokai-theme :ensure t)
  ;;(load-theme 'zonokai t)
  (el-get-bundle 'tomorrow-theme)
  ;;(use-package tomorrow-theme :ensure t :pin manual)
  (load-theme 'tomorrow-night-bright t)
  ))

;;; ** frame decolations at daemon mode

(defun setup-frame-hook (frame)
  "This function will be applied to all new emacs frames."
  (select-frame frame)

  ;; frame設定 (余計なものをすべて排除した設定）
  (set-frame-parameter frame 'alpha '(95 95)) ; translucency
  ;; mouse-avoidance : キー入力中にマウスポインタを移動するスタイル
  (mouse-avoidance-mode 'cat-and-mouse) ; avoid mouse
  ;; 余白：linenumでは数字と本文の間。左３右０
  (tool-bar-mode 0)                     ; no toolbar
  (menu-bar-mode 0)                     ; no menubar
  (when (not (eq window-system nil))
    (scroll-bar-mode 0)                 ; no scrollbar
    (fringe-mode (cons 3 nil))          ; make fringes smaller
    )
  ;; フォント設定
  (cond
   ((eq window-system nil) nil) ;; ttyなら何もしない。
   ((eq window-system 'x)
    ;; Motoyaフォントのinstallが必要
    (set-frame-font "MotoyaLCedar-12")
    (set-fontset-font (frame-parameter nil 'font)
		      'japanese-jisx0208
		      '("MotoyaLCedar" . "unicode-bmp")
		      ))
   ((eq window-system 'ns)
    ;; Rictyって等倍じゃないの？？→1.5の倍数のみ、等倍らしい。
    ;; http://blog.sanojimaru.com/post/19807398882/cocoa-emacs-ricty
    (let* ((size 15)
	   (asciifont "Ricty")
	   (jpfont "Ricty")
	   (h (* size 10))
	   (fontspec (font-spec :family asciifont))
	   (jp-fontspec (font-spec :family jpfont)))
      (set-face-attribute 'default frame :family asciifont :height h)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      (set-fontset-font nil '(#x0080 . #x024F) fontspec)
      (set-fontset-font nil '(#x0370 . #x03FF) fontspec))
    )
   ;; gnupack-windows版は初期設定(Migu M1)で何もしない。
   (t nil))

  ;; 明示的にbackground-modeを指定
  (if **use-light-theme**
      (setq frame-background-mode 'light)
    (setq frame-background-mode 'dark))

  ;; emacs --daemon のとき、user themeはenableじゃないらしい。
  ;; custom-set-faceなどの設定が有効にならないのでenableに。
  (enable-theme 'user)

  (when (not (eq window-system 'ns))
    ;; ime offのカーソル色の保存
    (setq saved-cursor-color (get-cursor-color))
    )
  )


;; emacsclientなどで新規に生成されたframeに上記設定を適応
(add-hook 'after-make-frame-functions 'setup-frame-hook)

;; emacsの通常起動で適用するようhookに登録
(add-hook 'window-setup-hook
          (lambda ()
	    (setup-frame-hook (selected-frame))
	    ))

;; *** ime切り替え時のカーソル色を変える。
(when (not (eq window-system 'ns))
  ;;   MACは独自にもっているのでそれを利用する。
  ;;   注：フレームが複数あり状態が連動してないときダメな実装 面倒なのでこのまま。。。

  ;; IME OFF時の初期カーソルカラー
  (defun get-cursor-color ()
    (frame-parameter (selected-frame) 'cursor-color))

  (defun set-cursor-color (color)
    (modify-frame-parameters (selected-frame) `((cursor-color . ,color))))

  (defvar saved-cursor-color "#cccccc"
    "default cursor color in current frame.")

  ;; IME ON/OFF時のカーソルカラー
  (add-hook 'input-method-activate-hook
	    #'(lambda()
		;;(setq saved-cursor-color (get-cursor-color))
		(set-cursor-color "LightSlateBlue")))
  (add-hook 'input-method-deactivate-hook
	    #'(lambda() (set-cursor-color saved-cursor-color)))
  )
;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 30_frame.el ends here



