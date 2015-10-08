;;; 01-settings.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: SENDA Shigeya
;; Maintainer: SENDA Shigeya
;; Created: <2014-05-09 00:57:50 shigeya>
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-02 10:24:32 shigeya>

;; from http://quantumfluctuation.blogspot.jp/2011/07/gnupack-cygwin-emacs.html

;;; Code:

;;; * settings

;;; ** startup settings
(custom-set-variables
 ;; 起動メッセージの非表示
 '(inhibit-startup-message t)
 ;; スタートアップ時のエコー領域メッセージの非表示
 '(inhibit-startup-echo-area-message -1)
 )

;;; ** directory settings
;;    from : http://qiita.com/ShingoFukuyama/items/19b02cd1679a6ea0bfdb

(custom-set-variables '(user-full-name (cl-substitute ?_ ?/ user-full-name))) ;; normalize!

;; /path/to/userhome/.emacs.d/
(defvar my-emacs-dir (expand-file-name user-emacs-directory))
;; /path/to/userhome/.emacs.d/hist/
(defvar my-history-dir (concat my-emacs-dir "hist/"))
;; /path/to/userhome/.emacs.d/backup/
(defvar my-backup-dir (concat my-emacs-dir "backup/"))
(if (not (file-directory-p my-history-dir)) (make-directory my-history-dir))
(if (not (file-directory-p my-backup-dir))  (make-directory my-backup-dir))

(defun my-set-history (&rest args)
  (concat my-history-dir (mapconcat 'identity args "")))

;; backup
(add-to-list 'backup-directory-alist (cons "." my-backup-dir))

(if (eq system-type 'windows-nt) ;; no user-full-name on windows emacs env.
    (custom-set-variables '(user-full-name (getenv "USERNAME"))))

;; trash
(when (not (functionp 'system-move-file-to-trash))
  ;; system-move-file-to-trashが環境で定義されていないときに使われる。
  (custom-set-variables '(trash-directory "~/.Trash"))
  (if (not (file-directory-p trash-directory)) (make-directory trash-directory)))

(custom-set-variables
 ;; auto-save
 '(auto-save-file-name-transforms `((".*" ,my-backup-dir t)))
 ;; auto-save-list
 '(auto-save-list-file-prefix (my-set-history "auto-save-list/.saves-"))
 ;; bookmark
 '(bookmark-default-file (my-set-history "bookmark-" user-full-name))
 ;; tramp cache
 '(tramp-persistency-file-name (my-set-history "tramp-" user-full-name))
 ;; tramp backup
 '(tramp-backup-directory-alist backup-directory-alist)
 ;; savehist (minibuffer histoy is saved if savehist-mode is on)
 '(savehist-file (my-set-history "savehist-" user-full-name))
 ;; recentf
 '(recentf-save-file (my-set-history "recentf-" user-full-name))
 ;; save-place
 '(save-place-file (my-set-history "places-" user-full-name))

 ;; 外部拡張

 ;; save-kill  - helm-ringがあるから使わないんだが。。
 '(save-kill-file-name (my-set-history "kill-ring-saved"))
 ;; undohist
 '(undohist-directory (my-set-history "undohist"))
 ;; auto-complete
 '(ac-comphist-file (my-set-history "ac-comphist.dat"))
 ;; multiple-cursors
 '(mc/list-file (my-set-history "mc-lists.el"))
 ;; request
 '(request-storage-directory (my-set-history "request-" user-full-name))
 ;; helm-github-stars
 '(helm-github-stars-cache-file (my-set-history "helm-github-stars-cache"))
 ;; helm-recentd
 '(helm-recentd-file (my-set-history "helm-recentd-" user-full-name))
 ;; smex history
 '(smex-save-file (my-set-history "smex-items-" user-full-name))
)

;;; ** save actions and parameters

;;; ** historyのセーブを有効に。
(use-package savehist
  :init
  (savehist-mode 1)
  )

;;; ** 前回編集していた場所を記憶し，ファイルを開いた時にそこへカーソルを移動
(use-package saveplace
  :init
  (custom-set-variables '(save-place t))
  )

;;; ** 最後の行に必ず改行をいれてセーブする。
(custom-set-variables '(require-final-newline t))

;;; ** write time stamp string when the file save.
(add-hook 'before-save-hook 'time-stamp)
;; 以下のように書いてあるとタイムスタンプが残る。
;;      Time-stamp: ＜＞
;;      Time-stamp: ””
;; Time-stamp: " "

;;; ** undohist
(el-get-bundle 'undohist)
(use-package undohist
  :ensure t
  ;;  :loader el-get
  :config
  (progn
    (undohist-initialize)
    )
  :pin manual
  )

;;; ** redo+
(use-package redo+
  :disabled ;; evilをつかうとundo-treeが入る。これを設定する必要なし。
  :ensure t
  :bind
  (("C-M-/" . redo))
  :init
  (progn
    (setq undo-no-redo t)
    ))

;;; ** parameters
(custom-set-variables
 ;; 区切り文字に全角スペースや、・を含める
 '(paragraph-start '"^\\([ 　・○<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

 ;; ガベッジコレクション閾値 を 40MB に
 '(gc-cons-threshold 41943040)

 ;; ログの記録行数を増やす
 '(message-log-max 10000)

 ;; undo をいっぱい
 '(undo-limit 100000)
 '(undo-strong-limit 130000)

 ;; キーストロークの表示速度を上げる
 '(echo-keystrokes 0.1)

 ;; でかいファイルも普通に開く（デフォルトは 10MB ）
 '(large-file-warning-threshold (* 50 1024 1024)) ;50MB
 )

;;; ** いちいち yes とか無理だから y にする
(defalias 'yes-or-no-p 'y-or-n-p)

;;; ** BS や Delete キーでリージョン内の文字を削除
(delete-selection-mode 1)

;;; ** kill-ring はテキスト属性（色情報など）を保存しなくていい
;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#yankoff
(defadvice kill-new (around my-kill-ring-disable-text-property activate)
  (let ((new (ad-get-arg 0)))
    (set-text-properties 0 (length new) nil new)
    ad-do-it))

;;; ** 括弧の対応表示
;; メモ：
;; 簡単な長い範囲でのカッコ対応確認方法：
;;    →  ')'の後ろでC-M-bで対応する'('に飛ぶ。C-M-fで元に。
(use-package mic-paren ; loading
  :disabled ;; smartparen使うので。
  :ensure t
  :init
  (progn
    (custom-set-variables
     '(parse-sexp-ignore-comments t) ;; コメント内のカッコは無視。(c-mode)
     )
    (paren-activate)     ; activating
    ))

;;; ** smartparens
(el-get-bundle 'smartparens)
(use-package smartparens ;; smsmartparens-config
  :ensure t ; smartparens
  :init
  (progn
    (custom-set-variables
     '(parse-sexp-ignore-comments t) ;; コメント内のカッコは無視。(c-mode)
     )
    (show-smartparens-global-mode 1) ;; 表示
    (smartparens-global-mode t)
    )
  :pin manual
  )

;;; ** recentf
;;  - 最近開いたファイルを保存しておいて開くときに選択
(use-package recentf
  :init
  (progn
    ;; http://masutaka.net/chalow/2011-10-30-2.html
    ;;   うるさいメッセージをminibufferに吐かないように。
    (defvar my-recentf-list-prev nil)
 
    (defadvice recentf-save-list
      (around no-message activate)
      "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
      (unless (equal recentf-list my-recentf-list-prev)
	(cl-flet ((message (format-string &rest args)
			   (eval `(format ,format-string ,@args)))
		  (write-file (file &optional confirm)
			      (let ((str (buffer-string)))
				(with-temp-file file
				  (insert str)))))
	  ad-do-it
	  (setq my-recentf-list-prev recentf-list))))
 
    (defadvice recentf-cleanup
      (around no-message activate)
      "suppress the output from `message' to minibuffer"
      (cl-flet ((message (format-string &rest args)
			 (eval `(format ,format-string ,@args))))
	ad-do-it))
    (custom-set-variables '(recentf-max-saved-items 500)
			  '(recentf-exclude '("/TAGS$" "/var/tmp/"
					      "recentf" ".cask"
					      "archive-contents"
					      ".*-autoloads.el"
					      ".*loaddefs.el"
					      ))
			  ;;'(recentf-save-file "~/.emacs.d/recentf")
			  '(recentf-auto-cleanup 30)
			  )
    (use-package recentf-ext)
    (run-with-idle-timer 30 t 'recentf-save-list)
    (recentf-mode 1)
    ))

;;; ** bookmarks
;;「C-x r m」カレントバッファをブックマークに追加
;;   (デフォルトではファイル名＝ブックマーク名)
;;「C-x r b」ブックマーク名を指定して開く
;;「C-x r l」ブックマークの一覧を表示
;;「d」一覧表示で、ブックマークに削除マークをつける
;;「x」一覧表示で、削除マークのついたものを削除
(use-package bookmark
  :config
  (progn
    (custom-set-variables
     '(bookmark-save-flag 1)
     )
    ))

;;; ** bind-keyによるキーカスタマイズ

(use-package bind-key
  :ensure t
  :init
  (progn
    ;;(global-unset-key "\C-z") ;; I use it as prefix key.
    ;;(define-prefix-command 'user-specific-command-prefix)
    ;;(defvar mode-specific-map (symbol-function 'user-specific-command-prefix)
    ;;"Keymap for characters following C-z.")
    ;;(define-key global-map "\C-z" 'user-specific-command-prefix)

    ;; you can see your bindings by (describe-personal-keybindings).
    (bind-key "M-g ?"   'describe-personal-keybindings)

    (bind-key "M-g M-s" 'speedbar)   ;; call speedbar by C-zs
    ;;(bind-key "C-x 4 l" 'goto-line)
    (bind-key "M-g M-z" 'compile)
    (bind-key "M-g M-x" 'clipboard-kill-region)
    (bind-key "M-g M-c" 'clipboard-kill-ring-save)
    (bind-key "M-g M-v" 'clipboard-yank)
    (use-package whitespace)
    (bind-key "M-g M-w" 'whitespace-mode)     ;; toggle whitespace-mode
    (bind-key "C-|"     'toggle-input-method) ;; define it instead of C-\
    (bind-key "S-SPC"   'toggle-input-method)
    ;;(bind-key "s-SPC"   'toggle-input-method)
    )
  :pin manual
)

;;; ** Tramp
;; リモートのファイル・ディレクトリを透過的に開く。
;; http://tramp.sourceforge.net/tramp_ja.html
;; 標準で入っているのでやり方さえ知っていればよい。
(use-package tramp
  :init
  (custom-set-variables '(tramp-default-method "scpc"))
  :pin manual
  )

;;; ** backup

(custom-set-variables
 ;; 変更ファイルのバックアップ
 ;; '(make-backup-files nil)  ;; nilのとき、ファイルのbackupが無効化される。

 ;; 変更ファイルの番号つきバックアップ
 '(version-control nil)

 ;; 編集中ファイルのバックアップ間隔（秒）
 '(auto-save-timeout 30)      ;; デフォルト30秒

 ;; 編集中ファイルのバックアップ間隔（打鍵）
 '(auto-save-interval 500)    ;; デフォルト300

 ;; バックアップ世代数
 '(kept-old-versions 1)
 '(kept-new-versions 2)

 ;; 古いバックアップファイルの削除
 '(delete-old-versions t)

 ;; 自動保存ファイルのリストファイルを管理する
 ;; '(auto-save-list-file-prefix nil) ;; nilに設定するとリストファイルをつくらない。
 )
;;(setq auto-save-list-file-name nil) ;; 自動保存ファイルリスト。

;; 以下はディレクトリ設定で設定済み
;; 編集中ファイルのバックアップ先 (pathで先に設定済み)
;; '(auto-save-file-name-transforms `((".*" ,my-backup-dir t)))

;; 上書き時の警告表示
;; (setq trim-versions-without-asking nil)

;;; ** auto-save-buffer-enhanched

(use-package auto-save-buffers-enhanced
  :ensure t
  :config
  (progn
    ;; git/cvs/svn checkout file
    (auto-save-buffers-enhanced-include-only-checkout-path t)
    ;; .orgは自動保存
    (setq auto-save-buffers-enhanced-include-regexps (list "\\.org$"))
    (auto-save-buffers-enhanced t)
    ))

;;; ** expand-region

(use-package expand-region :ensure t)

;;; ** multiple-cursors

(use-package multiple-cursors
  :ensure t
  ;; キーバインドはevil-leaderで設定する。
  ;; "C-\ c" 'mc/mark-next-like-this
  ;; "C-\ C" 'mc/mark-all-like-this
  ;;  C-\ C-c 'mc/mark-more--like-this-extended
  ;;  C-\ E : mc/edit-lines
  )

(use-package smartrep
  :disabled
  :ensure t
  )

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 01-settings.el ends here


