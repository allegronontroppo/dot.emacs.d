;;; 10_modes.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: SENDA Shigeya
;; Maintainer: SENDA Shigeya
;; Created: <2014-05-09 00:57:50 shigeya>
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-06 17:10:27 shigeya>

;;; Code:

;;; * modes
;;; ** font-lock mode
(use-package font-lock
  :config
  (progn
    ;; タブ, 全角スペースを色つき表示 (色名は M-x list-color-display で調べる)
    ;;  http://homepage1.nifty.com/blankspace/emacs/color.html
    (defface my-face-b-1 ;; 全角スペース
      '((((class color)(background dark)) (:background "khaki4"))
	(((class color)(background light))(:background "khaki"))
	(t (:background "LightGoldenrod4"))) nil)
    (defface my-face-b-2 ;; タブ
      '((((class color)(background dark)) (:background "gray16"))
	(((class color)(background light))(:background "white"))
	(t (:background "gray16"))) nil)
    (defface my-face-u-1 ;; 行末空白
      '((((class color)(background dark)) (:foreground "orange4" :underline t))
	(((class color)(background light))(:foreground "orange" :underline t))
	(t (:foreground "LightSkyBlue4" :underline t))) nil)
    (defvar my-face-b-1 'my-face-b-1)
    (defvar my-face-b-2 'my-face-b-2)
    (defvar my-face-u-1 'my-face-u-1)
    (defadvice font-lock-mode (before my-font-lock-mode ())
      (font-lock-add-keywords
       major-mode
       '(("\t" 0 my-face-b-2 append)		; タブ
	 ("　" 0 my-face-b-1 append)		; 全角スペース
	 ("[ \t]+$" 0 my-face-u-1 append)	; 行末空白
	 )))
    (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
    (ad-activate 'font-lock-mode)

    (global-font-lock-mode t)	; 常に有効
    ))

;;; ** cua-mode
;;     C-return で矩形選択モードに。

;;M-p 	矩形の幅を固定
;;M-b 	空白文字で埋める。 open-rectangle と同等
;;M-s 	文字列で置き換える。 string-rectangle と同等
;;M-f 	1種類の文字で埋める。 string-rectangle で1文字指定したときと同等
;;M-i 	矩形領域内の数字をインクリメントする
;;M-n 	矩形領域を連番で埋める。フォーマット指定可
;; http://kreisel.fam.cx/webmaster/clog/2010-09-20-1.html

(use-package cua-base
  :config
  (progn
    (cua-mode t)
    (custom-set-variables
     '(cua-enable-cua-keys nil))
    ;;disable using C-z, C-x, C-c, and C-v for undo, cut, copy, and paste.
    ;; http://vimeo.com/1168225?pg=embed&sec=1168225
    ))

;;; ** w3m
(defun dired-w3m-find-file ()
  (interactive)
  (require 'w3m)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
        (w3m-find-file file))))

(use-package w3m
  :commands (w3m w3m-browse-url w3m-find-file w3m-session-crash-recovery-remove)
  :config
  (progn
    (eval-when-compile
      (autoload 'w3m-search-escape-query-string "w3m-search")
      (autoload 'w3m-search "w3m-search" "Search words using emacs-w3m." t)
      (autoload 'w3m-weather "w3m-weather" "Display a weather report." t)
      (autoload 'w3m-antenna "w3m-antenna" "Report changes of web sites." t)
      (autoload 'w3m-namazu "w3m-namazu" "Search files with Namazu." t))
    (custom-set-variables '(w3m-init-file (locate-user-emacs-file "w3m.el"))))
  )

;;; **dired

;;; *** 汎用関連付けアプリでファイルを開く
(defun my-x-open (file)
  "open file by a associated program."
  (interactive "FOpen File: ")
  (message "Opening %s..." file)
  (cond ((not window-system)
         (find-file file))
        ((eq system-type 'windows-nt)
         (call-process "cmd.exe" nil 0 nil "/c" "start" ""
		       (convert-standard-filename file)))
        ((eq system-type 'darwin)
         (call-process "open" nil 0 nil file))
        (t
         (call-process "xdg-open" nil 0 nil file)))
  (if (functionp 'recentf-add-file)
    (recentf-add-file file))
  (message "Opening %s...done" file))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (my-x-open (dired-get-filename)))

;;; *** dired+
;; diredの拡張版。mapやhookはdiredと共通になってる。
;; 拡張のポイント：
;; 【w】 to copy file name. Works with multiple marked files too.
;; 【F】 to open all marked files. 
;; 【A】 to regex search marked files.
;; 【B】 to byte compile a elisp file.
;; 【L】 to load a elisp file.
;;  *. で拡張子指定マーク
;;  %. でregexでマーク
(use-package dired+
  :disabled t  ;; disable!!
  :ensure t
  :bind
  (("C-x d"   . diredp-dired-files)
   ("C-x 4 d" . diredp-dired-files-other-window))
  :init
  (progn
    ;; 以下の関数リストがfind-fileでdirだった時に呼び出される。
    (custom-set-variables
     ;;'(find-directory-functions '(cvs-dired-noselect diredp-dired-files))
     '(find-directory-functions '(diredp-dired-files))
    ))
  :config
  (progn
    (bind-key "\'" 'dired-open-file dired-mode-map) ;; [']で関連付けopen
    (bind-key "C-x m" 'dired-w3m-find-file dired-mode-map)
    (bind-key "M-g" nil dired-mode-map) ;; M-gは使いまくってるので奪われたくない。
    (custom-set-variables
     ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先を
     ;; もう一方のdiredで開いているディレクトリにする
     '(dired-dwim-target t)
     ;; ディレクトリを再帰的にコピーする
     '(dired-recursive-copies 'always)
     ;; diredバッファでC-sした時にファイル名だけにマッチするように
     '(dired-isearch-filenames t)
     ;; 時間順表示
     ;; macはglsを使うのが前提。設定はcocoa-init.elで行う。
     '(dired-listing-switches "-latFL --time-style=long-iso"))
    ))


;;; ** auto-complete
(use-package auto-complete
  
  )

;;; ** ediff
(use-package ediff-hook)

;;; ** diff-modeの色づけ
;; http://www.clear-code.com/blog/2012/4/3.html

;; diffの表示方法を変更
(defun diff-mode-setup-faces ()
  ;; 追加された行は緑で表示
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "dark green")
  ;; 削除された行は赤で表示
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "dark red")
  ;; 文字単位での変更箇所は色を反転して強調
  (set-face-attribute 'diff-refine-change nil
                      :foreground nil :background nil
                      :weight 'bold :inverse-video t))
(add-hook 'diff-mode-hook 'diff-mode-setup-faces)

;; diffを表示したらすぐに文字単位での強調表示も行う
(defun diff-mode-refine-automatically ()
  (diff-auto-refine-mode t))
(add-hook 'diff-mode-hook 'diff-mode-refine-automatically)

;; diff関連の設定
(defun magit-setup-diff ()
  ;; diffを表示しているときに文字単位での変更箇所も強調表示する
  ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
  (setq magit-diff-refine-hunk 'all)
  ;; diff用のfaceを設定する
  (diff-mode-setup-faces)
  ;; diffの表示設定が上書きされてしまうのでハイライトを無効にする
  (set-face-attribute 'magit-item-highlight nil :inherit nil))

(eval-after-load "magit"
  '(add-hook 'magit-mode-hook 'magit-setup-diff))

;;; ** slime lisp mode

;(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system -> sbcl
;(setq inferior-lisp-program "/usr/bin/sbcl") ;; your Lisp system -> sbcl
(use-package slime-autoloads
  :disabled
  :ensure slime
  :config
  (progn
    (require 'slime)
    (use-package slime-fuzzy :ensure t)
    (custom-set-variables
     '(slime-net-coding-system 'utf-8-unix)
     '(slime-contribs '(slime-fancy slime-repl slime-fuzzy slime-scheme))
     )
    (eval-after-load "auto-complete"
      (use-package ac-slime
	:ensure t
	:config
	(progn
	  (add-hook 'slime-mode-hook      'set-up-slime-ac)
	  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
	  (add-to-list 'ac-modes 'slime-repl-mode)
	  )))
    ;(require 'slime-autoloads)
    ;; ac

    ;; slime gauche
    ;; Gaucheのソースを持っていて、かつ、コンパイル済の場合、ソースのトップ
    ;; ディレクトリへのパスを設定して下さい。Gaucheのマニュアルに記載されている
    ;; オペレータの引数名がルックアップ出来るようになります。
;    (setq swank-gauche-gauche-source-path nil)

    ;; - (push swank-gauche-path load-path)
;    (require 'swank-gauche)

;    (setq slime-lisp-implementations
;	  '((gauche ("gosh") :init gauche-init :coding-system utf-8-unix)))

    ;; バッファのモジュールを決定するための設定
;    (setq slime-find-buffer-package-function 'find-gauche-package)
    ;; c-p-c補完に設定
;    (setq slime-complete-symbol-function 'slime-complete-symbol*)
    ;; web上のGaucheリファレンスマニュアルを引く設定
;    (define-key slime-mode-map (kbd "C-c C-d H") 'gauche-ref-lookup)
    ))

;;; ** eldoc
;; minibufferにカーソル位置周辺の関数の説明を表示する。
;;   http://yohshiy.blog.fc2.com/blog-entry-251.html
(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (eldoc-mode 1)))
  )

;;カッコの色づけ。微妙なので使わない。
;;(require 'rainbow-delimiters)
;;(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode 1)))

;; edebug :   C-u C-M-x でedebug対象関数になる。 C-M-xで関数定義されedebugから外れる。
;; http://d.hatena.ne.jp/rubikitch/20101116/edebug

;;; ** elisp slime navi
;; slimeのナビゲーションをelispに移植したもの
(use-package elisp-slime-nav
  :config
  (progn
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode))
    )
  )
;; M-x ielm  emacs-lisp のrepl環境 (標準ライブラリ)
;; M-. 関数定義元にジャンプ
;; M-, 定義元へのジャンプから元の位置に戻る。
;; C-c C-d d / C-c C-d C-d  関数シンボルのドキュメントを表示する。

;;; ** clojure

(el-get-bundle 'cider)
(use-package cider
  :pin manual
  :ensure cider
  :commands (cider-mode)
  :config
  (progn
    (add-hook 'cider-mode-hook 'cide-turn-on-eldoc-mode)
    ))

(el-get-bundle 'clojure-mode)
(use-package clojure-mode
  :pin manual
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook 'cide-mode)
    ))

;;; ** Perl
;(el-get-bundle 'cperl-mode)
(use-package cperl-mode
  :disabled    ;; mode-compileがel-getでうまく見つからないのでdisableへ
  :pin manual
  :config
  (progn
    (setq cperl-indent-level 4
	  cperl-continued-statement-offset 4
	  cperl-close-paren-offset -4
	  cperl-label-offset -4
	  cperl-comment-column 40
	  cperl-highlight-variables-indiscriminately t
	  cperl-indent-parens-as-block t
	  cperl-tab-always-indent nil
	  cperl-font-lock t)
    (setq cperl-hairy t)
    ;;
    ;; - (use-package perl-completion)
    ;; - (add-to-list 'ac-sources 'ac-source-perl-completion)
    ;;
    (add-hook 'cperl-mode-hook
	      '(lambda ()
		 (progn
		   (setq indent-tabs-mode nil)
		   (setq tab-width nil)
		   ;; - (perl-completion-mode t)
		   (require 'auto-complete)
		   (add-to-list 'ac-sources 'ac-source-my-perl-completion)
              )))
    )
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\.\([pP][Llm]\|al\|t\|cgi\)\'" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
    ;; cperl-mode is preferred to perl-mode
    ;; "Brevity is the soul of wit" <foo at acm.org>
    (defalias 'perl-mode 'cperl-mode)

    (defvar ac-source-my-perl-completion
      '((candidates . plcmp-ac-make-cands)))
    ;; http://d.hatena.ne.jp/sugyan/20120103/1325523629
    ))

;;; ** c-mode
(add-hook 'c-mode-hook
	  '(lambda ()
	     ; 
	     ; 
;;	     (my-set-file-encoding default-buffer-file-coding-system)
	     ; EUC
;;	     (my-set-file-encoding-quick 'euc-japan)
	    ;;(setq c-basic-offset 4)
;;	     (setq tab-width 4)
;;	     (setq c-auto-newline t)
	    (c-set-style "bsd")
	     ) t)

;(setq auto-mode-alist (cons '("\\.cpp$" . c++-mode) auto-mode-alist))

;;; ** etags ac-etags
;;
;;(use-package ac-etags
;;  :config
;;  (custom-set-variables
;;   '(ac-etags-requires 1))

;;  (eval-after-load "etags"
;;    '(progn
;;       (ac-etags-setup)))

;;  (defun my/c-mode-common-hook ()
;;    (add-to-list 'ac-sources 'ac-source-etags))

;;  (add-hook 'c-mode-common-hook 'my/c-mode-common-hook)
;;  )

;;; ** python
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))
;; python-modeは \C-c ? でだいたいわかるらしい。
;;    http://d.hatena.ne.jp/podhmo/20110320/1300606681

;;; ** flycheck
;(el-get-bundle 'flycheck)
;(use-package flycheck
;  :disabled   ;; texinfo 5 : makeinfo version 5 required.
;  :pin manual
;  :ensure t
;  :config
;  (progn
;    (setq flycheck-c/c++-cppcheck-executable (executable-find "cppcheck"))
;    (use-package checkdoc :ensure t)
;    (add-hook 'after-init-hook #'global-flycheck-mode)
;    )
;  :init
;  )

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 10_modes.el ends here

