;;; 12-org-mode.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:20:49 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2015-10-06 13:10:57 shigeya>

;;; Code:

;;; ** org-mode

;(el-get-bundle 'org-mode)
(use-package org
  :ensure t
;  :pin manual
  :mode
  (("\\.org$" . org-mode))    ;; 拡張子がorgのファイルはorg-modeに
  :bind
  (("\C-cl" . org-store-link) ;; C-c C-l (org-stored-links)で貼り付け
   ("\C-ca" . org-agenda)
   ;; ("\C-cr" . org-remember)  captureがあったら既に不要。
   ("\C-cc" . org-capture)
   ("\C-cb" . org-iswitchb)  ;; orgモードのバッファのみリスト
   )
  :config
  (progn
    (use-package org-install :ensure org)
    (use-package org-capture :ensure org)
    (use-package org-agenda :ensure org)
    (custom-set-variables
     ;; TODO状態
     '(org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
     ;; DONEの時刻を記録
     '(org-log-done 'time)
     ;; 見出しの余分な*を消す
     ;;'(org-hide-leading-stars t)
     )
    ;; org-modeでの強調表示を可能にする
    (add-hook 'org-mode-hook 'turn-on-font-lock)
    ;; org-default-notes-fileのディレクトリ
    (defvar dropbox-dir)
    (if (file-directory-p "W:/Dropbox/")
	(setq dropbox-dir "W:/Dropbox/")
      (if (file-directory-p "M:/Dropbox/")
	  (setq dropbox-dir "M:/Dropbox/")
	(if (file-directory-p "~/Dropbox/")
	    (setq dropbox-dir "~/Dropbox/"))))
    (setq org-directory (concat dropbox-dir "org/"))

    ;; directory structure
    ;;   http://qiita.com/tamurashingo@github/items/ee033dadab64269edf63
    ;; ------ my configuration -----
    ;;      org-directory/proj/*.org : TODO <-- capture p
    ;;                   /memo.org   : MEMO <-- capture m
    ;;                   /doc/...    : other document
    ;; http://qiita.com/tamurashingo@github/items/ee033dadab64269edf63
    ;; capture templates
    (setq org-capture-templates
	  '(("p" "Project Task" entry (file+headline (expand-file-name (concat org-directory "proj/project.org")) "Inbox")
             "** TODO %?\n    %i\n    %a\n    %T")
	    ("m" "memo" entry (file (expand-file-name (concat org-directory "memo.org")))
             "* %?\n    %i\n    %a\n    %T")
	    ("n" "diaryのInboxに投げる" item
	     (file+headline (expand-file-name (concat org-directory "diary.org")) "Inbox")
	     "  %?\n    %a\n    %T")
	    ))

    ;; agenda
    ;; dir指定はその下のファイルがagenda対象となる。
    (setq org-agenda-files
	  (list
	   ;;(expand-file-name org-directory)
	   (expand-file-name (concat org-directory "/proj/"))))

    ;; doing
    ;;   http://qiita.com/takaxp/items/4dfa11a81e18b29143ec#1-2
    (defun my-sparse-doing-tree ()
      (interactive)
      (org-tags-view nil "Doing"))
    (define-key org-mode-map (kbd "C-c 3") 'my-sparse-doing-tree)

    (org-defkey org-agenda-mode-map [(tab)]
		'(lambda () (interactive)
		   (org-agenda-goto)
		   (with-current-buffer "*Org Agenda*"
		     (org-agenda-quit))))

    (add-to-list 'org-capture-templates
		 '("d" "Doingタグ付きのタスクをdiaryのInboxに投げる" entry
		   (file+headline (expand-file-name
				   (concat org-directory "diary.org")) "Inbox")
		   "** TODO %? :Doing:\n"))

    (defvar my-doing-tag "Doing")
    ;; Doingタグをトグルする
    (defun my-toggle-doing-tag ()
      (interactive)
      (when (eq major-mode 'org-mode)
	(save-excursion
	  (save-restriction
	    (unless (org-at-heading-p)
	      (outline-previous-heading))
	    (if (string-match (concat ":" my-doing-tag ":") (org-get-tags-string))
		(org-toggle-tag my-doing-tag 'off)
	      (org-toggle-tag my-doing-tag 'on))
	    (org-reveal)))))
    (global-set-key (kbd "<f5>") 'my-toggle-doing-tag)

    ;; MobileOrg
    (use-package org-mobile
      :config
      (progn
	(setq org-mobile-directory (concat dropbox-dir "Apps/MobileOrg/"))
	;;(setq org-agenda-files (list (concat org-directory "notes.org")))

	;; http://ichiroc.hatenablog.com/entry/2013/09/04/213538
	;; drivee.jpのWebDAVで同期する。

	;; それぞれに ShellScript のパスをセットしておく
	(defvar my-org-pull-from-server-command
	  (concat org-directory "../emacs/scripts/org-download.sh")
	  "do script before pull")
	(defvar my-org-push-to-server-command
	  (concat org-directory "../emacs/scripts/org-upload.sh")
	  "do script after push")
	(defadvice org-mobile-pull (before org-mobile-download activate)
	  (if (=   (shell-command my-org-pull-from-server-command) 0)
	      (progn (kill-buffer "*Shell Command Output*")
		     (delete-other-windows))))
	(defadvice org-mobile-push (after org-mobile-upload activate)
	  (if (=   (shell-command my-org-push-to-server-command) 0)
	      (progn (kill-buffer "*Shell Command Output*")
		     (delete-other-windows))))
	))

    ;; plantUML
    (let ((jar "~/bin/plantuml.jar"))
      (when (file-exists-p jar)
	(use-package ob-plantuml
	  :ensure org
	  :init
	  (progn
	    (setq org-plantuml-jar-path jar)
	    (add-hook 'org-mode-hook
		      #'(lambda ()
			  (org-babel-do-load-languages
			   'org-babel-load-languages
			   (add-to-list 'org-babel-load-languages '(plantuml . t)))
			  ))
	    ))))

    (use-package helm-orgcard
      :ensure t
      :init
      (progn
	(bind-key "M-4" 'helm-orgcard org-mode-map)
	))

    ;; smartrep setting.
;    (smartrep-define-key
;     org-mode-map "C-c" ‘(("C-n" . (outline-next-visible-heading 1))
;			  ("C-p" . (outline-previous-visible-heading 1))))
    ))

;;; ** functions

(defun org-proj ()
  (interactive)
  (find-file (concat org-directory "proj/project.org")))

(defun org-memo ()
  (interactive)
  (find-file (concat org-directory "memo.org")))

(defun org-diary ()
  (interactive)
  (find-file (concat org-directory "diary.org")))

;;; ** mobile org functions
;; source編集
;;     cf. http://unknownplace.org/archives/orgmode-meets-blosxom.html
(setq org-src-fontify-natively t)
;; <s + TAB で以下のようなものが挿入される。
;;  #+BEGIN_SRC c
;;  #+END_SRC
;; で囲めばよい。C-c , で編集バッファが立ち上がる。

;; convert opml <-> org
;;    from http://karasunoblog.blog20.fc2.com/blog-entry-42.html
;;
(defun opml2org ()
  (interactive)
  (let* ((script (concat dropbox-dir "emacs/scripts/opml2org.rb")) ;script file
         (input-dir (concat dropbox-dir "Outliner/")) ;input
         (output-file (concat dropbox-dir "org/outliner.org")) ;output
         (command (concat "ruby -Ku " script " " input-dir " " output-file))) 
    (shell-command command)
    (if (not (eq (get-buffer "outliner.org") nil)) ;もし、ファイルが開かれていれば
        (find-file-noselect output-file) ;ファイルを開き直すか聞く
      ())
    ))

(defun org2opml ()
  (interactive)
  (let* ((script (concat dropbox-dir "emacs/scripts/org2opml.rb")) ;script file
         (outliner-dir (concat dropbox-dir "Outliner/")) ;input
         (org-file (concat dropbox-dir "org/outliner.org")) ;output
         (command (concat "ruby -Ku " script " " org-file " " outliner-dir)))
    (shell-command command)
    ))


;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:

;;; 12-org-modes.el ends here
