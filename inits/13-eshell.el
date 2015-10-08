;;; 13-eshell.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:31:33 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2014-05-11 16:31:44 shigeya>

;;; Code:

;;; ** eshell special settings
;; from  http://nishikawasasaki.hatenablog.com/entry/2012/09/12/233116

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'eshell-mode))
(eval-after-load "auto-complete"
  '(ac-define-source pcomplete
     '((candidates . pcomplete-completions))))

;; 普段の補完は TAB を使って auto-complete 、
;; 途中まで入力して一覧から補完したい時は M-n で helm-esh-pcomplete 、
;; 途中まで入力した内容から履歴をたどりたい時は M-p で helm-eshell-history 。
(defun my-ac-eshell-mode ()
  (setq ac-sources ; buffer-local value
        '(ac-source-pcomplete
          ac-source-filename
          ac-source-files-in-current-dir
          ac-source-words-in-buffer
          ac-source-dictionary)))
(add-hook 'eshell-mode-hook
          (lambda ()
            (my-ac-eshell-mode)
            (define-key eshell-mode-map (kbd "C-i") 'auto-complete)
            (define-key eshell-mode-map [(tab)] 'auto-complete)))
;; helm で履歴から入力
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history)))
;; helm で補完
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                (kbd "M-n")
                'helm-esh-pcomplete)))

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; 13-eshell.el ends here
