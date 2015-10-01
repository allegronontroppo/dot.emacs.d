;;; cocoa-emacs-input.el --- mode settings  -*- mode: emacs-lisp ; coding: utf-8 ; lexical-binding: t  -*-
;; Author: shigeya
;; Maintainer: shigeya
;; Created: Sun May 11 16:34:51 2014
;; Keywords: emacs
;;; Commentary:
;;; Change Log:
;; Time-stamp: <2014-05-11 22:51:02 shigeya>

;;; Code:

;;; ** mac ime settings
;
; http://mgrace.info/?p=1128
;

(setq default-input-method "MacOSX")

;; emacs 起動時は英数モードから始める
(add-hook 'after-init-hook 'mac-change-language-to-us)
 
;; minibuffer 内は英数モードにする
(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
(mac-translate-from-yen-to-backslash)
 
;; buffer 切替時、IMEの状態をアップデート
;(require 'cl-lib)
(add-hook
 'post-command-hook
 (lexical-let ((previous-buffer nil))
   #'(lambda ()
       (unless (eq (current-buffer) previous-buffer)
	 (if (bufferp previous-buffer) (mac-handle-input-method-change))
	 (setq previous-buffer (current-buffer))))))

;; [migemo]isearch のとき IME を英数モードにする
(add-hook 'isearch-mode-hook 'mac-change-language-to-us)

(mac-set-input-method-parameter
 "com.justsystems.inputmethod.atok26.Japanese" 'title "漢")
(mac-set-input-method-parameter
 "com.justsystems.inputmethod.atok26.Japanese" `cursor-type 'box)
(mac-set-input-method-parameter
 "com.justsystems.inputmethod.atok26.Japanese" `cursor-color "magenta")

(mac-set-input-method-parameter
"com.apple.keylayout.Dvorak" 'title "DV")
(mac-set-input-method-parameter
"com.apple.keylayout.Dvorak" `cursor-type 'box)
(mac-set-input-method-parameter
"com.apple.keylayout.Dvorak" `cursor-color
(frame-parameter (selected-frame) 'cursor-color))

;; Local Variables:
;; truncate-lines: nil
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: "^;;; "
;; End:
;;; cocoa-emacs-input.el ends here

