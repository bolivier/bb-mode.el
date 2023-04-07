;;; bb-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Brandon Olivier
;;
;; Author: Brandon Olivier <brandon@brandonolivier.com>
;; Maintainer: Brandon Olivier <brandon@brandonolivier.com>
;; Created: April 07, 2023
;; Modified: April 07, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/brandon/bb-mode
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(defun bb-mode-something ()
  (interactive)
  (message "using bb mode"))

(defvar bb-mode--modeline-name " bb")

(defgroup bb-mode nil
  "Customization group for bb-mode"
  :group 'convenience)

(defcustom bb-mode--command-prefix "C-x j"
  "Prefix for bb-mode commands"
  :group 'bb-mode)

(defun bb-mode-greet ()
  (interactive)
  (message "hello bb-mode!"))

(defvar bb-mode--command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'bb-mode-greet)
    (define-key map "t" 'bb-mode-greet)
    map)
  "Keymap for `bb-mode' commands")

(defvar bb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd bb-mode--command-prefix) bb-mode--command-map)
    map)
  "Keymap for `bb-mode'")

(defun bb-mode--get-bb-tasks ()
  (interactive)
  (let* ((dir (locate-dominating-file default-directory "bb.edn"))
         (filename (concat dir "bb.edn")))
    (with-file-contents! filename
      (let ((bb-contents (car (parseedn-read))))
        (-remove
         #'keywordp
         (hash-table-keys (gethash
                           :tasks
                           bb-contents)))))))

(define-minor-mode bb-mode
  "Mode for working with bb.edn tasks within Emacs"
  :keymap bb-mode-map
  :lighter bb-mode--modline-name
  :group 'bb-mode)

(provide 'bb-mode)
;;; bb-mode.el ends here
