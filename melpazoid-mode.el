;;; melpazoid-mode.el --- Major mode for editing Melpazoid files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/riscy/melpazoid
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Melpazoid files.

;;; Code:


;;; main

(defvar cask-mode-font-lock-keywords
  `((,(regexp-opt
       '("package" "source")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("gnu" "melpa" "melpa-stable" "marmalade" "org")
       'symbols)
     . cask-mode-source-face)
    (,(rx symbol-start
          (or ":github" ":gitlab" "bitbucket" "wiki"
              ":git" ":bzr" ":hg" ":darcs" ":fossil" ":svn" ":cvs")
          symbol-end)
     . cask-mode-symbol-face)))

;;;###autoload
(define-derived-mode melpazoid-mode prog-mode "Melpazoid"
  "Major mode for editing Melpazoid files."
  (lisp-mode-variables))

;;;###autoload
(add-to-list 'auto-mode-alist '("/Melpazoid\\'" . melpazoid-mode))

(with-current-buffer "Melpazoid"
  (setq melpazoid-mode-font-lock-keywords
        `( ;; Regexp negated char group.
         ("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)
         ;; Control structures.  Common Lisp forms.
         ;; (,(concat "(" cl-kws-re "\\_>") . 1)
         ;; Exit/Feature symbols as constants.
         (,(concat "(\\(catch\\|throw\\|provide\\|require\\)\\_>"
                   "[ \t']*\\(" lisp-mode-symbol-regexp "\\)?")
           (1 font-lock-keyword-face)
           (2 font-lock-constant-face nil t))
         ;; Erroneous structures.
         ;; (,(concat "(" cl-errs-re "\\_>")
         ;;   (1 font-lock-warning-face))
         ;; Words inside ‘’ and `' tend to be symbol names.
         (,(concat "[`‘]\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)"
                   lisp-mode-symbol-regexp "\\)['’]")
          (1 font-lock-constant-face prepend))
         ;; Constant values.
         (,(concat "\\_<:" lisp-mode-symbol-regexp "\\_>")
          (0 font-lock-builtin-face))
         ;; ELisp and CLisp `&' keywords as types.
         (,(concat "\\_<\\&" lisp-mode-symbol-regexp "\\_>")
          . font-lock-type-face)
         ;; This is too general -- rms.
         ;; A user complained that he has functions whose names start with `do'
         ;; and that they get the wrong color.
         ;; ;; CL `with-' and `do-' constructs
         ;;("(\\(\\(do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
         (lisp--match-hidden-arg
          (0 '(face font-lock-warning-face
               help-echo "Hidden behind deeper element; move to another line?")))
         )
        ;; `((,(concat "\\_<:" lisp-mode-symbol-regexp "\\_>")
        ;;    (0 font-lock-builtin-face))
        ;;   (,(rx-to-string `(and "("
        ;;                         (group "source")
        ;;                         (* space)
        ;;                         (group (regexp ,lisp-mode-symbol-regexp)))
        ;;                   'no-group)
        ;;    (1 font-lock-keyword-face)
        ;;    (2 font-lock-constant-face nil t))
        ;;   (,(rx-to-string `(and (group ";" (* any)) eol)
        ;;                   'no-group)
        ;;    (1 font-lock-comment-face))
        ;;   ;; (,(rx-to-string `(and "("
        ;;   ;;                       (group "package")
        ;;   ;;                       (* space)
        ;;   ;;                       (group (regexp ,lisp-mode-symbol-regexp)))
        ;;   ;;                 'no-group)
        ;;   ;;  (1 font-lock-keyword-face)
        ;;   ;;  (2 font-lock-constant-face nil t))
        ;;   )
        )
  (setq font-lock-defaults '(melpazoid-mode-font-lock-keywords))
  (font-lock-refresh-defaults))

(provide 'melpazoid-mode)
;;; melpazoid-mode.el ends here
