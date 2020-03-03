;;; melpazoid-checkers.el --- Checkers for melpazoid  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience

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

;;

;;; Code:

(defun melpazoid--check-lexical-binding ()
  "Warn about lack of lexical binding."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "lexical-binding:[[:blank:]]*t" nil t)
      (melpazoid-insert "- Per CONTRIBUTING.org, consider using lexical binding.")
      (melpazoid-insert "  See: https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org#fixing-typical-problems")
      (melpazoid-insert "- Lexical binding will be used in what follows"))))

(defun melpazoid--remove-no-compile ()
  "Warn about and remove `no-byte-compile' directive."
  (save-excursion
    (let ((melpazoid--misc-header-printed-p t))  ; HACK: don't print a header
      (melpazoid-misc "no-byte-compile: t" "Don't set `no-byte-compile: t`." nil t))
    (when melpazoid-can-modify-buffers
      (goto-char (point-min))
      (while (re-search-forward "no-byte-compile:[\s\t]*t" nil t)
        (delete-char -1)
        (insert "nil")
        (melpazoid-insert "  Byte-compiling is enabled in what follows")
        (save-buffer)))))

(defun melpazoid--run-package-lint-p ()
  "Return non-nil if buffer's file is not the package's 'main' file."
  (or (not (getenv "PACKAGE_NAME"))
      ;; TODO: can we use buffer-file-name instead of (buffer-file-name)?
      (string= (getenv "PACKAGE_NAME") (file-name-base (buffer-file-name)))
      (zerop (length (getenv "PACKAGE_NAME")))))

(defun melpazoid-misc (regexp msg &optional no-smart-space include-comments)
  "If a search for REGEXP passes, report MSG as a misc check.
If NO-SMART-SPACE is nil, use smart spaces -- i.e. replace all
SPC characters in REGEXP with [[:space:]]+.  If INCLUDE-COMMENTS
then also scan comments for REGEXP."
  (unless no-smart-space
    (setq regexp (replace-regexp-in-string " " "[[:space:]]+" regexp)))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (when (or include-comments
                (not (comment-only-p (point-at-bol) (point-at-eol))))
        ;; print a header unless it's already been printed:
        (unless melpazoid--misc-header-printed-p
          (melpazoid-insert "Experimental static checks/suggestions:")
          (setq melpazoid--misc-header-printed-p t))
        (melpazoid-insert "- %s#L%s: %s"
                          (file-name-nondirectory (buffer-file-name))
                          (line-number-at-pos)
                          msg)))))

(defun melpazoid-insert (f-str &rest objects)
  "Insert F-STR in a way determined by whether we're in script mode.
OBJECTS are objects to interpolate into the string using `format'."
  (let* ((str (concat f-str "\n"))
         (str (apply #'format str objects)))
    (if noninteractive
        (send-string-to-terminal str)
      (with-current-buffer (get-buffer-create melpazoid-buffer)
        (insert str)))))

(defun melpazoid--newline-trim (str)
  "Sanitize STR by removing newlines."
  (let* ((str (replace-regexp-in-string "[\n]+$" "" str))
         (str (replace-regexp-in-string "^[\n]+" "" str)))
    str))

(defun melpazoid--reset-state ()
  "Reset melpazoid's current state variables."
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)
  (setq melpazoid--misc-header-printed-p nil)
  (setq melpazoid-error-p nil)
  (ignore-errors (kill-buffer melpazoid-buffer)))

(defun melpazoid--byte-compile (info)
  "Byte-compile INFO."
  (let-alist info
    (let ((package-user-dir .sandboxdir))
      (require 'package)
      (package-initialize)

      ;; TODO: use flycheck or its pattern for cleanroom byte-compiling
      (with-current-buffer (find-file-noselect .tmpfile)
        (melpazoid-insert "byte-compile-file (using Emacs %s.%s):" emacs-major-version emacs-minor-version)
        (melpazoid--remove-no-compile)
        (melpazoid--check-lexical-binding)
        (let ((lexical-binding t))
          (byte-compile-file filename))
        (with-current-buffer (get-buffer-create "*Compile-Log*")
          (if (<= (- (point-max) (point)) 3)
              (melpazoid-insert "- No issues!")
            (goto-char (point-min)) (forward-line 2)
            (melpazoid-insert "```")
            (melpazoid-insert
             (melpazoid--newline-trim (buffer-substring (point) (point-max))))
            (melpazoid-insert "```")
            (setq melpazoid-error-p t)))
        (melpazoid-insert "")))))

(defun melpazoid--checkdoc (info)
  "Checkdoc with INFO."
  (let-alist info
    (let ((package-user-dir .sandboxdir))
      (require 'package)
      (package-initialize)

      (require 'checkdoc)
      (melpazoid-insert "checkdoc (using version %s):" checkdoc-version)
      (ignore-errors
        (with-current-buffer (find-file-noselect .tmpfile)
          (let ((sentence-end-double-space nil)  ; be a little more leniant
                (checkdoc-proper-noun-list nil)
                (checkdoc-common-verbs-wrong-voice nil))
            (checkdoc-current-buffer))))
      (if (not (get-buffer "*Warnings*"))
          (melpazoid-insert "- No issues!")
        (with-current-buffer "*Warnings*"
          (melpazoid-insert "```")
          (melpazoid-insert
           (melpazoid--newline-trim (buffer-substring (point-min) (point-max))))
          (melpazoid-insert "```")
          (setq melpazoid-error-p t)))
      (melpazoid-insert ""))))

(defun melpazoid--package-lint (info)
  "Package-lint with INFO."
  (let-alist info
    (let ((package-user-dir .sandboxdir))
      (require 'package)
      (package-initialize)

      (require 'package-lint)
      (if (not (melpazoid--run-package-lint-p))
          (melpazoid-insert "(Skipping package-lint on this file)")
        (melpazoid-insert
         "package-lint-current-buffer (using version %s):"
         (pkg-info-format-version (pkg-info-package-version "package-lint")))
        (ignore-errors
          (with-current-buffer (find-file-noselect .tmpfile)
            (package-lint-current-buffer)))
        (with-current-buffer (get-buffer-create "*Package-Lint*")
          (let ((output (melpazoid--newline-trim (buffer-substring (point-min) (point-max)))))
            (if (string= "No issues found." output)
                (melpazoid-insert "- No issues!")
              (melpazoid-insert "```")
              (melpazoid-insert
               (if (string= output "")
                   "package-lint:Error: No output.  Did you remember to (provide 'your-package)?"
                 output))
              (melpazoid-insert "```")
              (setq melpazoid-error-p t))))))))

(defun melpazoid--check-declare (info)
  "Check `declare-defun' with INFO."
  (let-alist info
    (with-current-buffer (find-file-noselect .tmpfile)
      (melpazoid-insert "check-declare-file (optional):")
      (ignore-errors (kill-buffer "*Check Declarations Warnings*"))
      (check-declare-file (buffer-file-name (current-buffer)))
      (with-current-buffer (get-buffer-create "*Check Declarations Warnings*")
        (if (melpazoid--buffer-almost-empty-p)
            (melpazoid-insert "- No issues!")
          (melpazoid-insert "```")
          (melpazoid-insert (buffer-substring (point-min) (point-max)))
          (melpazoid-insert "```")
          (setq melpazoid-error-p t)))
      (melpazoid-insert ""))))

(defun melpazoid--check-sharp-quotes (info)
  "Check sharp-quotes with INFO."
  (let-alist info
    (with-current-buffer (find-file-noselect .tmpfile)
      (melpazoid-misc "#'(lambda" "There is no need to quote lambdas (neither #' nor ')")
      (melpazoid-misc "[^#]'(lambda" "Don't quote lambdas; this prevents them from being compiled")
      (let ((msg "It's safer to sharp-quote function names; use `#'`"))
        (melpazoid-misc "(apply-on-rectangle '[^,]" msg)
        (melpazoid-misc "(apply-partially '[^,]" msg)
        (melpazoid-misc "(apply '[^,]" msg)
        (melpazoid-misc "(seq-mapcat '[^,]" msg)
        (melpazoid-misc "(seq-map '[^,]" msg)
        (melpazoid-misc "(seq-mapn '[^,]" msg)
        (melpazoid-misc "(mapconcat '[^,]" msg)
        (melpazoid-misc "(functionp '[^,]" msg)
        (melpazoid-misc "(mapcar '[^,]" msg)
        (melpazoid-misc "(funcall '[^,]" msg)
        (melpazoid-misc "(cl-assoc-if '[^,]" msg)
        (melpazoid-misc "(call-interactively '" msg)
        (melpazoid-misc "(callf '[^,]" msg)
        (melpazoid-misc "(run-at-time[^(#]*[^#]'" msg)
        (melpazoid-misc "(seq-find '" msg)
        (melpazoid-misc "(add-hook '[^[:space:]]+ '" msg)
        (melpazoid-misc "(remove-hook '[^[:space:]]+ '" msg)
        (melpazoid-misc "(advice-add [^#)]*)" msg)
        (melpazoid-misc "(defalias [^#()]*)" msg)
        (melpazoid-misc "(run-with-idle-timer[^(#]*[^#]'" msg)))))

(defun melpazoid--check-misc (info)
  "Check misc with INFO."
  (let-alist info
    (with-current-buffer (find-file-noselect .tmpfile)
      (melpazoid-misc "(string-equal major-mode" "Check major mode with eq, e.g.: `(eq major-mode 'dired-mode)`")
      (melpazoid-misc "/tmp\\>" "Use `temporary-file-directory` instead of /tmp in code")
      (melpazoid-misc "(s-starts-with-p" "Using `string-prefix-p` may allow dropping the dependency on `s`")
      (melpazoid-misc "(s-ends-with-p" "Using `string-suffix-p` may allow dropping the dependency on `s`")
      (melpazoid-misc "Copyright.*Free Software Foundation" "Did you really do the paperwork to assign your copyright?")
      (melpazoid-misc "(add-to-list 'auto-mode-alist.*\\$" "Terminate auto-mode-alist entries with `\\\\'`")
      (melpazoid-misc "This file is part of GNU Emacs." "This statement may not currently be accurate")
      (melpazoid-misc "lighter \"[^ \"]" "Minor mode lighters should start with a space")
      (melpazoid-misc "(fset" "Ensure this `fset` isn't being used as a surrogate `defalias`")
      (melpazoid-misc "(fmakunbound" "Use of `fmakunbound` in a package is usually unnecessary")
      (melpazoid-misc "(setq major-mode" "Directly setting major-mode is odd (if defining a mode, prefer define-derived-mode)")
      (melpazoid-misc "([^ ]*read-string \"[^\"]*[^ ]\"" "Many `*-read-string` prompts should end with a space")
      (melpazoid-misc "(define-derived-mode .*fundamental-mode" "It is unusual to derive from fundamental-mode; try special-mode")
      (melpazoid-misc ";;;###autoload\n(defcustom" "Don't autoload `defcustom`")
      (melpazoid-misc ";;;###autoload\n(add-hook" "Don't autoload `add-hook`")
      (melpazoid-misc "url-copy-file" "Be aware that url-copy-file can't handle redirects (ensure it works)")
      (melpazoid-misc ";; Package-Version" "Prefer `;; Version` instead of `;; Package-Version` (MELPA automatically adds `Package-Version`)")
      (melpazoid-misc "^(define-key" "This define-key could overwrite a user's keybindings.  Try: `(defvar my-map (let ((km (make-sparse-keymap))) (define-key ...) km))`")
      (melpazoid-misc "(string-match[^(](symbol-name" "Prefer to use `eq` on symbols")
      (melpazoid-misc "(defcustom [^ ]*--" "Customizable variables shouldn't be private")
      (melpazoid-misc "(ignore-errors (re-search-[fb]" "Use `re-search-*`'s built-in NOERROR argument")
      (melpazoid-misc "(ignore-errors (search-[fb]" "Use `search-*`'s built-in NOERROR argument")
      (melpazoid-misc "(user-error (format" "No `format` required; messages are already f-strings")
      (melpazoid-misc "(message (concat" "No `concat` required; messages are already f-strings")
      (melpazoid-misc "(message (format" "No `format` required; messages are already f-strings")
      (melpazoid-misc "^ ;[^;]" "Single-line comments should (usually) begin with `;;`")
      (melpazoid-misc "(unless (null " "Consider `when ...` instead of `unless (not ...)`")
      (melpazoid-misc "(unless (not " "Consider `when ...` instead of `unless (null ...)`")
      (melpazoid-misc "(when (not " "Consider `unless ...` instead of `when (not ...)`")
      (melpazoid-misc "(when (null " "Consider `unless ...` instead of `when (null ...)`")
      (melpazoid-misc "http://" "Prefer `https` over `http` (if possible)" nil t)
      (melpazoid-misc "(eq[^()]*\\<nil\\>.*)" "You can use `not` or `null`"))))

(provide 'melpazoid-checkers)
;;; melpazoid-checkers.el ends here
