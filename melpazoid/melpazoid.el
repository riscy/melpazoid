;;; melpazoid.el --- A MELPA review tool  -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: June 9 2019
;; Keywords: tools, convenience
;; URL: https://github.com/riscy/melpazoid
;; Package-Requires: ((emacs "25.1") (pkg-info "0.6") (epl "0.9"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.0.0

;;; Commentary:

;; A MELPA review tool to run the MELPA checklist in addition
;; to some other checks that might point to other issues.

;;; Code:

(require 'package)
(defvar checkdoc-version)
(declare-function pkg-info-format-version "ext:pkg-info.el" t t)
(declare-function pkg-info-package-version "ext:pkg-info.el" t t)
(declare-function package-lint-current-buffer "ext:package-lint.el" t t)
(defconst melpazoid-buffer "*melpazoid*" "Name of the 'melpazoid' buffer.")
(defvar melpazoid-can-modify-buffers nil "Whether melpazoid can modify buffers.")
(defvar melpazoid--insertion "" "Text that will (maybe) be appended to the report.")

(defun melpazoid-byte-compile (filename)
  "Wrapper for running `byte-compile-file' against FILENAME."
  ;; TODO: use flycheck or its pattern for cleanroom byte-compiling
  (melpazoid-insert "\n`%s` with byte-compile using Emacs %s:"
                    (file-name-nondirectory filename)
                    emacs-version)
  (melpazoid--remove-no-compile)
  (ignore-errors (kill-buffer "*Compile-Log*"))
  (let ((inhibit-message t)) (byte-compile-file filename))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (if (melpazoid--buffer-almost-empty-p)
        (melpazoid-discard-inserted)
      (goto-char (point-min))
      (forward-line 2)
      (melpazoid-insert "```")
      (melpazoid-insert
       (melpazoid--newline-trim (buffer-substring (point) (point-max))))
      (melpazoid-insert "```")
      (melpazoid-commit-inserted))))

(defun melpazoid--remove-no-compile ()
  "Remove `no-byte-compile' directive.
It only be set to t for themes."
  (save-excursion
    (when melpazoid-can-modify-buffers
      (goto-char (point-min))
      (while (re-search-forward "no-byte-compile:[\s\t]*t" nil t)
        (delete-char -1)
        (insert "nil")
        (melpazoid-insert "- Temporarily ignoring `no-byte-compile` flag")
        (save-buffer)))))

(defun melpazoid--buffer-almost-empty-p ()
  "Return non-nil if current buffer is 'almost' empty."
  (<= (- (point-max) (point)) 3))

(defvar checkdoc-proper-noun-list)         ; compiler pacifier
(defvar checkdoc-common-verbs-wrong-voice) ; compiler pacifier
(defun melpazoid-checkdoc (filename)
  "Wrapper for running `checkdoc-file' against FILENAME."
  (require 'checkdoc)  ; to retain cleaner byte-compilation in script mode
  (melpazoid-discard-inserted)
  (melpazoid-insert "\n`%s` with checkdoc %s:"
                    (file-name-nondirectory filename)
                    checkdoc-version)
  (ignore-errors (kill-buffer "*Warnings*"))
  (let ((sentence-end-double-space nil)  ; be a little more lenient
        (checkdoc-proper-noun-list nil)
        (checkdoc-verb-check-experimental-flag nil)
        (inhibit-message t))
    (checkdoc-file filename))
  (when (get-buffer "*Warnings*")
    (let* ((issues
            (with-current-buffer "*Warnings*"
              (buffer-substring (point-min) (point-max))))
           (issues (melpazoid--newline-trim issues))
           (issues (replace-regexp-in-string "^Warning (emacs): \n" "" issues)))
      (melpazoid-insert "```")
      (melpazoid-insert issues)
      (melpazoid-insert "```")
      (melpazoid-commit-inserted))))

(defvar package-lint-main-file)         ; compiler pacifier
(defun melpazoid-package-lint ()
  "Wrapper for running `package-lint' against the current buffer."
  (require 'package-lint)    ; to retain cleaner byte-compilation in script mode
  (require 'pkg-info)        ; to retain cleaner byte-compilation in script mode
  (melpazoid-insert "\n`%s` with package-lint %s:"
                    (buffer-name)
                    (pkg-info-format-version
                     (pkg-info-package-version "package-lint")))
  (ignore-errors (kill-buffer "*Package-Lint*"))
  (let ((package-lint-main-file (melpazoid--package-lint-main-file)))
    (ignore-errors (package-lint-current-buffer)))
  (with-current-buffer (get-buffer-create "*Package-Lint*")
    (let ((issues
           (melpazoid--newline-trim (buffer-substring (point-min) (point-max)))))
      (if (string= "No issues found." issues)
          (melpazoid-discard-inserted)
        (melpazoid-insert "```")
        (melpazoid-insert issues)
        (melpazoid-insert "```")
        (melpazoid-commit-inserted)))))

(defun melpazoid-elint ()
  "Experimental elint call."
  (melpazoid-insert "\nelint (experimental):")
  (ignore-errors (kill-buffer "*Elint*"))
  (elint-file (buffer-file-name))
  (with-current-buffer "*Elint*"
    (goto-char (point-min))
    (forward-line 3)
    (if (melpazoid--buffer-almost-empty-p)
        (melpazoid-discard-inserted)
      (forward-line)
      (melpazoid-insert "```")
      (melpazoid-insert (buffer-substring (point) (point-max)))
      (melpazoid-insert "```")))
  (melpazoid-commit-inserted))

(defun melpazoid--package-lint-main-file ()
  "Return suitable value for `package-lint-main-file'."
  (let ((package-main (getenv "PACKAGE_MAIN")))
    (cond
     ((eq package-main nil)
      nil)
     ((string= package-main "")
      nil)
     (t package-main))))

(defun melpazoid-check-declare ()
  "Wrapper for `melpazoid' check-declare.
NOTE: this sometimes backfires when running checks automatically inside
a Docker container, e.g. kellyk/emacs does not include the .el files."
  (melpazoid-insert "\ncheck-declare-file (optional):")
  (ignore-errors (kill-buffer "*Check Declarations Warnings*"))
  (check-declare-file (buffer-file-name (current-buffer)))
  (with-current-buffer (get-buffer-create "*Check Declarations Warnings*")
    (if (melpazoid--buffer-almost-empty-p)
        (melpazoid-discard-inserted)
      (melpazoid-insert "```")
      (melpazoid-insert (buffer-substring (point-min) (point-max)))
      (melpazoid-insert "```")
      (melpazoid-commit-inserted))))

(defun melpazoid-check-experimentals ()
  "Run miscs checker."
  (melpazoid-check-sharp-quotes)
  (melpazoid-check-misc)
  (unless (string-empty-p melpazoid--insertion)
    (setq melpazoid--insertion
          (format
           "\n`%s` with [melpazoid](https://github.com/riscy/melpazoid):\n%s"
           (buffer-name)
           melpazoid--insertion))
    (melpazoid-commit-inserted)))

(defun melpazoid-check-sharp-quotes ()
  "Check for missing sharp quotes."
  (melpazoid-misc
   "#'(lambda " "There is no need to quote lambdas (neither #' nor ')")
  (melpazoid-misc
   "[^#]'(lambda " "Quoting this lambda may prevent it from being compiled")
  (let ((msg "It's safer to sharp-quote function names; use `#'`"))
    (melpazoid-misc "(apply-on-rectangle '[^,]" msg)
    (melpazoid-misc "(apply-partially '[^,]" msg)
    (melpazoid-misc "(apply '[^,]" msg)
    (melpazoid-misc "(cancel-function-timers '[^,]" msg)
    (melpazoid-misc "(seq-do '[^,]" msg)
    (melpazoid-misc "(seq-do-indexed '[^,]" msg)
    (melpazoid-misc "(seq-filter '[^,]" msg)
    (melpazoid-misc "(seq-mapcat '[^,]" msg)
    (melpazoid-misc "(seq-map '[^,]" msg)
    (melpazoid-misc "(seq-map-indexed '[^,]" msg)
    (melpazoid-misc "(seq-mapn '[^,]" msg)
    (melpazoid-misc "(mapconcat '[^,]" msg)
    (melpazoid-misc "(setq indent-line-function '[^,]" msg)
    (melpazoid-misc "(setq-local indent-line-function '[^,]" msg)
    (melpazoid-misc "(map-filter '[^,]" msg)
    (melpazoid-misc "(map-remove '[^,]" msg)
    (melpazoid-misc "(mapcar '[^,]" msg)
    (melpazoid-misc "(funcall '[^,]" msg)
    (melpazoid-misc "(cl-assoc-if '[^,]" msg)
    (melpazoid-misc "(call-interactively '" msg)
    (melpazoid-misc "(callf '[^,]" msg)
    (melpazoid-misc "(run-at-time[^(#]*[^#]'" msg)
    (melpazoid-misc "(seq-find '" msg)
    (melpazoid-misc "(add-hook '[^[:space:]]+ '[^(]" msg)
    (melpazoid-misc "(remove-hook '[^[:space:]]+ '" msg)
    (melpazoid-misc "(advice-add '[^#)]*)" msg)
    (melpazoid-misc "(advice-remove '[^#)]*)" msg)
    (melpazoid-misc "(defalias '[^#()]*)" msg)
    (melpazoid-misc
     "(define-obsolete-function-alias '[[:graph:]]+ '[[:graph:]]" msg)
    (melpazoid-misc "(run-with-idle-timer[^(#]*[^#]'" msg)))

(defun melpazoid-check-picky ()
  "Miscellaneous checker (picky edition)."
  (melpazoid-misc "http://" "Prefer `https` over `http` if possible ([why?](https://news.ycombinator.com/item?id=22933774))" nil t t) ; nofmt
  (melpazoid-misc "(when (not " "Optionally use `unless ...` instead of `when (not ...)`") ; nofmt
  (melpazoid-misc "(when (null " "Optionally use `unless ...` instead of `when (null ...)`") ; nofmt
  (melpazoid-misc "line-number-at-pos" "line-number-at-pos is surprisingly slow - avoid it")
  (melpazoid-misc "^;;; Commentary:\n;;\n" "Use a blank line instead of `;;` by itself under your `;;; Commentary` header"))

(defun melpazoid-check-misc ()
  "Miscellaneous checker."
  (melpazoid-misc "\n.*lexical-binding:" "`lexical-binding` must be on the end of the first line" nil t)
  (melpazoid-misc "(with-temp-buffer (set-buffer " "`set-buffer` is unnecessary here") ; nofmt
  (melpazoid-misc "^(setq-default " "Packages should use `defvar-local`, not `setq-default`") ; nofmt
  (melpazoid-misc "\"/tmp/" "Use `temporary-file-directory` instead of /tmp in code") ; nofmt
  (melpazoid-misc "Copyright.*Free Software Foundation" "Have you done the paperwork to assign this copyright?" nil t) ; nofmt
  (melpazoid-misc "This file is part of GNU Emacs." "This may be a copy-paste error?" nil t)
  (melpazoid-misc "(fset" "Ensure this `fset` isn't being used as a surrogate `defalias`") ; nofmt
  (melpazoid-misc "(fmakunbound" "`fmakunbound` should rarely occur in packages") ; nofmt
  (melpazoid-misc "([^ ]*read-string \"[^\"]+[^ \"]\"" "`read-string` prompts should often end with a space" t) ; nofmt
  (melpazoid-misc ";; Package-Version" "Prefer `;; Version` over `;; Package-Version` (MELPA automatically adds `Package-Version`)" nil t) ; nofmt
  (melpazoid-misc "(string-match[^(](symbol-name" "Prefer to use `eq` on symbols") ; nofmt
  (melpazoid-misc "(defcustom [^ ]*--" "Customizable variables shouldn't be private" t) ; nofmt
  (melpazoid-misc "(eval-when-compile (progn" "No `progn` required under `eval-when-compile`") ; nofmt
  (melpazoid-misc "(ignore-errors (progn" "No `progn` required under `ignore-errors`") ; nofmt
  (melpazoid-misc "(ignore-errors (re-search-[fb]" "Use `re-search-*`'s NOERROR argument") ; nofmt
  (melpazoid-misc "(setq inhibit-read-only" "Use `(let ((inhibit-read-only t)) ...)`") ; nofmt
  (melpazoid-misc "(ignore-errors (search-[fb]" "Use `search-*`'s NOERROR argument") ; nofmt
  (melpazoid-misc "^ ;[^;]" "Single-line comments should usually begin with `;;`" nil t) ; nofmt
  ;; simplified conditionals
  (melpazoid-misc "([<>eq/=]+ (point) (line-beginning-position))" "Could this point/line-beginning-position comparison use `bolp`?") ; nofmt
  (melpazoid-misc "([<>eq/=]+ (point) (line-end-position))" "Could this point/line-end-position comparison use `eolp`?") ; nofmt
  (melpazoid-misc "([<>eq/=]+ (point) (point-at-bol))" "Could this point/point-at-bol comparison use `bolp`?") ; nofmt
  (melpazoid-misc "([<>eq/=]+ (point) (point-at-eol))" "Could this point/point-at-eol comparison use `eolp`?") ; nofmt
  (melpazoid-misc "([<>eq/=]+ (point) (point-max))" "Could this point/point-max comparison use `eobp`?") ; nofmt
  (melpazoid-misc "([<>eq/=]+ (point) (point-min))" "Could this point/point-min comparison use `bobp`?") ; nofmt
  (melpazoid-misc "(eq [^()]*\\<nil\\>.*)" "You can use `not` or `null`")
  (melpazoid-misc "(not (not " "This double negation can be collapsed") ; nofmt
  (melpazoid-misc "(not (null " "This double negation can be collapsed (`not` aliases `null`)") ; nofmt
  (melpazoid-misc "(unless (not " "Use `when ...` instead of `unless (not ...)`") ; nofmt
  (melpazoid-misc "(unless (null " "Use `when ...` instead of `unless (null ...)`") ; nofmt
  ;; working with modes
  (melpazoid-misc "(equal major-mode \"" "Prefer `(eq major-mode 'xyz)`")
  (melpazoid-misc "(setq auto-mode-alist" "Prefer `add-to-list` to add to auto-mode-alist") ; nofmt
  (melpazoid-misc "(setq major-mode" "Prefer `define-derived-mode`")
  (melpazoid-misc "(string-equal major-mode" "Prefer `(eq major-mode 'xyz)`")
  (melpazoid-misc "(string= major-mode" "Prefer `(eq major-mode 'xyz)`")
  (melpazoid-misc "lighter \".+ \"" "Lighter should start, but not end, with a space" t) ; nofmt
  (melpazoid-misc "lighter \"[^ \"]" "Lighter should start with a space" t)
  ;; modifying Emacs on load
  (melpazoid-misc "(global-set-key" "Don't set global bindings; tell users how in your `;;; Commentary`.") ; nofmt
  (melpazoid-misc "^(add-hook" "Loading a package should rarely add hooks" nil t) ; nofmt
  (melpazoid-misc "^(add-to-list 'auto-mode-alist.*\\$" "Terminate auto-mode-alist entries with `\\\\'`") ; nofmt
  (melpazoid-misc "^(advice-add" "Loading a package should rarely add advice" nil t) ; nofmt
  (melpazoid-misc "^(autoload" "It may be simpler to just `require` this dependency") ; nofmt
  (melpazoid-misc "^(bind-keys" "Top-level bind-keys can sometimes overwrite user keybindings.  Try: `(defvar my-map (let ((km (make-sparse-keymap))) (bind-keys ...) km))`") ; nofmt
  (melpazoid-misc "^(define-key" "Top-level `define-key` can sometimes overwrite user bindings.  Try: `(defvar my-map (let ((km (make-sparse-keymap))) (define-key ...) km))`") ; nofmt
  (melpazoid-misc "^(progn" "`progn` is usually not required at the top level")
  ;; simpler expressions around strings:
  (melpazoid-misc "(error (format" "No `format` required; `error` takes an f-string") ; nofmt
  (melpazoid-misc "(message (format" "No `format` required; `message` takes an f-string") ; nofmt
  (melpazoid-misc "(user-error (format" "No `format` required; user-error takes an f-string") ; nofmt
  (melpazoid-misc "(insert (concat" "`concat` may be unneeded; `insert` can take multiple arguments") ; nofmt
  (melpazoid-misc "(warn (format" "No `format` required; `warn` takes an f-string") ; nofmt
  ;; n.b. the opposite, (concat (format ...)), often can't be combined cleanly:
  (melpazoid-misc "(format (concat" "Can the `format` and `concat` be combined?") ; nofmt
  )

(defun melpazoid-misc (regexp msg &optional no-smart-space include-comments include-strings)
  "If a search for REGEXP passes, report MSG as a misc check.
If NO-SMART-SPACE is nil, use smart spaces -- i.e. replace all
SPC characters in REGEXP with [[:space:]]+.  If INCLUDE-COMMENTS
then also scan comments for REGEXP; similar for INCLUDE-STRINGS."
  (unless no-smart-space
    (setq regexp (replace-regexp-in-string " " "[[:space:]\n]+" regexp)))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (save-excursion
        (goto-char (match-beginning 0))
        (when (and
               (or include-comments (not (nth 4 (syntax-ppss))))
               (or include-strings (not (nth 3 (syntax-ppss)))))
          (melpazoid--annotate-line msg))))))

(defun melpazoid--annotate-line (msg)
  "Annotate the current line with MSG."
  (melpazoid-insert "- %s#L%s: %s"
                    (file-name-nondirectory (buffer-file-name))
                    (line-number-at-pos)
                    msg))

(defun melpazoid-insert (f-str &rest objects)
  "Insert F-STR in a way determined by whether we're in script mode.
OBJECTS are objects to interpolate into the string using `format'."
  (setq melpazoid--insertion
        (concat melpazoid--insertion (apply #'format f-str objects) "\n")))

(defun melpazoid-discard-inserted ()
  "Clear the current value in `melpazoid--insertion'."
  (setq melpazoid--insertion ""))

(defun melpazoid-commit-inserted ()
  "Commit whatever is accrued to the report with PREFIX and SUFFIX."
  (if noninteractive
      (send-string-to-terminal melpazoid--insertion)
    (with-current-buffer (get-buffer-create melpazoid-buffer)
      (insert melpazoid--insertion)))
  (melpazoid-discard-inserted))

(defun melpazoid--newline-trim (str)
  "Sanitize STR by removing newlines."
  (let* ((str (replace-regexp-in-string "[\n]+$" "" str))
         (str (replace-regexp-in-string "^[\n]+" "" str)))
    str))

;;;###autoload
(defun melpazoid (&optional filename)
  "Check current buffer, or FILENAME's buffer if given."
  (interactive)
  (melpazoid--reset-state)
  (let ((filename (or filename (buffer-file-name (current-buffer)))))
    (save-window-excursion
      (set-buffer (find-file filename))
      (melpazoid-byte-compile filename)
      (melpazoid-checkdoc filename)
      ;; (melpazoid-check-declare)
      (melpazoid-package-lint)
      ;; (melpazoid-elint)
      (melpazoid-check-experimentals))
    (pop-to-buffer melpazoid-buffer)
    (goto-char (point-min))))

(defun melpazoid--reset-state ()
  "Reset melpazoid's current state variables."
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)
  (melpazoid-discard-inserted)
  (ignore-errors (kill-buffer melpazoid-buffer)))

(defun melpazoid--check-file-p (filename)
  "Return non-nil if FILENAME should be checked."
  (and
   (not (string= (file-name-base filename) "melpazoid"))
   (not (string-match ".*-pkg[.]el$" filename))
   (not (string-match "[.]el~$" filename))  ; file-name-extension misses these
   (not (string-match "^[.]#" filename))    ; file storing unsaved changes
   (string= (file-name-extension filename) "el")))

(when noninteractive
  ;; Check every elisp file in `default-directory' (except melpazoid.el)
  (setq melpazoid-can-modify-buffers t)
  (add-to-list 'load-path ".")

  (let ((filename nil)
        (filenames (directory-files ".")))
    (while filenames
      (setq filename (car filenames) filenames (cdr filenames))
      (when (melpazoid--check-file-p filename) (melpazoid filename))))

  (let ((load-error nil))
    ;; check whether FILENAMEs can be simply loaded
    (melpazoid-insert "\n`load`-check on each file:")
    (melpazoid-insert "```")
    (let ((filename nil)
          (filenames (directory-files ".")))
      (while filenames
        (setq filename (car filenames) filenames (cdr filenames))
        (when (melpazoid--check-file-p filename)
          (melpazoid-insert "Loading %s" filename)
          (condition-case err
              (load (expand-file-name filename) nil t t)
            (error
             (setq load-error t)
             (melpazoid-insert "  %s:Error: Emacs %s couldn't load:\n  %S"
                               filename emacs-version err))))))
    (melpazoid-insert "```")
    (when load-error (melpazoid-commit-inserted))))

(provide 'melpazoid)
;;; melpazoid.el ends here
