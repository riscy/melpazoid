;;; melpazoid.el --- A MELPA review tool  ; -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: June 9 2019
;; Keywords: tools, convenience
;; URL: https://github.com/riscy/melpazoid
;; Package-Requires: ((emacs "25.1") (pkg-info "0.6") (epl "0.9"))
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
(defvar melpazoid--misc-header-printed-p nil "Whether misc-header was printed.")
(defvar melpazoid-can-modify-buffers nil "Whether melpazoid can modify buffers.")

(defun melpazoid-byte-compile (filename)
  "Wrapper for running `byte-compile-file' against FILENAME."
  ;; TODO: use flycheck or its pattern for cleanroom byte-compiling
  (melpazoid-insert "byte-compile (using Emacs %s):" emacs-version)
  (melpazoid--remove-no-compile)
  (ignore-errors (kill-buffer "*Compile-Log*"))
  (let ((inhibit-message t))
    (melpazoid--check-lexical-binding)
    (let ((lexical-binding t)) (byte-compile-file filename)))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (if (melpazoid--buffer-almost-empty-p)
        (melpazoid-insert "- No issues!")
      (goto-char (point-min)) (forward-line 2)
      (melpazoid-insert "```")
      (melpazoid-insert
       (melpazoid--newline-trim (buffer-substring (point) (point-max))))
      (melpazoid-insert "```")))
  (melpazoid-insert ""))

(defun melpazoid--check-lexical-binding ()
  "Warn about lack of lexical binding."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "lexical-binding:[[:blank:]]*t"))
      (unless (re-search-forward regexp (point-at-eol) t)
        (if (re-search-forward regexp nil t)
            (melpazoid--annotate-line "Move `lexical-binding: t` to the first line")
          (melpazoid-insert "- Consider using lexical binding.")
          (melpazoid-insert "  See: https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org#fixing-typical-problems")
          (melpazoid-insert "- Lexical binding will be used in what follows"))))))

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

(defun melpazoid--buffer-almost-empty-p ()
  "Return non-nil if current buffer is 'almost' empty."
  (<= (- (point-max) (point)) 3))

(defun melpazoid-checkdoc (filename)
  "Wrapper for running `checkdoc-file' against FILENAME."
  (require 'checkdoc)  ; to retain cleaner byte-compilation in script mode
  (melpazoid-insert "checkdoc (using version %s):" checkdoc-version)
  (ignore-errors (kill-buffer "*Warnings*"))
  (let ((sentence-end-double-space nil)  ; be a little more lenient
        (checkdoc-proper-noun-list nil)
        (checkdoc-common-verbs-wrong-voice nil)
        (inhibit-message t))
    (ignore-errors (checkdoc-file filename)))
  (if (not (get-buffer "*Warnings*"))
      (melpazoid-insert "- No issues!")
    (with-current-buffer "*Warnings*"
      (melpazoid-insert "```")
      (melpazoid-insert
       (melpazoid--newline-trim (buffer-substring (point-min) (point-max))))
      (melpazoid-insert "```")))
  (melpazoid-insert ""))

(defun melpazoid-package-lint ()
  "Wrapper for running `package-lint' against the current buffer."
  (require 'package-lint)    ; to retain cleaner byte-compilation in script mode
  (require 'pkg-info)        ; to retain cleaner byte-compilation in script mode
  (melpazoid-insert
   "package-lint (using version %s):"
   (pkg-info-format-version (pkg-info-package-version "package-lint")))
  (ignore-errors (kill-buffer "*Package-Lint*"))
  (let ((package-lint-main-file (melpazoid--package-lint-main-file)))
    (ignore-errors (package-lint-current-buffer)))
  (with-current-buffer (get-buffer-create "*Package-Lint*")
    (let ((output (melpazoid--newline-trim (buffer-substring (point-min) (point-max)))))
      (if (string= "No issues found." output)
          (melpazoid-insert "- No issues!")
        (melpazoid-insert "```")
        (melpazoid-insert
         (if (string= output "")
             "package-lint:Error: No output.  Did you remember to (provide 'your-package)?"
           output))
        (melpazoid-insert "```"))))
  (melpazoid-insert ""))

(defun melpazoid--package-lint-main-file ()
  "Return suitable value for `package-lint-main-file'."
  (let ((package-main (getenv "PACKAGE_MAIN")))
    (cond ((eq package-main nil) nil)
          ((string= package-main "") nil)
          (t package-main))))

(defun melpazoid-check-declare ()
  "Wrapper for `melpazoid' check-declare.
NOTE: this sometimes backfires when running checks automatically inside
a Docker container, e.g. kellyk/emacs does not include the .el files."
  (melpazoid-insert "check-declare-file (optional):")
  (ignore-errors (kill-buffer "*Check Declarations Warnings*"))
  (check-declare-file (buffer-file-name (current-buffer)))
  (with-current-buffer (get-buffer-create "*Check Declarations Warnings*")
    (if (melpazoid--buffer-almost-empty-p)
        (melpazoid-insert "- No issues!")
      (melpazoid-insert "```")
      (melpazoid-insert (buffer-substring (point-min) (point-max)))
      (melpazoid-insert "```")))
  (melpazoid-insert ""))

(defun melpazoid-check-sharp-quotes ()
  "Check for missing sharp quotes."
  (melpazoid-misc "#'(lambda " "There is no need to quote lambdas (neither #' nor ')")
  (melpazoid-misc "[^#]'(lambda " "Quoting this lambda may prevent it from being compiled")
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
    (melpazoid-misc "(defalias '[^#()]*)" msg)
    (melpazoid-misc "(define-obsolete-function-alias '[[:graph:]]+ '[[:graph:]]" msg)
    (melpazoid-misc "(run-with-idle-timer[^(#]*[^#]'" msg)))

(defun melpazoid-check-misc ()
  "Miscellaneous checker."
  (melpazoid-misc "(string-equal major-mode" "Prefer `(eq major-mode 'xyz)`")
  (melpazoid-misc "(setq major-mode" "Prefer `define-derived-mode`")
  (melpazoid-misc "(global-set-key" "Don't set global bindings; tell users how in your `;;; Commentary`.")
  (melpazoid-misc "(setq-default " "Packages should use `defvar-local`, not `setq-default`")
  (melpazoid-misc "(string= major-mode" "Prefer `(eq major-mode 'xyz)`")
  (melpazoid-misc "(equal major-mode \"" "Prefer `(eq major-mode 'xyz)`")
  (melpazoid-misc "(add-to-list 'auto-mode-alist.*\\$" "Terminate auto-mode-alist entries with `\\\\'`")
  (melpazoid-misc "(setq auto-mode-alist" "Prefer `add-to-list` to add to auto-mode-alist")
  (melpazoid-misc "/tmp\\>" "Use `temporary-file-directory` instead of /tmp in code")
  (melpazoid-misc "Copyright.*Free Software Foundation" "Have you done the paperwork or is this copy-pasted?" nil t)
  (melpazoid-misc "This file is part of GNU Emacs." "Copy-paste error?" nil t)
  (melpazoid-misc "lighter \"[^ \"]" "Lighter should start with a space" t)
  (melpazoid-misc "lighter \".+ \"" "Lighter should start, but not end, with a space" t)
  (melpazoid-misc "(fset" "Ensure this `fset` isn't being used as a surrogate `defalias`")
  (melpazoid-misc "(fmakunbound" "`fmakunbound` should rarely occur in packages")
  (melpazoid-misc "^(progn" "`progn` is usually not required at the top level")
  (melpazoid-misc "([^ ]*read-string \"[^\"]+[^ \"]\"" "`read-string` prompts should often end with a space" t)
  (melpazoid-misc ";;;###autoload\n(add-hook" "Loading a package should rarely add hooks" nil t)
  (melpazoid-misc ";;;###autoload\n(advice-add" "Loading a package should rarely add advice" nil t)
  (melpazoid-misc ";; Package-Version" "Prefer `;; Version` over `;; Package-Version` (MELPA automatically adds `Package-Version`)" nil t)
  (melpazoid-misc "([<>eq/=]+ (point) (point-max))" "Could this point/point-max comparison use `eobp`?")
  (melpazoid-misc "([<>eq/=]+ (point) (point-min))" "Could this point/point-min comparison use `bobp`?")
  (melpazoid-misc "([<>eq/=]+ (point) (point-at-bol))" "Could this point/point-at-bol comparison use `bolp`?")
  (melpazoid-misc "([<>eq/=]+ (point) (point-at-eol))" "Could this point/point-at-eol comparison use `eolp`?")
  (melpazoid-misc "([<>eq/=]+ (point) (line-beginning-position))" "Could this point/point-at-bol comparison use `bolp`?")
  (melpazoid-misc "([<>eq/=]+ (point) (line-end-position))" "Could this point/point-at-eol comparison use `eolp`?")
  (melpazoid-misc "^(define-key" "Top-level `define-key` can overwrite user bindings.  Try: `(defvar my-map (let ((km (make-sparse-keymap))) (define-key ...) km))`")
  (melpazoid-misc "^(bind-keys" "Top-level bind-keys can overwrite user keybindings.  Try: `(defvar my-map (let ((km (make-sparse-keymap))) (bind-keys ...) km))`")
  (melpazoid-misc "(string-match[^(](symbol-name" "Prefer to use `eq` on symbols")
  (melpazoid-misc "(defcustom [^ ]*--" "Customizable variables shouldn't be private" t)
  (melpazoid-misc "(eval-when-compile (progn" "No `progn` required under `eval-when-compile`")
  (melpazoid-misc "(ignore-errors (progn" "No `progn` required under `ignore-errors`")
  (melpazoid-misc "(ignore-errors (re-search-[fb]" "Use `re-search-*`'s NOERROR argument")
  (melpazoid-misc "(ignore-errors (search-[fb]" "Use `search-*`'s NOERROR argument")
  (melpazoid-misc "(error (format" "No `format` required; errors are already f-strings")
  (melpazoid-misc "(user-error (format" "No `format` required; user-errors are already f-strings")
  (melpazoid-misc "(message (format" "No `format` required; messages are already f-strings")
  (melpazoid-misc "^ ;[^;]" "Single-line comments should usually begin with `;;`" nil t)
  (melpazoid-misc "(unless (not " "Use `when ...` instead of `unless (not ...)`")
  (melpazoid-misc "(unless (null " "Use `when ...` instead of `unless (null ...)`")
  (melpazoid-misc "(when (not " "Optionally use `unless ...` instead of `when (not ...)`")
  (melpazoid-misc "(when (null " "Optionally use `unless ...` instead of `when (null ...)`")
  (melpazoid-misc "(with-temp-buffer (set-buffer " "`set-buffer` is unnecessary here")
  (melpazoid-misc "^(autoload" "It may be simpler to just `require` this dependency")
  (melpazoid-misc "http://" "Prefer `https` over `http` if possible ([why?](https://news.ycombinator.com/item?id=22933774))" nil t t)
  (melpazoid-misc "(eq [^()]*\\<nil\\>.*)" "You can use `not` or `null`")
  (melpazoid-misc "(concat (format" "Can the `format` and `concat` be combined?")
  (melpazoid-misc "(format (concat" "Can the `format` and `concat` be combined?")
  ;; (melpazoid-misc "line-number-at-pos" "line-number-at-pos is surprisingly slow - avoid it")
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
          ;; print a header unless it's already been printed:
          (unless melpazoid--misc-header-printed-p
            (melpazoid-insert "Other possible lints:")
            (setq melpazoid--misc-header-printed-p t))
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

;;;###autoload
(defun melpazoid (&optional filename)
  "Check current buffer, or FILENAME's buffer if given."
  (interactive)
  (melpazoid--reset-state)
  (let ((filename (or filename (buffer-file-name (current-buffer)))))
    (melpazoid-insert "\n### %s ###\n" (file-name-nondirectory filename))
    (save-window-excursion
      (set-buffer (find-file filename))
      (melpazoid-byte-compile filename)
      (melpazoid-checkdoc filename)
      ;; (melpazoid-check-declare)
      (melpazoid-package-lint)
      (melpazoid-check-sharp-quotes)
      (melpazoid-check-misc))
    (pop-to-buffer melpazoid-buffer)
    (goto-char (point-min))))

(defun melpazoid--reset-state ()
  "Reset melpazoid's current state variables."
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)
  (setq melpazoid--misc-header-printed-p nil)
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

  (let ((filename nil) (filenames (directory-files ".")))
    (while filenames
      (setq filename (car filenames) filenames (cdr filenames))
      (when (melpazoid--check-file-p filename)
       (melpazoid filename))))

  ;; check whether FILENAMEs can be simply loaded (TODO: offer backtrace)
  (melpazoid-insert "\n### Loadability ###\n")
  (melpazoid-insert "Verifying ability to #'load each file:")
  (melpazoid-insert "```")

  (let ((filename nil) (filenames (directory-files ".")))
    (while filenames
      (setq filename (car filenames) filenames (cdr filenames))
      (when (melpazoid--check-file-p filename)
        (melpazoid-insert "Loading %s" filename)
        (unless (ignore-errors (load (expand-file-name filename) nil t t))
          (melpazoid-insert "%s:Error: Emacs %s errored during load"
                            filename emacs-version)))))
  (melpazoid-insert "Done.")
  (melpazoid-insert "```"))

(provide 'melpazoid)
;;; melpazoid.el ends here
