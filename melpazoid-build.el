;;; melpazoid-build.el --- Build tool for .melpazoid  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/riscy/melpazoid

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

;; Build tool for .melpazoid.

;;; Code:

(require 'subr-x)
(require 'package)
(require 'pkg-info)

(defvar melpazoid-build-dependency-packages '(package-lint))


;;; MELPA package-build functions

(defconst melpazoid-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.

See `package-build-default-files-spec' from MELPA package-build.")

(defun melpazoid-build--expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error.

See `package-build-expand-file-specs' from MELPA package-build."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs lst)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (melpazoid-build--expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (melpazoid-build--expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.el\\.in\\'"
                                            ".el"
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

(defun melpazoid-build--config-file-list (recipe)
  "Build full source file specification from RECIPE.
See `package-build--config-file-list' from MELPA package-build."
  (let ((file-list (plist-get (cdr recipe) :files)))
    (cond
     ((null file-list)
      melpazoid-build-default-files-spec)
     ((eq :defaults (car file-list))
      (append melpazoid-build-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun melpazoid-build--expand-source-file-list (recipe dir)
  "Resolve source file from RECIPE in DIR.
See `package-build--expand-source-file-list' from MELPA package-build."
  (mapcar 'car
          (melpazoid-build--expand-file-specs
           dir
           (melpazoid-build--config-file-list recipe))))


;;; functions

(defun melpazoid-build--get-melpazoid-path (&optional dir)
  "Get directory path which Melpazoid located from DIR.
If no found the directory, returns nil.
If DIR is omitted, assume `default-directory'."
  (let ((file "Melpazoid")
        (dir* (or dir default-directory)))
    (expand-file-name file (locate-dominating-file dir* file))))

(defun melpazoid-build--read-melpazoid-file (&optional dir)
  "Return sexp from Melpazoid file in DIR.
If no found the Melpazoid file, returns nil.
If DIR is omitted, assume `default-directory'."
  (when-let (path (melpazoid-build--get-melpazoid-path dir))
    (read (with-temp-buffer
            (insert-file-contents path)
            (buffer-string)))))

(defun melpazoid-build--read-melpazoid-file-with-pkg (pkg &optional dir)
  "Return sexp from Melpazoid file in DIR for PKG.
If no found the Melpazoid file, returns nil.
If DIR is omitted, assume `default-directory'."
  (when-let (contents (melpazoid-build--read-melpazoid-file dir))
    (plist-get (cadr contents) pkg)))

(defun melpazoid-build--resolve-duplicate-reqs (reqs)
  "Resolve duplicate REQS."
  (let (ret)
    (dolist (req reqs)
      (let ((sym (car  req))
            (ver (cadr req)))
        (if (assq sym ret)
            (when (version-list-< (car (alist-get sym ret)) ver)
              (setf (alist-get sym ret) (list ver)))
          (push req ret))))
    (nreverse ret)))

(defun melpazoid-build--get-dependency-from-elisp-files (files)
  "Get package dependency from Package-Require header from FILES.
Duplicate requires are resolved by more restrictive."
  (let (ret)
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (when-let (package-desc (ignore-errors (package-buffer-info)))
            (push (package-desc-reqs package-desc) ret)))))
    (mapcan 'identity ret)))

(defun melpazoid-build--get-dependency-from-melpazoid-file (pkg &optional dir)
  "Get development package dependency from Melpazoid located DIR for PKG.
Currently, ignore any args for development.
If DIR is omitted, assume `default-directory'."
  (when-let* ((contents (melpazoid-build--read-melpazoid-file-with-pkg pkg dir))
              (devs (alist-get 'development contents)))
    (let (ret)
      (dolist (req devs)
        (if (listp req)
            (push `(,(car req) (0 0 1)) ret)
          (push `(,req (0 0 1)) ret)))
      (nreverse ret))))

(defun melpazoid-build--resolve-dependency (sandboxdir deps)
  "Fetch and build dependency in SANDBOXDIR.
DEPS is pkg symbol of list.
NOTE:
  - Version specification is ignored for now."
  (let ((package-user-dir sandboxdir)
        (package-archives package-archives))
    (require 'package)
    (package-initialize)
    (dolist (pkg deps)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

(provide 'melpazoid-build)
;;; melpazoid-build.el ends here
