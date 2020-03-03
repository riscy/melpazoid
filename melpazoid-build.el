;;; melpazoid-build.el --- Build tool for .melpazoid  -*- lexical-binding: t; -*-

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

(defconst melpazoid-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.

See `package-build-default-files-spec' from MELPA package-build.")

(defun melpazoid--expand-file-specs (dir specs &optional subdir allow-empty)
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
                                        (melpazoid--expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (melpazoid--expand-file-specs
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

(defun melpazoid--config-file-list (recipe)
  "Build full source file specification from RECIPE.
See `package-build--config-file-list' from MELPA package-build."
  (let ((file-list (plist-get (cdr recipe) :files)))
    (cond
     ((null file-list)
      melpazoid-default-files-spec)
     ((eq :defaults (car file-list))
      (append melpazoid-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun melpazoid--expand-source-file-list (recipe dir)
  "Resolve source file from RECIPE in DIR.
See `package-build--expand-source-file-list' from MELPA package-build."
  (mapcar 'car
          (melpazoid--expand-file-specs
           dir
           (melpazoid--config-file-list recipe))))

(defun melpazoid--resolve-dependency (sandboxdir deps)
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
