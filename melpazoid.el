;;; melpazoid.el --- A MELPA review tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Chris Rayner <dchrisrayner@gmail.com>
;; Copyright (C) 2020       Naoya Yamashita <conao3@gmail.com>

;; Author: Chris Rayner <dchrisrayner@gmail.com>
;; Created: June 9 2019
;; Keywords: tools convenience
;; URL: https://github.com/riscy/melpazoid
;; Package-Requires: ((emacs "25.1") (package-lint "0.12") (f "0.20"))
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

;; A MELPA review tool to run the MELPA checklist in addition
;; to some other checks that might point to other issues.

;; Melpazoid generate and manage .melpazoid directory
;; It support multi package / multi Emacs version.
;; A template .malpazoid directory is like below structure.

;; leaf (rootdir)
;;   ├── .melpazoid
;;   │   ├── leaf
;;   │   │   ├── 26.3 (sandboxdir)
;;   │   │   │   ├── elpa
;;   │   │   │   ├── build
;;   │   │   │   │   └── leaf.elc
;;   │   │   │   └── dist
;;   │   │   │       ├── leaf-1.3.tar
;;   │   │   │       ├── leaf-1.3.readme
;;   │   │   │       └── leaf-1.3.entry
;;   │   │   └── 27.0 (sandboxdir)
;;   │   │       ├── elpa
;;   │   │       ├── build
;;   │   │       └── dist
;;   │   └── leaf-keywords
;;   │       ├── 26.3 (sandboxdir)
;;   │       │   ├── elpa
;;   │       │   ├── build
;;   │       │   └── dist
;;   │       └── 27.0 (sandboxdir)
;;   │           ├── elpa
;;   │           ├── build
;;   │           └── dist
;;   ├── Melpazoid
;;   │
;;   ... (any toplevel files are fine)
;;   │
;;   ├── leaf.el
;;   └── leaf-keywords.el

;;; Code:

(require 'package)
(require 'checkdoc)
(require 'pkg-info)
(require 'package-lint)
(require 'f)
(require 'melpazoid-build)
(require 'melpazoid-checker)

(defgroup melpazoid nil
  "A MELPA review tool."
  :group 'tool)

(defcustom melpazoid-checkers '(melpazoid-checker--byte-compile
                                melpazoid-checker--checkdoc
                                melpazoid-checker--package-lint
                                ;; melpazoid-checker--declare
                                melpazoid-checker--sharp-quotes
                                melpazoid-checker--misc)
  "List of checker which is called with 1 argument.
Argument is alist contain below information.
  - sandboxdir
  - elpadir
  - builddir
  - distdir
  - tmpfile"
  :type 'sexp
  :group 'melpazoid)

(defcustom melpazoid-buffer "*Melpazoid*"
  "Name of the `melpazoid' buffer."
  :type 'string
  :group 'melpazoid)


;;; functions

(defvar melpazoid--misc-header-printed-p nil "Whether misc-header was printed.")
(defvar melpazoid-error-p nil)
(defvar melpazoid-dependency-packages '(package-lint))


;;;###autoload
(defun melpazoid (&optional dir)
  "Specifies the DIR where the Melpazoid file located.
If the argument is omitted, the current directory is assumed."
  (let* ((file (locate-dominating-file (or dir default-directory) "Melpazoid"))
         (rootdir (file-name-directory file)))
    (if (not file)
        (error "File of 'Melpazoid' is missing")
      (let ((contents (eval (read (with-temp-buffer (insert-file-contents file))))))
        (while contents
          (let* ((pkg (pop contents))
                 (spec (pop contents)))
            (let-alist spec
              (let* ((sandboxdir (expand-file-name
                                  (format "%s.%s"
                                          emacs-major-version
                                          emacs-minor-version)
                                  (expand-file-name
                                   (symbol-name pkg)
                                   (expand-file-name ".melpazoid" dir))))
                     (builddir (expand-file-name "build" sandboxdir))
                     (sources (melpazoid--expand-source-file-list .recipe rootdir))
                     (reqs (append (melpazoid--get-dependency-from-elisp-files sources)
                                   (melpazoid--get-dependency-from-melpazoid-file .development)
                                   melpazoid-dependency-packages)))
                (melpazoid--resolve-dependency sandboxdir reqs)
                (dolist (source sources)
                  (let* ((tmpfile (expand-file-name (file-name-nondirectory source) builddir))
                         (info `((sandboxdir . ,sandboxdir)
                                 (elpadir    . ,(expand-file-name "elpa" sandboxdir))
                                 (builddir   . ,(expand-file-name "build" sandboxdir))
                                 (distdir    . ,(expand-file-name "dist" sandboxdir))
                                 (tmpfile    . ,tmpfile))))
                    (copy-file source tmpfile 'overwrite)
                    (dolist (checker melpazoid-checkers)
                      (funcall checker info))))
                (melpazoid-insert "\n## %s ##\n" (symbol-name pkg))
                (dolist (filename sources)
                  (melpazoid-insert "\n### %s ###\n" (file-name-nondirectory filename))
                  (save-window-excursion
                    (set-buffer (find-file filename))
                    ;; (melpazoid-byte-compile filename)
                    ;; (melpazoid-checkdoc filename)
                    ;; (melpazoid--check-declare)
                    ;; (melpazoid-package-lint)
                    ;; (melpazoid-check-sharp-quotes)
                    ;; (melpazoid-check-misc)
                    )
                  (pop-to-buffer melpazoid-buffer)
                  (goto-char (point-min)))))))))))

(provide 'melpazoid)
;;; melpazoid.el ends here
