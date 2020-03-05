;;; melpazoid-file.el --- Manage tool for .melpazoid  -*- lexical-binding: t; -*-

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

;; Manage tool for .melpazoid.

;;; Code:

(require 'subr-x)

(defun melpazoid-file--path (&optional dir)
  "Get directory path which Melpazoid located from DIR.
If no found the directory, returns nil.
If DIR is omitted, assume `default-directory'."
  (let ((file "Melpazoid")
        (dir* (or dir default-directory)))
    (expand-file-name file (locate-dominating-file dir* file))))

(defun melpazoid-file--read (&optional dir)
  "Return sexp from Melpazoid file in DIR.
If no found the Melpazoid file, returns nil.
If DIR is omitted, assume `default-directory'."
  (when-let (path (melpazoid-file--path dir))
    (read (with-temp-buffer
            (insert-file-contents path)
            (buffer-string)))))

(defun melpazoid-file--read-pkg (pkg &optional dir)
  "Return sexp from Melpazoid file in DIR for PKG.
If no found the Melpazoid file, returns nil.
If DIR is omitted, assume `default-directory'."
  (when-let (contents (melpazoid-file--read dir))
    (plist-get (cadr contents) pkg)))

(provide 'melpazoid-file)
;;; melpazoid-file.el ends here
