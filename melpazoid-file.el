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

(provide 'melpazoid-file)
;;; melpazoid-file.el ends here
