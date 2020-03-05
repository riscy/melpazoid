;;; melpazoid-test.el --- Test definition for melpazoid  -*- lexical-binding: t; -*-

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

;; Test definition for melpazoid.

;;; Code:

(require 'buttercup)
(require 'melpazoid)

(defconst melpazoid-directory (file-name-directory
                               (directory-file-name
                                (file-name-directory
                                 (or byte-compile-current-file
                                     load-file-name
                                     (buffer-file-name)))))
  "Path to melpazoid root.")

(describe "A suite"
  (it "contains a spec with an expectation"
      (expect t :to-be t)))

(describe "Function ported from MELPA package-build"
  (it "config-file-list"
    (expect
     (melpazoid-build--config-file-list
      '(melpazoid :fetcher github :repo "riscy/melpazoid"
                  :files (:defaults (:exclude "melpazoid-async.el"))))
     :to-equal
     '("*.el" "*.el.in" "dir"
       "*.info" "*.texi" "*.texinfo"
       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el")
       (:exclude "melpazoid-async.el")))

    (expect
     (melpazoid-build--config-file-list
      '(melpazoid-async :fetcher github :repo "riscy/melpazoid"
                        :files ("melpazoid-async.el")))
     :to-equal
     '("melpazoid-async.el")))

  (it "expand-source-file-list"
    (expect
     (melpazoid-build--expand-source-file-list
      '(melpazoid :fetcher github :repo "riscy/melpazoid"
                  :files (:defaults (:exclude "melpazoid-async.el")))
      melpazoid-directory)
     :to-equal
     '("melpazoid-build.el" "melpazoid-checker.el" "melpazoid.el"))

    (expect
     (melpazoid-build--expand-source-file-list
      '(melpazoid-async :fetcher github :repo "riscy/melpazoid"
                        :files ("melpazoid-async.el"))
      melpazoid-directory)
     :to-equal
     '("melpazoid-async.el"))))

;; (provide 'melpazoid-test)
;;; melpazoid-test.el ends here
