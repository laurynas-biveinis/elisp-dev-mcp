;;; elisp-dev-mcp-no-checkdoc-test.el --- Test fixtures exempt from checkdoc -*- lexical-binding: t -*-
;; jscpd:ignore-start

;; Copyright (C) 2025 Laurynas Biveinis

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Laurynas Biveinis
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; Test fixture functions that are exempt from checkdoc validation.
;; This file contains functions that intentionally violate documentation
;; requirements for testing purposes.

;; jscpd:ignore-end
;;; Code:

(defun elisp-dev-mcp-no-checkdoc-test--no-docstring (x y)
  (+ x y))

(defun elisp-dev-mcp-no-checkdoc-test--empty-docstring (x y)
  ""
  (+ x y))

(defvar elisp-dev-mcp-no-checkdoc-test--empty-docstring-var nil
  "")

(provide 'elisp-dev-mcp-no-checkdoc-test)

;; Local Variables:
;; elisp-lint-ignored-validators: ("checkdoc")
;; package-lint-main-file: "elisp-dev-mcp.el"
;; End:

;;; elisp-dev-mcp-no-checkdoc-test.el ends here
