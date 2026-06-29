;;; elisp-dev-mcp-bytecode-test.el --- Bytecode test functions -*- lexical-binding: t -*-
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
;; Keywords: tools, development
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; Test fixture functions that will be byte-compiled for testing the
;; elisp-dev-mcp package.  These functions are used to verify that MCP
;; tools work correctly with byte-compiled code.  Some functions
;; intentionally lack documentation for testing purposes.

;; jscpd:ignore-end
;;; Code:

;; Header comment for byte-compiled function
;; This should be preserved in the function definition
(defun elisp-dev-mcp-bytecode-test--with-header (x y)
  "A byte-compiled function with header comment.
X and Y are numbers to be added."
  (+ x y))

(defun elisp-dev-mcp-bytecode-test--no-docstring (a b)
  (* a b))

(defun elisp-dev-mcp-bytecode-test--empty-docstring (n)
  ""
  (* n 2))

(provide 'elisp-dev-mcp-bytecode-test)

;; Local Variables:
;; elisp-lint-ignored-validators: ("checkdoc")
;; package-lint-main-file: "elisp-dev-mcp.el"
;; End:

;;; elisp-dev-mcp-bytecode-test.el ends here
