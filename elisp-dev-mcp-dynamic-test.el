;;; elisp-dev-mcp-dynamic-test.el --- Test fixtures for dynamic binding -*- lexical-binding: nil -*-
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

;; Test fixture functions for dynamic binding context testing.
;; This file intentionally uses dynamic binding to create
;; functions with dynamic binding behavior for testing purposes.

;; jscpd:ignore-end
;;; Code:

;; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-dynamic-test--with-header-comment (arg1 arg2)
  "Sample function with a header comment in dynamic binding context.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2."
  (+ arg1 arg2))

(provide 'elisp-dev-mcp-dynamic-test)

;; Local Variables:
;; package-lint-main-file: "elisp-dev-mcp.el"
;; End:

;;; elisp-dev-mcp-dynamic-test.el ends here
