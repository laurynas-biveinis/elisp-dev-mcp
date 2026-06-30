;;; elisp-dev-mcp-test.el --- Tests for elisp-dev-mcp -*- lexical-binding: t -*-
;; jscpd:ignore-start

;; Copyright (C) 2025-2026 elisp-dev-mcp.el contributors

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
;; Package-Requires: ((emacs "24.1"))
;; Keywords: tools, development
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; Tests for the elisp-dev-mcp package.

;; jscpd:ignore-end
;;; Code:

(require 'ert)
(require 'json)
(require 'mcp-server-lib)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-ert)
(require 'elisp-dev-mcp)
(require 'elisp-dev-mcp-no-checkdoc-test)

(setq mcp-server-lib-ert-server-id "elisp-dev-mcp")

;;; Test functions used for function definition retrieval tests. Should be the
;;; first code in the file to keep the test line numbers stable.

;; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-test--with-header-comment (arg1 arg2)
  "Sample function with a header comment.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2."
  (+ arg1 arg2))

;; This comment is separated by an empty line from the next function and should
;; not be returned together with it.

(defun elisp-dev-mcp-test--without-header-comment (value)
  "Simple function without a header comment.
VALUE is multiplied by 2."
  (* value 2))

;; An inline function for testing describe-function with defsubst
(defsubst elisp-dev-mcp-test--inline-function (x)
  "An inline function for testing purposes.
X is the input value that will be doubled."
  (* 2 x))

(defalias
  'elisp-dev-mcp-test--aliased-function
  #'elisp-dev-mcp-test--with-header-comment
  "This is an alias for elisp-dev-mcp-test--with-header-comment.")

(defmacro elisp-dev-mcp-test--with-server (&rest body)
  "Execute BODY with running MCP server and elisp-dev-mcp enabled."
  (declare (indent defun) (debug t))
  `(unwind-protect
       (progn
         (mcp-server-lib-start)
         (elisp-dev-mcp-enable)
         ,@body)
     (elisp-dev-mcp-disable)
     (mcp-server-lib-stop)))

(defmacro elisp-dev-mcp-test--with-bytecode-file (&rest body)
  "Execute BODY with bytecode test file compiled and loaded.
Handles compilation, loading, and cleanup of elisp-dev-mcp-bytecode-test.el."
  (declare (indent defun) (debug t))
  `(let* ((source-file
           (expand-file-name "elisp-dev-mcp-bytecode-test.el"))
          (bytecode-file
           (concat (file-name-sans-extension source-file) ".elc")))
     (unwind-protect
         (progn
           (should (byte-compile-file source-file))
           (should (load bytecode-file nil t t))
           ,@body)
       (when (file-exists-p bytecode-file)
         (delete-file bytecode-file)))))

(defmacro elisp-dev-mcp-test--with-compressed-file (&rest body)
  "Execute BODY with compressed test file loaded.
Loads elisp-dev-mcp-compressed-test.el.gz and cleans up the loaded function."
  (declare (indent defun) (debug t))
  `(let* ((test-dir
           (file-name-directory
            (locate-library "elisp-dev-mcp-test")))
          (source-file
           (expand-file-name "elisp-dev-mcp-compressed-test.el"
                             test-dir))
          (original-load-path load-path))
     (unwind-protect
         (progn
           ;; Add test dir to load-path so load can find the file
           (push test-dir load-path)
           ;; Verify setup: only .gz exists, no .el
           (should (file-exists-p (concat source-file ".gz")))
           (should-not (file-exists-p source-file))
           ;; Load using library name (Emacs transparently decompresses .gz)
           (should (load "elisp-dev-mcp-compressed-test" nil t))
           ,@body)
       (setq load-path original-load-path)
       (fmakunbound
        'elisp-dev-mcp-compressed-test--sample-function))))

(defmacro elisp-dev-mcp-test--with-temp-dir (var prefix &rest body)
  "Execute BODY with VAR bound to a temp directory.
The directory is deleted recursively on cleanup.
PREFIX is passed to `make-temp-file'."
  (declare (indent 2) (debug t))
  `(let ((,var (make-temp-file ,prefix t)))
     (unwind-protect
         (progn
           ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun elisp-dev-mcp-test--with-bytecode-describe-function
    (function-name)
  "Describe FUNCTION-NAME after loading bytecode file.
Returns the description text."
  (elisp-dev-mcp-test--with-bytecode-file
    (elisp-dev-mcp-test--with-server
      (mcp-server-lib-ert-call-tool
       "elisp-describe-function" `((function . ,function-name))))))

(defun elisp-dev-mcp-test--with-bytecode-get-definition
    (function-name)
  "Get definition data for FUNCTION-NAME after loading bytecode file.
Returns the parsed JSON response."
  (elisp-dev-mcp-test--with-bytecode-file
    (elisp-dev-mcp-test--with-server
      (elisp-dev-mcp-test--get-definition-response-data
       function-name))))

;;; Test variables

(defvar elisp-dev-mcp-test--undocumented-var)

(defcustom elisp-dev-mcp-test--custom-var "default"
  "A custom variable for testing."
  :type 'string
  :group 'elisp-dev-mcp)

(defcustom elisp-dev-mcp-test--custom-choice-var 'option1
  "A custom variable with choice type for testing."
  :type
  '(choice
    (const :tag "Option 1" option1)
    (const :tag "Option 2" option2)
    (string :tag "Custom string"))
  :group 'elisp-dev-mcp)

(defvaralias 'elisp-dev-mcp-test--a 'elisp-dev-mcp-test--b "x")

(defvar elisp-dev-mcp-test--b "test-value"
  "A regular variable for alias testing.")

(defvar elisp-dev-mcp-test--obsolete-var "old-value"
  "An obsolete variable for testing.")
(make-obsolete-variable
 'elisp-dev-mcp-test--obsolete-var 'elisp-dev-mcp-test--new-var "1.0")

(defvar elisp-dev-mcp-test--unbound-documented-var nil
  "A documented variable that will be unbound for testing.")
(makunbound 'elisp-dev-mcp-test--unbound-documented-var)

;;; Helpers to create JSON requests

(defun elisp-dev-mcp-test--check-closure-text (text)
  "Check TEXT containing expected closure description for current Emacs."
  (if (>= emacs-major-version 30)
      (should (string-match-p "interpreted-function" text))
    (should (string-match-p "Lisp closure" text))))

(defun elisp-dev-mcp-test--check-dynamic-text (text)
  "Check TEXT containing expected dynamic binding function description."
  (if (>= emacs-major-version 30)
      (should (string-match-p "interpreted-function" text))
    (should (string-match-p "Lisp function" text))))

(defun elisp-dev-mcp-test--check-empty-docstring (source)
  "Check SOURCE for empty docstring based on Emacs version."
  (if (>= emacs-major-version 30)
      ;; Emacs 30+ strips empty docstrings
      (should-not (string-match-p "\"\"" source))
    ;; Older versions preserve empty docstrings
    (should (string-match-p "\"\"" source))))

;;; Helpers to analyze response JSON

(defun elisp-dev-mcp-test--verify-error-resp (response error-pattern)
  "Verify that RESPONSE is an error response matching ERROR-PATTERN."
  (should
   (string-match-p
    error-pattern
    (mcp-server-lib-ert-check-text-response response t))))

(defun elisp-dev-mcp-test--verify-tool-call-error
    (tool-name args expected-msg)
  "Call TOOL-NAME with ARGS and verify the error response is EXPECTED-MSG."
  (elisp-dev-mcp-test--with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             tool-name 1 args))
           (resp
            (mcp-server-lib-process-jsonrpc-parsed
             req mcp-server-lib-ert-server-id)))
      (elisp-dev-mcp-test--verify-error-resp resp expected-msg))))

(defun elisp-dev-mcp-test--verify-empty-name (tool-name param-name)
  "Verify empty name handling for TOOL-NAME with PARAM-NAME."
  (elisp-dev-mcp-test--verify-tool-call-error
   tool-name
   `((,param-name . ""))
   (format "Empty %s name" param-name)))

(defun elisp-dev-mcp-test--verify-invalid-type (tool-name param-name)
  "Verify invalid type handling for TOOL-NAME with PARAM-NAME."
  (elisp-dev-mcp-test--verify-tool-call-error
   tool-name
   `((,param-name . 123))
   (format "Invalid %s name" param-name)))

(defun elisp-dev-mcp-test--get-definition-response-data
    (function-name)
  "Get function definition response data for FUNCTION-NAME.
Returns the parsed JSON response object."
  (let ((text
         (mcp-server-lib-ert-call-tool
          "elisp-get-function-definition"
          `((function . ,function-name)))))
    (json-read-from-string text)))

(defun elisp-dev-mcp-test--definition-fields
    (parsed-resp expected-file)
  "Assert PARSED-RESP is from EXPECTED-FILE; return (source start-line end-line)."
  (let ((source (assoc-default 'source parsed-resp))
        (file-path (assoc-default 'file-path parsed-resp))
        (start-line (assoc-default 'start-line parsed-resp))
        (end-line (assoc-default 'end-line parsed-resp)))
    (should
     (string= (file-name-nondirectory file-path) expected-file))
    (list source start-line end-line)))

(defun elisp-dev-mcp-test--verify-compressed-sample-definition ()
  "Verify `elisp-get-function-definition' reads the compressed sample function."
  (elisp-dev-mcp-test--with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-compressed-test--sample-function"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp)))
      (should source)
      (should
       (string-match-p
        "defun elisp-dev-mcp-compressed-test--sample-function"
        source))
      (should (string-match-p "Header comment" source))
      (should
       (string-match-p
        "elisp-dev-mcp-compressed-test\\.el" file-path)))))

(defun elisp-dev-mcp-test--verify-header-comment-docstring (text)
  "Verify describe-function TEXT shows the with-header-comment docstring/params."
  ;; Should include the docstring
  (should
   (string-match-p "Sample function with a header comment" text))
  ;; Should include parameter documentation
  (should (string-match-p "ARG1 is the first argument" text)))

(defun elisp-dev-mcp-test--verify-mcp-server-lib-source (text)
  "Verify TEXT is the mcp-server-lib.el source (header, metadata, footer)."
  ;; Should contain the file header
  (should (string-match-p ";;; mcp-server-lib.el" text))
  ;; Should contain package metadata
  (should (string-match-p "Model Context Protocol" text))
  ;; Should end with proper footer
  (should (string-match-p ";;; mcp-server-lib.el ends here" text)))

(defun elisp-dev-mcp-test--verify-definition-in-test-file
    (function-name
     expected-start-line expected-end-line expected-source)
  "Verify function definition for FUNCTION-NAME is in test file.
Checks that the function is defined in elisp-dev-mcp-test.el with
EXPECTED-START-LINE, EXPECTED-END-LINE and EXPECTED-SOURCE."
  (let* ((fields
          (elisp-dev-mcp-test--definition-fields
           (elisp-dev-mcp-test--get-definition-response-data
            function-name)
           "elisp-dev-mcp-test.el"))
         (source (nth 0 fields))
         (start-line (nth 1 fields))
         (end-line (nth 2 fields)))
    (should (= start-line expected-start-line))
    (should (= end-line expected-end-line))
    (should (string= source expected-source))))

(defun elisp-dev-mcp-test--verify-interactive-definition
    (function-name expected-patterns)
  "Verify interactive function definition for FUNCTION-NAME.
EXPECTED-PATTERNS is a list of regex patterns that should match in the source."
  (let* ((parsed-resp
          (elisp-dev-mcp-test--get-definition-response-data
           function-name))
         (source (assoc-default 'source parsed-resp))
         (file-path (assoc-default 'file-path parsed-resp)))

    ;; Basic verifications
    (should source)
    (should (string-match-p "defun" source))
    (should (string-match-p function-name source))
    (should (string-match-p "interactive" file-path))

    ;; Check all expected patterns
    (dolist (pattern expected-patterns)
      (should (string-match-p pattern source)))))

(defun elisp-dev-mcp-test--get-parsed-response (tool-name args)
  "Get parsed JSON response for TOOL-NAME with ARGS.
ARGS should be an alist of parameter names and values."
  (elisp-dev-mcp-test--with-server
    (let ((text (mcp-server-lib-ert-call-tool tool-name args)))
      (json-read-from-string text))))

(defun elisp-dev-mcp-test--read-source-file (library-or-path)
  "Read source file at LIBRARY-OR-PATH using elisp-read-source-file tool.
LIBRARY-OR-PATH can be a library name or absolute file path.
Returns the file contents as a string."
  (elisp-dev-mcp-test--with-server
    (mcp-server-lib-ert-call-tool
     "elisp-read-source-file"
     `((library-or-path . ,library-or-path)))))

(defun elisp-dev-mcp-test--verify-read-source-file-fails
    (library-or-path error-pattern)
  "Verify that reading LIBRARY-OR-PATH produces error matching ERROR-PATTERN."
  (elisp-dev-mcp-test--with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file"
             1
             `((library-or-path . ,library-or-path))))
           (resp
            (mcp-server-lib-process-jsonrpc-parsed
             req mcp-server-lib-ert-server-id)))
      (elisp-dev-mcp-test--verify-error-resp resp error-pattern))))

;;; Tests

(ert-deftest elisp-dev-mcp-test-describe-function ()
  "Test that `describe-function' MCP handler works correctly."
  (elisp-dev-mcp-test--with-server
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function" `((function . "defun")))))
      (should (string-match-p "defun" text)))))

(ert-deftest elisp-dev-mcp-test-describe-nonexistent-function ()
  "Test that `describe-function' MCP handler handles non-existent functions."
  (elisp-dev-mcp-test--verify-tool-call-error
   "elisp-describe-function"
   '((function . "non-existent-function-xyz"))
   "Function non-existent-function-xyz is void"))

(ert-deftest elisp-dev-mcp-test-describe-invalid-function-type ()
  "Test that `describe-function' handles non-string function names properly."
  (elisp-dev-mcp-test--verify-invalid-type
   "elisp-describe-function" 'function))

(ert-deftest elisp-dev-mcp-test-describe-empty-string-function ()
  "Test that `describe-function' MCP handler handles empty string properly."
  (elisp-dev-mcp-test--verify-empty-name
   "elisp-describe-function" 'function))

(ert-deftest elisp-dev-mcp-test-describe-variable-as-function ()
  "Test that `describe-function' MCP handler handles variable names properly."
  (elisp-dev-mcp-test--with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-function"
             1
             `((function . "user-emacs-directory"))))
           (resp
            (mcp-server-lib-process-jsonrpc-parsed
             req mcp-server-lib-ert-server-id)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Function user-emacs-directory is void"))))

(ert-deftest elisp-dev-mcp-test-describe-macro ()
  "Test that `describe-function' MCP handler works correctly with macros."
  (elisp-dev-mcp-test--with-server
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function" `((function . "when")))))
      (should (string-match-p "when" text))
      (should (string-match-p "macro" text)))))

(ert-deftest elisp-dev-mcp-test-describe-inline-function ()
  "Test that `describe-function' MCP handler works with inline functions."
  (elisp-dev-mcp-test--with-server
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function"
            `((function . "elisp-dev-mcp-test--inline-function")))))
      (should
       (string-match-p "elisp-dev-mcp-test--inline-function" text))
      (should (string-match-p "inline" text)))))

(ert-deftest elisp-dev-mcp-test-describe-regular-function ()
  "Test that `describe-function' MCP handler works with regular defun."
  (elisp-dev-mcp-test--with-server
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function"
            `((function .
                        "elisp-dev-mcp-test--with-header-comment")))))
      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-test--with-header-comment" text))
      ;; Should show it's in the test file
      (should (string-match-p "elisp-dev-mcp-test\\.el" text))
      ;; Should show the function signature with arguments
      (should
       (string-match-p
        "elisp-dev-mcp-test--with-header-comment ARG1 ARG2)" text))
      (elisp-dev-mcp-test--verify-header-comment-docstring text))))

(ert-deftest elisp-dev-mcp-test-describe-function-no-docstring ()
  "Test `describe-function' MCP handler with undocumented functions."
  (elisp-dev-mcp-test--with-server
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function"
            `((function
               .
               "elisp-dev-mcp-no-checkdoc-test--no-docstring")))))
      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-no-checkdoc-test--no-docstring" text))
      (elisp-dev-mcp-test--check-closure-text text)
      ;; Should show argument list (uppercase) in the signature
      (should
       (string-match-p
        "elisp-dev-mcp-no-checkdoc-test--no-docstring X Y)" text))
      ;; Should indicate lack of documentation
      (should (string-match-p "Not documented" text)))))

(ert-deftest elisp-dev-mcp-test-describe-function-empty-docstring ()
  "Test `describe-function' MCP handler with empty docstring functions."
  (elisp-dev-mcp-test--with-server
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function"
            `((function
               .
               "elisp-dev-mcp-no-checkdoc-test--empty-docstring")))))
      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-no-checkdoc-test--empty-docstring" text))
      (elisp-dev-mcp-test--check-closure-text text)
      ;; Should show argument list (uppercase) in the signature
      (should
       (string-match-p
        "elisp-dev-mcp-no-checkdoc-test--empty-docstring X Y)" text))
      ;; Should show it's in the test file
      (should
       (string-match-p "elisp-dev-mcp-no-checkdoc-test\\.el" text))
      ;; Should show the function signature with arguments
      (should
       (string-match-p
        "elisp-dev-mcp-no-checkdoc-test--empty-docstring X Y)"
        text)))))

(defun elisp-dev-mcp-test--find-tools-in-tools-list ()
  "Get the current list of MCP tools as returned by the server.
Returns a list of our registered tools in the order:
\(describe-function-tool get-definition-tool describe-variable-tool
info-lookup-tool read-source-file-tool).
Any tool not found will be nil in the list."
  (let* ((req (mcp-server-lib-create-tools-list-request))
         (resp
          (mcp-server-lib-process-jsonrpc-parsed
           req mcp-server-lib-ert-server-id))
         (result (assoc-default 'result resp))
         (tools (assoc-default 'tools result))
         (describe-function-tool nil)
         (get-definition-tool nil)
         (describe-variable-tool nil)
         (info-lookup-tool nil)
         (read-source-file-tool nil))

    ;; Find our tools in the list
    (dotimes (i (length tools))
      (let* ((tool (aref tools i))
             (name (assoc-default 'name tool)))
        (cond
         ((string= name "elisp-describe-function")
          (setq describe-function-tool tool))
         ((string= name "elisp-get-function-definition")
          (setq get-definition-tool tool))
         ((string= name "elisp-describe-variable")
          (setq describe-variable-tool tool))
         ((string= name "elisp-info-lookup-symbol")
          (setq info-lookup-tool tool))
         ((string= name "elisp-read-source-file")
          (setq read-source-file-tool tool)))))

    (list
     describe-function-tool
     get-definition-tool
     describe-variable-tool
     info-lookup-tool
     read-source-file-tool)))

(ert-deftest elisp-dev-mcp-test-read-source-file-empty-string ()
  "Test that empty string input is rejected with appropriate error."
  (elisp-dev-mcp-test--verify-read-source-file-fails "" "Invalid"))

(ert-deftest elisp-dev-mcp-test-read-source-file-whitespace-only ()
  "Test that whitespace-only string input is rejected with appropriate error."
  (elisp-dev-mcp-test--verify-read-source-file-fails "   " "Invalid"))

(ert-deftest elisp-dev-mcp-test-tools-registration-and-unregistration
    ()
  "Test tools registration, annotations, and proper unregistration."
  ;; First test that tools are properly registered with annotations
  (unwind-protect
      (progn
        (mcp-server-lib-start)
        (elisp-dev-mcp-enable)

        ;; Check tool registration
        (let* ((tools (elisp-dev-mcp-test--find-tools-in-tools-list))
               (describe-function-tool (nth 0 tools))
               (get-definition-tool (nth 1 tools))
               (describe-variable-tool (nth 2 tools))
               (info-lookup-tool (nth 3 tools))
               (read-source-file-tool (nth 4 tools)))

          ;; Verify all tools are registered
          (should describe-function-tool)
          (should get-definition-tool)
          (should describe-variable-tool)
          (should info-lookup-tool)
          (should read-source-file-tool)

          ;; Verify read-only annotations for all tools
          (dolist (tool
                   (list
                    describe-function-tool
                    get-definition-tool
                    describe-variable-tool
                    info-lookup-tool
                    read-source-file-tool))
            (let ((annotations (assoc-default 'annotations tool)))
              (should annotations)
              (should
               (eq (assoc-default 'readOnlyHint annotations) t))))

          ;; Now test unregistration
          (elisp-dev-mcp-disable)

          ;; Get updated tools list and verify tools are unregistered
          (let ((tools
                 (elisp-dev-mcp-test--find-tools-in-tools-list)))
            (should-not (nth 0 tools)) ;; describe-function should be gone
            (should-not (nth 1 tools)) ;; get-definition should be gone
            (should-not (nth 2 tools)) ;; describe-variable should be gone
            (should-not (nth 3 tools)) ;; info-lookup should be gone
            (should-not (nth 4 tools)) ;; read-source-file should be gone
            )))

    ;; Clean up
    (elisp-dev-mcp-disable)
    (mcp-server-lib-stop)))

(ert-deftest elisp-dev-mcp-test-get-function-definition ()
  "Test that `elisp-get-function-definition' MCP handler works correctly."
  (elisp-dev-mcp-test--with-server
    (elisp-dev-mcp-test--verify-definition-in-test-file
     "elisp-dev-mcp-test--without-header-comment" 59 62
     "(defun elisp-dev-mcp-test--without-header-comment (value)
  \"Simple function without a header comment.
VALUE is multiplied by 2.\"
  (* value 2))")))

(ert-deftest elisp-dev-mcp-test-get-nonexistent-function-definition ()
  "Test that `elisp-get-function-definition' handles non-existent functions."
  (elisp-dev-mcp-test--verify-tool-call-error
   "elisp-get-function-definition"
   '((function . "non-existent-function-xyz"))
   "Function non-existent-function-xyz is not found"))

(ert-deftest elisp-dev-mcp-test-get-function-definition-invalid-type
    ()
  "Test that `elisp-get-function-definition' handles non-string names."
  (elisp-dev-mcp-test--verify-invalid-type
   "elisp-get-function-definition" 'function))

(ert-deftest elisp-dev-mcp-test-get-c-function-definition ()
  "Test that `elisp-get-function-definition' handles C-implemented functions."
  (elisp-dev-mcp-test--with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data "car"))
           (is-c-function (assoc-default 'is-c-function parsed-resp))
           (function-name (assoc-default 'function-name parsed-resp))
           (message (assoc-default 'message parsed-resp)))

      (should (eq is-c-function t))
      (should (string= function-name "car"))
      (should (stringp message))
      (should (string-match-p "C source code" message))
      (should (string-match-p "car" message))
      (should (string-match-p "elisp-describe-function" message))
      (should (string-match-p "docstring" message)))))

(ert-deftest elisp-dev-mcp-test-get-function-with-header-comment ()
  "Test that `elisp-get-function-definition' includes header comments."
  (elisp-dev-mcp-test--with-server
    (elisp-dev-mcp-test--verify-definition-in-test-file
     "elisp-dev-mcp-test--with-header-comment" 44 54
     ";; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-test--with-header-comment (arg1 arg2)
  \"Sample function with a header comment.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2.\"
  (+ arg1 arg2))")))

(ert-deftest elisp-dev-mcp-test-get-interactively-defined-function ()
  "Test interactively defined functions with get-function-definition."
  (elisp-dev-mcp-test--with-server
    ;; Define a function interactively (not from a file)
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-function")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function
            (eval
             `(defun ,sym ()
                "An interactively defined function with no file location."
                'interactive-result))

            ;; Now try to get its definition
            (elisp-dev-mcp-test--verify-interactive-definition
             test-function-name '()))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-get-interactive-function-with-args ()
  "Test interactively defined functions with complex args."
  (elisp-dev-mcp-test--with-server
    ;; Define a function interactively with special parameter specifiers
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-complex-args")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function
            (eval
             `(defun ,sym (a b &optional c &rest d)
                "A function with complex argument list.
A and B are required arguments.
C is optional.
D captures remaining arguments."
                (list a b c d)))

            ;; Now try to get its definition
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     test-function-name))
                   (source (assoc-default 'source parsed-resp))
                   (file-path (assoc-default 'file-path parsed-resp)))

              ;; Verify that we get a definition back with correct argument list
              (should source)
              (should (string-match-p "defun" source))
              (should (string-match-p test-function-name source))
              (should
               (string-match-p "(a b &optional c &rest d)" source))
              (should (string-match-p "list a b c d" source))
              (should (string-match-p "A and B are required" source))
              (should (string-match-p "interactive" file-path))))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-describe-function-alias ()
  "Test that `describe-function' MCP handler works with function aliases."
  (elisp-dev-mcp-test--with-server
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function"
            `((function . "elisp-dev-mcp-test--aliased-function")))))

      ;; Should indicate it's an alias
      (should (string-match-p "alias" text))

      ;; Should mention the original function
      (should
       (string-match-p
        "elisp-dev-mcp-test--with-header-comment" text))

      ;; Should include the custom docstring of the alias
      (should (string-match-p "This is an alias for" text)))))

(ert-deftest elisp-dev-mcp-test-get-function-definition-alias ()
  "Test that `elisp-get-function-definition' works with function aliases."
  (elisp-dev-mcp-test--with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-test--aliased-function"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp)))


      ;; The source should include a complete defalias form with all components:
      ;; 1. The defalias keyword
      (should (string-match-p "defalias" source))
      ;; 2. The alias function name (quoted)
      (should
       (string-match-p
        "'elisp-dev-mcp-test--aliased-function" source))
      ;; 3. The target function name (quoted with hash)
      (should
       (string-match-p
        "#'elisp-dev-mcp-test--with-header-comment" source))
      ;; 4. The docstring (important component)
      (should (string-match-p "\"This is an alias for" source))

      ;; Verify file path is correct
      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-test.el")))))

(ert-deftest elisp-dev-mcp-test-get-special-form-definition ()
  "Test that `elisp-get-function-definition' handles special forms correctly."
  (elisp-dev-mcp-test--with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data "if"))
           (is-c-function (assoc-default 'is-c-function parsed-resp))
           (function-name (assoc-default 'function-name parsed-resp))
           (message (assoc-default 'message parsed-resp)))

      (should (eq is-c-function t))
      (should (string= function-name "if"))
      (should (stringp message))
      (should (string-match-p "C source code" message))
      (should (string-match-p "if" message))
      (should (string-match-p "elisp-describe-function" message))
      (should (string-match-p "docstring" message)))))

(ert-deftest elisp-dev-mcp-test-get-function-definition-compressed ()
  "Test `elisp-get-function-definition' reads from compressed .el.gz files."
  (elisp-dev-mcp-test--with-compressed-file
    (elisp-dev-mcp-test--verify-compressed-sample-definition)))

(ert-deftest
    elisp-dev-mcp-test-get-function-definition-compressed-no-auto-mode
    ()
  "Read .el.gz via `elisp-get-function-definition' with compression mode off.
This verifies that the code can read from .gz files even without
`auto-compression-mode' enabled."
  (let ((test-dir
         (file-name-directory (locate-library "elisp-dev-mcp-test")))
        (original-load-path load-path)
        (original-auto-compression-mode auto-compression-mode))
    (unwind-protect
        (progn
          ;; Add test dir to load-path so find-lisp-object-file-name works
          (push test-dir load-path)
          ;; Verify setup: only .gz exists
          (should
           (file-exists-p
            (expand-file-name "elisp-dev-mcp-compressed-test.el.gz"
                              test-dir)))
          (should-not
           (file-exists-p
            (expand-file-name "elisp-dev-mcp-compressed-test.el"
                              test-dir)))
          ;; Load using library name (not full path) so load-history is correct
          (load "elisp-dev-mcp-compressed-test" nil t)
          ;; Verify find-lisp-object-file-name returns the .gz path
          (should
           (string-suffix-p
            ".gz"
            (find-lisp-object-file-name
             'elisp-dev-mcp-compressed-test--sample-function 'defun)))
          ;; Now disable auto-compression-mode
          (auto-compression-mode -1)
          ;; Call the MCP tool - should succeed in reading .gz file
          (elisp-dev-mcp-test--verify-compressed-sample-definition))
      ;; Cleanup
      (setq load-path original-load-path)
      (auto-compression-mode
       (if original-auto-compression-mode
           1
         -1))
      (fmakunbound 'elisp-dev-mcp-compressed-test--sample-function))))

(ert-deftest elisp-dev-mcp-test-get-empty-string-function-definition
    ()
  "Test that `elisp-get-function-definition' handles empty string properly."
  (elisp-dev-mcp-test--verify-empty-name
   "elisp-get-function-definition" 'function))

(ert-deftest elisp-dev-mcp-test-get-variable-as-function-definition ()
  "Test that `elisp-get-function-definition' handles variable names properly."
  (elisp-dev-mcp-test--with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-get-function-definition"
             1
             `((function . "load-path"))))
           (resp
            (mcp-server-lib-process-jsonrpc-parsed
             req mcp-server-lib-ert-server-id)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Function load-path is not found"))))

(ert-deftest elisp-dev-mcp-test-get-function-definition-no-docstring
    ()
  "Test `elisp-get-function-definition' with undocumented functions."
  (elisp-dev-mcp-test--with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-no-checkdoc-test--no-docstring"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp)))

      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-no-checkdoc-test.el"))
      (should
       (string=
        source
        "(defun elisp-dev-mcp-no-checkdoc-test--no-docstring (x y)
  (+ x y))")))))

(ert-deftest
    elisp-dev-mcp-test-get-function-definition-empty-docstring
    ()
  "Test `elisp-get-function-definition' with empty docstring functions."
  (elisp-dev-mcp-test--with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-no-checkdoc-test--empty-docstring"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp)))

      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-no-checkdoc-test.el"))
      (should
       (string=
        source
        "(defun elisp-dev-mcp-no-checkdoc-test--empty-docstring (x y)
  \"\"
  (+ x y))")))))

(ert-deftest
    elisp-dev-mcp-test-get-interactive-function-definition-no-docstring
    ()
  "Test get-function-definition for dynamically defined function w/o docstring."
  (elisp-dev-mcp-test--with-server
    ;; Define a function interactively without a docstring
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-no-docstring")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function without docstring
            (eval
             `(defun ,sym (a b)
                (+ a b)))

            ;; Now try to get its definition and verify specific patterns
            (elisp-dev-mcp-test--verify-interactive-definition
             test-function-name '("(a b)" "(\\+ a b)"))

            ;; Additional check: Should not contain a docstring
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     test-function-name))
                   (source (assoc-default 'source parsed-resp)))
              (should-not (string-match-p "\"" source))))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest
    elisp-dev-mcp-test-get-interactive-function-definition-empty-docstring
    ()
  "Test get-function-definition for dynamically defined function w/ empty doc."
  (elisp-dev-mcp-test--with-server
    ;; Define a function interactively with an empty docstring
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-empty-docstring")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function with empty docstring
            (eval
             `(defun ,sym (a b)
                ""
                (+ a b)))

            ;; Now try to get its definition and verify specific patterns
            (elisp-dev-mcp-test--verify-interactive-definition
             test-function-name '("(a b)" "(\\+ a b)"))

            ;; Additional check: Should contain an empty docstring
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     test-function-name))
                   (source (assoc-default 'source parsed-resp)))
              (elisp-dev-mcp-test--check-empty-docstring source)))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-describe-dynamic-binding-function ()
  "Test `describe-function' with functions from lexical-binding: nil files."
  (elisp-dev-mcp-test--with-server
    ;; Load the dynamic binding test file
    (require 'elisp-dev-mcp-dynamic-test)

    ;; Test describe-function with dynamic binding function.
    (let ((text
           (mcp-server-lib-ert-call-tool
            "elisp-describe-function"
            `((function
               .
               "elisp-dev-mcp-dynamic-test--with-header-comment")))))

      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-dynamic-test--with-header-comment" text))
      ;; Should show it's in the dynamic binding test file
      (should (string-match-p "elisp-dev-mcp-dynamic-test\\.el" text))
      ;; Should show as "Lisp function" not "Lisp closure"
      (elisp-dev-mcp-test--check-dynamic-text text)
      (should-not (string-match-p "closure" text))
      (elisp-dev-mcp-test--verify-header-comment-docstring text))))

(ert-deftest
    elisp-dev-mcp-test-get-dynamic-binding-function-definition
    ()
  "Test 'get-function-definition' with lexical-binding: nil functions."
  (elisp-dev-mcp-test--with-server
    ;; Load the dynamic binding test file
    (require 'elisp-dev-mcp-dynamic-test)

    ;; Test get-function-definition with dynamic binding function.
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-dynamic-test--with-header-comment"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp))
           (start-line (assoc-default 'start-line parsed-resp))
           (end-line (assoc-default 'end-line parsed-resp)))

      ;; Verify file path is the dynamic binding file
      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-dynamic-test.el"))
      (should (= start-line 32))
      (should (= end-line 42))
      (should
       (string=
        source
        ";; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-dynamic-test--with-header-comment (arg1 arg2)
  \"Sample function with a header comment in dynamic binding context.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2.\"
  (+ arg1 arg2))")))))

(ert-deftest elisp-dev-mcp-test-describe-interactive-dynamic-function
    ()
  "Test 'describe-function' with interactively defined dynamic function."
  (elisp-dev-mcp-test--with-server
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-dynamic-func")
           (sym (intern test-function-name))
           (lexical-binding nil))
      (unwind-protect
          (progn
            (eval `(defun ,sym (x y)
                     "A dynamically scoped interactive function.
X and Y are dynamically scoped arguments."
                     (let ((z (+ x y)))
                       (* z 2)))
                  nil)

            (let ((text
                   (mcp-server-lib-ert-call-tool
                    "elisp-describe-function"
                    `((function . ,test-function-name)))))
              (should (string-match-p test-function-name text))
              (elisp-dev-mcp-test--check-dynamic-text text)
              (should
               (string-match-p "dynamically scoped interactive" text))
              (should
               (string-match-p
                (format "%s X Y)" test-function-name) text))))

        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-describe-variable ()
  "Test that `describe-variable' MCP handler works correctly."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable" '((variable . "load-path")))))
    ;; Basic checks for a well-known variable
    (should (string= (assoc-default 'name parsed) "load-path"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "cons"))
    (should (stringp (assoc-default 'documentation parsed)))))

(ert-deftest elisp-dev-mcp-test-describe-nonexistent-variable ()
  "Test that `describe-variable' MCP handler handles non-existent variables."
  (elisp-dev-mcp-test--with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "non-existent-variable-xyz"))))
           (resp
            (mcp-server-lib-process-jsonrpc-parsed
             req mcp-server-lib-ert-server-id)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Variable non-existent-variable-xyz is not bound"))))

(ert-deftest elisp-dev-mcp-test-describe-invalid-variable-type ()
  "Test that `describe-variable' handles non-string variable names properly."
  (elisp-dev-mcp-test--verify-invalid-type
   "elisp-describe-variable" 'variable))

(ert-deftest elisp-dev-mcp-test-describe-empty-string-variable ()
  "Test that `describe-variable' MCP handler handles empty string properly."
  (elisp-dev-mcp-test--verify-empty-name
   "elisp-describe-variable" 'variable))

(ert-deftest elisp-dev-mcp-test-describe-variable-no-docstring ()
  "Test `describe-variable' MCP handler with undocumented variables."
  ;; Create a variable without documentation
  (setq elisp-dev-mcp-test--undocumented-var 42)
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--undocumented-var")))))
    ;; Check the response
    (should
     (string=
      (assoc-default 'name parsed)
      "elisp-dev-mcp-test--undocumented-var"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "integer"))
    ;; Documentation should be null for undocumented variables
    (should (null (assoc-default 'documentation parsed)))
    ;; Should NOT be a custom variable
    (should (eq (assoc-default 'is-custom parsed) :json-false))))

(ert-deftest elisp-dev-mcp-test-describe-variable-empty-docstring ()
  "Test `describe-variable' MCP handler with empty docstring variables."
  (let
      ((parsed
        (elisp-dev-mcp-test--get-parsed-response
         "elisp-describe-variable"
         '((variable
            .
            "elisp-dev-mcp-no-checkdoc-test--empty-docstring-var")))))
    ;; Check the response
    (should
     (string=
      (assoc-default 'name parsed)
      "elisp-dev-mcp-no-checkdoc-test--empty-docstring-var"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "symbol"))
    ;; Empty docstring should be returned as empty string
    (should (string= (assoc-default 'documentation parsed) ""))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable ()
  "Test `describe-variable' MCP handler with custom variables."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--custom-var")))))
    ;; Check the response
    (should
     (string=
      (assoc-default 'name parsed) "elisp-dev-mcp-test--custom-var"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "string"))
    (should
     (string=
      (assoc-default 'documentation parsed)
      "A custom variable for testing."))
    ;; Should show it's in the test file
    (let ((source-file (assoc-default 'source-file parsed)))
      (should (stringp source-file))
      (should (string-match-p "elisp-dev-mcp-test\\.el" source-file)))
    ;; Should indicate it's a custom variable
    (should (eq (assoc-default 'is-custom parsed) t))))

(ert-deftest elisp-dev-mcp-test-describe-interactive-variable ()
  "Test `describe-variable' MCP handler with interactively defined variables."
  ;; Define a variable interactively (not from a file)
  (eval
   '(defvar elisp-dev-mcp-test--interactive-var 123
      "An interactively defined variable."))
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--interactive-var")))))
    ;; Check the response
    (should
     (string=
      (assoc-default 'name parsed)
      "elisp-dev-mcp-test--interactive-var"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "integer"))
    (should
     (string=
      (assoc-default 'documentation parsed)
      "An interactively defined variable."))
    (should
     (string=
      (assoc-default 'source-file parsed)
      "<interactively defined>"))))

(ert-deftest elisp-dev-mcp-test-describe-obsolete-variable ()
  "Test `describe-variable' MCP handler with obsolete variables."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--obsolete-var")))))
    ;; Check the response
    (should
     (string=
      (assoc-default 'name parsed)
      "elisp-dev-mcp-test--obsolete-var"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "string"))
    (should
     (string=
      (assoc-default 'documentation parsed)
      "An obsolete variable for testing."))
    ;; Should indicate it's obsolete
    (should (eq (assoc-default 'is-obsolete parsed) t))
    ;; Should include obsolete metadata
    (should (string= (assoc-default 'obsolete-since parsed) "1.0"))
    (should
     (string=
      (assoc-default 'obsolete-replacement parsed)
      "elisp-dev-mcp-test--new-var"))))

(ert-deftest elisp-dev-mcp-test-describe-unbound-documented-variable
    ()
  "Test `describe-variable' MCP handler with unbound but documented variables."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable
             . "elisp-dev-mcp-test--unbound-documented-var")))))
    (should
     (string=
      (assoc-default 'name parsed)
      "elisp-dev-mcp-test--unbound-documented-var"))
    (should (eq (assoc-default 'bound parsed) :json-false))
    (should-not (assoc-default 'value-type parsed))
    (should
     (string=
      (assoc-default 'documentation parsed)
      "A documented variable that will be unbound for testing."))
    (let ((source-file (assoc-default 'source-file parsed)))
      (should (stringp source-file))
      (should (string-match-p "elisp-dev-mcp-test\\.el" source-file)))
    (should (eq (assoc-default 'is-custom parsed) :json-false))
    (should (eq (assoc-default 'is-obsolete parsed) :json-false))))

(ert-deftest elisp-dev-mcp-test-describe-variable-alias ()
  "Test `describe-variable' MCP handler with variable aliases."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--a")))))
    (should
     (string= (assoc-default 'name parsed) "elisp-dev-mcp-test--a"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "string"))
    (should (string= (assoc-default 'documentation parsed) "x"))
    (should (eq (assoc-default 'is-alias parsed) t))
    (should
     (string=
      (assoc-default 'alias-target parsed) "elisp-dev-mcp-test--b"))))

(ert-deftest elisp-dev-mcp-test-describe-special-variable ()
  "Test `describe-variable' MCP handler with special variables."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable" '((variable . "load-path")))))
    (should (string= (assoc-default 'name parsed) "load-path"))
    (should (eq (assoc-default 'bound parsed) t))
    (should (string= (assoc-default 'value-type parsed) "cons"))
    (should (eq (assoc-default 'is-special parsed) t))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable-group ()
  "Test `describe-variable' returns custom group for defcustom variables."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--custom-var")))))
    (should (eq (assoc-default 'is-custom parsed) t))
    (should
     (string= (assoc-default 'custom-group parsed) "elisp-dev-mcp"))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable-type ()
  "Test `describe-variable' returns custom type for defcustom variables."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--custom-var")))))
    (should (eq (assoc-default 'is-custom parsed) t))
    (should (string= (assoc-default 'custom-type parsed) "string"))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable-complex-type
    ()
  "Test `describe-variable' returns complex custom type for defcustom vars."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-describe-variable"
          '((variable . "elisp-dev-mcp-test--custom-choice-var")))))
    (should (eq (assoc-default 'is-custom parsed) t))
    (let ((custom-type (assoc-default 'custom-type parsed)))
      (should (stringp custom-type))
      (should (string-match-p "choice" custom-type))
      (should (string-match-p "option1" custom-type))
      (should (string-match-p "option2" custom-type)))))

(ert-deftest elisp-dev-mcp-test-info-lookup-symbol ()
  "Test that `elisp-info-lookup-symbol' MCP handler works correctly."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-info-lookup-symbol" '((symbol . "defun")))))
    (should (eq (assoc-default 'found parsed) t))
    (should (string= (assoc-default 'symbol parsed) "defun"))
    (should (stringp (assoc-default 'node parsed)))
    (should (string= (assoc-default 'manual parsed) "elisp"))
    (should (stringp (assoc-default 'content parsed)))
    (should (string-match-p "defun" (assoc-default 'content parsed)))
    (should (stringp (assoc-default 'info-ref parsed)))
    (should
     (string-match-p "(elisp)" (assoc-default 'info-ref parsed)))))

(ert-deftest elisp-dev-mcp-test-info-lookup-nonexistent-symbol ()
  "Test that `elisp-info-lookup-symbol' handles non-existent symbols."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-info-lookup-symbol"
          '((symbol . "non-existent-symbol-xyz")))))
    (should (eq (assoc-default 'found parsed) :json-false))
    (should
     (string=
      (assoc-default 'symbol parsed) "non-existent-symbol-xyz"))
    (should (stringp (assoc-default 'message parsed)))
    (should
     (string-match-p "not found" (assoc-default 'message parsed)))))

(ert-deftest elisp-dev-mcp-test-info-lookup-empty-string ()
  "Test that `elisp-info-lookup-symbol' handles empty string properly."
  (elisp-dev-mcp-test--verify-empty-name
   "elisp-info-lookup-symbol" 'symbol))

(ert-deftest elisp-dev-mcp-test-info-lookup-invalid-type ()
  "Test that `elisp-info-lookup-symbol' handles non-string symbols."
  (elisp-dev-mcp-test--verify-invalid-type
   "elisp-info-lookup-symbol" 'symbol))

(ert-deftest elisp-dev-mcp-test-info-lookup-function ()
  "Test `elisp-info-lookup-symbol' with a well-known function."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-info-lookup-symbol" '((symbol . "mapcar")))))
    (should (eq (assoc-default 'found parsed) t))
    (should (string= (assoc-default 'symbol parsed) "mapcar"))
    (should (stringp (assoc-default 'node parsed)))
    (let ((content (assoc-default 'content parsed)))
      (should (stringp content))
      (should (string-match-p "mapcar" content))
      (should (string-match-p "FUNCTION" content))
      (should (string-match-p "SEQUENCE" content)))))

(ert-deftest elisp-dev-mcp-test-info-lookup-special-form ()
  "Test `elisp-info-lookup-symbol' with a special form."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-info-lookup-symbol" '((symbol . "let")))))
    (should (eq (assoc-default 'found parsed) t))
    (should (string= (assoc-default 'symbol parsed) "let"))
    (let ((content (assoc-default 'content parsed)))
      (should (stringp content))
      (should (string-match-p "let" content))
      (should (string-match-p "binding" content)))))

(ert-deftest elisp-dev-mcp-test-info-lookup-variable ()
  "Test `elisp-info-lookup-symbol' with a variable."
  (let ((parsed
         (elisp-dev-mcp-test--get-parsed-response
          "elisp-info-lookup-symbol" '((symbol . "load-path")))))
    (should (eq (assoc-default 'found parsed) t))
    (should (string= (assoc-default 'symbol parsed) "load-path"))
    (let ((content (assoc-default 'content parsed)))
      (should (stringp content))
      (should (string-match-p "load-path" content))
      (should (string-match-p "directories" content)))))

(ert-deftest elisp-dev-mcp-test-read-source-file-elpa ()
  "Test that `elisp-read-source-file` can read installed ELPA package files."
  ;; Test reading mcp-server-lib main file - it's a mandatory dependency.
  (let* ((mcp-dir
          (car
           (directory-files
            (expand-file-name "elpa/" user-emacs-directory)
            t "^mcp-server-lib\\(-\\|$\\)")))
         (elpa-path (expand-file-name "mcp-server-lib.el" mcp-dir))
         (text (elisp-dev-mcp-test--read-source-file elpa-path)))
    (elisp-dev-mcp-test--verify-mcp-server-lib-source text)))

(ert-deftest elisp-dev-mcp-test-read-source-file-system ()
  "Test that `elisp-read-source-file` can read Emacs system files."
  ;; Test reading a system file that should exist in all Emacs installations.
  (let*
      ((located (locate-library "subr"))
       ;; Handle both .el and .el.gz files by stripping extensions properly
       (base-path
        (if (string-suffix-p ".gz" located)
            ;; For .el.gz or .elc.gz: strip .gz, then strip .el/.elc
            (file-name-sans-extension
             (file-name-sans-extension located))
          ;; For .el or .elc: just strip the extension
          (file-name-sans-extension located)))
       ;; Check which file actually exists (.el or .el.gz)
       (el-path (concat base-path ".el"))
       (el-gz-path (concat base-path ".el.gz"))
       (system-file
        (cond
         ((file-exists-p el-path)
          el-path)
         ((file-exists-p el-gz-path)
          el-gz-path)
         (t
          (error "Neither %s nor %s exists" el-path el-gz-path))))
       (text (elisp-dev-mcp-test--read-source-file system-file)))
    ;; Should contain typical Emacs system file content
    ;; (works with both compressed .el.gz and uncompressed .el files)
    (should (string-match-p ";;; subr.el" text))
    (should (string-match-p "GNU Emacs" text))))

(ert-deftest
    elisp-dev-mcp-test-read-source-file-by-library-name-system
    ()
  "Test that `elisp-read-source-file` can read system library by name."
  ;; Test reading a system library by name (not absolute path).
  (let ((text (elisp-dev-mcp-test--read-source-file "subr")))
    ;; Should contain the subr.el file header
    (should (string-match-p ";;; subr.el" text))
    ;; Should contain typical system library content
    (should (string-match-p "GNU Emacs" text))))

(ert-deftest elisp-dev-mcp-test-read-source-file-by-library-name-elpa
    ()
  "Test that `elisp-read-source-file` can read ELPA library by name."
  ;; Test reading an ELPA package by library name.
  ;; In test environments, we need to ensure locate-library finds the package
  ;; in the test package directory, not in the user's global emacs directory.
  ;; We do this by temporarily modifying load-path.
  (let* ((test-pkg-dir
          (expand-file-name "elpa/" user-emacs-directory))
         ;; Save original load-path
         (original-load-path load-path)
         ;; Build new load-path with only test and system directories
         (test-load-path
          (append
           ;; Add all subdirectories of test package directory
           (when (file-directory-p test-pkg-dir)
             (directory-files test-pkg-dir t "^[^.]"))
           ;; Keep system directories
           (cl-remove-if
            (lambda (dir)
              (and (stringp dir)
                   (string-prefix-p
                    (expand-file-name "~/.emacs.d/") dir)))
            load-path))))
    (unwind-protect
        (progn
          (setq load-path test-load-path)
          (let ((text
                 (elisp-dev-mcp-test--read-source-file
                  "mcp-server-lib")))
            (elisp-dev-mcp-test--verify-mcp-server-lib-source text)))
      ;; Restore original load-path
      (setq load-path original-load-path))))

(ert-deftest elisp-dev-mcp-test-read-source-file-library-not-found ()
  "Test that `elisp-read-source-file` handles nonexistent libraries."
  ;; Test that a nonexistent library name produces an appropriate error.
  (elisp-dev-mcp-test--verify-read-source-file-fails
   "nonexistent-library-xyz-12345"
   "Library not found: nonexistent-library-xyz-12345"))

(ert-deftest
    elisp-dev-mcp-test-read-source-file-by-library-name-elc-gz
    ()
  "Test that `elisp-read-source-file` handles .elc.gz compressed bytecode."
  ;; Test reading a library when locate-library returns .elc.gz.
  ;; This tests the fix for CR-001: proper handling of .elc.gz files.
  ;; The system should correctly resolve .elc.gz -> .el and find the source.
  (elisp-dev-mcp-test--with-temp-dir temp-dir
      "elisp-dev-mcp-elc-gz-test"
    (let* ((lib-name "test-elc-gz-lib")
           (el-file
            (expand-file-name (concat lib-name ".el") temp-dir))
           (elc-file
            (expand-file-name (concat lib-name ".elc") temp-dir))
           (elc-gz-file (concat elc-file ".gz"))
           (test-content
            (concat
             ";;; test-elc-gz-lib.el --- Test library\n"
             "(defun test-func () \"test\")\n"
             "(provide 'test-elc-gz-lib)\n"
             ";;; test-elc-gz-lib.el ends here\n"))
           (original-load-path load-path))
      ;; Create the source file
      (with-temp-file el-file
        (insert test-content))
      ;; Byte-compile it
      (byte-compile-file el-file)
      ;; Compress the .elc file to .elc.gz
      (call-process "gzip" nil nil nil "-f" elc-file)
      ;; Verify both .el and .elc.gz exist (mimics real package structure)
      (should (file-exists-p el-file))
      (should (file-exists-p elc-gz-file))
      (should-not (file-exists-p elc-file))
      ;; Add temp dir to load-path and try to read by library name
      (unwind-protect
          (progn
            (setq load-path (cons temp-dir load-path))
            ;; locate-library should find the .elc.gz file (prefers bytecode)
            (let ((located (locate-library lib-name)))
              (should located)
              (should (string-suffix-p ".elc.gz" located)))
            ;; Reading by library name should correctly resolve to .el source
            ;; This verifies CR-001 fix: .elc.gz -> .elc -> .el conversion works
            (let ((elisp-dev-mcp-additional-allowed-dirs
                   (list temp-dir)))
              (let ((text
                     (elisp-dev-mcp-test--read-source-file lib-name)))
                (should (string-match-p "test-elc-gz-lib.el" text))
                (should (string-match-p "test-func" text)))))
        ;; Restore original load-path
        (setq load-path original-load-path)))))

(ert-deftest elisp-dev-mcp-test-read-source-file-invalid-format ()
  "Test that `elisp-read-source-file` rejects invalid formats."
  ;; Test relative path.
  (let ((test-path "relative/path.el")
        (error-pattern
         "Invalid path format: must be absolute path ending in .el"))
    (elisp-dev-mcp-test--verify-read-source-file-fails
     test-path error-pattern))

  ;; Test path not ending in .el.
  (let ((error-pattern
         "Invalid path format: must be absolute path ending in .el"))
    (elisp-dev-mcp-test--verify-read-source-file-fails
     "/absolute/path/file.elc" error-pattern))

  ;; Test path with .. traversal.
  (let ((error-pattern "Path contains illegal '..' traversal"))
    (elisp-dev-mcp-test--verify-read-source-file-fails
     "/some/path/../../../etc/passwd.el" error-pattern)))

(ert-deftest elisp-dev-mcp-test-read-source-file-not-found ()
  "Test that `elisp-read-source-file` handles missing files gracefully."
  ;; Test non-existent file in a valid package directory.
  (let* ((test-package-dir
          (expand-file-name "test-package-1.0/" package-user-dir))
         (non-existent-path
          (expand-file-name "non-existent-file.el" test-package-dir))
         (error-pattern "File not found:"))
    (elisp-dev-mcp-test--verify-read-source-file-fails
     non-existent-path error-pattern)))

(ert-deftest elisp-dev-mcp-test-read-source-file-security ()
  "Test that `elisp-read-source-file` enforces security restrictions."
  ;; Test access outside allowed directories.
  (let ((error-pattern
         "Access denied: path outside allowed directories"))
    (elisp-dev-mcp-test--verify-read-source-file-fails
     "/etc/passwd.el" error-pattern)))

(ert-deftest elisp-dev-mcp-test-additional-allowed-dirs-default ()
  "Test that `elisp-dev-mcp-additional-allowed-dirs` has correct default value."
  (should (null elisp-dev-mcp-additional-allowed-dirs)))

(ert-deftest elisp-dev-mcp-test-read-source-file-additional-dirs ()
  "Test that `elisp-read-source-file` respects additional allowed directories."
  (elisp-dev-mcp-test--with-temp-dir temp-dir "elisp-dev-mcp-test"
    (let* ((test-file (expand-file-name "test-package.el" temp-dir))
           (test-content
            (concat
             ";;; test-package.el --- Test package\n"
             "(provide 'test-package)\n"
             ";;; test-package.el ends here\n")))
      ;; Create test file
      (with-temp-file test-file
        (insert test-content))
      ;; First verify access is denied without configuration
      (let ((elisp-dev-mcp-additional-allowed-dirs nil))
        (elisp-dev-mcp-test--verify-read-source-file-fails
         test-file "Access denied: path outside allowed directories"))
      ;; Now add the directory to allowed list and verify access works
      (let ((elisp-dev-mcp-additional-allowed-dirs (list temp-dir)))
        (let ((content
               (elisp-dev-mcp-test--read-source-file test-file)))
          (should (string= content test-content)))))))

(ert-deftest
    elisp-dev-mcp-test-read-source-file-additional-dirs-security
    ()
  "Test that additional directories don't compromise security."
  (elisp-dev-mcp-test--with-temp-dir temp-dir "elisp-dev-mcp-test"
    (let ((allowed-file (expand-file-name "allowed.el" temp-dir))
          (forbidden-file "/etc/passwd.el"))
      ;; Create test file in allowed directory
      (with-temp-file allowed-file
        (insert
         ";;; allowed.el --- Allowed file\n(provide 'allowed)\n"))
      ;; Configure additional directory
      (let ((elisp-dev-mcp-additional-allowed-dirs (list temp-dir)))
        ;; Should be able to read allowed file
        (let ((content
               (elisp-dev-mcp-test--read-source-file allowed-file)))
          (should (string-match-p "allowed.el" content)))
        ;; Should still be denied access to system files
        (elisp-dev-mcp-test--verify-read-source-file-fails
         forbidden-file
         "Access denied: path outside allowed directories")))))

(ert-deftest
    elisp-dev-mcp-test-read-source-file-multiple-additional-dirs
    ()
  "Test that multiple additional directories work correctly."
  (elisp-dev-mcp-test--with-temp-dir temp-dir1 "elisp-dev-mcp-test1"
    (elisp-dev-mcp-test--with-temp-dir temp-dir2 "elisp-dev-mcp-test2"
      (let ((test-file1 (expand-file-name "package1.el" temp-dir1))
            (test-file2 (expand-file-name "package2.el" temp-dir2)))
        ;; Create test files
        (with-temp-file test-file1
          (insert
           ";;; package1.el --- Package 1\n(provide 'package1)\n"))
        (with-temp-file test-file2
          (insert
           ";;; package2.el --- Package 2\n(provide 'package2)\n"))
        ;; Configure multiple additional directories
        (let ((elisp-dev-mcp-additional-allowed-dirs
               (list temp-dir1 temp-dir2)))
          ;; Should be able to read from both directories
          (let ((content1
                 (elisp-dev-mcp-test--read-source-file test-file1))
                (content2
                 (elisp-dev-mcp-test--read-source-file test-file2)))
            (should (string-match-p "package1.el" content1))
            (should (string-match-p "package2.el" content2))))))))

(ert-deftest
    elisp-dev-mcp-test-read-source-file-additional-dirs-normalization
    ()
  "Test additional directories are normalized via `file-truename'."
  (elisp-dev-mcp-test--with-temp-dir temp-dir "elisp-dev-mcp-test"
    (let ((test-file (expand-file-name "normalize-test.el" temp-dir))
          ;; Create a path without trailing slash
          (dir-without-slash (directory-file-name temp-dir)))
      ;; Create test file
      (with-temp-file test-file
        (insert
         (concat
          ";;; normalize-test.el --- Normalization test\n"
          "(provide 'normalize-test)\n")))
      ;; Configure directory without trailing slash
      (let ((elisp-dev-mcp-additional-allowed-dirs
             (list dir-without-slash)))
        ;; Should still be able to read the file: path normalization should
        ;; work without the trailing slash.
        (let ((content
               (elisp-dev-mcp-test--read-source-file test-file)))
          (should (string-match-p "normalize-test.el" content)))))))

(ert-deftest elisp-dev-mcp-test-describe-bytecode-function ()
  "Test `describe-function' with byte-compiled functions."
  (let ((text
         (elisp-dev-mcp-test--with-bytecode-describe-function
          "elisp-dev-mcp-bytecode-test--with-header")))
    (should
     (string-match-p "elisp-dev-mcp-bytecode-test--with-header" text))
    (should (string-match-p "byte-compiled" text))
    (should
     (string-match-p
      "A byte-compiled function with header comment" text))
    (should
     (string-match-p "elisp-dev-mcp-bytecode-test\\.el" text))))

(ert-deftest
    elisp-dev-mcp-test-get-bytecode-function-definition-with-header
    ()
  "Test `get-function-definition' with byte-compiled function with header."
  (let* ((fields
          (elisp-dev-mcp-test--definition-fields
           (elisp-dev-mcp-test--with-bytecode-get-definition
            "elisp-dev-mcp-bytecode-test--with-header")
           "elisp-dev-mcp-bytecode-test.el"))
         (source (nth 0 fields))
         (start-line (nth 1 fields))
         (end-line (nth 2 fields)))
    (should (= start-line 33))
    (should (= end-line 38))
    (should
     (string-match-p
      ";; Header comment for byte-compiled function" source))
    (should
     (string-match-p
      ";; This should be preserved in the function definition"
      source))
    (should
     (string-match-p
      "defun elisp-dev-mcp-bytecode-test--with-header" source))))

(ert-deftest
    elisp-dev-mcp-test-get-bytecode-function-definition-no-docstring
    ()
  "Test `get-function-definition' with byte-compiled function w/o docstring."
  (elisp-dev-mcp-test--with-bytecode-file
    (elisp-dev-mcp-test--with-server
      (let* ((parsed-resp
              (elisp-dev-mcp-test--get-definition-response-data
               "elisp-dev-mcp-bytecode-test--no-docstring"))
             (source (assoc-default 'source parsed-resp)))

        (should
         (string=
          source
          (concat
           "(defun elisp-dev-mcp-bytecode-test--no-docstring (a b)\n"
           "  (* a b))")))))))

(ert-deftest elisp-dev-mcp-test-get-bytecode-function-empty-docstring
    ()
  "Test `get-function-definition' with byte-compiled empty docstring function."
  (let* ((parsed-resp
          (elisp-dev-mcp-test--with-bytecode-get-definition
           "elisp-dev-mcp-bytecode-test--empty-docstring"))
         (source (assoc-default 'source parsed-resp)))

    (should
     (string=
      source
      (concat
       "(defun elisp-dev-mcp-bytecode-test--empty-docstring (n)\n"
       "  \"\"\n"
       "  (* n 2))")))))

;;; Meta tests

(defconst elisp-dev-mcp-test--repo-root
  (file-name-directory (locate-library "elisp-dev-mcp-test"))
  "Repository root directory, for meta tests that read project files by name.")

(defun elisp-dev-mcp-test--read-repo-file (relative-name)
  "Return the contents of RELATIVE-NAME under the repository root."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative-name elisp-dev-mcp-test--repo-root))
    (buffer-string)))

(ert-deftest elisp-dev-mcp-test-meta-version-strings-agree ()
  "Test that the package version is consistent across all sites.
The `Eask' file's `(package ...)' form, `elisp-dev-mcp.el's
`;; Version:' header, the `:version' reported to MCP clients by the
server registration, and `NEWS' top-section heading must all report
the same version; drift between them leads to MELPA/Eask metadata
inconsistencies at release time, to a wrong `serverInfo.version' over
the protocol, and to changelog/release-state confusion."
  (let* ((eask-content (elisp-dev-mcp-test--read-repo-file "Eask"))
         (el-content
          (elisp-dev-mcp-test--read-repo-file "elisp-dev-mcp.el"))
         (news-content (elisp-dev-mcp-test--read-repo-file "NEWS"))
         (eask-version
          (and (string-match
                "(package[ \t\n]+\"[^\"]+\"[ \t\n]+\"\\([^\"]+\\)\""
                eask-content)
               (match-string 1 eask-content)))
         (el-version
          (and (string-match
                "^;;[ \t]+Version:[ \t]+\\(\\S-+\\)" el-content)
               (match-string 1 el-content)))
         (reg-version
          (and (string-match
                "^[ \t]+:version[ \t]+\"\\([^\"]+\\)\"" el-content)
               (match-string 1 el-content)))
         (news-version
          (and (string-match
                "^\\* elisp-dev-mcp \\(\\S-+\\)" news-content)
               (match-string 1 news-content))))
    (should (stringp eask-version))
    (should (stringp el-version))
    (should (stringp reg-version))
    (should (stringp news-version))
    (should (equal eask-version el-version))
    (should (equal eask-version reg-version))
    (should (equal eask-version news-version))))

(provide 'elisp-dev-mcp-test)
;;; elisp-dev-mcp-test.el ends here
