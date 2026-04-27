;;; elisp-dev-mcp.el --- MCP server for agentic Elisp development -*- lexical-binding: t -*-

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
;; Version: 1.2.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.2.0"))
;; Keywords: tools, development
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; This package provides an MCP server for agentic Elisp development.

;;; Code:

(require 'mcp-server-lib)
(require 'help-fns)
(require 'pp)
(require 'info-look)
(require 'cl-lib)


;;; System Directory Setup

(defvar elisp-dev-mcp--system-lisp-dir
  (let* ((data-parent
          (file-name-directory (directory-file-name data-directory)))
         (lisp-dir (expand-file-name "lisp/" data-parent)))
    (when (file-directory-p lisp-dir)
      lisp-dir))
  "System Lisp directory for Emacs installation.
Computed once at package load time from `data-directory'.")

(defconst elisp-dev-mcp--server-id "elisp-dev-mcp"
  "Server ID for this MCP server.")

(defgroup elisp-dev-mcp nil
  "MCP server for agentic Elisp development."
  :group 'tools
  :prefix "elisp-dev-mcp-")

(defcustom elisp-dev-mcp-additional-allowed-dirs nil
  "Additional directories to allow for elisp-read-source-file.
List of directory paths that should be accessible in addition to
the default Emacs system and ELPA directories.

This is useful for users of alternative package managers like
straight.el, elpaca, or custom package setups.

Example:
  (setq elisp-dev-mcp-additional-allowed-dirs
        \\='(\"~/.emacs.d/straight/build/\"
           \"~/.emacs.d/straight/repos/\"
           \"~/my-elisp-packages/\"))

Security note: Only add directories you trust, as this allows
the MCP server to read any .el files in these locations."
  :type '(repeat directory)
  :group 'elisp-dev-mcp
  :safe (lambda (val) (and (listp val) (cl-every #'stringp val))))

;;; Utility Functions

(defun elisp-dev-mcp--non-empty-docstring-p (doc)
  "Return t if DOC is a non-empty documentation string, nil otherwise."
  (and doc (not (string-empty-p doc))))

(defmacro elisp-dev-mcp--with-auto-compression (&rest body)
  "Execute BODY with `auto-compression-mode' temporarily enabled.
Restores the original mode state after BODY completes."
  (declare (indent 0) (debug t))
  `(let ((elisp-dev-mcp--was-enabled auto-compression-mode))
     (unwind-protect
         (progn
           (unless elisp-dev-mcp--was-enabled
             (auto-compression-mode 1))
           ,@body)
       (unless elisp-dev-mcp--was-enabled
         (auto-compression-mode -1)))))

;;; JSON Response Helpers

(defun elisp-dev-mcp--json-encode-source-location
    (source file-path start-line end-line)
  "Encode a source location response as JSON.
SOURCE is the source code string.
FILE-PATH is the absolute path to the source file.
START-LINE and END-LINE are 1-based line numbers."
  (json-encode
   `((source . ,source)
     (file-path . ,file-path)
     (start-line . ,start-line)
     (end-line . ,end-line))))

(defun elisp-dev-mcp--json-encode-not-found (symbol message)
  "Encode a not-found response as JSON.
SYMBOL is the symbol that was looked up.
MESSAGE is the error or not-found message."
  (json-encode
   `((found . :json-false) (symbol . ,symbol) (message . ,message))))

(defun elisp-dev-mcp--validate-symbol (name type &optional intern-p)
  "Validate that NAME is a non-empty string suitable for a symbol.
TYPE is a string describing the symbol type for error messages.
If INTERN-P is non-nil, return the interned symbol, otherwise just validate.
Throws an error if validation fails."
  (unless (stringp name)
    (mcp-server-lib-tool-throw (format "Invalid %s name" type)))
  (when (string-empty-p name)
    (mcp-server-lib-tool-throw (format "Empty %s name" type)))
  (when intern-p
    (intern name)))

;;; Property Collection

(defun elisp-dev-mcp--extract-function-properties (sym)
  "Extract all properties for function symbol SYM.
Returns an alist of properties or nil if not a function."
  (when (fboundp sym)
    (let* ((fn (symbol-function sym))
           (is-alias (symbolp fn))
           (aliased-to (and is-alias fn)))
      `((function . ,fn)
        (is-alias . ,is-alias)
        (aliased-to . ,aliased-to)
        (is-subr
         .
         ,(subrp
           (if is-alias
               aliased-to
             fn)))
        (doc . ,(documentation sym))
        (file . ,(find-lisp-object-file-name sym 'defun))))))

(defun elisp-dev-mcp--json-bool (value)
  "Convert Elisp boolean VALUE to JSON boolean representation.
In Elisp, nil is false and everything else is true.
For JSON encoding, returns t for truthy values and :json-false for nil.
This ensures proper JSON boolean serialization."
  (if value
      t
    :json-false))

(defun elisp-dev-mcp--variable-exists-p (props)
  "Check if variable exists based on its PROPS.
A variable exists if it is bound, documented, defined in a file,
is a custom variable, is obsolete, or is an alias."
  (or (alist-get 'bound-p props)
      (alist-get 'doc props)
      (alist-get 'file props)
      (alist-get 'custom-p props)
      (alist-get 'obsolete props)
      (alist-get 'is-alias props)))

;;; Tool Implementations

(defun elisp-dev-mcp--describe-function (function)
  "Get full documentation for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to describe"
  (mcp-server-lib-with-error-handling
   (let ((sym (elisp-dev-mcp--validate-symbol function "function" t)))
     (if (fboundp sym)
         (with-temp-buffer
           (let ((standard-output (current-buffer)))
             (describe-function-1 sym)
             (buffer-string)))
       (mcp-server-lib-tool-throw
        (format "Function %s is void" function))))))

;;; Function Definition Helpers

(defun elisp-dev-mcp--process-alias-source
    (source function aliased-to file-path start-line end-line)
  "Post-process SOURCE for function aliases to return a useful defalias form.
If source is just a quoted symbol, replace it with a synthetic defalias form.
Returns a JSON encoded response with enhanced alias information.

FUNCTION is the alias function name.
ALIASED-TO is the target function name.
FILE-PATH, START-LINE, and END-LINE specify source location information."
  (let ((doc (or (documentation (intern-soft function)) "")))
    (if (and source
             (string-match-p (format "['']%s\\>" function) source)
             (not (string-match-p "defalias" source)))
        ;; Generate synthetic defalias form
        (let ((func-def
               (format "(defalias '%s #'%s %S)"
                       function
                       aliased-to
                       doc)))
          (elisp-dev-mcp--json-encode-source-location
           func-def file-path start-line end-line))
      ;; Pass through original source
      (elisp-dev-mcp--json-encode-source-location
       source file-path start-line end-line))))

(defun elisp-dev-mcp--get-function-definition-c-function (fn-name)
  "Return response for C-implemented FN-NAME in get-function-definition."
  (json-encode
   `((is-c-function . t)
     (function-name . ,fn-name)
     (message .
              ,(format
                "Function `%s` is implemented in C source code. \
Use elisp-describe-function tool to get its docstring."
                fn-name)))))

(defun elisp-dev-mcp--extract-function-body (fn has-doc)
  "Extract body from function object FN.
HAS-DOC indicates whether the function has a docstring.
Returns nil if FN is not a function."
  (if (not (functionp fn))
      nil
    (cond
     ;; Emacs 30+ interpreted-function objects
     ((eq (type-of fn) 'interpreted-function)
      ;; Extract body from interpreted-function
      ;; Format: #[args body env bytecode doc]
      (aref fn 1))
     ;; Emacs 29 and earlier cons-based functions
     ((consp fn)
      ;; Function format: (closure ENV ARGS [DOCSTRING] . BODY)
      ;; or: (lambda ARGS [DOCSTRING] . BODY)
      ;; Skip: car (closure/lambda), cadr (env/args), caddr (args/docstring)
      ;; If has docstring, body starts at position 3 (0-indexed)
      ;; If no docstring, body starts at position 2 (0-indexed)
      (nthcdr
       (if has-doc
           3 ; Skip closure/lambda, env/args, and docstring
         2) ; Skip closure/lambda and args only
       fn))
     ;; Fallback for other types
     (t
      (mcp-server-lib-tool-throw
       (format "Don't know how to extract body from function type: %s"
               (type-of fn)))))))

(defun elisp-dev-mcp--reconstruct-function-definition
    (fn-name args doc body)
  "Reconstruct a function definition from its runtime components.
This is used for interactively defined functions where the source file
is not available.  Creates a synthetic defun form.

FN-NAME is the function name as a string.
ARGS is the argument list.
DOC is the documentation string (can be empty).
BODY is the list of body expressions."
  (unless body
    (mcp-server-lib-tool-throw
     (format "Failed to extract body for function %s" fn-name)))
  (let ((defun-form
         `(defun ,(intern fn-name) ,(or args '())
            ,@
            (when (elisp-dev-mcp--non-empty-docstring-p doc)
              (list doc))
            ,@body)))
    (pp-to-string defun-form)))

(defun elisp-dev-mcp--get-function-definition-interactive
    (fn-name sym fn)
  "Handle interactively defined function FN-NAME.
SYM is the function symbol, FN is the function object.
Returns JSON response for an interactively defined function."
  (let* ((args (help-function-arglist sym t))
         (doc (documentation sym))
         (body
          (elisp-dev-mcp--extract-function-body
           fn (elisp-dev-mcp--non-empty-docstring-p doc)))
         (func-def
          (elisp-dev-mcp--reconstruct-function-definition
           fn-name args doc body)))
    (elisp-dev-mcp--json-encode-source-location
     func-def "<interactively defined>" 1 1)))

;;; Variable Helpers

(defun elisp-dev-mcp--find-custom-group (sym)
  "Find the custom group that contain variable SYM.
Returns the group name as a string, or nil if not found."
  (catch 'found
    (mapatoms
     (lambda (group-sym)
       (when (get group-sym 'custom-group)
         (dolist (member (get group-sym 'custom-group))
           (when (and (eq (car member) sym)
                      (eq (cadr member) 'custom-variable))
             (throw 'found (symbol-name group-sym)))))))
    nil))

(defun elisp-dev-mcp--find-header-comment-start (point)
  "Find the start of header comments preceding POINT.
Returns the position of the first comment line, or POINT if no comments found."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (forward-line -1)

    ;; Check if there's a header comment
    (if (looking-at "^[ \t]*;;")
        (progn
          ;; Find first line of the consecutive comment block
          (while (and (looking-at "^[ \t]*;;")
                      (> (forward-line -1) -1)))
          ;; We went one line too far back
          (forward-line 1)
          (point))
      point)))

(defun elisp-dev-mcp--extract-source-region (start-point end-point)
  "Extract source code between START-POINT and END-POINT.
Returns a list of (source start-line end-line)."
  (list
   (buffer-substring-no-properties start-point end-point)
   (line-number-at-pos start-point)
   (line-number-at-pos end-point)))

(defun elisp-dev-mcp--extract-variable-properties (sym)
  "Extract all properties for variable symbol SYM.
Returns an alist of properties."
  (let* ((doc (documentation-property sym 'variable-documentation))
         (file (find-lisp-object-file-name sym 'defvar))
         (custom-p (custom-variable-p sym))
         (obsolete (get sym 'byte-obsolete-variable))
         (bound-p (boundp sym))
         (alias-target (indirect-variable sym))
         (is-alias (not (eq sym alias-target)))
         (is-special (special-variable-p sym))
         (custom-group
          (when custom-p
            (elisp-dev-mcp--find-custom-group sym)))
         (custom-type
          (when custom-p
            (get sym 'custom-type))))
    `((doc . ,doc)
      (file . ,file)
      (custom-p . ,custom-p)
      (obsolete . ,obsolete)
      (bound-p . ,bound-p)
      (alias-target . ,alias-target)
      (is-alias . ,is-alias)
      (is-special . ,is-special)
      (custom-group . ,custom-group)
      (custom-type . ,custom-type))))

(defun elisp-dev-mcp--build-variable-json-response (variable props)
  "Build JSON response for VARIABLE using collected PROPS.
VARIABLE is the variable name string, PROPS is an alist of properties."
  (let ((bound-p (alist-get 'bound-p props))
        (doc (alist-get 'doc props))
        (file (alist-get 'file props))
        (custom-p (alist-get 'custom-p props))
        (obsolete (alist-get 'obsolete props))
        (is-alias (alist-get 'is-alias props))
        (alias-target (alist-get 'alias-target props))
        (is-special (alist-get 'is-special props))
        (custom-group (alist-get 'custom-group props))
        (custom-type (alist-get 'custom-type props)))
    (json-encode
     `((name . ,variable)
       (bound . ,(elisp-dev-mcp--json-bool bound-p))
       ,@
       (when bound-p
         `((value-type
            .
            ,(symbol-name
              (type-of (symbol-value (intern variable)))))))
       (documentation . ,doc)
       (source-file . ,(or file "<interactively defined>"))
       (is-custom . ,(elisp-dev-mcp--json-bool custom-p))
       ,@
       (when custom-group
         `((custom-group . ,custom-group)))
       ,@
       (when custom-type
         `((custom-type . ,(format "%S" custom-type))))
       (is-obsolete . ,(elisp-dev-mcp--json-bool obsolete))
       (is-alias . ,(elisp-dev-mcp--json-bool is-alias))
       ,@
       (when obsolete
         `((obsolete-since . ,(nth 2 obsolete))
           (obsolete-replacement . ,(nth 0 obsolete))))
       (is-special . ,(elisp-dev-mcp--json-bool is-special))
       ,@
       (when is-alias
         `((alias-target . ,(symbol-name alias-target))))))))

(defun elisp-dev-mcp--describe-variable (variable)
  "Get information about Emacs Lisp VARIABLE without exposing its value.

MCP Parameters:
  variable - The name of the variable to describe"
  (let* ((sym (elisp-dev-mcp--validate-symbol variable "variable" t))
         (props (elisp-dev-mcp--extract-variable-properties sym)))
    (if (elisp-dev-mcp--variable-exists-p props)
        (elisp-dev-mcp--build-variable-json-response variable props)
      (mcp-server-lib-tool-throw
       (format "Variable %s is not bound" variable)))))

;;; File-based Function Extraction

(defun elisp-dev-mcp--get-function-definition-from-file
    (fn-name sym func-file is-alias aliased-to)
  "Extract function definition for FN-NAME from FUNC-FILE.
SYM is the function symbol.
IS-ALIAS and ALIASED-TO are used for special handling of aliases."
  (elisp-dev-mcp--with-auto-compression
    (let ((actual-file
           (cond
            ((file-exists-p func-file)
             func-file)
            ((file-exists-p (concat func-file ".gz"))
             (concat func-file ".gz"))
            (t
             (mcp-server-lib-tool-throw
              (format "File not found: %s (tried .el and .el.gz)"
                      func-file))))))
      (with-temp-buffer
        (insert-file-contents actual-file)
        (goto-char (point-min))
        (let ((def-pos
               (find-function-search-for-symbol sym nil func-file)))
          (unless def-pos
            (mcp-server-lib-tool-throw
             (format "Could not locate definition for %s" fn-name)))
          (goto-char (cdr def-pos))

          ;; Find the start point including any header comments
          (let* ((func-point (point))
                 (start-point
                  (elisp-dev-mcp--find-header-comment-start
                   func-point))
                 (end-point
                  (progn
                    (goto-char func-point)
                    (forward-sexp)
                    (point)))
                 (source-info
                  (elisp-dev-mcp--extract-source-region
                   start-point end-point)))

            ;; Return the result, with special handling for aliases
            (if is-alias
                (elisp-dev-mcp--process-alias-source
                 (nth 0 source-info)
                 fn-name
                 aliased-to
                 func-file
                 (nth 1 source-info)
                 (nth 2 source-info))
              (elisp-dev-mcp--json-encode-source-location
               (nth 0 source-info)
               func-file
               (nth 1 source-info)
               (nth 2 source-info)))))))))

(defun elisp-dev-mcp--extract-function-info (sym)
  "Extract function information for symbol SYM.
Returns (fn is-alias aliased-to) or nil if not a function."
  (when (fboundp sym)
    (let* ((fn (symbol-function sym))
           (is-alias (symbolp fn))
           (aliased-to (and is-alias (symbol-name fn))))
      (list fn is-alias aliased-to))))

(defun elisp-dev-mcp--get-function-definition-dispatch
    (function sym fn-info)
  "Dispatch to appropriate handler based on function type.
FUNCTION is the function name string.
SYM is the function symbol.
FN-INFO is the result from `elisp-dev-mcp--extract-function-info`."
  (let ((fn (nth 0 fn-info))
        (is-alias (nth 1 fn-info))
        (aliased-to (nth 2 fn-info)))
    (cond
     ;; C-implemented function
     ((subrp fn)
      (elisp-dev-mcp--get-function-definition-c-function function))

     ;; Has source file
     ((find-lisp-object-file-name sym 'defun)
      (elisp-dev-mcp--get-function-definition-from-file
       function
       sym
       (find-lisp-object-file-name sym 'defun)
       is-alias
       aliased-to))

     ;; Interactive alias
     (is-alias
      (elisp-dev-mcp--process-alias-source
       (format "'%s" function)
       function
       aliased-to
       "<interactively defined>"
       1
       1))

     ;; Interactive function
     (t
      (elisp-dev-mcp--get-function-definition-interactive
       function sym fn)))))

(defun elisp-dev-mcp--get-function-definition (function)
  "Get the source code definition for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to retrieve"
  (let* ((sym (elisp-dev-mcp--validate-symbol function "function" t))
         (fn-info (elisp-dev-mcp--extract-function-info sym)))
    (unless fn-info
      (mcp-server-lib-tool-throw
       (format "Function %s is not found" function)))
    (elisp-dev-mcp--get-function-definition-dispatch
     function sym fn-info)))

;;; Info Documentation Helpers

(defun elisp-dev-mcp--extract-info-node-content ()
  "Extract the complete content of the current Info node.
Assumes we're in an Info buffer at the correct node."
  (let ((start nil)
        (end nil))
    ;; Find the start of actual content (after the node header)
    (goto-char (point-min))
    (when (re-search-forward "^File: [^,]+,  Node: [^,\n]+.*\n" nil t)
      (setq start (point)))

    ;; Find the end of content
    (when start
      (goto-char start)
      ;; Look for the next node boundary or end of buffer
      (if (re-search-forward "^\^_" nil t)
          (setq end (match-beginning 0))
        (setq end (point-max))))

    ;; Extract and clean up the content
    (when (and start end)
      (elisp-dev-mcp--clean-info-content
       (buffer-substring-no-properties start end)))))

(defun elisp-dev-mcp--clean-info-content (content)
  "Clean up Info formatting from CONTENT.
Removes navigation markers while preserving documentation structure."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))

    ;; Remove footnote references like (*note ...)
    (while (re-search-forward "\\*[Nn]ote[ \n][^:]*::" nil t)
      (replace-match "[See: \\&]"))

    ;; Return cleaned content
    (buffer-string)))

(defun elisp-dev-mcp--perform-info-lookup (symbol)
  "Perform the actual Info lookup for SYMBOL.
Returns an alist with lookup results or nil if not found."
  (condition-case nil
      (with-temp-buffer
        ;; Set up for info-lookup
        (let ((mode 'emacs-lisp-mode)
              (info-buf nil)
              (node nil)
              (manual nil)
              (content nil))

          ;; info-lookup-symbol needs a buffer in the right mode
          (emacs-lisp-mode)

          ;; Perform the lookup - this will open an Info buffer
          (save-window-excursion
            (info-lookup-symbol symbol mode)

            ;; Get the Info buffer that was opened
            (setq info-buf (get-buffer "*info*"))

            (when info-buf
              (with-current-buffer info-buf
                ;; Extract node information
                (goto-char (point-min))
                (when (re-search-forward
                       "^File: \\([^,]+\\),  Node: \\([^,\n]+\\)"
                       nil t)
                  (setq manual (match-string 1))
                  (setq node (match-string 2))
                  ;; Remove .info extension if present
                  (when (string-match "\\.info\\'" manual)
                    (setq manual
                          (substring manual 0 (match-beginning 0)))))

                ;; Extract content
                (setq content
                      (elisp-dev-mcp--extract-info-node-content)))))

          ;; Return results if we found something
          (when (and node content)
            `((found . t)
              (symbol . ,symbol)
              (node . ,node)
              (manual . ,manual)
              (content . ,content)
              (info-ref . ,(format "(%s)%s" manual node))))))
    ;; If lookup fails, return nil
    (error
     nil)))

(defun elisp-dev-mcp--info-lookup-symbol (symbol)
  "Look up SYMBOL in Elisp Info documentation.

MCP Parameters:
  symbol - The symbol to look up (string)"
  (mcp-server-lib-with-error-handling
   ;; Validate input
   (elisp-dev-mcp--validate-symbol symbol "symbol")
   ;; Perform lookup
   (let ((result (elisp-dev-mcp--perform-info-lookup symbol)))
     (if result
         (json-encode result)
       (elisp-dev-mcp--json-encode-not-found
        symbol
        (format "Symbol '%s' not found in Elisp Info documentation"
                symbol))))))

(defun elisp-dev-mcp--library-name-p (name)
  "Return non-nil if NAME is a library name, not a file path.
Library names are non-blank strings without path separators.
Strings containing '/' or '\\\\' are treated as paths, not library names."
  (and (stringp name)
       (not (string-blank-p name))
       (not (file-name-absolute-p name))
       (not (string-match-p "[/\\]" name))))

(defun elisp-dev-mcp--resolve-library-to-source-path (library-name)
  "Resolve LIBRARY-NAME to its source file path that exists on disk.

Uses `locate-library' to find the library, then:
  - Converts .elc → .el (source, not bytecode)
  - Checks if .el exists, otherwise tries .el.gz

Returns the actual file path (.el or .el.gz) that exists on disk.
Throws an error if the library or source file is not found.

Example transformations:
  `locate-library' returns /path/file.el     → /path/file.el
  `locate-library' returns /path/file.el.gz  → /path/file.el.gz
  `locate-library' returns /path/file.elc    → /path/file.el (or .el.gz)
  `locate-library' returns /path/file.elc.gz → /path/file.el (or .el.gz)"
  (let ((library-path (locate-library library-name)))
    (unless library-path
      (mcp-server-lib-tool-throw
       (format "Library not found: %s" library-name)))
    ;; Remove .gz extension first if present
    ;; This must be done before .elc conversion to handle .elc.gz correctly
    (when (string-suffix-p ".gz" library-path)
      (setq library-path (file-name-sans-extension library-path)))
    ;; Convert .elc to .el if needed (after .gz removal)
    (when (string-suffix-p ".elc" library-path)
      (setq library-path
            (concat (file-name-sans-extension library-path) ".el")))
    ;; Find the actual source file that exists on disk
    (let ((actual-file
           (cond
            ((file-exists-p library-path)
             library-path)
            ((file-exists-p (concat library-path ".gz"))
             (concat library-path ".gz"))
            (t
             nil))))
      (unless actual-file
        (mcp-server-lib-tool-throw
         (format
          "Source file not found for library %s (tried %s and %s.gz)"
          library-name library-path library-path)))
      actual-file)))

(defun elisp-dev-mcp--read-source-file (library-or-path)
  "Read Elisp source file from allowed locations.
Accepts either a library name or absolute path via LIBRARY-OR-PATH.

Library names (e.g., \"subr\", \"mcp-server-lib\") are resolved via
`locate-library' and validated against the allowed directories.

Absolute paths must be as returned by other elisp-dev tools.

Handles both .el and .el.gz files transparently.

MCP Parameters:
  library-or-path - Library name (e.g., \"subr\") or absolute .el path"
  (mcp-server-lib-with-error-handling
   ;; 1. Resolve library name to absolute path if needed
   (let ((file-path
          (if (elisp-dev-mcp--library-name-p library-or-path)
              (elisp-dev-mcp--resolve-library-to-source-path
               library-or-path)
            library-or-path)))

     ;; 2. Validate input format
     (unless (and (stringp file-path)
                  (file-name-absolute-p file-path)
                  (or (string-suffix-p ".el" file-path)
                      (string-suffix-p ".el.gz" file-path)))
       (mcp-server-lib-tool-throw
        "Invalid path format: must be absolute path ending in .el or .el.gz"))

     ;; 3. Check for path traversal
     (when (string-match-p "\\.\\." file-path)
       (mcp-server-lib-tool-throw
        "Path contains illegal '..' traversal"))

     ;; 4. Resolve symlinks and validate location
     (let* ((true-path (file-truename file-path))
            ;; Build list of allowed package directories
            (allowed-dirs
             (append
              ;; Current package-user-dir
              (when (boundp 'package-user-dir)
                (list
                 (file-truename
                  (file-name-as-directory package-user-dir))))
              ;; All dirs from package-directory-list
              (mapcar
               (lambda (dir)
                 (file-truename (file-name-as-directory dir)))
               package-directory-list)
              ;; System lisp directory
              (when elisp-dev-mcp--system-lisp-dir
                (list
                 (file-truename
                  (file-name-as-directory
                   elisp-dev-mcp--system-lisp-dir))))
              ;; User-configured additional directories
              (mapcar
               (lambda (dir)
                 (file-truename (file-name-as-directory dir)))
               elisp-dev-mcp-additional-allowed-dirs)))
            ;; Check if file is under any allowed directory
            (allowed-p
             (cl-some
              (lambda (dir)
                (and dir (string-prefix-p dir true-path)))
              allowed-dirs)))

       (unless allowed-p
         (mcp-server-lib-tool-throw
          "Access denied: path outside allowed directories"))

       ;; 5. Verify file exists and read contents
       (unless (file-exists-p true-path)
         (mcp-server-lib-tool-throw
          (format "File not found: %s" library-or-path)))

       (elisp-dev-mcp--with-auto-compression
         (with-temp-buffer
           (insert-file-contents true-path)
           (buffer-string)))))))

;;; Structural File Tools

(defun elisp-dev-mcp--read-any-el-file (path)
  "Read PATH (absolute) as a string.  Handles .el and .el.gz files.
Unlike `elisp-dev-mcp--read-source-file', imposes no directory restriction."
  (unless (file-exists-p path)
    (mcp-server-lib-tool-throw (format "File not found: %s" path)))
  (elisp-dev-mcp--with-auto-compression
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun elisp-dev-mcp--validate-writable-el-path (file-path)
  "Validate FILE-PATH as an absolute, writable .el file.
Returns the expanded path or signals a tool error."
  (unless (stringp file-path)
    (mcp-server-lib-tool-throw "file-path must be a string"))
  (unless (file-name-absolute-p file-path)
    (mcp-server-lib-tool-throw "file-path must be an absolute path"))
  (when (string-match-p "\\.\\." file-path)
    (mcp-server-lib-tool-throw "Path contains illegal '..' traversal"))
  (let ((expanded (expand-file-name file-path)))
    (unless (string-suffix-p ".el" expanded)
      (mcp-server-lib-tool-throw "file-path must end in .el"))
    (unless (file-exists-p expanded)
      (mcp-server-lib-tool-throw (format "File not found: %s" expanded)))
    (unless (file-writable-p expanded)
      (mcp-server-lib-tool-throw (format "File not writable: %s" expanded)))
    expanded))

(defun elisp-dev-mcp--resolve-el-path (file-path)
  "Resolve FILE-PATH to an absolute path.
Accepts library names (e.g. \"org\") or absolute paths."
  (if (elisp-dev-mcp--library-name-p file-path)
      (elisp-dev-mcp--resolve-library-to-source-path file-path)
    (progn
      (unless (file-name-absolute-p file-path)
        (mcp-server-lib-tool-throw "file-path must be an absolute path"))
      (unless (or (string-suffix-p ".el" file-path)
                  (string-suffix-p ".el.gz" file-path))
        (mcp-server-lib-tool-throw "file-path must end in .el or .el.gz"))
      (expand-file-name file-path))))

(defun elisp-dev-mcp--scan-forms-in-buffer ()
  "Return list of (START . END) buffer positions for all top-level forms.
Operates on the current buffer.  Skips top-level comments and blank lines."
  (save-excursion
    (let ((forms '()))
      (goto-char (point-min))
      (while (not (eobp))
        (skip-chars-forward " \t\n\r")
        (while (and (not (eobp)) (looking-at "[ \t]*;"))
          (forward-line 1)
          (skip-chars-forward " \t\n\r"))
        (unless (eobp)
          (let ((start (point)))
            (condition-case nil
                (progn
                  (forward-sexp 1)
                  (push (cons start (point)) forms))
              (scan-error
               (goto-char (point-max)))))))
      (nreverse forms))))

(defun elisp-dev-mcp--form-type-and-name (start end)
  "Return (TYPE NAME) for the form at buffer positions START..END.
TYPE is a downcased string.  Returns nil when the form cannot be identified."
  (condition-case nil
      (let* ((text (buffer-substring-no-properties start end))
             (form (car (read-from-string text)))
             (car-sym (and (consp form) (car form))))
        (when (symbolp car-sym)
          (let* ((type (downcase (symbol-name car-sym)))
                 (raw-name (cadr form))
                 (name
                  (cond
                   ((symbolp raw-name) (symbol-name raw-name))
                   ((and (consp raw-name)
                         (eq (car raw-name) 'setf)
                         (symbolp (cadr raw-name)))
                    (format "(setf %s)" (cadr raw-name)))
                   ((stringp raw-name) raw-name)
                   (t nil))))
            (when name (list type name)))))
    (error nil)))

(defun elisp-dev-mcp--form-signature (start end)
  "Return a collapsed one-line signature for the form at START..END.
Includes a line-number prefix for navigation."
  (condition-case nil
      (let* ((text (buffer-substring-no-properties start end))
             (form (car (read-from-string text)))
             (car-sym (and (consp form) (car form)))
             (type (and (symbolp car-sym) (symbol-name car-sym)))
             (type-lc (and type (downcase type)))
             (name (cadr form))
             (line (line-number-at-pos start)))
        (cond
         ((member type-lc '("defun" "defmacro" "defsubst" "defun*"
                            "cl-defun" "cl-defmacro"))
          (format "L%4d  (%s %s %S ...)" line type name (caddr form)))
         ((member type-lc '("defvar" "defconst" "defcustom" "defface"
                            "defgroup" "deftheme" "defvar-local"))
          (format "L%4d  (%s %s ...)" line type name))
         ((string= type-lc "use-package")
          (format "L%4d  (use-package %s ...)" line name))
         ((member type-lc '("require" "provide"))
          (format "L%4d  (%s '%s)" line type name))
         (t
          (let ((first-line
                 (string-trim-right (car (split-string text "\n")))))
            (format "L%4d  %s ..."
                    line
                    (if (> (length first-line) 80)
                        (substring first-line 0 80)
                      first-line))))))
    (error
     (let* ((text (buffer-substring-no-properties start end))
            (first-line (string-trim-right (car (split-string text "\n"))))
            (line (line-number-at-pos start)))
       (format "L%4d  %s ..."
               line
               (if (> (length first-line) 80)
                   (substring first-line 0 80)
                 first-line))))))

(defun elisp-dev-mcp--find-form-bounds (form-type form-name)
  "Find form (FORM-TYPE FORM-NAME ...) in the current buffer.
Returns (start . end) buffer positions or signals a tool error."
  (let* ((type-lc (downcase form-type))
         (forms (elisp-dev-mcp--scan-forms-in-buffer))
         (matches '()))
    (dolist (bounds forms)
      (let ((tn (elisp-dev-mcp--form-type-and-name
                 (car bounds) (cdr bounds))))
        (when (and tn
                   (string= (car tn) type-lc)
                   (string= (cadr tn) form-name))
          (push bounds matches))))
    (cond
     ((null matches)
      (mcp-server-lib-tool-throw
       (format "Form (%s %s) not found" form-type form-name)))
     ((> (length matches) 1)
      (mcp-server-lib-tool-throw
       (format "Multiple matches for (%s %s); be more specific"
               form-type form-name)))
     (t (car matches)))))

;;; Tool: elisp-eval

(defun elisp-dev-mcp--eval (code)
  "Evaluate Emacs Lisp CODE in the running Emacs instance.
Wraps multiple expressions in `progn'.  Returns result and any message output.

MCP Parameters:
  code - Emacs Lisp expression(s) to evaluate"
  (mcp-server-lib-with-error-handling
   (unless (and (stringp code) (not (string-empty-p code)))
     (mcp-server-lib-tool-throw "code must be a non-empty string"))
   (let (result output-list)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) output-list)
                  nil)))
       (condition-case err
           (setq result
                 (eval (read (concat "(progn " code ")")) t))
         (error
          (mcp-server-lib-tool-throw
           (format "Eval error: %s" (error-message-string err))))))
     (json-encode
      `((result . ,(format "%S" result))
        (output . ,(mapconcat #'identity (nreverse output-list) "\n")))))))

;;; Tool: elisp-read-file

(defun elisp-dev-mcp--read-file (file-path &optional collapsed
                                            name-pattern content-pattern
                                            offset limit)
  "Read an Elisp file with an optional collapsed form-signature view.
In collapsed mode (default) each top-level form is shown as a one-line
signature with its line number.  Forms whose name or body match the
supplied regex patterns are expanded to their full text.

MCP Parameters:
  file-path - Library name (e.g. \"org\") or absolute .el/.el.gz path
  collapsed - \"true\" (default) for signatures, \"false\" for raw lines
  name-pattern - Regex: expand forms whose name matches (collapsed mode)
  content-pattern - Regex: expand forms whose body matches (collapsed mode)
  offset - Starting line number for raw mode (default 0)
  limit - Maximum lines for raw mode (default 500)"
  (mcp-server-lib-with-error-handling
   (when (or (not (stringp file-path)) (string-empty-p file-path))
     (mcp-server-lib-tool-throw "file-path is required"))
   (let* ((resolved (elisp-dev-mcp--resolve-el-path file-path))
          (content (elisp-dev-mcp--read-any-el-file resolved))
          (collapsed-p (not (equal collapsed "false"))))
     (if collapsed-p
         (with-temp-buffer
           (insert content)
           (let* ((forms (elisp-dev-mcp--scan-forms-in-buffer))
                  (name-rx (and (stringp name-pattern)
                                (not (string-empty-p name-pattern))
                                name-pattern))
                  (content-rx (and (stringp content-pattern)
                                   (not (string-empty-p content-pattern))
                                   content-pattern))
                  (parts '()))
             (dolist (bounds forms)
               (let* ((start (car bounds))
                      (end (cdr bounds))
                      (text (buffer-substring-no-properties start end))
                      (tn (elisp-dev-mcp--form-type-and-name start end))
                      (form-name (and tn (cadr tn)))
                      (expand-p
                       (or (and name-rx form-name
                                (condition-case nil
                                    (string-match-p name-rx form-name)
                                  (error nil)))
                           (and content-rx
                                (condition-case nil
                                    (string-match-p content-rx text)
                                  (error nil))))))
                 (push (if expand-p text
                         (elisp-dev-mcp--form-signature start end))
                       parts)))
             (mapconcat #'identity (nreverse parts) "\n\n")))
       ;; Raw slice mode
       (let* ((lines (split-string content "\n"))
              (total (length lines))
              (off (if (and (stringp offset) (not (string-empty-p offset)))
                       (max 0 (string-to-number offset))
                     0))
              (lim (if (and (stringp limit) (not (string-empty-p limit)))
                       (string-to-number limit)
                     500))
              (end-line (min (+ off lim) total))
              (slice (mapconcat #'identity
                                (seq-subseq lines off end-line)
                                "\n")))
         (if (< end-line total)
             (format
              "%s\n[Showing lines %d-%d of %d. Use offset=%d to read more.]"
              slice (1+ off) end-line total end-line)
           slice))))))

;;; Tool: elisp-check-parens

(defun elisp-dev-mcp--check-parens (file-path &optional code)
  "Check for unbalanced parentheses/brackets in a file or code string.
Returns JSON with ok=true, or ok=false with an error message.

MCP Parameters:
  file-path - Absolute path to .el file to check (pass empty string to use code)
  code - Elisp code string to check (used when file-path is empty)"
  (mcp-server-lib-with-error-handling
   (let ((content
          (cond
           ((and (stringp file-path) (not (string-empty-p file-path)))
            (elisp-dev-mcp--read-any-el-file
             (expand-file-name file-path)))
           ((and (stringp code) (not (string-empty-p code)))
            code)
           (t
            (mcp-server-lib-tool-throw
             "Provide file-path or code")))))
     (with-temp-buffer
       (emacs-lisp-mode)
       (insert content)
       (condition-case err
           (progn
             (check-parens)
             (json-encode '((ok . t))))
         (error
          (json-encode
           `((ok . :json-false)
             (message . ,(error-message-string err))))))))))

;;; Tool: elisp-edit-form

(defun elisp-dev-mcp--edit-form (file-path form-type form-name operation
                                            content &optional dry-run)
  "Structurally edit a top-level Elisp form using replace or insert operations.
Finds the form by type and name, applies the operation, and writes the file.
If the file is open in Emacs the buffer is reverted automatically.

MCP Parameters:
  file-path - Absolute path to .el file to edit
  form-type - Form type: defun, defvar, defcustom, use-package, etc.
  form-name - Name of the form to locate
  operation - One of: replace, insert_before, insert_after
  content - Complete form text to use in the operation
  dry-run - \"true\" to preview without writing (default: false)"
  (mcp-server-lib-with-error-handling
   (dolist (pair `(("file-path" . ,file-path) ("form-type" . ,form-type)
                   ("form-name" . ,form-name) ("operation" . ,operation)
                   ("content" . ,content)))
     (when (or (not (stringp (cdr pair))) (string-empty-p (cdr pair)))
       (mcp-server-lib-tool-throw
        (format "%s must be a non-empty string" (car pair)))))
   (let* ((abs-path (elisp-dev-mcp--validate-writable-el-path file-path))
          (dry-run-p (equal dry-run "true"))
          (op-lc (downcase operation))
          (file-content (elisp-dev-mcp--read-any-el-file abs-path)))
     (unless dry-run-p
       (let ((visiting-buf (find-buffer-visiting abs-path)))
         (when (and visiting-buf (buffer-modified-p visiting-buf))
           (mcp-server-lib-tool-throw
            (format "Buffer visiting %s has unsaved changes; save it first"
                    abs-path)))))
     (unless (member op-lc '("replace" "insert_before" "insert_after"))
       (mcp-server-lib-tool-throw
        (format "Unknown operation: %s. Use replace, insert_before, or insert_after"
                operation)))
     (with-temp-buffer
       (insert file-content)
       (let* ((bounds (elisp-dev-mcp--find-form-bounds form-type form-name))
              (start (car bounds))
              (end (cdr bounds))
              (original-form (buffer-substring-no-properties start end)))
         (cond
          ((string= op-lc "replace")
           (delete-region start end)
           (goto-char start)
           (insert content))
          ((string= op-lc "insert_before")
           (goto-char start)
           (insert content "\n\n"))
          ((string= op-lc "insert_after")
           (goto-char end)
           (insert "\n\n" content)))
         (let* ((updated (buffer-string))
                (changed (not (string= file-content updated))))
           (if dry-run-p
               (json-encode
                `((would-change . ,(elisp-dev-mcp--json-bool changed))
                  (operation . ,op-lc)
                  (form-type . ,form-type)
                  (form-name . ,form-name)
                  (original . ,original-form)
                  (preview . ,updated)))
             (when changed
               (with-temp-file abs-path
                 (insert updated))
               (let ((buf (find-buffer-visiting abs-path)))
                 (when buf
                   (with-current-buffer buf
                     (revert-buffer t t t)))))
             (json-encode
              `((would-change . ,(elisp-dev-mcp--json-bool changed))
                (operation . ,op-lc)
                (form-type . ,form-type)
                (form-name . ,form-name)
                (file-path . ,abs-path))))))))))

;;; Tool: elisp-patch-form

(defun elisp-dev-mcp--patch-form (file-path form-type form-name
                                             old-text new-text &optional dry-run)
  "Replace exact text within a top-level Elisp form (token-efficient small edits).
old-text must appear exactly once within the matched form.

MCP Parameters:
  file-path - Absolute path to .el file to edit
  form-type - Form type: defun, defvar, etc.
  form-name - Name of the form to target
  old-text - Exact text to find within the form (must match exactly once)
  new-text - Replacement text
  dry-run - \"true\" to preview without writing (default: false)"
  (mcp-server-lib-with-error-handling
   (dolist (pair `(("file-path" . ,file-path) ("form-type" . ,form-type)
                   ("form-name" . ,form-name) ("old-text" . ,old-text)))
     (when (not (stringp (cdr pair)))
       (mcp-server-lib-tool-throw
        (format "%s must be a string" (car pair)))))
   (unless (stringp new-text)
     (mcp-server-lib-tool-throw "new-text must be a string"))
   (when (string-empty-p old-text)
     (mcp-server-lib-tool-throw "old-text must be a non-empty string"))
   (let* ((abs-path (elisp-dev-mcp--validate-writable-el-path file-path))
          (dry-run-p (equal dry-run "true"))
          (file-content (elisp-dev-mcp--read-any-el-file abs-path)))
     (unless dry-run-p
       (let ((visiting-buf (find-buffer-visiting abs-path)))
         (when (and visiting-buf (buffer-modified-p visiting-buf))
           (mcp-server-lib-tool-throw
            (format "Buffer visiting %s has unsaved changes; save it first"
                    abs-path)))))
     (with-temp-buffer
       (insert file-content)
       (let* ((bounds (elisp-dev-mcp--find-form-bounds form-type form-name))
              (form-start (car bounds))
              (form-end (cdr bounds))
              (form-text (buffer-substring-no-properties form-start form-end))
              (occurrences 0)
              (search-pos 0))
         ;; Count occurrences of old-text within the form
         (while (setq search-pos
                      (cl-search old-text form-text :start2 search-pos))
           (setq occurrences (1+ occurrences))
           (setq search-pos (1+ search-pos)))
         (cond
          ((= occurrences 0)
           (mcp-server-lib-tool-throw
            (format "old-text not found in (%s %s)" form-type form-name)))
          ((> occurrences 1)
           (mcp-server-lib-tool-throw
            (format "old-text matches %d times in (%s %s); must match exactly once"
                    occurrences form-type form-name)))
          (t
           (let* ((match-pos (cl-search old-text form-text))
                  (new-form
                   (concat (substring form-text 0 match-pos)
                           new-text
                           (substring form-text
                                      (+ match-pos (length old-text)))))
                  ;; Buffer positions are 1-based; convert to string indices
                  (str-start (1- form-start))
                  (str-end (1- form-end))
                  (updated
                   (concat (substring file-content 0 str-start)
                           new-form
                           (substring file-content str-end)))
                  (changed (not (string= file-content updated))))
             (if dry-run-p
                 (json-encode
                  `((would-change . ,(elisp-dev-mcp--json-bool changed))
                    (form-type . ,form-type)
                    (form-name . ,form-name)
                    (original . ,form-text)
                    (preview . ,new-form)))
               (when changed
                 (with-temp-file abs-path
                   (insert updated))
                 (let ((buf (find-buffer-visiting abs-path)))
                   (when buf
                     (with-current-buffer buf
                       (revert-buffer t t t)))))
               (json-encode
                `((would-change . ,(elisp-dev-mcp--json-bool changed))
                  (form-type . ,form-type)
                  (form-name . ,form-name)
                  (file-path . ,abs-path))))))))))))

;;;###autoload
(defun elisp-dev-mcp-enable ()
  "Enable the Elisp development MCP tools."
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--describe-function
   :id "elisp-describe-function"
   :server-id elisp-dev-mcp--server-id
   :description
   "Get documentation for an Emacs Lisp function or check if it exists. Returns
function documentation from the current running Emacs environment, including all
currently loaded packages and libraries.

Supports:
- Regular functions (defun), macros (defmacro), inline functions (defsubst)
- Function aliases (shows both alias info and target function docs)
- Built-in C functions (subr)
- Byte-compiled functions
- Functions with or without documentation

Returns formatted documentation including:
- Function signature with argument names
- Full docstring with parameter descriptions
- Source file location
- Function type (closure, macro, subr, etc.)

Error cases:
- Non-existent functions return 'Function X is void'
- Invalid input types return 'Error: ...'"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--get-function-definition
   :id "elisp-get-function-definition"
   :server-id elisp-dev-mcp--server-id
   :description
   "Get the source code definition of an Emacs Lisp function with any header
comments. Returns source code with file path and 1-based line numbers. For
functions defined in C, returns a suggestion to call elisp-describe-function
tool instead.

Returns JSON with:
- source: Complete function definition including header comments
- file-path: Absolute path to source file or '<interactively defined>'
- start-line: Line number where definition starts (1-based)
- end-line: Line number where definition ends

Special handling:
- Function aliases: Returns the defalias form with docstring
- C functions: Returns is-c-function=true with suggestion message
- Interactive functions: Reconstructs defun from runtime representation
- Byte-compiled functions: Retrieves original source if available

Error cases:
- Non-existent functions return 'Function X is not found'
- Non-string input returns 'Invalid function name'

Use this tool when you need to:
- View or analyze function implementation
- Extract function source for modification
- Understand function structure with comments"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--describe-variable
   :id "elisp-describe-variable"
   :server-id elisp-dev-mcp--server-id
   :description
   "Get comprehensive information about an Emacs Lisp variable without
exposing its value. Essential for understanding variable definitions,
types, and relationships in Elisp code.

Parameters:
  variable - Variable name as a string (e.g., \"load-path\", \"custom-file\")

Returns JSON object with these fields:
  name - Variable name (string, always present)
  bound - Whether variable has a value (boolean, always present)
  value-type - Type of the current value like \"string\", \"cons\", \"integer\",
               \"symbol\" (string, only when bound is true)
  documentation - Variable's docstring (string or null, always present)
  source-file - File where defined, or \"<interactively defined>\"
                (string, always present)
  is-custom - Whether it's a defcustom variable (boolean, always present)
  custom-group - Which customization group it belongs to
                 (string, only when is-custom is true)
  custom-type - Type specification for customization like \"string\" or
                complex types (string, only when is-custom is true)
  is-obsolete - Whether marked as obsolete (boolean, always present)
  obsolete-since - Version when obsoleted
                   (string, only when is-obsolete is true)
  obsolete-replacement - Suggested replacement
                         (string, only when is-obsolete is true)
  is-alias - Whether this is an alias to another variable
             (boolean, always present)
  alias-target - The actual variable this aliases to
                 (string, only when is-alias is true)
  is-special - Whether it's a special/dynamic variable in lexical-binding
               context (boolean, always present)

Common use cases:
- Check if a configuration variable exists before using it
- Understand variable relationships (aliases, obsolescence)
- Verify variable types before setting values
- Find documentation for Emacs configuration options
- Discover which customization group a setting belongs to

Security: Never exposes actual values to prevent leaking sensitive data
like API keys, passwords, or personal information. Use this instead of
eval when exploring variables.

Error cases return error messages for:
- Non-string input
- Completely undefined variables (no binding, no documentation, no properties)"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--info-lookup-symbol
   :id "elisp-info-lookup-symbol"
   :server-id elisp-dev-mcp--server-id
   :description
   "Look up Elisp symbols in Info documentation and return the complete
documentation node. Returns the full content of the Info node containing
the symbol's documentation from the Emacs Lisp Reference Manual.

Parameters:
  symbol - The Elisp symbol to look up (string)

Returns JSON with:
  found - Whether documentation was found (boolean)
  symbol - The symbol that was looked up (string)
  node - The Info node name containing the documentation (string, when found)
  manual - The Info manual name, typically 'elisp' (string, when found)
  content - The complete Info node content including all examples,
            cross-references, and related information (string, when found)
  info-ref - Info reference like '(elisp)Node Name' for direct access
             (string, when found)
  message - Error or not-found message (string, when not found)

The content field contains the entire Info node, ensuring you have full
context including:
- Complete function/variable descriptions
- All code examples and usage patterns
- Cross-references to related concepts
- Any warnings, notes, or special considerations

Common symbols that can be looked up:
- Special forms: defun, defvar, let, if, cond, lambda
- Functions: mapcar, apply, funcall, concat
- Macros: when, unless, dolist, defmacro
- Variables: load-path, emacs-version
- Concepts: 'lexical binding', 'dynamic binding'

Error cases:
- Symbol not found in documentation
- Invalid symbol name
- Info system unavailable"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--read-source-file
   :id "elisp-read-source-file"
   :server-id elisp-dev-mcp--server-id
   :description
   "Read Elisp source files from Emacs system directories or ELPA packages.
Accepts either library names or absolute file paths.

Parameters:
  library-or-path - Library name (e.g., \\='subr', \\='mcp-server-lib') or
                    absolute path to .el file (string)

Input modes:
1. Library names (recommended for built-in and installed packages):
   - Simple names without path separators (e.g., \\='subr', \\='files')
   - Resolved via Emacs locate-library function
   - Examples: \\='subr', \\='mcp-server-lib', \\='org'

2. Absolute paths (for compatibility with other elisp-dev tools):
   - Full paths ending in .el (e.g., \\='/path/to/file.el')
   - Returned by elisp-get-function-definition
   - Examples: \\='/opt/homebrew/.../lisp/subr.el'

Security:
- Only reads from Emacs system lisp directories and ELPA directories
- Rejects paths with \"..\" traversal
- Resolves symlinks to prevent escaping allowed directories
- Library names must resolve to paths within allowed directories

Features:
- Transparently handles .el.gz compressed files
- Works with both built-in Emacs libraries and installed packages
- Returns complete file contents as string

Error cases:
- Library not found (locate-library returns nil)
- Invalid path format (paths must be absolute and end in .el)
- Path traversal attempts
- Access outside allowed directories
- File not found"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--eval
   :id "elisp-eval"
   :server-id elisp-dev-mcp--server-id
   :description
   "Evaluate Emacs Lisp code in the running Emacs instance and return the result.
Multiple expressions are wrapped in progn; the last value is returned.

Parameters:
  code - Emacs Lisp expression(s) to evaluate (string)

Returns JSON with:
  result - The return value formatted with %S
  output - Any message() output produced during evaluation

Error cases:
- Invalid or empty code returns an error
- Runtime errors (wrong-type-argument, void-variable, etc.) are reported")
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--read-file
   :id "elisp-read-file"
   :server-id elisp-dev-mcp--server-id
   :description
   "Read an Elisp file with a collapsed form-signature view for easy navigation.
Prefer this over elisp-read-source-file for your own .el config files.

In collapsed mode (default) each top-level form is shown as a one-line
signature prefixed with its line number (e.g. \"L  42  (defun foo (x) ...)\").
Supply name-pattern or content-pattern to expand matching forms to full text.
Use collapsed=false for a raw line-slice (offset/limit control the window).

Parameters:
  file-path - Library name (e.g. \"org\") or absolute path to .el/.el.gz
  collapsed - \"true\" (default) or \"false\" for raw line mode
  name-pattern - Regex: expand forms whose name matches (collapsed mode only)
  content-pattern - Regex: expand forms whose body matches (collapsed mode only)
  offset - Starting line for raw mode (default 0)
  limit - Max lines for raw mode (default 500)

Returns the formatted file content as a string."
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--check-parens
   :id "elisp-check-parens"
   :server-id elisp-dev-mcp--server-id
   :description
   "Check for unbalanced parentheses/brackets in an Elisp file or code string.
Uses Emacs's native check-parens for accurate detection.

Parameters:
  file-path - Absolute path to .el file (pass empty string to use code instead)
  code - Elisp code string (used when file-path is empty)

Returns JSON with:
  ok - true if balanced, false if not
  message - Error description with location when ok is false

Error cases:
- Neither file-path nor code provided
- File not found"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--edit-form
   :id "elisp-edit-form"
   :server-id elisp-dev-mcp--server-id
   :description
   "Structurally edit a top-level Elisp form in a file.
Finds the form by (form-type, form-name) and applies replace or insert.
If the file is open in Emacs the buffer is reverted automatically after writing.

Parameters:
  file-path - Absolute path to .el file to edit
  form-type - Form constructor: defun, defmacro, defvar, defcustom,
              defface, defgroup, use-package, etc.
  form-name - Name of the form (e.g. \"my-function\" or \"(setf my-accessor)\")
  operation - One of: replace, insert_before, insert_after
  content - Complete form text for the operation (including outer parens)
  dry-run - \"true\" to preview without writing (default: \"false\")

Returns JSON with:
  would-change - Whether the operation modifies the file
  operation, form-type, form-name - Echoed inputs
  file-path - Absolute path written (non-dry-run only)
  original - Matched form text before change (dry-run only)
  preview - Full file preview with changes applied (dry-run only)

Error cases:
- Form not found, multiple matches, unknown operation, unwritable file")
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--patch-form
   :id "elisp-patch-form"
   :server-id elisp-dev-mcp--server-id
   :description
   "Token-efficient text replacement within a single top-level Elisp form.
Prefer this over elisp-edit-form for small edits inside large forms.
old-text is matched literally (whitespace-sensitive) and must occur exactly once.

Parameters:
  file-path - Absolute path to .el file to edit
  form-type - Form type: defun, defvar, defcustom, etc.
  form-name - Name of the form to target
  old-text - Exact text to replace (must match exactly once within the form)
  new-text - Replacement text
  dry-run - \"true\" to preview without writing (default: \"false\")

Returns JSON with:
  would-change - Whether old-text differs from new-text
  form-type, form-name - Echoed inputs
  file-path - Absolute path written (non-dry-run only)
  original - Full original form text (dry-run only)
  preview - Modified form text after replacement (dry-run only)

Error cases:
- old-text not found in form, old-text matches more than once, file not writable"))

;;;###autoload
(defun elisp-dev-mcp-disable ()
  "Disable the Elisp development MCP tools."
  (mcp-server-lib-unregister-tool
   "elisp-describe-function" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-get-function-definition" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-describe-variable" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-info-lookup-symbol" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-read-source-file" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-eval" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-read-file" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-check-parens" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-edit-form" elisp-dev-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "elisp-patch-form" elisp-dev-mcp--server-id))

(provide 'elisp-dev-mcp)
;;; elisp-dev-mcp.el ends here
