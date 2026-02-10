(package "elisp-dev-mcp"
         "1.2.0"
         "MCP server for agentic Elisp development")

(website-url "https://github.com/laurynas-biveinis/elisp-dev-mcp")
(keywords "tools" "development")

(package-file "elisp-dev-mcp.el")

(source "melpa")

(depends-on "emacs" "27.1")
(depends-on "mcp-server-lib" "0.2.0")

(setq network-security-level 'low)

(files "elisp-dev-mcp.el")

(script "test" "eask test ert elisp-dev-mcp-test.el")

(script "org-lint" 
        (concat "emacs -Q --batch "
                "--eval \"(require 'org)\" "
                "--eval \"(require 'org-lint)\" "
                "--eval \"(let ((all-checks-passed t) "
                        "(files '(\\\"README.org\\\"))) "
                    "(dolist (file files all-checks-passed) "
                      "(with-temp-buffer "
                        "(insert-file-contents file) "
                        "(org-mode) "
                        "(let ((results (org-lint))) "
                          "(when results "
                            "(message \\\"Found issues in %s: %S\\\" file results) "
                            "(setq all-checks-passed nil))))) "
                    "(unless all-checks-passed (kill-emacs 1)))\""))