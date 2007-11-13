(module info setup/infotab
  (define name "Test Suites")
  (define doc-subcollections (list "tester"))
  (define compile-subcollections '(("tests" "drscheme")
				   ("tests" "framework")
				   ("tests" "utils")))
  (define tools (list '("tool.ss" "drscheme")))
  (define tool-names (list "DrScheme Test Suites")))
