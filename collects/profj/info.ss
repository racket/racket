(module info (lib "infotab.ss" "setup")
  (require (lib "string-constant.ss" "string-constants"))
  (define name "ProfessorJ")
  (define doc.txt "doc.txt")
  (define tools (list (list "tool.ss") (list "test-tool.ss")))
  (define tool-names '("ProfessorJ" "ProfessorJ Testing"))
  (define install-collection "installer.ss")
  (define pre-install-collection "pre-installer.ss")
  (define compile-subcollections
    '(("profj" "parsers")
      ("profj" "libs" "java" "lang")
      ("profj" "libs" "java" "io")
      ("profj" "libs" "java" "util")))
  (define textbook-pls
    (list (list '("htdch-icon.png" "profj")
                "How to Design Classes"
                (string-constant experimental-languages)
                "ProfessorJ"
                "Beginner"))))
