(module info (lib "infotab.ss" "setup")
  (require (lib "string-constant.ss" "string-constants"))
  
  (define name "ProfessorJ")
  (define doc.txt "doc.txt")
  (define tools (list (list "tool.ss")))
  (define install-collection "installer.ss")
  (define pre-install-collection "pre-installer.ss")
  (define compile-subcollections (list (list "profj" "parsers")
                                       (list "profj" "libs" "java" "lang")
				       (list "profj" "libs" "java" "io")
                                       (list "profj" "libs" "java" "util")))
  (define textbook-pls
    (list (list '("htdch-icon.png" "profj")
                "How to Design Class Hierarchies"
                (string-constant experimental-languages)
                "ProfessorJ"
                "Beginner"))))
