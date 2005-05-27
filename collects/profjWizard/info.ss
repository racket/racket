(module info (lib "infotab.ss" "setup")
  (define name "ProfessorJ Wizard")
  (define tools (list (list "tool.ss")))
  (define comment '(define compile-subcollections (list (list "profj" "parsers")
                                       (list "profj" "libs" "java" "lang")
				       (list "profj" "libs" "java" "io"))))
  (define compile-omit-files
    '("draw-txt0.ss" "macro-class.scm" "view0.scm" "data-defs0.scm"))

  )
