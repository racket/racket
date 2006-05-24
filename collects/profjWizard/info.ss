(module info (lib "infotab.ss" "setup")
  (define name "ProfessorJ Wizard")
  (define tools '(("tool.ss")))
  (define tool-names '("ProfessorJ Wizard"))
  ;; (define compile-subcollections
  ;;   '(("profj" "parsers")
  ;;     ("profj" "libs" "java" "lang")
  ;;     ("profj" "libs" "java" "io")))
  (define compile-omit-files
    '("draw-txt0.ss"
      "macro-class.scm"
      "view0.scm"
      "data-defs0.scm"))
  )
