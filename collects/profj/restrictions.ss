#cs
(module restrictions mzscheme
  
  (provide is-field-restricted? is-method-restricted? forbidden-lang-class? is-import-restricted?)
  
  (define disallowed-methods 
    (list `("clone" ("Object" "java" "lang"))
          `("finalize" ("Object" "java" "lang"))
          `("getClass" ("Object" "java" "lang"))
          `("notify" ("Object" "java" "lang"))
          `("notifyAll" ("Object" "java" "lang"))
          `("wait" ("Object" "java" "lang"))
          
          ))
  
  ;is-method-restricted?: string (list string) -> bool
  (define (is-method-restricted? method class)
    (member (list method class) disallowed-methods))
  
  (define disallowed-fields
    (list `("TYPE" ("Boolean" "java" "lang"))
          `("TYPE" ("Byte" "java" "lang"))
          `("TYPE" ("Character" "java" "lang"))
          `("TYPE" ("Double" "java" "lang"))
          `("TYPE" ("Float" "java" "lang"))
          `("TYPE" ("Integer" "java" "lang"))
          `("TYPE" ("Long" "java" "lang"))
          `("TYPE" ("Short" "java" "lang"))
          
          ))
  
  ;is-field-restricted: string (list string) -> bool
  (define (is-field-restricted? field class)
    (member (list field class) disallowed-fields))

  (define (disallowed-imports level)
    (let ((teaching-levels (list `("Class" "java" "lang")
                                 `("ClassLoader" "java" "lang")
                                 `("Compiler" "java" "lang")
                                 `("InheritableThreadLocal" "java" "lang")
                                 `("Package" "java" "lang")
                                 `("Process" "java" "lang")
                                 `("Runtime" "java" "lang")
                                 `("RuntimePermission" "java" "lang")
                                 `("SecurityManager" "java" "lang")
                                 `("StackTraceElement" "java" "lang")
                                 `("Thread" "java" "lang")
                                 `("ThreadGroup" "java" "lang")
                                 `("ThreadLocal" "java" "lang")
                                 `("Throwable" "java" "lang")
                                 `("Void" "java" "lang")
                                 `("*" "java" "lang" "reflect")
                                 `("*" "java" "lang" "ref")
                                 `("*" "java" "applet")
                                 
                                 ))
          (beginner&intermediate (list `("System" "java" "lang")
                                        ;`("*" "java" "io")
                                        ;`("*" "java" "util")
                                        )))
      (case level
        ((beginner) (append beginner&intermediate
                            teaching-levels))
        ((intermediate) (append beginner&intermediate
                                teaching-levels))
        ((advanced) (append null
                            teaching-levels))
        ((full) null))))
  
  ;restricted-import? string (list string) symbol -> bool
  (define (is-import-restricted? class path level)
    (or (member (cons class path) (disallowed-imports level))
        (member (cons "*" path) (disallowed-imports level))))
  
  ;forbidden-lang-class? string symbol -> bool
  (define (forbidden-lang-class? class level)
    (member (list class "java" "lang") (disallowed-imports level)))
  
  )
