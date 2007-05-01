(module class mzscheme 
  (require "data-defs.scm"
           (lib "contract.ss")
           (lib "etc.ss") 
           (lib "list.ss")
           (lib "string.ss" "srfi" "13")
           (only (lib "1.ss" "srfi") zip))
  
  ;; ------------------------------------------------------------------------
  (provide/contract
   [make-class ((Class) (boolean? boolean? string? Fields (listof Method)) . opt-> . string?)] 
   [method     (Method . -> . string?)]
   ) 
  
  #| Usage: 
          (make-class a-class toString? template? language? Fields Methods) 
        create a Java class from the class specification (a-class) and add 
        Fields from super and Methods from the implementing interface. 

        The first optional boolean parameter specifies whether we want toString 
        in the class hiearchy. Default: true

        The second optional boolean parameter specifies whether we want 
        a draft method template in comments. Default: true

        The third optinal determines the language level. 
     |#
  
  ;; ------------------------------------------------------------------------
  ;; Construct individual Java classes 
  
  ;; create class definition (as string) from name, super and list of fields
  (define make-class 
    (opt-lambda (the-class [toString? #t][template? #t][language BEGINNER][super-fields '()][method-sigs '()])
      (define no-public (or (eq? language BEGINNER) (or (eq? language INTERMEDIATE))))
      (let ([type   (car the-class)]
            [super  (cadr the-class)]
            [fields (caddr the-class)]
            [prps   (class-purpose the-class)])
	(define templates 
          (if (or (not template?) (eq? language BEGINNER))
              ""
              (enumerate-with-separator
               (map (lambda (ms)
                      (apply string-append (make-template ms super fields template? (not no-public))))
                    method-sigs)
               "\n")))
        (apply string-append
               `(  ,@(purpose-statement prps)
                     ,(format classf type (implements super))
                     ;; fields
                     ,@(map (lambda (f) (format declaf (car f) (cadr f))) fields)
                     "\n"
                     ;; constructor
                     ,(class-constructor type fields super-fields)
                     ;; optional template draft:
                     ,templates
                     ;; optional toString method:
                     ,@(toString type fields no-public toString?)
                     ,endclf)))))
  
  ;; String -> String
  (define (implements super) (if (string=? "" super) "" (format implementsf super)))
  
  ;; String Fields Fields -> String
  (define (class-constructor type fields super-fields)
    (apply string-append
           `( ,(format constf type (parameters (append super-fields fields)))
               #| When we switch to super in beginner, this will need to change.
                  ;; call to super(super-fields)
                  ,@(if (null? super-fields) '()
                        (list (format superf (commas (map cadr super-fields)))))
                  |#
               ;; init for fields 
               ,@(map (lambda (f) (format initif (cadr f) (cadr f))) 
                      ;; When we switch to super in beginner, ... 
                      (append super-fields fields))
               ,endMet)))
  
  
  ;; Fields -> String 
  ;; create a paremeter list from a field specifications
  (define (parameters fs)
    (enumerate-with-separator
     (map (lambda (f) (format paraf (car f) (cadr f))) fs)))
  
  ;; String String Fields Boolean -> (listof String)
  (define (make-template sig super fields template? public?)
    (define head (format contef (method sig)))
    (if (not template?)
        (list "")
        `("\n"
          ;; template method
          ,cmnt/*
          ,warnin
          ,(if public? (string-append "  public " (string-trim head)) head)
          ,@(map (lambda (f) (if (string=? (car f) super)
                                 (format temprf (cadr f) (cadr sig))
                                 (format tempsf (cadr f))))
                 fields)
          ,endMet
          ,cmnt*/)))
  
  ;; Method -> String
  (define (method m)
    (define m+n (zip (cddr m) PARAS))
    (define sig (map (lambda (x) (format "~a ~a" (car x) (cadr x))) m+n))
    (format intff (car m) (cadr m) (enumerate-with-separator sig ",")))
  
  (define PARAS '("x" "y" "z" "u" "v" "w" "s" "t"))
  
  ;; String Fields -> (cons String (listof String))
  ;; create a toString method for class type with _fields_
  (define toString 
    (opt-lambda (type fields [special? #f][toString? #t])
      (if (not toString?)
          (list "")
          (list "\n"
                (string-append 
                 (if special? toStrf pbStrf)
                 (format prefix type)
                 ; (apply string-append)
                 (if (null? fields)
                     " + "
                     (format
                      " + ~a + " (enumerate-with-separator (map addToString fields) infix)))
                 postfix
                 endMet)))))
  
  ;(provide toString)
  
  ;; Field -> String
  ;; create a call to toString, if necessary 
  (define (addToString f)
    (let ([t (car f)] [s (cadr f)])
      (if (member t StringNative) s (format "~a.toString()" s))))
  
  
  ;; String -> (list String)
  (provide/contract
   [purpose-statement (-> string? (listof string?))])
  
  (define (purpose-statement prps)
    (if (string=? "" prps) '("") (list (format "// ~a~n" prps))))
  
  ;; identifiers ending in f are format strings, and require ~n for newline
  
  ;; Abstract Template 
  (define purpos "  // purpose statement \n")
  ;; Class 
  (define classf "class ~a ~a{~n") (define implementsf "implements ~a ")
  ;; Fields
  (define declaf "  ~a ~a;~n")
  ;; Constructor
  (define constf "  ~a(~a) {~n") (define paraf "~a ~a")
  (define superf "    super(~a);~n")
  (define initif "    this.~a = ~a;~n")
  (define endMet "  }\n")
  
  (provide endclf cmnt/* cmnt*/ warnin purpos)
  (define endclf "}\n")
  ;; Concrete Template
  (define warnin "  // ** DRAFT TEMPLATE ** Edit as needed.\n")
  (define intff  "~a ~a(~a)")
  (define contef "  ~a {\n")
  (define tempsf "    ... this.~a ...\n")
  (define temprf "    ... this.~a.~a(...) ...\n")
  ;; toString
  (define pbStrf "  public String toString() {\n")
  (define toStrf "  String toString() {\n")
  (define prefix "    return \"new ~a(\"")
  (define infix " + \",\" + ")
  (define postfix "\")\";\n")
  (define StringNative '("int" "boolean" "String" "double")) ;; and others
  ;; Comments
  (define cmnt/* "/*\n")
  (define cmnt*/ "*/\n")
  
  ;; ------------------------------------------------------------------------
  ;; Library 
  
  (provide/contract
   [enumerate-with-separator (opt-> ((listof string?)) (string?) string?)])
  
  ;; (Listof String) -> String
  ;; create a comma-separated string from a list of strings 
  (define (enumerate-with-separator l . comma)
    (let ([comma (if (null? comma) ", " (car comma))])
      (if (null? l) ""
          (apply string-append (car l)
                 (map (lambda (x) (string-append comma x)) (cdr l))))))
  
  ;; ------------------------------------------------------------------------
  ;; TESTS: 
  
  (define empty-template 
    `("/*\n" 
      ,warnin
      "  ??? mmm() {\n"
      "  }\n"
      "*/\n"
      "\n"))
  
  (define public-empty-template 
    `("/*\n" 
      ,warnin
      "  public ??? mmm() {\n"
      "  }\n"
      "*/\n"
      "\n"))
  
  #| Tests :
  (require (lib "testing.scm" "testing"))
  
  (test== (enumerate-with-separator '()) "")
  (test== (enumerate-with-separator '("x" "y") ) "x, y")
  (test== (enumerate-with-separator '("x" "y") " + ") "x + y")
  
  (test== (parameters '()) "")
  (test== (parameters '(("int" "x"))) "int x")
  (test== (parameters '(("int" "x") ("foo" "y"))) "int x, foo y")
  (test== (enumerate-with-separator '("x" "y") " + ") "x + y")
  (test== (enumerate-with-separator '("x" "y" "z") " + ") "x + y + z")
  
  (test== (cadr (toString "foo" '()))
          (string-append
           pbStrf
           "    return \"new foo(\" + \")\";\n"
           endMet))
  
  (test== (cadr (toString "foo" '() #t))
          (string-append
           toStrf
           "    return \"new foo(\" + \")\";\n"
           endMet))
  
  (test== (cadr (toString "Foo" '(("Foo" "x") ("Moo" "y"))))
          (string-append
           pbStrf
           "    return \"new Foo(\" + x.toString() + \",\" + y.toString() + \")\";\n"
           endMet))
  
  (test== (cadr (toString "Foo" '(("Foo" "x") ("Moo" "y")) #t))
          (string-append
           toStrf
           "    return \"new Foo(\" + x.toString() + \",\" + y.toString() + \")\";\n"
           endMet))
  
  (test== (cadr (toString "Foo" '(("int" "x"))))
          (string-append
           pbStrf
           "    return \"new Foo(\" + x + \")\";\n"
           endMet))
  
  (test== (cadr (toString "Foo" '(("Boolean" "x"))))
          (string-append
           pbStrf
           "    return \"new Foo(\" + x.toString() + \")\";\n"
           endMet))
  
  (test== (cadr (toString "Foo" '(("Foo" "x") ("int" "y") ("Z" "z"))))
          (string-append
           pbStrf
           "    return \"new Foo(\" + x.toString() + \",\" + y + \",\" + z.toString() + \")\";\n"
           endMet))
  
  (test== (toString "Foo" '(("Foo" "x") ("int" "y") ("Z" "z")) #f #f)
          (list ""))
  
  (test== (class-constructor "Node" '(("int" "x")) '(("Info" "i") ("ATree" "parent")))
          (string-append
           "  Node(Info i, ATree parent, int x) {\n"
           "    this.i = i;\n"
           "    this.parent = parent;\n"
           "    this.x = x;\n"
           "  }\n"
           )
          "class constructor with super fields")
  
  
  (test== (make-class (list "foo" "" '()) #t #t INTERMEDIATE '() '(("???" "mmm")))
          (apply string-append 
                 `( "class foo {\n"
                    "\n"
                    "  foo() {\n"
                    "  }\n"
                    "\n"
                    ,@empty-template
                    ,toStrf
                    "    return \"new foo(\" + \")\";\n"
                    "  }\n"
                    "}\n"))
          "class with template in INTERMEDIATE")
  
  (test== (make-class (list "moo" "foo" '())  #t #t PROFESSIONAL '() '(("???" "mmm")))
          (apply string-append 
                 `("class moo implements foo {\n"
                   "\n"
                   "  moo() {\n"
                   "  }\n"
                   "\n"
                   ,@public-empty-template
                   ,pbStrf
                   "    return \"new moo(\" + \")\";\n"
                   "  }\n"
                   "}\n")))
  
  (test== (make-class (list "moo" "foo" '(("int" "x") ("foo" "f")))  #t #t PROFESSIONAL '() '(("???" "mmm")))
          (apply string-append
                 `("class moo implements foo {\n"
                   "  int x;\n"
                   "  foo f;\n"
                   "\n"
                   "  moo(int x, foo f) {\n"
                   "    this.x = x;\n"
                   "    this.f = f;\n"
                   "  }\n"
                   "\n"
                   ,cmnt/*
                   ,warnin
                   "  public ??? mmm() {\n"
                   "    ... this.x ...\n"
                   "    ... this.f.mmm(...) ...\n"
                   "  }\n"
                   ,cmnt*/
                   "\n"
                   ,pbStrf
                   "    return \"new moo(\" + x + \",\" + f.toString() + \")\";\n"
                   "  }\n"
                   "}\n")))
  
  (test== (make-class (list "CartPt" "" '(("int" "x") ("int" "y")))  #t #t PROFESSIONAL '() '(("???" "mmm")))
          (apply string-append
                 `("class CartPt {\n"
                   "  int x;\n"
                   "  int y;\n"
                   "\n"
                   "  CartPt(int x, int y) {\n"
                   "    this.x = x;\n"
                   "    this.y = y;\n"
                   "  }\n"
                   "\n"
                   ,cmnt/*
                   ,warnin
                   "  public ??? mmm() {\n"
                   "    ... this.x ...\n"
                   "    ... this.y ...\n"
                   "  }\n"
                   ,cmnt*/
                   "\n"
                   ,pbStrf
                   "    return \"new CartPt(\" + x + \",\" + y + \")\";\n"
                   "  }\n"
                   "}\n")))
  
  (test== (make-class (list "foo" "" '() "hello world")  #t #t PROFESSIONAL '() '(("???" "mmm")))
          (apply string-append 
                 `( "// hello world\n"
                    "class foo {\n"
                    "\n"
                    "  foo() {\n"
                    "  }\n"
                    "\n"
                    ,@public-empty-template
                    ,pbStrf
                    "    return \"new foo(\" + \")\";\n"
                    "  }\n"
                    "}\n")))
  
  (test== (make-class (list "foo" "" '() "hello world") #t #t INTERMEDIATE '() '(("???" "mmm")))
          (apply string-append 
                 `( "// hello world\n"
                    "class foo {\n"
                    "\n"
                    "  foo() {\n"
                    "  }\n"
                    "\n"
                    ,@empty-template
                    ,toStrf
                    "    return \"new foo(\" + \")\";\n"
                    "  }\n"
                    "}\n")))
 |#
  )

