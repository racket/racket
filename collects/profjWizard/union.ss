#cs
(module union mzscheme 
  (require "data-defs.scm"
           "class.scm"
           mzlib/contract
           mzlib/etc 
           mzlib/list)
  

  (provide/contract
   [make-union ((Union) (boolean? boolean? string?) . opt-> . string?)]
   )

  #| Usage: 
         (make-union a-union toString? template?)
        create a Java implementation of a datatype specification using an 
        interface and N variants.

        The first optional boolean parameter specifies whether we want toString 
        in the class hiearchy. Default: true

        The second optional boolean parameter specifies whether we want 
        a draft method template in comments. Default: true
     |#
  
  ;; ------------------------------------------------------------------------
  ;; Construct a union as a collection of Java classes 
  
  ;; String (Listof VariantClass) -> String 
  (define make-union
    (opt-lambda (the-union [toString? #t][template? #t][language BEGINNER])
      (define type (dt-type the-union))
      (define met* (dt-methods the-union)) 
      (string-append
       (car (purpose-statement (dt-purpose the-union)))
       (interface type template? met*)
       "\n"
       (enumerate-with-separator
        (map (lambda (sc)
               (make-class `(,(car sc) ,type ,(cadr sc) ,(variant-purpose sc))
                           toString? 
                           template?
                           language
                           '()
                           met*))
             (dt-variants the-union))
        "\n"))))
  
  ;; String Boolean [Listof Method] -> String 
  (define (interface type template? methods)
    (apply string-append 
           `(,(format abstractF type)
              ;; constant fields, e.g., 
              ;;   int NORTH = 0;
              ;; shouldn't be allowed yet
              ,@(map (lambda (x) (format "  ~a;\n" (method x))) methods)
              ;; optional abstract template 
              ""
              ,endclf)))

  ;; String (listof Field) Boolean? -> String
  (define (abstractClass type fields template?)
    `(...
      ,@(if (not template?)
            (list "")
            `( ,cmnt/*
                ,warnin
                ,purpos                            
                ,absteg
                ,cmnt*/))
      ...))
  
  ;; Abstract Class 
  (define abstractF "interface ~a {\n")
  (define absteg    "  abstract ??? mmm();\n")       
  
  #| Tests: 
  
  (require (lib "testing/testing.scm"))
  
  (test== (interface "Foo" #f '())
          (string-append 
           "interface Foo {\n"
           "}\n")
          "simple interface")

  (test== (interface "Foo" #f '(("double" "distance" "int" "int")))
          (string-append 
           "interface Foo {\n"
           "  double distance(int x,int y);\n"
           "}\n")
          "simple interface with methods")

  (make-union
    (make-dt "AList" '() '(("MT" ()) ("Cons" (("int" "first") ("AList" "rest")))) "")
    #f #f)
  (test== 
   (make-union
    (make-dt "AList" '() '(("MT" ()) ("Cons" (("int" "first") ("AList" "rest")))) "")
    #f #f)
   (apply string-append 
          `(
            "interface AList {\n"
            "}\n"
            "\n"
            "class MT implements AList {\n"
            "\n"
            "  MT() {\n"
            "  }\n"
            "}\n"
            "\n"
            "class Cons implements AList {\n"
            "  int first;\n"
            "  AList rest;\n"
            "\n"
            "  Cons(int first, AList rest) {\n"
            "    this.first = first;\n"
            "    this.rest = rest;\n"
            "  }\n"
            "}\n"
            )
          )
   "partial make union")
  
  (test== 
   (make-union
    (make-dt "AList"
             ;; common methods: 
             '(("???" "mmm"))
             ;; variants: 
             '(("MT" ()) 
               ("Cons" (("int" "first") ("AList" "rest")))) "")
    #t #t PROFESSIONAL)
   (apply string-append 
          `(
            "interface AList {\n"
            "  ??? mmm();\n"
            "}\n"
            "\n"
            "class MT implements AList {\n"
            "\n"
            "  MT() {\n"
            "  }\n"
            "\n"
            "/*\n"
            ,warnin
            "  public ??? mmm() {\n"
            "  }\n"
            "*/\n"
            "\n"
            "  public String toString() {\n"
            "    return \"new MT(\" + \")\";\n"
            "  }\n"
            "}\n"
            "\n"
            "class Cons implements AList {\n"
            "  int first;\n"
            "  AList rest;\n"
            "\n"
            "  Cons(int first, AList rest) {\n"
            "    this.first = first;\n"
            "    this.rest = rest;\n"
            "  }\n"
            "\n"
            "/*\n"
            ,warnin
            "  public ??? mmm() {\n"
            "    ... this.first ...\n"
            "    ... this.rest.mmm(...) ...\n"
            "  }\n"
            "*/\n"
            "\n"
            "  public String toString() {\n"
            "    return \"new Cons(\" + first + \",\" + rest.toString() + \")\";\n"
            "  }\n"
            "}\n"
            )
          )
   "full make union")
  
  (test== 
   (make-union
    (make-dt "AList" '() '(("MT" ()) ("Cons" (("int" "first") ("AList" "rest")))) "hello world")
    #f #f)
   (apply string-append 
          `("// hello world\n"
            "interface AList {\n"
            "}\n"
            "\n"
            "class MT implements AList {\n"
            "\n"
            "  MT() {\n"
            "  }\n"
            "}\n"
            "\n"
            "class Cons implements AList {\n"
            "  int first;\n"
            "  AList rest;\n"
            "\n"
            "  Cons(int first, AList rest) {\n"
            "    this.first = first;\n"
            "    this.rest = rest;\n"
            "  }\n"
            "}\n"
            )
          )
   "make union with purpose statement")
  
  (test== 
   (make-union
    (make-dt
     "AList" '() '(("MT" ()) ("Cons" (("int" "first") ("AList" "rest")) "pair")) "hello world")
    #f #f)
   (apply string-append 
          `("// hello world\n"
            "interface AList {\n"
            "}\n"
            "\n"
            "class MT implements AList {\n"
            "\n"
            "  MT() {\n"
            "  }\n"
            "}\n"
            "\n"
            "// pair\n"
            "class Cons implements AList {\n"
            "  int first;\n"
            "  AList rest;\n"
            "\n"
            "  Cons(int first, AList rest) {\n"
            "    this.first = first;\n"
            "    this.rest = rest;\n"
            "  }\n"
            "}\n"
            )
          )
   "make union with purpose statement for variants")
  |#
  )
