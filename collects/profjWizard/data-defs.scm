#| Data Defs

 Class          = (list Name SuperClass Fields [Comment])
 ;; the name of the class, the name of the supertype ("" if none), and 
 ;; the class's fields

 DataType       = (make-union TypeName Fields VariantClasses Comment)
 ;; the name of the type and its variants
 
 VariantClasses = (Listof VariantClass)
 VariantClass   = (list Name Fields [Comment])

 Name           = String 
 TypeName       = String 
 SuperClass     = String 
 Fields         = (Listof Field)
 Field          = (list String String)
|#

#cs
(module data-defs mzscheme 
  
  (define-struct dt (type fields variants purpose))
  
  ;; Examples
  (define field1 '("int" "x"))
  (define field2 '("int" "y"))
  (define field3 '("boolean" "b"))
  (define fields (list field1 field2 field3))
  (define vc1    (list "Leaf" (list field1)))
  (define vc2    (list "Node" '(("ATree" "left") ("ATree" "right"))))
  (define datat1 (make-dt "ATree" '() (list vc1 vc2) "a tree for ints"))
  
  (require (file "aux-contract.scm"))
  (require (lib "contract.ss"))
  
  (provide 
   Class   ;; flat-contract
   Union   ;; flat-contract 
   Variant ;; flat-contract 
   Fields  ;; flat-contract
   java-id? ;; Any -> Boolean
   class-purpose ;; Class -> String
   variant-purpose ;; Variant -> String
   )
  
  ;; DataType -> String
  ;; (define (union-purpose dt) (if (null? (cddr dt)) "" (caddr dt)))
  
  ;; Class -> String
  (define (class-purpose c) (if (null? (cdddr c)) "" (cadddr c)))
  
  ;; Variant -> String 
  (define (variant-purpose c) (if (null? (cddr c)) "" (caddr c)))
  
  ;; Any -> Boolean
  ;; the string isn't empty and contains no spaces 
  ;; I should really import this from Kathy's parser or whatever
  ;; so I get qualified names and whatever right
  (define (java-id? s)
    (and (string? s) (not (string=? "" s)) (not (regexp-match "[ |\t|\n]" s))))
  
  (define-as-contract "<Class>" (class c)
    (and (pair? c) (pair? (cdr c)) (pair? (cddr c)) 
         (or (null? (cdddr c))
             (and (pair? (cdddr c))
                  (null? (cddddr c))
                  (string? (cadddr c))))
         ; (list? c) (= (length c) 3)
         (java-id? (car c))
         (let ([super (cadr c)])
           (or (java-id? super) (string=? super "")))
         (is-fields? (caddr c))))
  
  (define-as-contract "<Fields>" (fields l)
    (and (list? l) (andmap is-field? l)))
  
  (define-as-contract "<Field in Class>" (field l)
    (and (pair? l) (pair? (cdr l)) (null? (cddr l))
         (java-id? (car l)) (java-id? (cadr l))))
  
  (define-as-contract "<Union>" (union l) (dt? l))
  
  (define (is-variants? l) (andmap is-variant? l))
  
  (define-as-contract "<Variant>" (variant c) 
    (and (pair? c) (pair? (cdr c)) 
         (or
          (null? (cddr c))
          (and
           (pair? (cddr c))
           (null? (cdddr c))
           (string? (caddr c)))
          ; (list? c) (= (length c) 2)
          (java-id? (car c))
          (is-fields? (cadr c)))))
  
  (provide/contract 
   (struct dt ((type java-id?)
               (fields (listof is-field?))
               (variants (listof is-variant?))
               (purpose string?))))
  
  
  #| Tests:   
  (require (lib "testing.scm" "testing"))
  
  (test== (java-id? "oops no") #f)
  (test== (java-id? " oops 2") #f)
  (test== (java-id? " oops2 ") #f)
  (test== (java-id? "") #f)
  (test== (java-id? (string #\tab)) #f)
  (test== (java-id? (string #\newline)) #f)
  
  (test== (is-class? '("Foo" "" ())) #t)
  (test== (is-class? '("Foo" "" () "hello world")) #t)
  (test== (is-class? '("Foo" "Moo" (("int" "x") ("int" "y")) "hello world")) #t)
  
  (test== (is-class? '("Foo" "Moo")) #f "no fields")
  (test== (is-class? '("Foo" "Moo Oops" ())) #f "space in super name")
  
  (test== (class-purpose '("a" "b" ())) "")
  (test== (class-purpose '("a" "b" () "hello world")) "hello world")
  
  (test== (is-variant? (list "B" '())) #t "variant class")
  (test== (andmap is-variant?  (list (list "B" '()) (list "C" '()))) #t "variants")
  (test== (java-id? "A") #t)
  (test== (is-union? (make-dt "A" '() (list (list "B" '()) (list "C" '())) "")) #t)
  |#
  
  )
