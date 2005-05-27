#cs
(module draw-txt mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "contract.ss")
           (file "data-defs.scm"))
  
  (provide/contract
   [dt-draw    (Union . -> . string?)]
   [class-draw (Class . -> . string?)])
  
  ;; ---------------------------------------------------------------------------
  ;; Deal with a Union of classes 
  
  ;; DataType -> String
  (define (dt-draw dt)
    (let ([spr (dt-type dt)]
	  [vts (dt-variants dt)])
      (if (null? vts)
	  (class-draw (list spr "" (dt-fields dt)))
    (let*-values 
        ([(vts-as-strings recursive?) (variants*-to-strings vts spr)]
         [(width)
          (apply + (map (lambda (x) (string-length (car x))) vts-as-strings))]
         ;; calculate the connection point for the first and last variant
         ;; then create a line that connects those two points 
         [(last)            (caar (last-pair vts-as-strings))]
         [(fst-bar)         (find-bar (caar vts-as-strings))]
         [(last-bar)        (find-bar last)]
         [(width-of-last)   (string-length last)] 
         [(center-of-last)  (+ (- width width-of-last) (find-bar last))])
      (strings->string-as-lines
       (add-recursion-connector
        recursive?
        (append
         (abstract-to-string spr (dt-fields dt) width recursive?)
         (map (lambda (x) (centered x width)) REFINEMENT-ARROW)
         (list (string-append 
                (make-string fst-bar #\space)
                (make-string (- width fst-bar (- width-of-last +1 last-bar)) #\-)
                (make-string (- width-of-last +1 last-bar) #\space)))
         (flatten-string-matrix vts-as-strings))))))))
  
  ;; String -> Number
  (define (find-bar s)
    (let loop ([i (- (string-length s) 1)])
      (cond [(< i 0) (error 'find-bar "can't happen: ~e" s)]
            [(char=? (string-ref s i) #\|) i]
            [else (loop (- i 1))])))
 
  ;; (Listof String) -> (Listof String)
  ;; add 
  ;; 
  ;;  --+ // on second line 
  ;;    |
  ;;  --+ // on last line 
  ;; to los 
  (define/contract add-recursion-connector 
    (boolean?
     (and/c (listof string?) (lambda (los) (>= (length los) 3)))
     . -> .
     any)
    (lambda (r los)
      (if (not r) los
      (let* ([fst (car los)]
             [snd (cadr los)]
             [lst (car (last-pair los))]
             [BLK "   "]
             [CON "--+"]
             [LIN "  |"])
        (cons (string-append fst BLK)
              (cons (string-append snd CON)
                    (let loop ([l (cddr los)])
                      (cond
                        [(null? (cdr l)) (list (string-append lst CON))]
                        [else (cons (string-append (car l) LIN) 
                                    (loop (cdr l)))]))))))))
  
  (define REFINEMENT-ARROW
    (list "/ \\"
          "---"
          " | "))
  
  ;; Class Fields Number Boolean-> (Listof String)
  ;; create a list of strings that represent the abstract class of a union 
  ;; center the strings with respect to width, add a "recursion" arrow (needed?)
  (define/contract abstract-to-string
    (string? Fields natural-number/c boolean?
             . ->d . 
             (lambda (_1 _2 n _3)
               (lambda (out) (= (string-length (car out)) n))))
    (lambda (spr fields width recursive)
      (let* ([class-as-strings (class-to-strings (list spr "" fields))]
             [width-of-classes  (string-length (car class-as-strings))]
             [super-line (centered (cadr class-as-strings) width)])
        (cons
         (centered (car class-as-strings) width)
         (cons (if recursive (add-<-- super-line) super-line)
               (map (lambda (x) (centered x width)) 
                    (cddr class-as-strings)))))))
  
  ;; String -> String 
  ;; add the containment arrow to an abstract class from the right fringe
  (define (add-<-- x0) 
    (list->string
     (reverse!
      (let loop ([x (reverse! (string->list x0))])
        (cond
          [(char=? (cadr x) #\|) (cons #\< (cdr x))]
          [else (cons #\- (loop (cdr x)))])))))
  
  ;; VariantClasses Super -> (Listof String)
  ;; for testing and printing only 
  (define (variants*-draw variants spr)
    (let-values ([(s b) (variants*-to-strings variants spr)])
      (flatten-string-matrix s)))
  
  ;; VariantClasses Super -> (Listof (Listof String)) Boolean
  ;; turns a list of Variants into a list of strings, one per line 
  (define (variants*-to-strings variants spr)
    (let* ([d (apply max (map (lambda (vc) (length (second vc))) variants))]
           [recursion #f])
      (values 
       (let loop ([v variants][cnnctd #f])
         (cond
           [(null? v) (set! recursion cnnctd) '()]
           [else (let-values ([(s b) (variant-to-strings (car v) spr cnnctd d)])
                   (cons s (loop (cdr v) (or cnnctd b))))]))
       ;; ordering: begin
       recursion)))
  
  ;; VariantClass Super Boolean Number -> String 
  (define (variant-draw class super left-connected depth)
    (let-values ([(s b) (variant-to-strings class super left-connected depth)])
      (strings->string-as-lines s)))
  
  ;; VariantClass Super Boolean Number ->* String Boolean 
  ;; turns a variant class into a list of strings, 
  ;; computes whether the class points to super
  ;; with hooks for refinement arrows and for recursive containment arrows
  ;; with depth :: max number of fields in a variant class of the uion 
  ;; with left-connected :: whether or not a variant to the left is already rec
  ;; with super :: the name of the datatype
  (define (variant-to-strings variant super left-connected depth)
    (let* ([cs
            (class-to-strings (cons (car variant) (cons super (cdr variant))))]
           [head (list (car cs) (cadr cs) (caddr cs))]
           [tail (cdddr cs)]
           [fields (second variant)]
           [types (map first fields)]
           [recursion #f]
           [CON "-+"]
           [CN2 " +"]
           [STG "--"] ;; (= (string-length CON) (string-length BLK))
           [BLK "  "] ;; (= (string-length CON) (string-length BLK))          
           [LIN " |"] ;; (= (string-length CON) (string-length LIN))
           [junk (lambda _ (symbol->string (gensym)))]
           [width (string-length (car cs))]
           [mkln (lambda (lft ch str) 
                   (string-append lft (make-string width ch) str))])
      (values 
       (append
        (list (string-append BLK (centered "|" width) BLK))
        (map (lambda (line type)
               (string-append BLK 
                              line 
                              (cond
                                [(string=? type super) (set! recursion #t) CON]
                                [recursion LIN]
                                [else BLK])))
             cs
             ;; pad types with junk lines for class header and class bottom
             (append (map junk head) types (list (junk))))
        (build-list (- depth -1 (length fields))
                    (lambda _  (mkln BLK #\space (if recursion LIN BLK))))
        (list
         (if left-connected
             (mkln STG #\- (if recursion CON STG))
             (mkln BLK #\space (if recursion CN2 BLK)))))
       recursion)))
  
  ;; ---------------------------------------------------------------------------
  ;; Deal with a single class 
  
  ;; Class -> String
  (define (class-draw class) 
    (strings->string-as-lines (class-to-strings class)))
  
  ;; Class -> (cons String (cons String (cons String (Listof String))))
  ;; turns a class into a list of strings that represents the class 
  (define (class-to-strings class)
    (let* ([name    (first class)]
           [super   (second class)]
           [fields  (third class)]
           [types   (map first fields)]
           [names   (map second fields)]   
           ;; start drawing 
           [fields  (create-field-declarations fields)]                         
           [width   (width-class name fields)] 
           [separat (make-separator-line width)])
      `(,separat
         ,((make-line width) name)
         ,separat
         ,@(map (make-line width) fields)
         ,separat)))
  
  ;; (Listof Field) -> (Listof String)
  ;; create text lines from Fields
  (define (create-field-declarations fields)
    (map (lambda (f) (string-append (string-append (first f) " " (second f)))) 
         fields))
  
  ;; Number -> String
  ;; make a separator line (+----+) of width
  (define (make-separator-line width)
    (string-append 
     LFT+ 
     (make-string (- width (string-length LFT+) (string-length RGT+)) #\-)
     RGT+))
  
  ;; Number -> (String -> String)
  ;; make one line in class of width from txt
  (define (make-line width)
    (lambda (txt)
      (string-append 
       LEFT 
       txt
       (make-string
        (- width (string-length txt) (string-length LEFT) (string-length RIGHT))
        #\space)
       RIGHT)))
  
  ;; String (Cons String (Listof String)) -> Number
  ;; compute width of class as the widest field/name 
  (define (width-class name fields)
    (+ (string-length LEFT)
       ;; longest field spec of name/class and fields
       (apply max (map string-length (cons name fields)))
       (string-length RIGHT)))
  
  ;; ---------------------------------------------------------------------------
  ;; Library
  
  ;; (Listof String) -> String
  ;; turn the list of strings into a single string, separating lines with newline 
  (define (strings->string-as-lines s)
    (apply string-append (map (lambda (x) (string-append x "\n")) s)))
  
  
  ;; (Listof (Listof String)) -> (Listof String)
  ;; contract: (apply = (map length smatrix))
  ;; this requires Pretty Big, it could be written in Intermediate 
  (define (flatten-string-matrix smatrix) 
    (apply map (lambda l (apply string-append l)) smatrix))
  
    
  ;; String Number -> String
  ;; place str in the center of an otherwise blank string
  (define (centered str width)
    (let* ([len-str (string-length str)]
           [lft (quotient (- width len-str) 2)]
           [rgt (- width len-str lft)])
      (string-append (make-string lft #\space) str (make-string rgt #\space))))
    
  ;; ---------------------------------------------------------------------------
  ;; Constants
  
  (define LEFT  "| ")
  (define LFT+  "+-")
  (define RIGHT " |")
  (define RGT+  "-+") ; (= (string-length RIGHT) (string-length RGT+))
  ; (define HOOK  " |-o") ; (= (string-length RIGHT) (string-length HOOK))
  
  
  #|Tests: 
  (require (lib "testing.scm" "testing"))

  (test== (centered "|" 2) "| ")
  (test== (centered "|" 3) " | ")

  "testing classes"
  (define class1 (list "Class" "Super" '(("int" "capacity") ("hello" "world"))))
  (define class2 (list "Class" "Super" '()))
  
  (define expected-class
    (list
     "+--------------+"
     "| Class        |"
     "+--------------+"
     "| int capacity |"
     "| hello world  |"
     "+--------------+"))
  
  (define expected-class2
    (list
     "+-------+"
     "| Class |"
     "+-------+"
     "+-------+"))
  
  (test== (class-draw class1) (strings->string-as-lines expected-class))
  (test== (class-draw class2) (strings->string-as-lines expected-class2))
  
  ; (printf "~a~n" (class-draw class1))
  
  "testing variants"
  (define vclass1 (list "Variant1" '()))
  (define vclass2 (list "Variant2" '(("int" "x") ("boolean" "y") ("Super" "z"))))
  (define vclass3 (list "Variant3" '(("String" "x") ("Super" "y") ("Super" "z"))))
  
  (define expected-variant1
    (list
     "       |        "
     "  +----------+  "
     "  | Variant1 |  "
     "  +----------+  "
     "  +----------+  "
     "                "
     "                "
     "                "
     "                "
     "                "))
  
  (define expected-variant2
    (list
     "        |        "
     "  +-----------+  "
     "  | Variant2  |  "
     "  +-----------+  "
     "  | int x     |  "
     "  | boolean y |  "
     "  | Super z   |-+"
     "  +-----------+ |"
     "                |"
     "                +"))
  
  (define expected-variant3
    (list
     "       |        "
     "  +----------+  "
     "  | Variant3 |  "
     "  +----------+  "
     "  | String x |  "
     "  | Super y  |-+"
     "  | Super z  |-+"
     "  +----------+ |"
     "               |"
     "---------------+"))
  
  (test== (let-values ([(s b) (variant-to-strings vclass1 "Super" #f 3)]) s)
          expected-variant1)
  (test== (variant-draw vclass1 "Super" #f 3) 
          (strings->string-as-lines expected-variant1))
  (test== (variant-draw vclass2 "Super" #f 3) 
          (strings->string-as-lines expected-variant2))
  (test== (variant-draw vclass3 "Super" #t 3) 
          (strings->string-as-lines expected-variant3))
  
  
  (test== (let-values
              ([(s b) 
                (variants*-to-strings (list vclass1 vclass2 vclass3) "Super")])
            s)
          (list expected-variant1 expected-variant2 expected-variant3))
  
  (test== (variants*-draw (list vclass1 vclass2 vclass3) "Super") 
          (flatten-string-matrix
           (list expected-variant1 expected-variant2 expected-variant3)))

  (define aclass-exp ;; 19
    (list "     +-------+     "
          "     | Super |<----"
          "     +-------+     "
          "     +-------+     "))
  
  (test== (abstract-to-string "Super" '() 19 #t)
          aclass-exp)
  
  (test== (dt-draw
           (make-dt "Class"
                    '()
                    '()
                    ""))
          (strings->string-as-lines expected-class2))
  
  (test== (dt-draw 
           (make-dt "Super" '() (list vclass1 vclass2 vclass3) ""))
          (strings->string-as-lines
           '(
"                    +-------+                       "
"                    | Super |<---------------------+"
"                    +-------+                      |"
"                    +-------+                      |"
"                       / \\                         |" ;; note escape 
"                       ---                         |"
"                        |                          |"
"       ----------------------------------          |"
"       |                |               |          |"
"  +----------+    +-----------+    +----------+    |"
"  | Variant1 |    | Variant2  |    | Variant3 |    |"
"  +----------+    +-----------+    +----------+    |"
"  +----------+    | int x     |    | String x |    |"
"                  | boolean y |    | Super y  |-+  |"
"                  | Super z   |-+  | Super z  |-+  |"
"                  +-----------+ |  +----------+ |  |"
"                                |               |  |"
"                                +---------------+--+"
             )))
  
  (test== (dt-draw
           (make-dt "Super" 
                    '(("int" "x"))
                    '(("VC1" (("int" "x")))
                      ("VC2" (("boolean" "b") ("int" "y")))
                      ("VC3" (("String" "s"))))
                    ""))
          (strings->string-as-lines
          '(
"                  +-------+                   "
"                  | Super |                   "
"                  +-------+                   "
"                  | int x |                   "
"                  +-------+                   "
"                     / \\                      "
"                     ---                      "
"                      |                       "
"      --------------------------------        "
"      |              |               |        "
"  +-------+    +-----------+    +----------+  "
"  | VC1   |    | VC2       |    | VC3      |  "
"  +-------+    +-----------+    +----------+  "
"  | int x |    | boolean b |    | String s |  "
"  +-------+    | int y     |    +----------+  "
"               +-----------+                  "            
"                                              "
"                                              "
            )))
  
  |#
)
