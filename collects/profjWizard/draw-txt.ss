#cs
(module draw-txt mzscheme
  (require "data-defs.scm"
           "class.scm"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "contract.ss"))
  
  (provide/contract
   [dt-draw    (Union . -> . string?)]
   [class-draw ((Class) (listof Method) . opt-> . string?)])
  
  ;; ---------------------------------------------------------------------------
  ;; Deal with a Union of classes 
  
  ;; DataType -> String
  (define (dt-draw dt)
    (define spr (dt-type dt))
    (define vts (dt-variants dt))
    (define mt* (dt-methods dt))
    ;; String -> Number
    (define (vbar-pos s)
      (let loop ([i (- (string-length s) 1)])
        (cond [(< i 0) (error 'find-bar "can't happen: ~e" s)]
              [(char=? (string-ref s i) #\|) i]
              [else (loop (- i 1))])))
    (if (null? vts)
        (class-draw (list spr "" '()))
        (let-values ([(vts:str rec?) (variants*-to-strings vts spr mt*)])
          (define wth (sum (map (lambda (x) (string-length (car x))) vts:str)))
          (define fst-vrt (car (car vts:str)))
          (define lst-vrt (car (last vts:str)))
          (define fst-bar (vbar-pos fst-vrt))
          (define lst-bar (vbar-pos lst-vrt))
          (define diagram 
            (append
             (type-to-string spr mt* wth rec?)
             (refinement-connector wth fst-bar (string-length lst-vrt) lst-bar)
             (flatten-string-matrix vts:str)))
          (define d/recur (if rec? (add-recursion-connector diagram) diagram))
          (strings->string-as-lines d/recur))))
  
  ;; (Listof String) -> (Listof String)
  ;; add 
  ;; 
  ;;  --+ // on second line 
  ;;    |
  ;;  --+ // on last line 
  ;; to los 
  (define (add-recursion-connector los)
    (unless (and (andmap string? los) (>= (length los) 3))
      (error 'add-recursive-connector "bad inputs: ~s\n" los))
    
    (let* ([fst (car los)]
           [snd (cadr los)]
           [lst (last los)]
           [BLK "   "]
           [CON "--+"]
           [LIN "  |"])
      (define (frame l)
        (cond
          [(null? (cdr l)) (list (string-append lst CON))]
          [else (cons (string-append (car l) LIN) (frame (cdr l)))]))
      (cons (string-append fst BLK)
            (cons (string-append snd CON) (frame (cddr los))))))
  
  ;; Number Number Number Number -> (Listof String)
  ;; create a refinement connector of the proper width like this
  ;; 
  ;;      |
  ;;     / \
  ;;     ---
  ;;      |
  ;; +----------+
  ;; |          |
  ;; 
  (define (refinement-connector width frst-bar width-of-last last-bar)
    (define REFINEMENT-ARROW
      (list " | "
            "/ \\"
            "---"
            " | "))
    (append 
     (map (lambda (x) (centered x width)) REFINEMENT-ARROW)
     (list (string-append 
            (make-string frst-bar #\space)
            (make-string (- width frst-bar (- width-of-last +1 last-bar)) #\-)
            (make-string (- width-of-last +1 last-bar) #\space)))))
  
  ;; Class Fields Number Boolean-> (Listof String)
  ;; create a list of strings that represent the abstract class of a union 
  ;; center the strings with respect to width, add a "recursion" arrow (needed?)
  (define (type-to-string spr methods width recursive)
    (define (range-contract result)
      (unless (= (string-length (car result)) width)
        (error 'type-to-string "bad result: ~s\n" result))
      result)
    (define class-as-strings (class-to-strings (list spr "" '()) methods))
    (define width-of-classes (string-length (car class-as-strings)))
    (define super-line       (centered (cadr class-as-strings) width))
    (range-contract
     (cons
      (centered (car class-as-strings) width)
      (cons (if recursive (add-<-- super-line) super-line)
            (map (lambda (x) (centered x width)) (cddr class-as-strings))))))
  
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
  (define (variants*-draw variants spr methods)
    (let-values ([(s b) (variants*-to-strings variants spr methods)])
      (flatten-string-matrix s)))
  
  ;; VariantClasses Super (Listof Method) -> (Listof (Listof String)) Boolean
  ;; turns a list of Variants into a list of strings, one per line 
  (define (variants*-to-strings variants spr methods)
    (let* ([d (apply max (map (lambda (vc) (length (second vc))) variants))]
           [recursion #f])
      (values 
       (let loop ([v variants][cnnctd #f])
         (cond
           [(null? v) (set! recursion cnnctd) '()]
           [else (let-values ([(s b) (variant-to-strings (car v) spr cnnctd d methods)])
                   (cons s (loop (cdr v) (or cnnctd b))))]))
       ;; ordering: begin
       recursion)))
  
  ;; VariantClass Super Boolean Number -> String 
  #;(define (variant-draw class super left-connected depth)
    (define-values (s b) (variant-to-strings class super left-connected depth))
    (strings->string-as-lines s))
  
  ;; VariantClass Super Boolean Number (Listof Method) ->* String Boolean 
  ;; turns a variant class into a list of strings, 
  ;; computes whether the class points to super
  ;; with hooks for refinement arrows and for recursive containment arrows
  ;; with depth :: max number of fields in a variant class of the uion 
  ;; with left-connected :: whether or not a variant to the left is already rec
  ;; with super :: the name of the datatype
  (define (variant-to-strings variant super left-connected depth methods)
    (let* ([cs (class-to-strings (cons (car variant) (cons super (cdr variant))) methods)]
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
           [junk  (lambda _ (symbol->string (gensym)))]
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
             (append (map junk head) types (list (junk)) 
                     (map junk methods)
                     (if (null? methods) '() (list (junk)))))
        (build-list (- depth -1 (length fields))
                    (lambda _  (mkln BLK #\space (if recursion LIN BLK))))
        (list
         (if left-connected
             (mkln STG #\- (if recursion CON STG))
             (mkln BLK #\space (if recursion CN2 BLK)))))
       recursion)))
  
  ;; ---------------------------------------------------------------------------
  ;; Deal with a single class 
  
  ;; Class [(listof Method)] opt-> String
  (define (class-draw class . ms) 
    (strings->string-as-lines (apply class-to-strings class ms)))
  
  ;; Class [(Listof Method)] opt-> (cons String (cons String (cons String (Listof String))))
  ;; turns a class into a list of strings that represents the class 
  (define (class-to-strings class . ms)
    (let* ([name    (first class)]
           [super   (second class)]
           [fields  (third class)]
           [types   (map first fields)]
           [names   (map second fields)]   
           ;; start drawing 
           [fields  (create-field-declarations fields)]                         
           [methds  (if (pair? ms) (apply create-method-declarations ms) ms)]
           [width   (width-class name (append methds fields))]
           [separat (make-separator-line width)])
      `(,separat
        ,((make-line width) name)
        ,separat
        ,@(map (make-line width) fields)
        ,separat
        ,@(if (null? methds) '() (map (make-line width) methds))
        ,@(if (null? methds) '() (list separat)))))
  
  ;; (Listof Field) -> (Listof String)
  ;; create text lines from Fields
  (define (create-field-declarations fields)
    (map (lambda (f) (string-append (string-append (first f) " " (second f)))) 
         fields))
  
  ;; (Listof Method) -> (Listof String)
  ;; create text lines from Methods 
  (define (create-method-declarations fields) (map method fields))
  
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
  
  ;; (cons X (listof X)) -> X 
  (define (last l) (car (last-pair l)))
  
  ;; (Listof Number) -> Number
  (define (sum l) (foldr + 0 l))
  
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
  
  (test== (class-draw class1 '()) (strings->string-as-lines expected-class))
  (test== (class-draw class2 '()) (strings->string-as-lines expected-class2))
  
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
  
  (test== (let-values ([(s b) (variant-to-strings vclass1 "Super" #f 3 '())]) s)
          expected-variant1)
  #;(test== (variant-draw vclass1 "Super" #f 3) 
          (strings->string-as-lines expected-variant1))
  #;(test== (variant-draw vclass2 "Super" #f 3) 
          (strings->string-as-lines expected-variant2))
  #;(test== (variant-draw vclass3 "Super" #t 3) 
          (strings->string-as-lines expected-variant3))
  
  
  (test== (let-values
              ([(s b) 
                (variants*-to-strings (list vclass1 vclass2 vclass3) "Super" '())])
            s)
          (list expected-variant1 expected-variant2 expected-variant3))
  
  (test== (variants*-draw (list vclass1 vclass2 vclass3) "Super" '()) 
          (flatten-string-matrix
           (list expected-variant1 expected-variant2 expected-variant3)))
  
  (define aclass-exp ;; 19
    (list "     +-------+     "
          "     | Super |<----"
          "     +-------+     "
          "     +-------+     "))
  
  (test== (type-to-string "Super" '() 19 #t)
          aclass-exp)
  
  (test== (dt-draw
           (make-dt "Class"
                    '()
                    '()
                    ""))
          (strings->string-as-lines expected-class2))
  
  (dt-draw (make-dt "Super" '() (list vclass1 vclass2 vclass3) ""))
  
  (test== (dt-draw 
           (make-dt "Super" '() (list vclass1 vclass2 vclass3) ""))
          (strings->string-as-lines
           '(
             "                    +-------+                       "
             "                    | Super |<---------------------+"
             "                    +-------+                      |"
             "                    +-------+                      |"
             "                        |                          |"             
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
  
  
  (string-delta (dt-draw
           (make-dt "Super" 
                    '(("int" "x"))
                    '(("VC1" (("int" "x")))
                      ("VC2" (("boolean" "b") ("int" "y")))
                      ("VC3" (("String" "s"))))
                    ""))
          (strings->string-as-lines
           '(
             "                 +---------+                  "
             "                 | Super   |                  "
             "                 +---------+                  "
             "                 +---------+                  "             
             "                 | int x() |                  "
             "                 +---------+                  "
             "                      |                       " 
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
