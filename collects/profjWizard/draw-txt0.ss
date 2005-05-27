#| Class Representation: 
   
   Class = (list String (Listof Field))
   Field = (list String String)

|#
(module draw-txt mzscheme
  (require (lib "etc.ss")
           (lib "list.ss"))
  
  ;; Class (Listof Classes) -> (Listof String)
  (define (class-union-to-strings utype variants) 
    (let* ([ac (class-to-strings utype)]
           [classes (classes-to-strings variants (car utype))]
           [v  (flatten-string-matrix classes)]
           [Lv (string-length (first v))]
           [the-core
            (append 
             (center-picture Lv ac)
             (center-picture Lv REFINEMENT-ARROW)
             (center-picture Lv (refinement-connectors classes))
             v)]
           [foo 0])
      (map (lambda (x) 
             (set! foo (+ foo 1))
             (cond
               [(> foo 2) (string-append x "|")]
               [(= foo 1) x]
               [(= foo 2) (replace-end-with-back-arrow x)]))
           the-core)))
  
  ;; String -> String 
  ;; add the containment arrow to an abstract class from the right fringe
  (define (replace-end-with-back-arrow x0)
    (list->string
     (reverse!
      (cons #\+ 
            (let loop ([x (reverse (string->list x0))])
              (cond
                [(char=? (cadr x) #\|) (cons #\< (cdr x))]
                [else (cons #\- (loop (cdr x)))]))))))
  
  ;; (Listof String) -> (Listof String)
  (define (refinement-connectors class-pictures)
    (let ([center-char 
           (lambda (line c l r)
             (car (center-picture (string-length line) (list c)  l r)))])
      (list 
       (string-append
        (center-char (caar class-pictures) "+" #\space #\-)
        (let loop ([cp (rest class-pictures)])
          (cond
            [(null? (rest cp))
             (center-char (caar cp) "+" #\- #\space)]
            [else (string-append (center-char (caar cp) "+" #\- #\-)
                                 (loop (rest cp)))])))
       (foldr (lambda (f r) 
                (string-append (car (center-picture (string-length (car f)) (list "|"))) r))
              ""
              class-pictures)
       )))
  
  ;; Number (Listof String) -> (Listof String)
  (define center-picture 
    (opt-lambda (Lv ac (l #\space)[r #\space])
      (let* ([delta (- Lv (string-length (first ac)))]
             [lft (quotient delta 2)])
        (map (pad-lines (make-string lft l) (make-string (- delta lft) r)) ac))))
  
  (define REFINEMENT-ARROW
    (list "/ \\"
          "---"
          " | "))
  
  ;; Class String *-> String
  (define (classes-draw classes . super) 
    (strings->string-as-lines (apply classes-to-strings classes super)))
  
  ;; Class -> String
  (define (class-draw class) 
    (strings->string-as-lines (class-to-strings class)))
  
  ;; (Listof Class) String *-> (Listof (Listof String))
  ;; take a list of classes and produce a single "line" (listof string-lines)
  (define (classes-to-strings classes0 . super)
    (let* (;; (Listof (Listof String))
           [classes (map (lambda (c) (apply class-to-strings c super)) classes0)] 
           [L (apply max (map length classes))]
           [FOO "   "]
           [classes (foldr (lambda (class-los is-self-recursive rest)
                             (if (null? super)
                                 (map (pad-lines FOO FOO)
                                    (if (>= (length class-los) L)
                                        class-los
                                        (pad-class-with-blank-lines L class-los)))
                             (cons
                              (append 
                               (map (pad-lines FOO FOO)
                                    (if (>= (length class-los) L)
                                        class-los
                                        (pad-class-with-blank-lines L class-los)))
                               (list
                                (case is-self-recursive
                                  [(long)
                                   (string-append 
                                    "--"
                                    (make-string (string-length (first class-los)) #\-)
                                    CROSS
                                    (if (null? rest) "--" ""))]
                                  [(short)
                                   (string-append 
                                    "  "
                                    (make-string (string-length (first class-los)) #\space)
                                    CROSS
                                    (if (null? rest) "--" ""))]
                                  [(conn)
                                   (string-append 
                                    "--"
                                    (make-string (string-length (first class-los)) #\-)
                                    NOCROSS
                                    (if (null? rest) "--" ""))
                                   ]
                                  [(none)
                                   (string-append 
                                    "  "
                                    (make-string (string-length (first class-los)) #\space)
                                    FOO)])))
                              rest)))
                           '() 
                           classes
                           (let loop ([c classes0][prior #f])
                             (cond
                               [(null? c) '()]
                               [else (let ([self-recursive (apply is-recursive? (car c) super)])
                                       (cond
                                         [(and self-recursive prior) (cons 'long (loop (cdr c) #t))]
                                         [self-recursive (cons 'short (loop (cdr c) #t))]
                                         [prior (cons 'conn (loop (cdr c) #t))]
                                         [else (cons 'none (loop (cdr c) #f))]))])))])
      classes))
  
  
  
  (define is-recursive? 
    (case-lambda 
      [(class) #f]
      [(class super)
       (let* ([name    (first class)]
              [fields  (second class)]
              [types   (map first fields)])
         (member super types))]))
  
  
  (define CROSS "-+-")
  (define NOCROSS "---")
  
  ;; Number (Cons String (Listof String)) -> (Listof String)
  (define (pad-class-with-blank-lines n l)
    (let ([blanks (make-string (string-length (first l)) #\space)])
      (append l (build-list (- n (length l)) (lambda (i) blanks)))))
  
  ;; Class String *-> (Listof String)
  ;; the presence of super suggests that we want to draw a line to super 
  (define (class-to-strings class . super)
    (let* ([is-super? (if (null? super)
                          (lambda (x) false) 
                          (lambda (x) (string=? (car super) x)))]
           [name    (first class)]
           [fields  (second class)]
           [types   (map first fields)]
           [names   (map second fields)]                  
           [fields  (create-field-declarations fields)]                         
           [width   (width-class name fields)] 
           [separat (make-separator-line width)]
           [separat-hasa (make-separator-line width #t)]
           [start-hasa   #f])
      (cond
        [(and (cons? super) (ormap is-super? types))
         `(,separat
            ,((make-line width) name)
            ,separat
            ,@(map (lambda (line type)
                     (cond
                       [(is-super? type) (set! start-hasa #t) [(make-line width HOOK) line]]
                       [start-hasa [(make-line width HASA) line]]
                       [else ((make-line width) line)]))
                   fields types)
            ,separat-hasa)]
        [else
         `(,separat
            ,((make-line width) name)
            ,separat
            ,@(map (make-line width) fields)
            ,separat)])))
  
  ;; (Listof Field) -> (Listof String)
  (define (create-field-declarations fields)
    (map (lambda (f) (string-append (string-append (first f) " " (second f)))) 
         fields))
  
  ;; String Boolean *-> String
  (define (make-separator-line width . hasa)
    (string-append LFT+ (make-string (- width (string-length LFT+) (string-length RGT+)) #\-) 
                   (if (null? hasa) RGT+ HASA+)))
  
  ;; Number String -> String
  ;; make one line in class of width from txt
  (define (make-line width . rgt)
    (lambda (txt)
      (string-append 
       LEFT 
       txt
       (make-string 
        (- width (string-length txt) (string-length LEFT) (string-length RIGHT))
        #\space)
       (if (null? rgt) RIGHT (car rgt)))))
  
  (define LEFT  "  | ")
  (define LFT+  "  +-")
  (define RIGHT " |  ")
  (define HOOK  " |-o") ; (= (string-length RIGHT) (string-length HOOK))
  (define RGT+  "-+  ") ; (= (string-length RIGHT) (string-length RGT+))
  (define HASA  " | |") ; (= (string-length RIGHT) (string-length HASA))
  (define HASA+ "-+ |") ; (= (string-length RIGHT) (string-length RGT+))
  
  ;; String (Cons String (Listof String)) -> Number
  ;; compute width of class as the widest field/name 
  (define (width-class name fields)
    (+ (string-length LEFT)
       ;; longest field spec of name/class and fields
       (apply max (map string-length (cons name fields)))
       (string-length RIGHT)))
  
  ;; String String -> (String -> String)
  ;; add lft and rgt to txt 
  (define (pad-lines lft rgt) (lambda (txt) (string-append lft txt rgt)))
  
  ;; (Listof (Listof String)) -> (Listof String)
  ;; contract: (apply = (map length smatrix))
  ;; this requires Pretty Big, it could be written in Intermediate 
  (define (flatten-string-matrix smatrix) 
    (apply map (lambda l (apply string-append l)) smatrix))
  
  ;; (Listof String) -> String
  ;; turn the list of strings into a single string, separating lines with newline 
  (define (strings->string-as-lines s)
    (apply string-append (map (lambda (x) (string-append x "\n")) s)))
  
  ;; Basic Tests: 
  
  
  (equal? 
   (flatten-string-matrix
    (list (list "a1" "a2" "a3") (list "b1" "b2" "b3") (list "c1" "c2" "c3")))
   (list "a1b1c1" "a2b2c2" "a3b3c3"))
  
  ;; Tests
  
  
  (define aTrain (list"Train"'(("int" "capacity") ("hello" "world"))))
  (define sTrain (list"Train"'(("int" "capacity"))))
  
  (test== (width-class (car aTrain) (create-field-declarations (cadr aTrain)))
          (+ 12 (string-length LEFT) (string-length RIGHT)))
  
  (test== ([make-line (+ 12 (string-length LEFT) (string-length RIGHT))] "int capacity")
          "  | int capacity |  ")
  
  (test== (make-separator-line (+ 12 (string-length LEFT) (string-length RIGHT)))
          "  +--------------+  ")
  
  (define expected-class
    (list
     "  +--------------+  \n"
     "  | Train        |  \n"
     "  +--------------+  \n"
     "  | int capacity |  \n"
     "  | hello world  |  \n"
     "  +--------------+  \n"))
  
  (test== (class-draw aTrain) 
          (apply string-append expected-class))
  
  (test== (classes-to-strings (list sTrain aTrain))
          '(("     +--------------+      "
             "     | Train        |      "
             "     +--------------+      "
             "     | int capacity |      "
             "     +--------------+      "
             "                           ")
            ("     +--------------+      "
             "     | Train        |      "
             "     +--------------+      "
             "     | int capacity |      "
             "     | hello world  |      "
             "     +--------------+      ")))
  
  ;; Union: ARiver = Source(Location) | Confluence(Location, ARiver, ARiver)
  (define ARiver
    (list "ARiver" '()))
  (define Source
    (list "Source" '(("Location" "loc"))))
  (define Confluence
    (list "Confluence" '(("Location" "loc") ("ARiver" "left") ("ARiver" "right"))))
  
  (test== (strings->string-as-lines
           (class-union-to-strings ARiver (list Source Confluence)))
          (strings->string-as-lines
          '("                    +--------+                      "
            "                    | ARiver |<---------------------+"
            "                    +--------+                      |"
            "                    +--------+                      |"
            "                       / \\                          |" ;; note escape 
            "                       ---                          |"
            "                        |                           |"
            "           +--------------------------+             |"
            "           |                          |             |"
            "   +--------------+           +--------------+      |"
            "   | Source       |           | Confluence   |      |"
            "   +--------------+           +--------------+      |"
            "   | Location loc |           | Location loc |      |"
            "   +--------------+           | ARiver left  |-o    |"
            "                              | ARiver right |-o    |"
            "                              +--------------+ |    |"
            "                                              -+----|")))
  
  (test== (replace-end-with-back-arrow "| ARiver |     ")
          "| ARiver |<----+"
          "replace end with back arrow")
  
  (printf "~a" (strings->string-as-lines (class-union-to-strings ARiver (list Source Confluence))))
  
  )
