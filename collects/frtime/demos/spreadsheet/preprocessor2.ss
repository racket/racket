(module preprocessor2 mzscheme
  
  (require (lib "string.ss")
           (lib "etc.ss")
           )
  
  (define (drop l n)
    (if (zero? n)
        l
        (drop (cdr l) (sub1 n))))
  
  (define (caddddr lst)
    (car (cdr (cdr (cdr (cdr lst))))))
  
  (define (first lst)
    (car lst))
  
  (define (take l n)
    (if (zero? n)
        '()
        (cons (car l)
              (take (cdr l)
                    (sub1 n)))))
  
  ;; ('a -> bool) * 'a list -> (#f or num)
  (define position-of-first-satisfied-in-list
    (lambda (pred l)
      (let loop ((i 0) (l l))
        (if (null? l) #f
            (if (pred (car l)) i
                (loop (+ i 1) (cdr l)))))))
  
  (define position-of-object-in-list
    (lambda (o l)
      (position-of-first-satisfied-in-list (lambda (x)
                                             (eqv? o x))
                                           l)))
  
  (define capitals
    '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
  
  (define lower-case
    (let loop ([i 97])
      (if (<= i 122)
          (cons (integer->char i)
                (loop (add1 i)))
          '())))
  
  (define digits
    '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
  
  (define (contains? i lst)
    (and (not (null? lst))
         (or (equal? i (car lst))
             (contains? i (cdr lst)))))
  
  (define (capital? char)
    (contains? char capitals))
  (define (lower-case? char)
    (contains? char lower-case))
  (define (digit? char)
    (contains? char digits))
  (define (letter? char)
    (or (capital? char)
        (lower-case? char)))
  
  (define-struct cell-reference (row1 ; num
                                 row2 ; num 
                                 row-absolute? ; bool
                                 col1 ; num
                                 col2 ; num 
                                 col-absolute? ; bool
                                 ))
  
  (define (capital->col-num char)
    (- (char->integer char)
       65))
  
  (define (lower-case->col-num char)
    (- (char->integer char)
       97))      
  
  #;(define (letter->col-num char)
      (if (capital? char)
          (capital->row-num char)
          (lower-case->row-num char)))
  
  (define (digit->row-num char)
    (- (char->integer char)
       48))
  
  #;(define (letter-list->col-num llst)
      (let loop ([num -1]
                 [lst llst])
        (cond [(null? lst)
               num]
              [(capital? (car lst))
               (loop (+ (* (add1 num) 26)
                        (capital->row-num (car lst)))
                     (cdr lst))]
              [(lower-case? (car lst))
               (loop (+ (* (add1 num) 26)
                        (lower-case->col-num (car lst)))
                     (cdr lst))])))
  
  (define (digit-list->row-num nlist)
    (let loop ([num 0]
               [lst nlist])
      (cond [(null? lst)
             num]
            [(digit? (car lst))
             (loop (+  (* num 10)
                       (digit->row-num (car lst)))
                   (cdr lst))])))
  
  (define NOT-A-CELL-REFERENCE #f)
  
  ;; char list -> cell-reference
  (define (parse-ref char-list)
    (let loop ([lst char-list]
               [reading-col-section #t]
               [row1 #f]
               [row2 #f]
               [row-absolute? #f]
               [col1 #f]
               [col2 #f]
               [col-absolute? #f])
      (cond [(null? lst)
             (if (and row1 row2 col1 col2 (not reading-col-section))
                 (make-cell-reference row1 row2 row-absolute? col1 col2 col-absolute?)
                 NOT-A-CELL-REFERENCE)]
            [reading-col-section
             (cond [(equal? (car lst)
                            #\$)
                    (if (or col1 col-absolute?) ;; then already hit first $, so this must refer to numbers
                        (loop lst #f row1 row2 row-absolute? col1 col2 col-absolute?)
                        (loop (cdr lst)
                              #t
                              row1 row2 row-absolute?
                              col1 col2
                              #t))]
                   [(letter? (car lst))
                    (let ([next (position-of-first-satisfied-in-list 
                                 (lambda (o)
                                   (or (equal? o #\$)
                                       (equal? o #\:)
                                       (digit? o)))
                                 lst)])
                      (if col1
                          (if next
                              (if (equal? 'invalid
                                          (letter-list->col-num (take lst
                                                                      next)))
                                  NOT-A-CELL-REFERENCE
                                  (loop (drop lst next)
                                        #t ; should get switched in next iteration
                                        row1 row2 row-absolute? 
                                        col1 
                                        (letter-list->col-num (take lst
                                                                    next)) 
                                        col-absolute?))
                              NOT-A-CELL-REFERENCE
                              )
                          (if next
                              (if (equal? 'invalid
                                          (letter-list->col-num (take lst
                                                                      next)))
                                  NOT-A-CELL-REFERENCE
                                  (loop (drop lst
                                              next)
                                        #t
                                        row1 row2 row-absolute?
                                        (letter-list->col-num (take lst
                                                                    next))
                                        (letter-list->col-num (take lst next))
                                        col-absolute?))
                              NOT-A-CELL-REFERENCE
                              )))]
                   [(equal? (car lst) #\:)
                    (if col1
                        (loop (cdr lst) #t row1 row2 row-absolute? col1 col2 col-absolute?)
                        NOT-A-CELL-REFERENCE)]
                   [(digit? (car lst))
                    (loop lst #f row1 row2 row-absolute? col1 col2 col-absolute?)]
                   [else NOT-A-CELL-REFERENCE])]
            [else ; reading NUMBER/ROW section section
             (cond [(equal? (car lst)
                            #\$)
                    (loop (cdr lst) #f row1 row2 #t col1 col2 col-absolute? ;!!
                          )]
                   [(digit? (car lst))
                    (let ([next (position-of-first-satisfied-in-list
                                 (lambda (o)
                                   (not (digit? o)))
                                 lst)])
                      (if row1
                          (if next
                              NOT-A-CELL-REFERENCE
                              (loop '()
                                    #f
                                    row1 
                                    (digit-list->row-num lst)
                                    row-absolute?
                                    col1 col2 col-absolute?))
                          (if next
                              (loop (drop lst next)
                                    #f
                                    (digit-list->row-num (take lst next))
                                    (digit-list->row-num (take lst next))
                                    row-absolute?
                                    col1 col2 col-absolute?)
                              (loop '()
                                    #f
                                    (digit-list->row-num lst)
                                    (digit-list->row-num lst)
                                    row-absolute?
                                    col1 col2 col-absolute?))))]
                   [(equal? (car lst)
                            #\:)
                    (loop (cdr lst) #t
                          row1 row2 row-absolute?
                          col1 col2 col-absolute?)]
                   [else
                    NOT-A-CELL-REFERENCE])])))
  
  ;
  ;(define pt1 '(#\a #\b #\1 #\2))
  ;(define pt1- (make-cell-reference 12 12 #f 27 27 #f))
  ;(define pt2 '(#\$ #\a #\b #\4))
  ;(define pt2- (make-cell-reference 4 4 #f 27 27 #t))
  ;(define pt3 '(#\a #\$ #\4))
  ;(define pt3- (make-cell-reference 4 4 #t 0 0 #f))
  ;(define pt4 '(#\a #\: #\e #\5))
  ;(define pt4- (make-cell-reference 5 5 #f 0 4 #f))
  ;(define pt5 '(#\z #\1 #\: #\8))
  ;(define pt5- (make-cell-reference 1 8 #f 25 25 #f))
  ;(define pt6 '(#\$ #\a #\: #\d #\1))
  ;(define pt6- (make-cell-reference 1 1 #f 0 3 #t))
  ;(define pt7 '(#\$ #\a #\$ #\0))
  ;(define pt7- (make-cell-reference 0 0 #t 0 0 #t))
  ;
  ;(define (equal-cell-refs? c1 c2)
  ;  (and (equal? (cell-reference-row1 c1)
  ;               (cell-reference-row1 c2))
  ;       (equal? (cell-reference-row2 c1)
  ;               (cell-reference-row2 c2))
  ;       (equal? (cell-reference-row-absolute? c1)
  ;               (cell-reference-row-absolute? c2))
  ;       (equal? (cell-reference-col1 c1)
  ;               (cell-reference-col1 c2))
  ;       (equal? (cell-reference-col2 c1)
  ;               (cell-reference-col2 c2))
  ;       (equal? (cell-reference-col-absolute? c1)
  ;               (cell-reference-col-absolute? c2))))
  ;
  ;(equal-cell-refs? (parse-ref pt1) pt1-)
  ;(equal-cell-refs? (parse-ref pt2) pt2-)
  ;(equal-cell-refs? (parse-ref pt3) pt3-)
  ;(equal-cell-refs? (parse-ref pt4) pt4-)
  ;(equal-cell-refs? (parse-ref pt5) pt5-)
  ;(equal-cell-refs? (parse-ref pt6) pt6-)
  ;(equal-cell-refs? (parse-ref pt7) pt7-)
  
  
  (define mapped-symbols
    '(+
      length
      =
      if
      quote
      *
      seconds
      add1
      map))  
  
  
  ;process: sexp symbol symbol symbol symbol num num -> sexp
  ;INPUT: An expression EXPR, the name of lookup procedures LOOKUP, LOOKUP-ROW, LOOKUP-COL, LOOKUP-MATRIX and the current row and column
  ; for the cell being processed, ROW and COL.
  ;OUTPUT: blah blah blah
  (define (process expr lookup lookup-row lookup-col lookup-matrix row col)
    (define (cell-ref->sexp cref c-row c-col)
      ;; cell-reference * num * num -> sexp
      (let ([ref-row1 (cell-reference-row1 cref)]
            [ref-col1 (cell-reference-col1 cref)]
            [ref-row2 (cell-reference-row2 cref)]
            [ref-col2 (cell-reference-col2 cref)]
            [row-ref-expr (lambda (n)
                            (if (cell-reference-row-absolute? cref)
                                n
                                (list '+ 'row (- n c-row))))]
            [col-ref-expr (lambda (n)
                            (if (cell-reference-col-absolute? cref)
                                n
                                (list '+ 'col (- n c-col))))])
        (if (not (= ref-row1 ref-row2))
            (if (not (= ref-col1 ref-col2))
                (list lookup-matrix 
                      (row-ref-expr ref-row1)
                      (row-ref-expr ref-row2)
                      (col-ref-expr ref-col1)
                      (col-ref-expr ref-col2))
                (list lookup-col 
                      (row-ref-expr ref-row1)
                      (row-ref-expr ref-row2)
                      (col-ref-expr ref-col1)))
            (if (not (= ref-col1 ref-col2))
                (list lookup-row
                      (row-ref-expr ref-row1)
                      (col-ref-expr ref-col1)
                      (col-ref-expr ref-col2))
                (list lookup
                      (row-ref-expr ref-row1)
                      (col-ref-expr ref-col1))))))
    ;; end of cell reference handling
    (cond [(symbol? expr)
           (let ([parsed (parse-ref (string->list (symbol->string expr)))])
             (cond [parsed (cell-ref->sexp parsed row col)]
                   [else ;; currently allowing all symbols.
                    expr]))]
          [(list? expr)
           (if (and (not (null? expr))
                    (equal? 'quote (car expr)))
               expr
               (map (lambda (sexp)
                      (process sexp lookup lookup-row lookup-col lookup-matrix row col))
                    expr))]
          [else expr]))
  
  
  
  
  ;(define t1 "a:zz$1")
  ;(define t2 "length")
  ;(define t3 "(+ 3 4)")
  ;(define t4 "(+ 2 a3)")
  ;(define t5 "(if (= 3 (length A1)) (a2 $A3) 'dont-see)")
  ;(define t6 "(ZD941 A:d3:40)")
  ;
  ;(define p (lambda (e)
  ;            (process (read (open-input-string e)) 'lookup 'lookup-row 'lookup-col 'lookup-matrix 1 1)))
  ;t1 (p t1)
  ;t2 (p t2)
  ;t3 (p t3)
  ;t4 (p t4)
  ;t5 (p t5)
  ;t6 (p t6)
  
  
  ;;  ISSUES:
  ;;     - When going from cell-references to symbols, be careful about LETTERS referring to COLUMNS
  ;;       and NUMBERS referring to ROWS.  Some procedures are still BROKEN because of this.
  ;;     
  ;;     - Now a lookup expression has more information about what sort of region it is looking up
  ;;       (singleton, row, column, matrix).  Use this when converting from lookup expressions to
  ;;       cell references.
  ;;
  ;;     - Now that lookup expressions contain all the information contained within cell references,
  ;;       the intermediate step between expressions and cell references is UNNECESSARY.  I should
  ;;       have procedure's LOOKUP-EXPR->SYMBOL that take care of everything for the UNPROCESSOR.
  ;;
  (define (letter-list->col-num llst)
    (let ([col-num (let loop ([num -1]
                              [lst llst])
                     (cond [(null? lst)
                            num]
                           [(capital? (car lst))
                            (loop (+ (* (add1 num) 26)
                                     (capital->col-num (car lst)))
                                  (cdr lst))]
                           [(lower-case? (car lst))
                            (loop (+ (* (add1 num) 26)
                                     (lower-case->col-num (car lst)))
                                  (cdr lst))]))])
      (if (>= col-num 702)
          'invalid
          col-num)))
  
  ; num -> char list
  (define (col-num->letter-list n)
    (let loop ([num n]
               [lst '()])
      (cond [(= -1 num)
             lst]
            [else (loop (sub1 (quotient num 26))
                        (cons (integer->char (+ 97 (remainder num 26)))
                              lst))])))
  
  ; num -> char list
  (define (row-num->digit-list n)
    (define help 
      (lambda (n)
        (let ([r (remainder n 10)]
              [q (quotient n 10)])
          (if (zero? q)
              (list (integer->char (+ 48 n)))
              (cons (integer->char (+ 48 r))
                    (help q))))))
    (reverse (help n)))
  
  
  
  (define (lookup-expr->symbol expr local-row local-col)
    (let* ([row-expr (cadr expr)]
           [col-expr (caddr expr)]
           [row-chars (cond [(number? row-expr)
                             (cons #\$
                                   (row-num->digit-list row-expr))]
                            [(list? row-expr)
                             (row-num->digit-list (+ local-row
                                                     (caddr row-expr)))])]
           [col-chars (cond [(number? col-expr)
                             (cons #\$
                                   (col-num->letter-list col-expr))]
                            [(list? col-expr)
                             (col-num->letter-list (+ local-col
                                                      (caddr col-expr)))])])
      
      (string->symbol (list->string (append col-chars
                                            row-chars)))))
  
  (define (lookup-row-expr->symbol expr local-row local-col)
    (let* ([row-expr (cadr expr)]
           [col1-expr (caddr expr)]
           [col2-expr (cadddr expr)]
           [row-chars (cond [(number? row-expr)
                             (cons #\$
                                   (row-num->digit-list row-expr))]
                            [(list? row-expr)
                             (row-num->digit-list (+ local-row
                                                     (caddr row-expr)))])]
           [col1-chars (cond [(number? col1-expr)
                              (cons #\$
                                    (col-num->letter-list col1-expr))]
                             [(list? col1-expr)
                              (col-num->letter-list (+ local-col
                                                       (caddr col1-expr)))])]
           [col2-chars (cons #\:
                             (cond [(number? col2-expr)
                                    (col-num->letter-list col2-expr)]
                                   [(list? col2-expr)
                                    (col-num->letter-list (+ local-col
                                                             (caddr col2-expr)))]))])
      (string->symbol (list->string (append col1-chars
                                            col2-chars
                                            row-chars)))))
  
  (define (lookup-col-expr->symbol expr local-row local-col)
    (let* ([row1-expr (cadr expr)]
           [row2-expr (caddr expr)]
           [col-expr (cadddr expr)]
           [row1-chars (cond [(number? row1-expr)
                              (cons #\$
                                    (row-num->digit-list row1-expr))]
                             [(list? row1-expr)
                              (row-num->digit-list (+ local-row
                                                      (caddr row1-expr)))])]
           [row2-chars (cons #\:
                             (cond [(number? row2-expr)
                                    (row-num->digit-list row2-expr)]
                                   [(list? row2-expr)
                                    (row-num->digit-list (+ local-row
                                                            (caddr row2-expr)))]))]
           [col-chars (cond [(number? col-expr)
                             (cons #\$
                                   (col-num->letter-list col-expr))]
                            [(list? col-expr)
                             (col-num->letter-list (+ local-col
                                                      (caddr col-expr)))])])
      
      (string->symbol (list->string (append col-chars
                                            row1-chars
                                            row2-chars)))))
  
  (define (lookup-matrix-expr->symbol expr local-row local-col)
    (let* ([row1-expr (cadr expr)]
           [row2-expr (caddr expr)]
           [col1-expr (cadddr expr)]
           [col2-expr (caddddr expr)]
           [row1-chars (cond [(number? row1-expr)
                              (cons #\$
                                    (row-num->digit-list row1-expr))]
                             [(list? row1-expr)
                              (row-num->digit-list (+ local-row
                                                      (caddr row1-expr)))])]
           [row2-chars (cons #\:
                             (cond [(number? row2-expr)
                                    (row-num->digit-list row2-expr)]
                                   [(list? row2-expr)
                                    (row-num->digit-list (+ local-row
                                                            (caddr row2-expr)))]))]
           [col1-chars (cond [(number? col1-expr)
                              (cons #\$
                                    (col-num->letter-list col1-expr))]
                             [(list? col1-expr)
                              (col-num->letter-list (+ local-col
                                                       (caddr col1-expr)))])]
           [col2-chars (cons #\:
                             (cond [(number? col2-expr)
                                    (col-num->letter-list col2-expr)]
                                   [(list? col2-expr)
                                    (col-num->letter-list (+ local-col
                                                             (caddr col2-expr)))]))])
      (string->symbol (list->string (append col1-chars
                                            col2-chars
                                            row1-chars
                                            row2-chars)))))
  
  
  
  ; sexp * symbol * symbol *symbol *symbol num * num -> sexp
  (define (unprocess expr lookup lookup-row lookup-col lookup-matrix local-row local-col)
    (cond [(list? expr)
           (cond [(null? expr)
                  '()]
                 [(equal? (first expr) lookup)
                  (lookup-expr->symbol expr local-row local-col)]
                 [(equal? (first expr) lookup-row)
                  (lookup-row-expr->symbol expr local-row local-col)]
                 [(equal? (first expr) lookup-col)
                  (lookup-col-expr->symbol expr local-row local-col)]
                 [(equal? (first expr) lookup-matrix)
                  (lookup-matrix-expr->symbol expr local-row local-col)]
                 [(equal? (first expr) 'quote)
                  expr]
                 [else
                  (map (lambda (e)
                         (unprocess e lookup lookup-row lookup-col
                                    lookup-matrix local-row local-col))
                       expr)])]
          [else
           expr]))
  ;               
  ;(define te1
  ;  '((lookup 3 4) 
  ;    '(lookup 3 4) 
  ;    (1 2 (lookup 0 0)) 
  ;    (lookup-matrix (+ row 0) (+ row 5) (+ col 2) (+ col 5))
  ;    (lookup-row 5 (+ col 1) (+ col 20))
  ;    (lookup-col 3  39 (+ col 0))))
  ;
  ;(unprocess te1 
  ;           'lookup
  ;           'lookup-row
  ;           'lookup-col
  ;           'lookup-matrix
  ;           1
  ;           1)
  
  
  (provide process unprocess)
  
  )