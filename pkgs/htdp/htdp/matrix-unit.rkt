#lang racket/unit

(require htdp/matrix-sig
         htdp/matrix-render-sig
         mrlib/matrix-snip
         htdp/error
         lang/posn
         mzlib/class
         mzlib/pconvert
         mzlib/pretty)

(import matrix-render^)
(export matrix^)

;; [Matrix X] = [BST X]

(define matrix% 
  (class* object% (matrix<%>)
    (define/public (->rectangle) (matrix->rectangle this))
    (super-new)))

(define bmatrix%
  (class* matrix% (matrix<%>)
    (init-field n m mat)
    (define/public (get-n) n)
    (define/public (get-m) m)
    (define/public (get-mat) mat)
    ;; [InnerMatrix X] = Nat x Nat x [vectorof X] 
    ;; s.t. (= (* n m)(vector-length vector))
    (super-new)))

(define imatrix%
  (class* matrix% (matrix<%>)
    (init-field left i j info right)
    (super-new)))

(define imatrix-left (class-field-accessor imatrix% left))
(define imatrix-i (class-field-accessor imatrix% i))
(define imatrix-j (class-field-accessor imatrix% j))
(define imatrix-info (class-field-accessor imatrix% info))
(define imatrix-right (class-field-accessor imatrix% right))

(define (make-imatrix left i j info right) 
  (make-object imatrix% left i j info right))

(define (set-imatrix-left n x)
  (make-imatrix x (imatrix-i n) (imatrix-j n) (imatrix-info n) (imatrix-right n)))
(define (set-imatrix-right n x)
  (make-imatrix (imatrix-left n) (imatrix-i n) (imatrix-j n) (imatrix-info n) x))
(define (set-imatrix-info n x)
  (make-imatrix (imatrix-left n) (imatrix-i n) (imatrix-j n) x (imatrix-right n)))

(define (create-matrix n m mat)
  (new bmatrix% [n n] [m m] [mat mat]))

(define (matrix-get-mat bm) (send bm get-mat))
(define (matrix-get-n bm) (send bm get-n))
(define (matrix-get-m bm) (send bm get-m))

;; [BST X] = [InnerMatrix X] | (make-imatrix [BST X] Nat Nat X [BST X])

;; Nat Nat Nat Nat -> Boolean
(define (bst-< i j imatrix-i imatrix-j)
  (or (< i imatrix-i) (and (= i imatrix-i) (< j imatrix-j))))

;; Nat Nat Nat Nat -> Boolean
(define (bst-= i j imatrix-i imatrix-j)
  (and (= i imatrix-i) (= j imatrix-j)))

;; Nat Nat Nat Nat -> Boolean
(define (bst-> i j imatrix-i imatrix-j)
  (not (or (bst-< i j imatrix-i imatrix-j) (bst-= i j imatrix-i imatrix-j))))

;; bst-insert : [BST X] Nat Nat X -> [BST X]
(define (bst-insert t i j x)
  (cond
    [(is-a? t imatrix%)
     (cond
       [(bst-< i j (imatrix-i t) (imatrix-j t)) 
        (set-imatrix-left t (bst-insert (imatrix-left t) i j x))]
       [(bst-= i j (imatrix-i t) (imatrix-j t)) 
        (set-imatrix-info t x)]
       [(bst-> i j (imatrix-i t) (imatrix-j t)) 
        (set-imatrix-right t (bst-insert (imatrix-right t) i j x))])]
    [else (make-imatrix t i j x t)]))

;; bst-lookup : [BST X] Nat Nat -> X
(define (bst-lookup t i j)
  (cond
    [(is-a? t imatrix%)
     (cond
       [(bst-< i j (imatrix-i t) (imatrix-j t)) (bst-lookup (imatrix-left t) i j)]
       [(bst-= i j (imatrix-i t) (imatrix-j t)) (imatrix-info t)]
       [else (bst-lookup (imatrix-right t) i j)])]
    [else (inner-ref (matrix-get-mat t) (matrix-get-m t) i j)]))

;                                                   
;                                                   
;  ;;;;;;                                           
;   ;   ;                               ;           
;   ; ;   ;;  ;; ;; ;;    ;;;   ;; ;;  ;;;;;   ;;;; 
;   ;;;    ;  ;   ;;  ;  ;   ;   ;;     ;     ;   ; 
;   ; ;     ;;    ;   ;  ;   ;   ;      ;      ;;;  
;   ;       ;;    ;   ;  ;   ;   ;      ;         ; 
;   ;   ;  ;  ;   ;   ;  ;   ;   ;      ;   ; ;   ; 
;  ;;;;;; ;;  ;;  ;;;;    ;;;   ;;;;;    ;;;  ;;;;  
;                 ;                                 
;                ;;;                                
;                                                   
;                                                   

(define (matrix? x)
  (define m (if (visible? x) (visible-matrix x) x))
  (internal-matrix? m))

(define (internal-matrix? m)
  (or (is-a? m bmatrix%) (is-a? m imatrix%)))

(define (matrix-rows M*)
  (define-values (M n m) (check-matrix 'matrix-n M* 0 0))
  n)

(define (matrix-cols M*)
  (define-values (M n m) (check-matrix 'matrix-n M* 0 0))
  m)

(define (rectangle->matrix r)
  ;; check rectangleness of r 
  (define n (length r))
  (define m (length (car r)))
  (make-matrix n m (apply append r)))

(define (rectangle->imatrix r)
  ;; check rectangleness of r 
  (define n (length r))
  (define m (length (car r)))
  (make-matrix-aux n m (apply append r)))

(define (matrix->rectangle M*)
  (define-values (M n m) (check-matrix 'matrix->rectangle M* 0 0))
  (build-list n (lambda (i) (build-list m (lambda (j) (matrix-ref M i j))))))

;; -------------------------------------------------------------------------

(define (make-matrix n m l)
  (check-arg 'make-matrix (natural? n) 'Nat "first" n)
  (check-arg 'make-matrix (natural? m) 'Nat "second" m)
  (check-arg 'make-matrix (list? l) 'list "third" l)
  (check-arg 'make-matrix (= (length l) (* n m)) 
             (format "list of length ~a given, expected ~a items" 
                     (length l)
                     (* n m))
             "third" l)
  (make-visible (make-matrix-aux n m l)))
(define (make-matrix-aux n m l)
  (let ([mat (make-vector (* n m) 0)])
    (let loop ([l l][i 0][j 0])
      (cond
        [(null? l) (create-matrix n m mat)]
        [else (begin (vector-set! mat (+ (* i m) j) (car l))
                     (if (< j (- m 1))
                         (loop (cdr l) i (+ j 1))
                         (loop (cdr l) (+ i 1) 0)))]))))

(define (build-matrix n m f)
  (check-arg 'make-matrix (natural? n) 'Nat "first" n)
  (check-arg 'make-matrix (natural? m) 'Nat "second" m)
  (check-proc 'make-matrix f 2 "third" "2 arguments")
  (build-matrix-aux n m f))
(define (build-matrix-aux n m f)
  (define ns (build-list n (lambda (x) x)))
  (define ms (build-list m (lambda (x) x)))
  (define ** 
    (apply append (map (lambda (i) (map (lambda (j) (f i j)) ms)) ns)))
  (make-matrix n m **))

(define (matrix-ref M* i j)
  (define-values (M n m) (check-matrix 'matrix-ref M* i j))
  (bst-lookup M i j))

(define (matrix-set M* i j x)
  (define-values (M m n) (check-matrix 'matrix-set M* i j))
  (make-visible (bst-insert M i j x)))

(define (matrix-render M*)
  (define-values (M n m) (check-matrix 'matrix-render M* 0 0))
  (define (make-row i)
    (let loop ([j 0])
      (if (= j m)
          '()
          (cons (element->string (matrix-ref M i j)) (loop (+ j 1))))))
  (let loop ([i 0])
    (if (= i n) '() (cons (make-row i) (loop (+ i 1))))))

(define (matrix-where? M* pred?)
  (define-values (M n m) (check-matrix 'matrix-where? M* 0 0))
  (define (select-from-row i)
    (define (select-cell j)
      (if (pred? (matrix-ref M i j)) (make-posn i j) #f))
    (filter posn? (build-list m select-cell)))
  (apply append (build-list n select-from-row)))

(define (matrix-minor M* i j)
  (define-values (M n m) (check-matrix 'matrix-minor M* i j))
  (build-matrix (- n 1) (- m 1) 
                (lambda (i* j*) 
                  (cond
                    [(< i* i) 
                     (cond 
                       [(< j* j) (matrix-ref M i* j*)]
                       [(>= j* j) (matrix-ref M i* (+ j* 1))])]
                    [(>= i* i)
                     (cond 
                       [(< j* j) (matrix-ref M (+ i* 1) j*)]
                       
                       [(>= j* j) (matrix-ref M (+ i* 1) (+ j* 1))])]))))

#| from bug report 13264:

I have two problems with matrix-set!. 

1. When I enable matrix-set! 

 (require htdp/matrix)
 (define M (build-matrix 2 3 (λ (i j) (* (expt 2 i) (expt 3 j)))))
 (matrix-set! M 1 2 'a)
 (eq? (matrix-ref M 1 2) 'a)
 (equal? M (build-matrix 2 3 (λ (i j) (* (expt 2 i) (expt 3 j)))))

I get the results 'true' and 'true' -- the second one should NOT be true but false. 

Because I can't change the snip either, M also displays wrong. 

2. I really don't want to enable it in language levels strictly below ASL. 
Otherwise the functional model breaks. ** But I do not know how to make teachpacks
depend on the language into which they linked. ** 
|#

(define (matrix-set! M* i j x)
  (when (is-a? M imatrix%)
    (error 'matrix-set! "use functional updates instead"))
  (define-values (M n m) (check-matrix 'matrix-ref M* i j))
  (vector-set! (matrix-get-mat M) (+ (* i m) j) x)
  M*)

;                              
;                              
;    ;;                        
;     ;                        
;    ; ;  ;;  ;; ;;  ;;   ;;;; 
;    ; ;   ;   ;  ;  ;   ;   ; 
;    ; ;   ;   ;   ;;     ;;;  
;   ;;;;;  ;   ;   ;;        ; 
;   ;   ;  ;  ;;  ;  ;   ;   ; 
;  ;;; ;;;  ;; ;;;;  ;;  ;;;;  
;                              
;                              
;                              
;                              

;; [Vectorof X] Nat Nat Nat -> Nat 
;; the internal referencing for element (i,j) in an _ x m matrix 
(define (inner-ref V m i j)
  (vector-ref V (+ (* i m) j)))

;; X -> String 
;; okay, not X, something renderable via constructor style printing
(define (element->string value)
  (parameterize ([constructor-style-printing #t]
                 [pretty-print-columns 40])
    (let ([value1 0]
          [text* (open-output-string)])
      (pretty-print (print-convert value) text*)
      (close-output-port text*)
      (let* ([s (get-output-string text*)]
             [l (string->list s)]
             [x (reverse l)])
        (if (char=? (car x) #\newline)
            (list->string (reverse (cdr x)))
            s)))))

;; Symbol [Matrix X] Nat Nat ->* [Vectorof X] Nat Nat 
;; contract checking for teaching languages; compute properties of matrix
(define (check-matrix tag M* i j)
  (define M (if (internal-matrix? M*)
                M* 
                (let ([r (if (visible? M*) (visible-matrix M*) M*)])
                  (cond
                    [(internal-matrix? r) r]
                    ;; this next line is suspicious! 
                    [(pair? r) (rectangle->imatrix r)]
                    [else (check-arg tag #f 'matrix "first" M*)]))))
  (check-arg tag (natural? i) 'Nat "second" i)
  (check-arg tag (natural? j) 'Nat "third" j)
  ;; --- now that M is a matrix, i and j are natural numbers:
  (check-aux tag M i j))
(define (check-aux tag M* i j)
  (define M (let loop ([M M*]) (if (is-a? M imatrix%) (loop (imatrix-left M)) M)))
  (define n (matrix-get-n M))
  (define m (matrix-get-m M))
  ;; i and j have to be in Nat
  (unless (< -1 i n)
    (error tag "~ax~a matrix given, can't index into ~a~a row" n m i (th i)))
  (unless (< -1 j m)
    (error tag "~ax~a matrix given, can't index into ~a~a column" n m j (th j)))
  (values M* n m))

;; Nat -> String 
(define (th n)
  (cond
    [(= n 0) "th"]
    [(= n 1) "st"]
    [(= n 2) "nd"]
    [(= n 3) "rd"]
    [(> n 3) "th"]
    [else (error 'th "can't happen ~e" n)]))

