#lang scheme

(require (lib "matrix-sig.ss" "htdp")
         (lib "matrix-render.ss" "htdp")
         (lib "error.ss" "htdp")
         (lib "posn.ss" "lang")
         (lib "class.ss")
         (lib "pconvert.ss")
         (lib "pretty.ss"))

(printf "loading module matrix-unit @ bmatrix\n")
;; [Matrix X] = [BST X]
(define bmatrix%
  (class object%
    (init-field n m mat)
    (define/public (get-n) n)
    (define/public (get-m) m)
    (define/public (get-mat) mat)
    ;; [InnerMatrix X] = Nat x Nat x [vectorof X] 
    ;; s.t. (= (* n m)(vector-length vector))
    (super-new)))

(define-struct imatrix (left i j info right) #:transparent)
(define (set-imatrix-left n x)
  (make-imatrix x (imatrix-i n) (imatrix-j n) (imatrix-info n) (imatrix-right n)))
(define (set-imatrix-right n x)
  (make-imatrix (imatrix-left n) (imatrix-i n) (imatrix-j n) (imatrix-info n) x))
(define (set-imatrix-info n x)
  (make-imatrix (imatrix-left n) (imatrix-i n) (imatrix-j n) x (imatrix-right n)))

(provide matrix@)

(define matrix@
  (unit 
    (import matrix-render^)
    (export matrix^)

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
    [(imatrix? t)
     (cond
       [(bst-< i j (imatrix-i t) (imatrix-j t)) (set-imatrix-left t (bst-insert (imatrix-left t) i j x))]
       [(bst-= i j (imatrix-i t) (imatrix-j t)) (set-imatrix-info t x)]
       [else ;; (bst-< i j (imatrix-i t) (imatrix-j t)) 
        (set-imatrix-right t (bst-insert (imatrix-right t) i j x))])]
    [else (make-imatrix t i j x t)]))

;; bst-lookup : [BST X] Nat Nat -> X
(define (bst-lookup t i j)
  (cond
    [(imatrix? t)
     (cond
       [(bst-< i j (imatrix-i t) (imatrix-j t)) (bst-lookup (imatrix-left t) i j)]
       [(bst-= i j (imatrix-i t) (imatrix-j t)) (imatrix-info t)]
       [else (bst-lookup (imatrix-left t) i j)])]
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

(define (matrix? x) (or (is-a? x bmatrix%) (imatrix? x)))

(define (matrix-n M*)
  (define-values (M n m) (check-matrix 'matrix-n M* 0 0))
  n)

(define (matrix-m M*)
  (define-values (M n m) (check-matrix 'matrix-n M* 0 0))
  m)

(define (rectangle->matrix r)
  ;; check rectangleness of r 
  (define n (length r))
  (define m (length (car r)))
  (make-matrix n m (apply append r)))

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
  (make-visible
   (let ([mat (make-vector (* n m) 0)])
     (let loop ([l l][i 0][j 0])
       (cond
         [(null? l) (create-matrix n m mat)]
         [else (begin (vector-set! mat (+ (* i m) j) (car l))
                      (if (< j (- m 1))
                          (loop (cdr l) i (+ j 1))
                          (loop (cdr l) (+ i 1) 0)))])))))

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

(define (matrix-set! M* i j x)
  (define _ (when (imatrix? M)
              (error 'matrix-set! "use functional updates instead")))
  (define-values (M n m) (check-matrix 'matrix-ref M* i j))
  (vector-set! (matrix-get-mat M) (+ (* i m) j) x)
  M)

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
  (define M (cond
	      [(matrix? M*) M*]
	      [(visible? M*) (visible-matrix M*)]
	      [else (error 'check-matrix "something is wrong: ~e ~e~e\n"
		      M* (visible? M*) (send M* get-M))]))
  (check-arg tag (matrix? M) 'matrix "first" M)
  (check-arg tag (natural? i) 'Nat "second" i)
  (check-arg tag (natural? j) 'Nat "third" j)
  ;; --- now that M is a matrix, i and j are natural numbers:
  (check-aux tag M i j))
(define (check-aux tag M* i j)
  (define M (let loop ([M M*]) (if (imatrix? M) (loop (imatrix-left M)) M)))
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
    [(= n 1) "st"]
    [(= n 2) "nd"]
    [(> n 3) "th"]
    [else (error 'th "can't happen")]))
))
