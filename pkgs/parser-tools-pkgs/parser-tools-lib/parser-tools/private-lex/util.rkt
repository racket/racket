#lang racket

(provide (all-defined-out))

(define max-char-num #x10FFFF)

(define-struct lex-abbrev (get-abbrev))
(define-struct lex-trans (f))

(module+ test 
  (require rackunit))

#;(define-syntax test-block
    (syntax-rules ()
      ((_ defs (code right-ans) ...)
       (let* defs
         (let ((real-ans code))
           (unless (equal? real-ans right-ans)
             (printf "Test failed: ~e gave ~e.  Expected ~e\n"
                     'code real-ans 'right-ans))) ...))))

(define-syntax test-block
  (syntax-rules ()
    ((_ x ...) (void))))


;; A cache is (X ( -> Y) -> Y)
;; make-cache : -> cache
;; table map Xs to Ys.  If key is mapped, its value is returned.
;; Otherwise, build is invoked and its result is placed in the table and
;; returned.
;; Xs are compared with equal?
(define (make-cache)
  (let ((table (make-hash)))
    (lambda (key build)
      (hash-ref table key
                (lambda ()
                  (let ((new (build)))
                    (hash-set! table key new)
                    new))))))

(module+ test
  (define cache (make-cache))
  (check-equal? (cache '(s 1 2) (lambda () 9)) 9)
  (check-equal? (cache '(s 2 1) (lambda () 8)) 8)
  (check-equal? (cache '(s 1 2) (lambda () 1)) 9)
  (check-equal? (cache (cons 's (cons 0 (cons +inf.0 10)))
                       (lambda () 22)) 22)
  (check-equal? (cache (cons 's (cons 0 (cons +inf.0 10)))
                       (lambda () 1)) 22))



;; make-counter : -> -> nat
;; makes a function that returns a higher number by 1, each time
;; it is called.
(define (make-counter)
  (let ((counter 0))
    (lambda ()
      (begin0
        counter
        (set! counter (add1 counter))))))

(module+ test 
  (define c (make-counter))
  (define d (make-counter))
  (check-equal? (c) 0)
  (check-equal? (d) 0)
  (check-equal? (c) 1)
  (check-equal? (d) 1)
  (check-equal? (c) 2))


;; remove-dups : (list-of X) (X -> number) -> (list-of X)
;; removes the entries from l that have the same index as a
;; previous entry.  l must be grouped by indexes.
(define (remove-dups l index acc)
  (cond
    ((null? l) (reverse acc))
    ((null? acc) (remove-dups (cdr l) index (cons (car l) acc)))
    ((= (index (car acc)) (index (car l)))
     (remove-dups (cdr l) index acc))
    (else 
     (remove-dups (cdr l) index (cons (car l) acc)))))


(module+ test
  (check-equal? (remove-dups '((1 2) (2 2) (1 3) (1 4)
                                     (100 4) (0 5)) cadr null)
   '((1 2) (1 3) (1 4) (0 5)))
  (check-equal? (remove-dups null error null) null))

;; do-simple-equiv : (list-of X) (X -> nat) -> (list-of X)
;; Sorts l according to index and removes the entries with duplicate
;; indexes.
(define (do-simple-equiv l index)
  (let ((ordered (sort l (lambda (a b) (< (index a) (index b))))))
    (remove-dups ordered index null)))

(module+ test
 (check-equal? (do-simple-equiv '((2 2) (1 4) (1 2)
                                        (100 4) (1 3)  (0 5)) 
                                cadr)
  '((2 2) (1 3) (1 4) (0 5)))
 (check-equal? (do-simple-equiv null error) null))

;; replace : (list-of X) (X -> bool) (X -> (list-of X)) (list-of X) ->
;;           (list-of X)
;; If (pred? r) for some r in l, splice (get r) in place of r in the resulting
;; list.
(define (replace l pred? get acc)
  (cond
    ((null? l) acc)
    ((pred? (car l)) (replace (cdr l) pred? get (append (get (car l)) acc)))
    (else (replace (cdr l) pred? get (cons (car l) acc)))))       


(module+ test
  (check-equal? (replace null void (lambda () (list 1)) null) null)
  (check-equal? (replace '(1 2 3 4 3 5)
                         (lambda (x) (= x 3))
                         (lambda (x) (list 1 2 3))
                         null)
                '(5 1 2 3 4 1 2 3 2 1)))
  
  
    
