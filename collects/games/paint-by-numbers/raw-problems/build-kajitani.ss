#lang scheme

;; AFAICT, the input to the thing is long gone.

#!/bin/sh

string=? ; exec mzscheme -qr $0

;; this builds raw-kajitani.ss from full-kajitani
;; raw-kajitani.ss is used in build-problems.ss

(define (make-strings-mutable sexp)
  (cond
   [(string? sexp) (string-copy sexp)]
   [(cons? sexp) (cons (make-strings-mutable (car sexp))
		       (make-strings-mutable (cdr sexp)))]
   [else sexp]))

(define raw-kajitani
  (make-strings-mutable
   (call-with-input-file (build-path (collection-path "games" "paint-by-numbers")
				     "full-kajitani")
     read)))

(define allowed-emails
  (map (lambda (x) (if (list? x) (car x) x))
       (call-with-input-file (build-path (collection-path "games" "paint-by-numbers")
					 "allowed-email")
	 read)))

(define counters (make-hasheq))

(define email-ht (make-hasheq))
(for-each (lambda (email) (hash-set! email-ht (string->symbol email) null))
          allowed-emails)

(define kajitani-sets
  (let ([ht (make-hasheq)])
    (for-each
     (lambda (kaj-set)
       (let* ([raw-email (cadddr kaj-set)]
              [email (if (string? raw-email)
                         (string-downcase raw-email)
                         raw-email)])
         (when (member email allowed-emails)
           (let ([email-sym (string->symbol email)])
             (hash-set! email-ht email-sym
                              (cons
                               (car kaj-set)
                               (hash-ref email-ht email-sym))))
           
           (let ([tag (string->symbol (format "~ax~a" (car (car kaj-set)) (cadr (car kaj-set))))]
                 [rows/cols (list (caddr (car kaj-set)) (cdr kaj-set))])
             (hash-set!
              ht
              tag
              (cons
               rows/cols
               (hash-ref
                ht
                tag
                (lambda ()
                  null))))))))
     raw-kajitani)
    (hash-map ht (lambda (x l) (list x (reverse l))))))
     
(printf "stats by email~n")
(let ([total 0])
  (for-each 
   (lambda (x)
     (let ([k (car x)]
	   [v (cadr x)])
       (let ([len (length v)])
	 (set! total (+ len total))
	 (printf "~s ~s~n" k
		 len
		 ;v
		 ))))
   (sort (hash-map email-ht list)
         (lambda (x y) (> (length (cadr x)) (length (cadr y))))))

  
  (printf "total: ~s~n" total))

(define (build-solutionless-kajitani kaj-set)
  (list
   (format "Kajitani ~a" (car kaj-set))
   (format "k~a" (car kaj-set))
   (map
    (let ([n 0])
      (lambda (kaj)
	(set! n (+ n 1))
	(list (format "~a (~a)" (car kaj) n)
	      (cadr (cadr kaj))
	      (car (cadr kaj)))))
    (cadr kaj-set))))

(call-with-output-file (build-path (collection-path "games" "paint-by-numbers")
				   "raw-kajitani.ss")
  (lambda (port)
    (pretty-print
     (sort (map build-solutionless-kajitani kajitani-sets)
           (lambda (s1 s2) (string<=? (car s1) (car s2))))
     port))
  'truncate)
