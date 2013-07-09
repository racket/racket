#lang racket/base
(require (for-syntax racket/base))
(define-syntax (this-dir stx)
  (define src (syntax-source stx))
  (cond
    [(path? src)
     (define-values (base name dir?) (split-path src))
     #`'#,base]
    [else #f]))

(define files 
  (for/list ([file (in-list (directory-list (this-dir)))]
             #:when 
             (and (regexp-match #rx"[.]rkt$" (path->string file))
                  (not (member (path->string file)
                               '("test-util.rkt" "all.rkt")))))
    file))

(define (find-deps file)
  (define deps #f)
  
  (define (remove-quote x)
    (cond
      [(and (pair? x) (eq? (car x) 'quote))
       (cadr x)]
      [else
       (error 'find-deps "found non-quote in argument to make-basic-contract-namespace in ~a" file)]))
  
  (let loop ([exp 
              (parameterize ([read-accept-reader #t])
                (call-with-input-file file read))])
    (cond
      [(and (list? exp)
            (pair? exp)
            (eq? (car exp) 'make-basic-contract-namespace))
       (when deps 
         (error 'find-deps 
                "found two calls to make-basic-contract-namespace in ~a"
                file))
       (set! deps (map remove-quote (cdr exp)))]
      [(list? exp)
       (for-each loop exp)]
      [else (void)]))
  deps)

(define (dep<? a b)
  (set! a (or a '()))
  (set! b (or b '()))
  (define (subset? a b)
    (for/and ([x (in-list a)])
      (member x b)))
  (or (and (subset? a b)
           (not (subset? b a)))
      (< (length a) (length b))))

(define files-to-run
  (sort 
   (sort 
    (for/list ([file (in-list files)])
      (list (path->string file)
            (find-deps (build-path (this-dir) file))))
    string<=?
    #:key car)
   dep<?
   #:key cadr))

(for ([file (in-list files-to-run)])
  (printf "RUNNING: ~a ~s\n" (car file) (cadr file))
  (dynamic-require (build-path (this-dir) (car file)) #f))
