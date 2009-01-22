#lang scheme/base
(require (for-syntax scheme/base "rep.ss")
         scheme/match)
(provide (for-syntax expectation-of-stxclass
                     expectation-of-constants)
         try
         empty-expectation?
         expectation->string)

(define-struct scdyn (name desc)
  #:transparent)

(define-struct expc (stxclasses pairs? data literals)
  #:transparent)

(begin-for-syntax
  (define certify (syntax-local-certifier))
  (define (expectation-of-stxclass stxclass)
    (if stxclass
        (with-syntax ([name (sc-name stxclass)]
                      [desc (sc-description stxclass)])
          (certify #'(make-expc (list (make-scdyn 'name 'desc)) #f null null)))
        #'#f))

  (define (expectation-of-constants pairs? data literals)
    (with-syntax ([(datum ...) data]
                  [(literal ...) literals]
                  [pairs? pairs?])
      (certify
       #'(make-expc null 'pairs? (list 'datum ...) (list (quote-syntax literal) ...))))))


(define-syntax try
  (syntax-rules ()
    [(try failvar (expr0))
     expr0]
    [(try failvar (expr0 . exprs))
     (let ([failvar
            (lambda (x1 p1 r1 f1)
              (let ([failvar
                     (lambda (x2 p2 r2 f2)
                       (choose-error failvar x1 x2 p1 p2 r1 r2 f1 f2))])
                (try failvar exprs)))])
       expr0)]))

(define (choose-error k x1 x2 p1 p2 r1 r2 frontier1 frontier2)
  (define (go1) (k x1 p1 r1 frontier1))
  (define (go2) (k x2 p2 r2 frontier2))
  (let loop ([f1 frontier1] [f2 frontier2])
    (cond [(and (null? f1) (null? f2))
           (let ([p (merge-expectations p1 p2)])
             (k x1 p #f frontier1))]
          [(and (pair? f1) (null? f2)) (go1)]
          [(and (null? f1) (pair? f2)) (go2)]
          [(and (pair? f1) (pair? f2))
           (let ([c1 (cadr f1)]
                 [c2 (cadr f2)])
             (cond [(> c1 c2) (go1)]
                   [(< c1 c2) (go2)]
                   [else (loop (cddr f1) (cddr f2))]))])))

(define (merge-expectations e1 e2)
  (make-expc (union (expc-stxclasses e1) (expc-stxclasses e2))
            (or (expc-pairs? e1) (expc-pairs? e2))
            (union (expc-data e1) (expc-data e2))
            (union (expc-literals e1) (expc-literals e2))))

(define union append)

(define (empty-expectation? e)
  (match e
    [(struct expc (scs pairs? data literals))
     (and (null? scs)
          (not pairs?)
          (null? literals)
          (and (pair? data) (null? (cdr data)))
          (equal? (car data) '()))]))

(define (expectation->string e)
  (match e
    [(struct expc (_ #t _ _))
     #f]
    [(struct expc (stxclasses pairs? data literals))
     (let ([s1 (and (pair? stxclasses) (string-of-stxclasses stxclasses))]
           [s2 (and (pair? data) (string-of-data data))]
           [s3 (and (pair? literals) (string-of-literals literals))]
           [s4 (and pairs? string-of-pairs?)])
       (join-sep (filter string? (list s1 s2 s3 s4))
                 ";"
                 "or"))]))

(define (string-of-stxclasses stxclasses)
  (comma-list (map string-of-stxclass stxclasses)))

(define (string-of-stxclass stxclass)
  (and stxclass
       (format "~a"
               (or (scdyn-desc stxclass)
                   (scdyn-name stxclass)))))

(define (string-of-literals literals0)
  (define literals
    (sort (map syntax-e literals0) 
          string<? 
          #:key symbol->string
          #:cache-keys? #t))
  (case (length literals)
    [(1) (format "the literal identifier ~s" (car literals))]
    [else (format "one of the following literal identifiers: ~a"
                  (comma-list (map ->string literals)))]))

(define (string-of-data data)
  (case (length data)
    [(1) (format "the literal ~s" (car data))]
    [else (format "one of the following literals: ~a"
                  (comma-list (map ->string data)))]))

(define (->string x) (format "~s" x))

(define string-of-pairs?
  "structured syntax")

(define (comma-list items)
  (join-sep items "," "or"))

(define (join-sep items sep0 ult0)
  (define sep (string-append sep0 " "))
  (define ult (string-append ult0 " "))
  (define (loop items)
    (cond [(null? items)
           null]
          [(null? (cdr items))
           (list sep ult (car items))]
          [else
           (list* sep (car items) (loop (cdr items)))]))
  (case (length items)
    [(2) (format "~a ~a~a" (car items) ult (cadr items))]
    [else (let ([strings (list* (car items) (loop (cdr items)))])
            (apply string-append strings))]))

;; (define (comma-list items0)
;;   (define items (for/list ([item items0]) (format "~s" item)))
;;   (define (loop items)
;;     (cond [(null? items)
;;            null]
;;           [(null? (cdr items))
;;            (list ", or " (car items))]
;;           [else
;;            (list* ", " (car items) (loop (cdr items)))]))
;;   (case (length items)
;;     [(2) (format "~a or ~a" (car items) (cadr items))]
;;     [else (let ([strings (list* (car items) (loop (cdr items)))])
;;             (apply string-append strings))]))

