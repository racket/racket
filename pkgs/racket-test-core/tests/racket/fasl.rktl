
(load-relative "loadtest.rktl")

(Section 'fasl)

(require racket/fasl)

(define immutables
  ;; If you update this list, then also update `immutable-regression-bstr`:
  `(0 #t #f ,(void) ,eof
      1 #\2 three "four" #"five" #:six #&7 #(8 9 10) (11 . 12) (13 14 . fifteen)
      #hash((sixteen . 17) (18 . nineteen))
      #hasheq((20 . "twenty-one") (22 . "twenty-three"))
      #hasheqv((24 . 25) (26 . 27))
      #s(twenty-eight 29 30 "31")
      -32
      3300
      34000
      350000
      3600000
      370000000
      38000000000
      390000000000000
      4000000000000000000
      41.0
      4.2
      43/100
      44+100i
      45.0+100.0i
      46f0))

;; The fasl format is meant to be forward-compatible:
(define immutables-regression-bstr
  #"rkt:\0\200\371\0\34\"n\4\3\6\ao\r2\16\5three\23\4four\25\4five\21\3six\"u \3vwx\36yz\35\2{|\16\afifteen%\1\2\200\16\bnineteen\16\asixteen\177%\0\2\202\23\ntwenty-one\204\23\ftwenty-three%\2\2\206\207\210\211#\16\ftwenty-eight\3\213\214\23\00231\b\340\b\200\344\f\b\201\320\204\0\0\b\2010W\5\0\b\201\200\3566\0\b\201\200\300\r\26\b\202\0\374\371\330\b\0\0\0\b\202\0`v\363\263b\1\0\b\202\0\0\220\235\316\332\2027\t\0\0\0\0\0\200D@\t\315\314\314\314\314\314\20@\v\231\322\f\232\322\f\t\0\0\0\0\0\200F@\t\0\0\0\0\0\0Y@\n\0\08B")

(for ([i (in-list immutables)])
  (test i fasl->s-exp (s-exp->fasl i)))

(test immutables fasl->s-exp (s-exp->fasl immutables))
(test (list immutables immutables) fasl->s-exp (s-exp->fasl (list immutables immutables)))

(test immutables-regression-bstr s-exp->fasl immutables)

(let* ([g (gensym)])
  (define s-exp (fasl->s-exp (s-exp->fasl (list g g g))))
  (test #f eq? g (car s-exp))
  (test #t eq? (car s-exp) (cadr s-exp))
  (test #t eq? (car s-exp) (caddr s-exp)))

(let* ([u (string->unreadable-symbol "unread")])
  (define s-exp (fasl->s-exp (s-exp->fasl (list u u))))
  (test #t eq? u (car s-exp))
  (test #t eq? u (cadr s-exp)))

;; check uses datum-intern-literal:
(test #t eq? "hello" (fasl->s-exp (s-exp->fasl "hello")))
(test #t eq? #"hello" (fasl->s-exp (s-exp->fasl #"hello")))

(test #f eq? "hello" (fasl->s-exp (s-exp->fasl "hello") #:datum-intern? #f))
(test #f eq? #"hello" (fasl->s-exp (s-exp->fasl #"hello") #:datum-intern? #f))
(test #f eq? #rx"hello" (fasl->s-exp (s-exp->fasl #rx"hello") #:datum-intern? #f))
(test #f eq? #px"hello" (fasl->s-exp (s-exp->fasl #px"hello") #:datum-intern? #f))
(test #f eq? #rx#"hello" (fasl->s-exp (s-exp->fasl #rx#"hello") #:datum-intern? #f))
(test #f eq? #px#"hello" (fasl->s-exp (s-exp->fasl #px#"hello") #:datum-intern? #f))

(define (check-hash make-hash hash)
  (let ([mut (make-hash)]
        [immut (hash 'one 2 'three 4)])
    (for ([(k v) (in-hash immut)])
      (hash-set! mut k v))
    (test immut fasl->s-exp (s-exp->fasl immut))
    (test #t equal? immut (fasl->s-exp (s-exp->fasl mut)))
    (test #f equal? mut (fasl->s-exp (s-exp->fasl mut)))
    (test #t equal? mut (fasl->s-exp (s-exp->fasl mut #:keep-mutable? #t)))))

(check-hash make-hash hash)
(check-hash make-hasheq hasheq)
(check-hash make-hasheqv hasheqv)

(define (check-mutable make-x)
  (test #t immutable? (fasl->s-exp (s-exp->fasl (make-x 3))))
  (test #f immutable? (fasl->s-exp (s-exp->fasl (make-x 3) #:keep-mutable? #t))))
(check-mutable make-vector)
(check-mutable make-string)
(check-mutable make-bytes)

(test (current-directory) fasl->s-exp (s-exp->fasl (current-directory)))
(parameterize ([current-write-relative-directory #f])
  (let ([unix-path (bytes->path #"here" 'unix)]
        [windows-path (bytes->path #"there" 'windows)])
    (test unix-path fasl->s-exp (s-exp->fasl unix-path))
    (test windows-path fasl->s-exp (s-exp->fasl windows-path))))

(let* ([rel-p (build-path "nested" "data.rktd")]
       [p (build-path (current-directory) rel-p)])
  (define bstr
    (parameterize ([current-write-relative-directory (current-directory)])
      (s-exp->fasl p)))
  (parameterize ([current-load-relative-directory #f])
    (test rel-p fasl->s-exp bstr))
  (parameterize ([current-load-relative-directory (current-directory)])
    (test p fasl->s-exp bstr)))

(report-errs)
