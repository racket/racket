
(load-relative "loadtest.rktl")

(Section 'fasl)

(require racket/fasl)

(define immutables
  ;; If you update this list, then also update `immutable-regression-bstr`:
  `(0 #t #f ,(void) ,eof
      1 #\2 three "four" #"five" #:six #&7 #(8 9 10) (11 . 12) (13 14 . fifteen)
      ;; Use only hash tables where the keys sort
      #hash((sixteen . 17) (nineteen . 18))
      #hasheq((20 . "twenty-one") (22 . "twenty-three"))
      #hasheqv((24 . 25) (26 . 27))
      #s(twenty-eight 29 30 "31")
      -32
      -275
      3300
      34000
      350000
      3600000
      370000000
      -370000001
      38000000000
      390000000000000
      4000000000000000000
      15511210043330985984000000
      -15511210043330985984000000
      41.0
      4.2
      43/100
      44+100i
      45.0+100.0i
      ;; 46f0 <- test separately, because RacketCS doesn't support single-precision
      (srcloc "x" 1 2 3 4)))

;; The fasl format is meant to be forward-compatible:
(define immutables-regression-bstr
  #"racket/fasl:\0\200@\1\34&n\4\3\6\ao\r2\16\5three\23\4four\25\4five\21\3six\"u \3vwx\36yz\35\2{|\16\afifteen%\1\2\16\bnineteen\200\16\asixteen\177%\0\2\202\23\ntwenty-one\204\23\ftwenty-three%\2\2\206\207\210\211#\16\ftwenty-eight\3\213\214\23\00231\b\340\b\200\355\376\b\200\344\f\b\201\320\204\0\0\b\2010W\5\0\b\201\200\3566\0\b\201\200\300\r\26\b\201\177?\362\351\b\202\0\374\371\330\b\0\0\0\b\202\0`v\363\263b\1\0\b\202\0\0\220\235\316\332\2027\b\203\25cd4a0619fb0907bc00000\b\203\26-cd4a0619fb0907bc00000\t\0\0\0\0\0\200D@\t\315\314\314\314\314\314\20@\v\231\322\f\232\322\f\t\0\0\0\0\0\200F@\t\0\0\0\0\0\0Y@\34\6\16\6srcloc\23\1xopqr")

(for ([i (in-list immutables)])
  (test i fasl->s-exp (s-exp->fasl i)))

(test 46f0 fasl->s-exp (s-exp->fasl 46f0))
(test (vector #t 46f0) fasl->s-exp (s-exp->fasl (vector #t 46f0)))

(test "4.5t0" format "~a" (fasl->s-exp (s-exp->fasl 4.5t0)))

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

(let* ([u (string->uninterned-symbol "unread")])
  (define vs-exp (vector-ref (fasl->s-exp (s-exp->fasl (vector (cons u u)))) 0))
  ;; these are not `eq?` to the original symbol, but are `eq? to each other
  (test #t eq? (car vs-exp) (cdr vs-exp))
  (define hs-exp (hash-ref (fasl->s-exp (s-exp->fasl (hasheq 0 (cons u u)))) 0))
  (test #t eq? (car hs-exp) (cdr hs-exp)))

(let ()
  ;; Check that a prefab struct key is not duplicated in fasled form
  (define s (s-exp->fasl (list #s(look-for-this-prefab-key 1)
                               #s(look-for-this-prefab-key 2))))
  (test #f regexp-match? #rx"look-for-this-prefab-key.*look-for-this-prefab-key" s))

;; check uses datum-intern-literal:
(test #t eq? "hello" (fasl->s-exp (s-exp->fasl "hello")))
(test #t eq? #"hello" (fasl->s-exp (s-exp->fasl #"hello")))

(test #f eq? "hello" (fasl->s-exp (s-exp->fasl "hello") #:datum-intern? #f))
(test #f eq? #"hello" (fasl->s-exp (s-exp->fasl #"hello") #:datum-intern? #f))
(test #f eq? #rx"hello" (fasl->s-exp (s-exp->fasl #rx"hello") #:datum-intern? #f))
(test #f eq? #px"hello" (fasl->s-exp (s-exp->fasl #px"hello") #:datum-intern? #f))
(test #f eq? #rx#"hello" (fasl->s-exp (s-exp->fasl #rx#"hello") #:datum-intern? #f))
(test #f eq? #px#"hello" (fasl->s-exp (s-exp->fasl #px#"hello") #:datum-intern? #f))

(let* ([r1 #rx"[/\u5C][. ]+ap"]
       [r2 #px"[/\u5C][. ]+ap"]
       [r3 #px#"[\\][. ]+ap*"])
  (test #t equal? r1 (fasl->s-exp (s-exp->fasl r1) #:datum-intern? #f))
  (test #t equal? r2 (fasl->s-exp (s-exp->fasl r2) #:datum-intern? #f))
  (test #t equal? r3 (fasl->s-exp (s-exp->fasl r3) #:datum-intern? #f)))

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
    (test windows-path fasl->s-exp (s-exp->fasl windows-path))
    (if (eq? (system-path-convention-type) 'unix)
        (test (srcloc "here" 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc unix-path 1 2 3 4)))
        (test (srcloc "there" 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc windows-path 1 2 3 4))))
    (let ([root (car (filesystem-root-list))])
      (test (srcloc (path->string root) 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc root 1 2 3 4)))
      (test (srcloc (path->string (build-path root "x")) 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path root "x") 1 2 3 4)))
      (test (srcloc (path->string (build-path root 'up)) 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path root 'up) 1 2 3 4)))
      (test (srcloc (path->string (build-path root 'same)) 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path root 'same) 1 2 3 4))))
    (test (srcloc ".../a/b" 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path (current-directory) "a" "b") 1 2 3 4)))))

(let* ([file-p (build-path "data.rktd")]
       [dir-p (build-path "nested")]
       [rel-p (build-path dir-p file-p)]
       [p (build-path (current-directory) rel-p)])
  (define-values (bstr srcloc-bstr)
    (parameterize ([current-write-relative-directory (current-directory)])
      (values
       (s-exp->fasl p)
       (s-exp->fasl (srcloc p 10 20 30 40)))))
  (parameterize ([current-load-relative-directory #f])
    (test rel-p fasl->s-exp bstr)
    (test (srcloc rel-p 10 20 30 40) fasl->s-exp srcloc-bstr))
  (parameterize ([current-load-relative-directory (current-directory)])
    (test p fasl->s-exp bstr)
    (test (srcloc p 10 20 30 40) fasl->s-exp srcloc-bstr))

  ;; Try a pair for `current-write-relative-directory`
  (let* ([alt-rel-p (build-path "alternate" "bytes.rktd")]
         [alt-p (build-path (current-directory) alt-rel-p)])
    (define-values (bstr srcloc-bstr bstr2 srcloc-bstr2)
      (parameterize ([current-write-relative-directory (cons (build-path (current-directory) dir-p)
                                                             (current-directory))])
        (values
         (s-exp->fasl p)
         (s-exp->fasl (srcloc p 10 20 30 40))
         (s-exp->fasl alt-p)
         (s-exp->fasl (srcloc alt-p 10 20 30 40)))))
    (parameterize ([current-load-relative-directory #f])
      (test file-p fasl->s-exp bstr)
      (test (srcloc file-p 10 20 30 40) fasl->s-exp srcloc-bstr)
      (test (build-path 'up alt-rel-p) fasl->s-exp bstr2)
      (test (srcloc (build-path 'up alt-rel-p) 10 20 30 40) fasl->s-exp srcloc-bstr2))
    (parameterize ([current-load-relative-directory (build-path (current-directory) dir-p)])
      (test p fasl->s-exp bstr)
      (test (srcloc p 10 20 30 40) fasl->s-exp srcloc-bstr)
      (let ([up-alt-p (build-path (current-directory) dir-p 'up alt-rel-p)])
        (test up-alt-p fasl->s-exp bstr2)
        (test (srcloc up-alt-p 10 20 30 40) fasl->s-exp srcloc-bstr2)))))

(test (srcloc ".../a" 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path 'up "a") 1 2 3 4)))
(test (srcloc ".../a" 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path 'same "a") 1 2 3 4)))
(test (srcloc ".../a/.." 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path "a" 'up) 1 2 3 4)))
(test (srcloc ".../a/." 1 2 3 4) fasl->s-exp (s-exp->fasl (srcloc (build-path "a" 'same) 1 2 3 4)))

(let ([root (car (filesystem-root-list))])
  (test
   root
   'longer-relative
   (parameterize ([current-write-relative-directory (build-path root "a")])
     (fasl->s-exp (s-exp->fasl root))))

  (test
   (build-path 'same)
   'this-dir-path
   (parameterize ([current-write-relative-directory root]
                  [current-load-relative-directory #f])
     (fasl->s-exp (s-exp->fasl (build-path root 'same))))))

(report-errs)
