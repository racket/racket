#lang racket/base

(require racket/generic rackunit)

(define proc
  (make-keyword-procedure
    (lambda (ks vs . xs)
      (append (map list ks vs) xs))))

(define-generics foo
  (bar1/1 foo)
  (bar1/2 foo x)
  (bar2/2 x foo)
  (bar3 foo x y)
  (bar4 foo x y z)
  (bar3+1 foo x y [z])
  (bar2+2 foo x [y] [z])
  (bar1+3 foo [x] [y] [z])
  (bar4* foo x y z . w)
  (bar3+1* foo x y [z] . w)
  (bar2+2* foo x [y] [z] . w)
  (bar1+3* foo [x] [y] [z] . w)
  (bar-abc foo #:a x #:b y #:c z)
  (bar-ab/c foo #:a x #:b y #:c [z])
  (bar-a/bc foo #:a x #:b [y] #:c [z])
  (bar/abc foo #:a [x] #:b [y] #:c [z])
  (bar
    foo
    aa ab ac ad
    ae af ag ah
    ai aj ak al
    am an ao ap
    [ba] [bb] [bc] [bd]
    [be] [bf] [bg] [bh]
    [bi] [bj] [bk] [bl]
    [bm] [bn] [bo] [bp]
    #:a ca #:b cb #:c cc #:d cd
    #:e ce #:f cf #:g cg #:h ch
    #:i ci #:j cj #:k ck #:l cl
    #:m cm #:n cn #:o co #:p cp
    #:A [da] #:B [db] #:C [dc] #:D [dd]
    #:E [de] #:F [df] #:G [dg] #:H [dh]
    #:I [di] #:J [dj] #:K [dk] #:L [dl]
    #:M [dm] #:N [dn] #:O [do] #:P [dp]
    . e*)
  #:defaults
  ([number?
    (define bar1/1 proc)
    (define bar1/2 proc)
    (define bar2/2 proc)
    (define bar3 proc)
    (define bar4 proc)
    (define bar3+1 proc)
    (define bar2+2 proc)
    (define bar1+3 proc)
    (define bar4* proc)
    (define bar3+1* proc)
    (define bar2+2* proc)
    (define bar1+3* proc)
    (define bar-abc proc)
    (define bar-ab/c proc)
    (define bar-a/bc proc)
    (define bar/abc proc)
    (define bar proc)]))

(check-equal? (bar1/1 1) '(1))
(check-equal? (bar1/2 1 2) '(1 2))
(check-equal? (bar2/2 1 2) '(1 2))
(check-equal? (bar3 1 2 3) '(1 2 3))
(check-equal? (bar4 1 2 3 4) '(1 2 3 4))
(check-equal? (bar3+1 1 2 3 4) '(1 2 3 4))
(check-equal? (bar3+1 1 2 3) '(1 2 3))
(check-equal? (bar2+2 1 2 3 4) '(1 2 3 4))
(check-equal? (bar2+2 1 2 3) '(1 2 3))
(check-equal? (bar2+2 1 2) '(1 2))
(check-equal? (bar1+3 1 2 3 4) '(1 2 3 4))
(check-equal? (bar1+3 1 2 3) '(1 2 3))
(check-equal? (bar1+3 1 2) '(1 2))
(check-equal? (bar1+3 1) '(1))
(check-equal? (bar4* 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7))
(check-equal? (bar4* 1 2 3 4) '(1 2 3 4))
(check-equal? (bar3+1* 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7))
(check-equal? (bar3+1* 1 2 3 4) '(1 2 3 4))
(check-equal? (bar3+1* 1 2 3) '(1 2 3))
(check-equal? (bar2+2* 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7))
(check-equal? (bar2+2* 1 2 3 4) '(1 2 3 4))
(check-equal? (bar2+2* 1 2 3) '(1 2 3))
(check-equal? (bar2+2* 1 2) '(1 2))
(check-equal? (bar1+3* 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7))
(check-equal? (bar1+3* 1 2 3 4) '(1 2 3 4))
(check-equal? (bar1+3* 1 2 3) '(1 2 3))
(check-equal? (bar1+3* 1 2) '(1 2))
(check-equal? (bar1+3* 1) '(1))

(check-equal? (bar-abc 1 #:a 2 #:b 3 #:c 4) '((#:a 2) (#:b 3) (#:c 4) 1))
(check-equal? (bar-ab/c 1 #:a 2 #:b 3 #:c 4) '((#:a 2) (#:b 3) (#:c 4) 1))
(check-equal? (bar-ab/c 1 #:a 2 #:b 3) '((#:a 2) (#:b 3) 1))
(check-equal? (bar-a/bc 1 #:a 2 #:b 3 #:c 4) '((#:a 2) (#:b 3) (#:c 4) 1))
(check-equal? (bar-a/bc 1 #:a 2 #:b 3) '((#:a 2) (#:b 3) 1))
(check-equal? (bar-a/bc 1 #:a 2 #:c 4) '((#:a 2) (#:c 4) 1))
(check-equal? (bar-a/bc 1 #:a 2) '((#:a 2) 1))
(check-equal? (bar/abc 1 #:a 2 #:b 3 #:c 4) '((#:a 2) (#:b 3) (#:c 4) 1))
(check-equal? (bar/abc 1 #:a 2 #:b 3) '((#:a 2) (#:b 3) 1))
(check-equal? (bar/abc 1 #:a 2 #:c 4) '((#:a 2) (#:c 4) 1))
(check-equal? (bar/abc 1 #:a 2) '((#:a 2) 1))
(check-equal? (bar/abc 1 #:b 3 #:c 4) '((#:b 3) (#:c 4) 1))
(check-equal? (bar/abc 1 #:b 3) '((#:b 3) 1))
(check-equal? (bar/abc 1 #:c 4) '((#:c 4) 1))
(check-equal? (bar/abc 1) '(1))

(check-equal?
  (bar
    00
    01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
    #:a 33 #:b 34 #:c 35 #:d 36 #:e 37 #:f 38 #:g 39 #:h 40
    #:i 41 #:j 42 #:k 43 #:l 44 #:m 45 #:n 46 #:o 47 #:p 48
    #:A 49 #:B 50 #:C 51 #:D 52 #:E 53 #:F 54 #:G 55 #:H 56
    #:I 57 #:J 58 #:K 59 #:L 60 #:M 61 #:N 62 #:O 63 #:P 64
    65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80)
  '((#:A 49) (#:B 50) (#:C 51) (#:D 52) (#:E 53) (#:F 54) (#:G 55) (#:H 56)
    (#:I 57) (#:J 58) (#:K 59) (#:L 60) (#:M 61) (#:N 62) (#:O 63) (#:P 64)
    (#:a 33) (#:b 34) (#:c 35) (#:d 36) (#:e 37) (#:f 38) (#:g 39) (#:h 40)
    (#:i 41) (#:j 42) (#:k 43) (#:l 44) (#:m 45) (#:n 46) (#:o 47) (#:p 48)
    00
    01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
    65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80))

(check-equal?
  (bar
    00
    01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24
    #:a 33 #:b 34 #:c 35 #:d 36 #:e 37 #:f 38 #:g 39 #:h 40
    #:i 41 #:j 42 #:k 43 #:l 44 #:m 45 #:n 46 #:o 47 #:p 48
    #:A 49 #:C 51 #:E 53 #:G 55
    #:I 57 #:K 59 #:M 61 #:O 63)
  '((#:A 49) (#:C 51) (#:E 53) (#:G 55)
    (#:I 57) (#:K 59) (#:M 61) (#:O 63)
    (#:a 33) (#:b 34) (#:c 35) (#:d 36) (#:e 37) (#:f 38) (#:g 39) (#:h 40)
    (#:i 41) (#:j 42) (#:k 43) (#:l 44) (#:m 45) (#:n 46) (#:o 47) (#:p 48)
    00
    01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24))

(check-equal?
  (bar
    00
    01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16
    #:a 33 #:b 34 #:c 35 #:d 36 #:e 37 #:f 38 #:g 39 #:h 40
    #:i 41 #:j 42 #:k 43 #:l 44 #:m 45 #:n 46 #:o 47 #:p 48)
  '((#:a 33) (#:b 34) (#:c 35) (#:d 36) (#:e 37) (#:f 38) (#:g 39) (#:h 40)
    (#:i 41) (#:j 42) (#:k 43) (#:l 44) (#:m 45) (#:n 46) (#:o 47) (#:p 48)
    00
    01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16))

(define pred exn:fail:contract?)

(check-exn pred (lambda () (bar1/1)))
(check-exn pred (lambda () (bar1/2 1)))
(check-exn pred (lambda () (bar2/2 1)))
(check-exn pred (lambda () (bar3 1 2)))
(check-exn pred (lambda () (bar4 1 2 3)))
(check-exn pred (lambda () (bar3+1 1 2)))
(check-exn pred (lambda () (bar2+2 1)))
(check-exn pred (lambda () (bar1+3)))
(check-exn pred (lambda () (bar4* 1 2 3)))
(check-exn pred (lambda () (bar3+1* 1 2)))
(check-exn pred (lambda () (bar2+2* 1)))
(check-exn pred (lambda () (bar1+3*)))
(check-exn pred (lambda () (bar-abc #:a 2 #:b 3 #:c 4)))
(check-exn pred (lambda () (bar-abc 1 #:b 3 #:c 4)))
(check-exn pred (lambda () (bar-abc 1 #:a 2 #:c 4)))
(check-exn pred (lambda () (bar-abc 1 #:a 2 #:b 3)))
(check-exn pred (lambda () (bar-ab/c #:a 2 #:b 3)))
(check-exn pred (lambda () (bar-ab/c 1 #:b 3)))
(check-exn pred (lambda () (bar-ab/c 1 #:a 2)))
(check-exn pred (lambda () (bar-a/bc #:a 2)))
(check-exn pred (lambda () (bar-a/bc 1)))
(check-exn pred (lambda () (bar/abc)))

(check-exn pred (lambda () (bar1/1 1 2)))
(check-exn pred (lambda () (bar1/2 1 2 3)))
(check-exn pred (lambda () (bar2/2 1 2 3)))
(check-exn pred (lambda () (bar3 1 2 3 4)))
(check-exn pred (lambda () (bar4 1 2 3 4 5)))
(check-exn pred (lambda () (bar3+1 1 2 3 4 5)))
(check-exn pred (lambda () (bar2+2 1 2 3 4 5)))
(check-exn pred (lambda () (bar1+3 1 2 3 4 5)))
(check-exn pred (lambda () (bar-abc 1 #:a 2 #:b 3 #:c 4 5)))
(check-exn pred (lambda () (bar-abc 1 #:a 2 #:b 3 #:c 4 #:d 5)))
(check-exn pred (lambda () (bar-ab/c 1 #:a 2 #:b 3 5)))
(check-exn pred (lambda () (bar-ab/c 1 #:a 2 #:b 3 #:d 5)))
(check-exn pred (lambda () (bar-a/bc 1 #:a 2 5)))
(check-exn pred (lambda () (bar-a/bc 1 #:a 2 #:d 5)))
(check-exn pred (lambda () (bar/abc 1 5)))
(check-exn pred (lambda () (bar/abc 1 #:d 5)))
