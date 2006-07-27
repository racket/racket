#!/usr/bin/mzscheme -qu
;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;;
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham

(module fannkuch mzscheme
  (define vector-for-each (lambda (pred v)
    (do ((i 0 (add1 i))
       (v-length (vector-length v)))
      ((>= i v-length))
    (pred (vector-ref v i)))))

  (define (vector-reverse-slice! v i j)
    (do ((i i (add1 i))
       (j (sub1 j) (sub1 j))) ; exclude position j
      ((<= j i))
    (vector-swap! v i j)))

  (define (vector-swap! v i j)
    (let ((t (vector-ref v i)))
      (vector-set! v i (vector-ref v j))
      (vector-set! v j t)))

  (define (count-flips pi)
    (do ((rho (vector-copy pi))
         (i 0 (add1 i)))
        ((= (vector-ref rho 0) 0) i)
      (vector-reverse-slice! rho 0 (add1 (vector-ref rho 0)))))

  (define (vector-copy source)
    (do ((vec (make-vector (vector-length source)))
         (i 0 (add1 i)))
        ((= i (vector-length source)) vec)
      (vector-set! vec i (vector-ref source i))))

  (define (fannkuch n)
    (let ((pi (do ((pi (make-vector n))
                   (i 0 (add1 i)))
                ((= i n) pi)
                (vector-set! pi i i)))
          (r n)
          (count (make-vector n)))
      (let loop ((flips 0)
                 (perms 0))
        (cond ((< perms 30)
               (vector-for-each (lambda (x)
                                  (display (add1 x)))
                                pi)
               (newline)))
        (do ()
          ((= r 1))
          (vector-set! count (sub1 r) r)
          (set! r (sub1 r)))
        (let ((flips2 (max (count-flips pi) flips)))
          (let ((result
                  (let loop2 ()
                    (if (= r n)
                      flips2
                      (let ((perm0 (vector-ref pi 0)))
                        (do ((i 0))
                          ((>= i r))
                          (let ((j (add1 i)))
                            (vector-set! pi i (vector-ref pi j))
                            (set! i j)))
                        (vector-set! pi r perm0)
                        (vector-set! count r (sub1 (vector-ref count r)))
                        (cond ((<= (vector-ref count r) 0)
                               (set! r (add1 r))
                               (loop2))
                              (else
                                #f)))))))
            (or result
                (loop flips2 (add1 perms)))
            )))))

  (define (main args)
    (if (< (vector-length args) 1)
      (begin (display "An argument is required") (newline) 2)
      (let ((n (string->number (vector-ref args 0))))
        (if (not (integer? n))
          (begin (display "An integer is required") (newline) 2)
          (printf "Pfannkuchen(~S) = ~S~%" n (fannkuch n))))))

  (main (current-command-line-arguments)))
