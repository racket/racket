#lang racket
(require racket/generic
         tests/eli-tester)

(define (massq idx l)
  (match l
    [(mcons (and v (mcons (? (curry equal? idx)) _)) _)
     v]
    [(mcons _ rst)
     (massq idx rst)]
    [null
     #f]))

(test #:on-pass 'quiet
 (local
   [(define-generics lots
      (f #:foo foo lots zog [def]))

    (define-struct ex ()
      #:methods gen:lots
      [(define (f #:foo foo lots zog [def #t])
         1)])]
   (test #:on-pass 'quiet
    (f #:foo 3 (make-ex) 2) => 1
    (f (make-ex) #:foo 3 2) => 1
    (f (make-ex) 2 #:foo 3) => 1))

 (local
   [(define-generics lots
      (f #:foo foo lots zog #:def [def]))

    (define-struct ex ()
      #:methods gen:lots
      [(define (f #:foo foo lots zog #:def [def #t])
         1)])]
   (test #:on-pass 'quiet
    (f #:foo 3 (make-ex) 2) => 1
    (f (make-ex) 4 #:foo 3 #:def 2) => 1
    (f (make-ex) 3 #:foo 1) => 1))

 (local
   [(define-generics lots
      (f lots idx val))

    (define-struct ex ()
      #:methods gen:lots
      [(define/generic gen:f f)
       (define (f lots idx val)
         (if (zero? idx)
             val
             (gen:f lots (sub1 idx) (* 2 val))))])]
   (test #:on-pass 'quiet
    (f (make-ex) 4 1) => (expt 2 4)))

 (local
   [(define-generics table
      (get table idx [default])
      (weird-get idx table)
      (put! table idx new))

    (define-struct alist ([l #:mutable])
      #:methods gen:table
      ((define (get table idx [default #f])
         (cond [(massq idx (alist-l table)) => mcdr]
               [else default]))
       (define (weird-get idx table)
         (get table idx))
       (define (put! table idx new)
         (let* ([l (alist-l table)]
                [prev (massq idx l)])
           (if prev
               (set-mcar! prev new)
               (set-alist-l! table (mcons (mcons idx new) (alist-l table))))))))]

   (test #:on-pass 'quiet
    (make-alist empty)

    (get (make-alist empty) 'foo) => #f

    (local [(define t (make-alist empty))]
      (put! t 'foo 1)
      (get t 'foo))
    =>
    1

    (weird-get 'foo (make-alist empty)) => #f

    (local [(define t (make-alist empty))]
      (put! t 'foo 1)
      (weird-get 'foo t))
    =>
    1))

 (test #:on-pass 'quiet
  (define-generics table
    (get idx [default]))
  =error>
  #px"generic(s group)? name.+required,? (by-position|positional) argument"

  (define-generics table
    (get idx [table] [default]))
  =error>
  #px"generic(s group)? name.+required,? (by-position|positional) argument")


 (local [(define-generics printable
           (gen-print printable [port])
           (gen-port-print port printable)
           (gen-print* printable [port] #:width width #:height [height]))

         (define-struct num (v)
           #:methods gen:printable
           ((define/generic super-print gen-print)
            (define (gen-print n [port (current-output-port)])
              (fprintf port "Num: ~a" (num-v n)))
            (define (gen-port-print port n)
              (super-print n port))
            (define (gen-print* n [port (current-output-port)] #:width w #:height [h 0])
              (fprintf port "Num (~ax~a): ~a" w h (num-v n)))))

         (define-struct bool (v)
           #:methods gen:printable
           ((define/generic super-print gen-print)
            (define (gen-print b [port (current-output-port)])
              (fprintf port "Bool ~a" (if (bool-v b) "Yes" "No")))
            (define (gen-port-print port b)
              (super-print b port))
            (define (gen-print* b [port (current-output-port)] #:width w #:height [h 0])
              (fprintf port "Bool (~ax~a): ~a" w h (if (bool-v b) "Yes" "No")))))

         (define x (make-num 10))
         (define y (make-bool #t))]
   (parameterize ([current-output-port (open-output-nowhere)])
     (test #:on-pass 'quiet
       (gen-print x)
       (gen-port-print (current-output-port) x)
       (gen-print* x #:width 100 #:height 90)

       (gen-print y)
       (gen-port-print (current-output-port) y)
       (gen-print* y #:width 100 #:height 90)))))
