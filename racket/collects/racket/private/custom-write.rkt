#lang racket/base


(provide make-constructor-style-printer
         (struct-out keyword-prefixed-field))


(require racket/contract
         racket/pretty)


;; make-constructor-style-printer : (Any -> (U String Symbol))
;;                                  (Any -> (Sequenceof Any))
;;                               -> (Any OutputPort (U #t #f 0 1)) -> Void
(define (make-constructor-style-printer get-constructor get-contents)
  (lambda (obj port mode)
    (define (recur x p)
      (case mode
        ((#t) (write x p))
        ((#f) (display x p))
        ((0 1) (print x p mode))))

    ;; Only two cases: 0 vs everything else
    (define (print-prefix p)
      (let ([prefix
             (case mode
               ((0) "(")
               (else "#<"))]
            [constructor
             (get-constructor obj)]
            [post-constr
             (case mode
               ((0) "")
               (else ":"))])
        (write-string prefix p)
        (display constructor p)
        (write-string post-constr p)))

    (define (print-suffix p)
      (let ([suffix
             (case mode
               ((0) ")")
               (else ">"))])
        (write-string suffix p)))

    (define (print-contents p leading-space)
      (let ([lead (if leading-space (make-string (add1 leading-space) #\space) " ")])
        (for ([elt (get-contents obj)]) ;; note: generic sequence
          (when leading-space
            (pretty-print-newline p (pretty-print-columns)))
          (write-string lead p)
          (recur elt p))))

    (define (print/one-line p)
      (print-prefix p)
      (print-contents p #f)
      (print-suffix p))

    (define (print/multi-line p)
      (let-values ([(line col pos) (port-next-location p)])
        (print-prefix p)
        (print-contents p col)
        (print-suffix p)))

    (cond [(and (pretty-printing)
                (integer? (pretty-print-columns)))
           ((let/ec esc
              (letrec ([tport
                        (make-tentative-pretty-print-output-port
                         port
                         (- (pretty-print-columns) 1)
                         (lambda () 
                           (esc
                            (lambda ()
                              (tentative-pretty-print-port-cancel tport)
                              (print/multi-line port)))))])
                (print/one-line tport)
                (tentative-pretty-print-port-transfer tport port))
              void))]
          [else
           (print/one-line port)])
    (void)))


(struct keyword-prefixed-field (keyword value)

  #:transparent

  #:guard (struct-guard/c keyword? any/c)

  #:methods gen:custom-write
  [(define (write-proc this port mode)
     (define keyword (keyword-prefixed-field-keyword this))
     (define field-value (keyword-prefixed-field-value this))

     (define (recur x p)
       (case mode
         ((#t) (write x p))
         ((#f) (display x p))
         ((0 1) (print x p mode))))

     (define (print-field p leading-space)
       (write-string "#:" p)
       (write-string (keyword->string keyword) p)
       (define lead (make-string (+ (or leading-space 0) 2) #\space))
       (when leading-space
         (pretty-print-newline p (pretty-print-columns)))
       (write-string lead p)
       (recur field-value p))

     (define (print/one-line p)
       (print-field p #f))

     (define (print/multi-line p)
       (define-values (unused-line col unused-pos) (port-next-location p))
       (print-field p col))

     (cond
       [(and (pretty-printing)
             (integer? (pretty-print-columns)))
        ((let/ec esc
           (define tport
             (make-tentative-pretty-print-output-port
              port
              (- (pretty-print-columns) 1)
              (λ () 
                (esc
                 (λ ()
                   (tentative-pretty-print-port-cancel tport)
                   (print/multi-line port))))))
           (print/one-line tport)
           (tentative-pretty-print-port-transfer tport port)
           void))]
       [else (print/one-line port)])
     (void))])
