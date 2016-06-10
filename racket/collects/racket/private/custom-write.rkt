#lang racket/base
(require racket/pretty)
(provide make-constructor-style-printer
         prop:constructor-style-printer
         constructor-style-printer?
         constructor-style-printer-constructor
         constructor-style-printer-contents
         )

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

(define (prop-constructor-style-printer-guard v info)
  (unless (and (list? v) (= (length v) 2)
               (procedure? (car v))
               (procedure? (cadr v))
               (procedure-arity-includes? (car v) 1)
               (procedure-arity-includes? (cadr v) 1))
    (error 'prop:constructor-style-printer
           "expected a list of two one-argument functions, given ~v"
           v))
  v)

;; prop:constructor-style-printer : (Struct-Type-Propertyof
;;                                   (List (Constructor-Style-Printer . -> . (U String Symbol))
;;                                         (Constructor-Style-Printer . -> . (Sequenceof Any))))
;; constructor-style-printer? : Any -> Boolean : Constructor-Style-Printer
;; constructor-style-printer-accessor : Constructor-Style-Printer
;;                                      ->
;;                                      (List (Constructor-Style-Printer . -> . (U String Symbol))
;;                                            (Constructor-Style-Printer . -> . (Sequenceof Any)))
(define-values [prop:constructor-style-printer
                constructor-style-printer?
                constructor-style-printer-accessor]
  (make-struct-type-property 'constructor-style-printer
                             prop-constructor-style-printer-guard
                             (list (cons prop:custom-write
                                         (λ (v)
                                           (make-constructor-style-printer
                                            (car v)
                                            (cadr v)))))))

;; constructor-style-printer-constructor : Constructor-Style-Printer -> (U String Symbol)
(define (constructor-style-printer-constructor s)
  ((car (constructor-style-printer-accessor s)) s))

;; constructor-style-printer-contents : Constructor-Style-Printer -> (Sequenceof Any)
(define (constructor-style-printer-contents s)
  ((cadr (constructor-style-printer-accessor s)) s))

