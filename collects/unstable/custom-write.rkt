#lang racket/base
(require racket/pretty
         racket/match
         racket/sequence
         racket/contract/base)
(provide (contract-out
          [make-constructor-style-printer
           (-> (-> any/c (or/c symbol? string?))
               (-> any/c sequence?)
               (-> any/c output-port? (or/c #t #f 0 1) void?))]
          [prop:auto-custom-write
           (struct-type-property/c 'constructor)]))

;; TODO: deal with super struct types better
;;   - see "Problem" below

#|
Constructor-style printer
  - eg 'set' printer
  - in mode 0, "(" + constructor + { " " + elem }* + ")"
  - else,      "#<" + constructor + ":" + ... + ">"
  - print elems w/ same mode
  - never quotable
|#

(define-values (prop:auto-custom-write auto-custom-write? auto-custom-write-proc)
  (make-struct-type-property
   'auto-custom-write
   (lambda (val info)
     (case val
       ((constructor)
        (struct-info->get-constructor+get-contents info))))
   (list (cons prop:custom-print-quotable
               (lambda (auto-write-val) 'never))
         (cons prop:custom-write
               (lambda (auto-write-val)
                 (make-constructor-style-printer
                  (car auto-write-val)
                  (cdr auto-write-val)))))))

(define (struct-info->get-constructor+get-contents info)
  (match info
    [(list name init-ct auto-ct accessor mutator imms super skipped?)
     (let ([get-super-contents
            ;; Problem: if super type was not transparent (to current
            ;; inspector), then we don't get it (ie, skipped? is #t), and so we
            ;; can't tell if super type also has prop:auto-custom-write
            ;; property.
            (cond [skipped?
                   (error 'prop:auto-custom-write
                          "struct super type is inaccessible")]
                  [(not super)
                   #f]
                  [(auto-custom-write? super)
                   (cdr (auto-custom-write-proc super))]
                  [else
                   (let ([super-getters
                          (struct-info->get-constructor+get-contents
                           (call-with-values (lambda () (struct-type-info super))
                             list))])
                     (cdr super-getters))])])
       (define (get-constructor obj)
         name)
       (define (get-new-contents obj)
         (for/list ([i (in-range (+ init-ct auto-ct))])
           (accessor obj i)))
       (cons get-constructor
             (if get-super-contents
                 (lambda (obj)
                   (sequence-append (get-super-contents obj)
                                    (get-new-contents obj)))
                 get-new-contents)))]))

;; ----

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
        (for ([elt (get-contents obj)])
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
