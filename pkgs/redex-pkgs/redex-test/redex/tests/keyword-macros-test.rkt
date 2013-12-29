#lang scheme

(require redex/private/keyword-macros
         "test-util.rkt")

(module test racket/base)

(reset-count)

(let* ([formals `((#:b ,#'1) (#:c ,#'2))]
       [parse 
        (λ (actuals) 
          (map syntax-e 
               (parse-kw-args formals (cdr (syntax-e actuals)) actuals 'dontcare)))])
  (let-syntax ([msg-src
                (syntax-rules ()
                  [(_ expr)
                   (with-handlers ([exn:fail:syntax?
                                    (λ (exn)
                                      (values (exn-message exn)
                                              (exn:fail:syntax-exprs exn)))])
                     (begin expr (values 'no-msg 'no-src)))])])
    (let ()
      (test (parse #'(a #:c 3 #:b 4)) '(4 3))
      (test (parse #'(a #:b 4 #:c 3)) '(4 3))
      (test (parse #'(a #:c 3)) '(1 3))
      (let*-values ([(kw) (syntax-taint #'#:b)]
                    [(msg src) (msg-src (parse #`(a #,kw)))])
        (test msg #rx"a: missing argument expression after keyword")
        (test src (list kw)))
      (let*-values ([(arg) (syntax-taint #'1)]
                    [(msg src) (msg-src (parse #`(a #:b 1 #,arg)))])
        (test msg #rx"a: expected a keyword")
        (test src (list arg)))
      (let*-values ([(kw) (syntax-taint #'#:c)]
                    [(msg src) (msg-src (parse #`(a #:c 1 #:b 2 #,kw 3)))])
        (test msg #rx"a: repeated keyword")
        (test src (list kw)))
      (let*-values ([(kw) (syntax-taint #'#:c)]
                    [(msg src) (msg-src (parse #`(a #:b #,kw 3)))])
        (test msg #rx"a: expected an argument expression")
        (test src (list kw)))
      (let*-values ([(kw) (syntax-taint #'#:typo)]
                    [(msg src) (msg-src (parse #`(a #:b 3 #,kw 4)))])
        (test msg #rx"a: invalid keyword")
        (test src (list kw))))))

(define-namespace-anchor test-module)

(let* ([default #'3]
       [formals `((#:a ,default (,#'(-> number? string?) "#:a arg")))]
       [form 'test-form]
       [parse (λ (actuals) (parse-kw-args formals actuals actuals form))])
  (test (first (parse #'())) default)
  (define arg
    (eval (first (parse #'(#:a (λ (x) 3)))) 
          (namespace-anchor->namespace test-module)))
  (test-contract-violation
   (arg 3)
   #:blaming "keyword-macros-test"
   #:message "#:a arg")
  (test-contract-violation
   (arg "NaN")
   #:blaming (format "~a" form)
   #:message "#:a arg"))

(print-tests-passed 'keyword-macros-test.rkt)
