(load-relative "loadtest.rkt")

(Section 'trace)

(require scheme/trace)

(define-syntax-rule (trace-output expr ...)
  (let ([out '()])
    (parameterize ([current-trace-notify
                    (lambda (e) (set! out (cons e out)))])
      expr ...
      (reverse out))))

(let ([n1 (let ([out '()])
            (parameterize ([current-trace-notify
                            (lambda (e) (set! out (cons e out)))])
              (define (foo x) x)
              (trace foo)
              (foo 2)
              out))])
  (test (reverse n1) 'test-it (list ">(foo 2)" "<2")))

(test (trace-output
        (define (foo x) x)
        (trace foo)
        (foo 2))
      'simple-trace
      (list ">(foo 2)"
            "<2"))

(test (trace-output
        (define (foo x) (add1 x))
        (trace foo)
        (foo 2))
      'simple-trace
      (list ">(foo 2)"
            "<3"))

(test (trace-output
        (define (a x) x)
        (define (b x) (a x))
        (define (c x) (+ (b x) (b x)))
        (trace a b c)
        (c 1))
      'trace2
      (list ">(c 1)"
            "> (b 1)"
            "> (a 1)"
            "< 1"
            "> (b 1)"
            "> (a 1)"
            "< 1"
            "<2"))
