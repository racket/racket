#lang at-exp racket/base

(require xrepl/xrepl)
(define-values [do-wrapped-output wrap-column]
  (parameterize ([current-namespace (module->namespace 'xrepl/xrepl)])
    (apply values (map namespace-variable-value
                       '(do-wrapped-output wrap-column)))))

(define test-num 0)

(define (test to-wrap expected)
  (parameterize ([current-output-port (open-output-string)])
    (do-wrapped-output (λ () (display to-wrap)) #rx#"^# *")
    (define result (get-output-string (current-output-port)))
    (set! test-num (add1 test-num))
    (unless (equal? result expected)
      (error
       'test-wrapped-output
       (string-append "test failure in test #~a\n----input----\n~a\n"
                      "----expected----\n~a\n----received----\n~a\n----")
       test-num to-wrap expected result))))

(define s string-append)
(define n "\n")

(provide test-wrapping-output)
(module+ main (test-wrapping-output))
(define (test-wrapping-output)
  (wrap-column 12)
  (test @s{blah} @s{blah})
  (test @s{blah@n} @s{blah@n})
  (test @s{#blah} @s{#blah})
  (test @s{#blah@n} @s{#blah@n})
  (test @s{#blah @n} @s{#blah@n})
  (test @s{# blah@n} @s{# blah@n})
  (test @s{# blah @n} @s{# blah@n})
  (test @s{#blah
           #blah@n}
        @s{#blah
           #blah@n})
  (test @s{#ab cd ef gh ij kl mn op qr st}
        @s{#ab cd ef gh
           #  ij kl mn
           #  op qr st})
  (test @s{#ab cd ef gh ij kl mn op qr st@n}
        @s{#ab cd ef gh
           #  ij kl mn
           #  op qr st@n})
  (test @s{#ab
           #cd ef gh ij kl mn op qr st@n}
        @s{#ab
           #cd ef gh ij
           #  kl mn op
           #  qr st@n})
  (test @s{#  ab
           #  cd ef gh ij kl mn op qr st@n}
        @s{#  ab
           #  cd ef gh
           #    ij kl
           #    mn op
           #    qr st@n})
  (test @s{# ab
           # cd ef gh ij kl mn         op   qr st@n}
        @s{# ab
           # cd ef gh
           #   ij kl mn
           #   op   qr
           #   st@n})
  (printf "~a wrapped output tests passed\n" test-num)
  #t)
