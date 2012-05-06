#lang at-exp racket/base

(require tests/eli-tester xrepl/xrepl racket/string)
(define-values [do-xrepl-wrapped-output wrap-width]
  (parameterize ([current-namespace (module->namespace 'xrepl/xrepl)])
    (apply values (map namespace-variable-value
                       '(do-xrepl-wrapped-output wrap-width)))))

(define (wrap . text)
  (parameterize ([wrap-width 12]
                 [current-output-port (open-output-string)])
    ;; use "#"s in the input to avoid messing up highlighting in emacs
    (do-xrepl-wrapped-output
      (λ () (display (regexp-replace* #rx"#" (string-append* text) ";"))))
    (regexp-replace* #rx";" (get-output-string (current-output-port)) "#")))

(provide test-wrapped-output)
(module+ main (test-wrapped-output))
(define (test-wrapped-output)
  (define n "\n")
  (define s string-append)
  (define w wrap)
  (test @w{blah}      => @s{blah}
        @w{blah@n}    => @s{blah@n}
        @w{#blah}     => @s{#blah}
        @w{#blah@n}   => @s{#blah@n}
        @w{#blah @n}  => @s{#blah@n}
        @w{# blah@n}  => @s{# blah@n}
        @w{# blah @n} => @s{# blah@n}
        @w{#blah@|n|#blah@n}
        => @s{#blah@|n|#blah@n}
        @w{#ab cd ef gh ij kl mn op qr st}
        => @s{#ab cd ef gh@|n|#  ij kl mn@|n|#  op qr st}
        @w{#ab cd ef gh ij kl mn op qr st@n}
        => @s{#ab cd ef gh@|n|#  ij kl mn@|n|#  op qr st@n}
        @w{#ab@|n|#cd ef gh ij kl mn op qr st@n}
        => @s{#ab@|n|#cd ef gh ij@|n|#  kl mn op@|n|#  qr st@n}
        @w{#  ab@|n|#  cd ef gh ij kl mn op qr st@n}
        => @s{#  ab@|n|#  cd ef gh@|n|#    ij kl@|n|#    mn op@|n|#    qr st@n}
        @w{# ab@|n|# cd ef gh ij kl mn         op   qr st@n}
        => @s{# ab@|n|# cd ef gh@|n|#   ij kl mn@|n|#   op   qr@|n|#   st@n}))
