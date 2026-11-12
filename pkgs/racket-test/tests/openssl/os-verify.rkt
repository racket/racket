#lang racket/base
(require openssl)

(begin
  (define hosts
    '("dv-g3b-emudhra.we-have-a-problem.com"
      "dv-g3b-aaa-emudhra.we-have-a-problem.com"
      "dv-g3b-noct-emudhra.we-have-a-problem.com"
      "dv-g3b-aaa-noct-emudhra.we-have-a-problem.com"))
  (define (check-host host)
    (define context (ssl-secure-client-context))
    (with-handlers ([exn:fail? (lambda (e) 'blocked)])
      (ssl-connect host 443 context)
      (list 'connected host)))
  (define (check-hosts) (map check-host hosts)))

(printf "after:\n")
(parameterize ((ssl-default-verify-sources '((os-verify))))
  (for ([host hosts]) (printf " ~s\n" (check-host host))))
