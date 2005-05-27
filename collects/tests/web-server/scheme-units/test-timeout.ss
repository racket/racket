;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test if the server does timeout
(module test-timeout mzscheme
  (require (lib "test.ss" "schemeunit")
           "assertions.ss"
           )

  (provide test-timeout)

  (define *timeout* 40)

  (define test-timeout
    (make-test-suite
      "Does the server time out?"
      (make-test-case
        (format "Does the server time out after ~a seconds?" *timeout*)
        (assert-with-server
          "/servlets/test1.ss"
          (lambda (http-port)
            (assert-pred
              (lambda (in)
                (sleep *timeout*) ;; Wait for it to timeout
                (and (char-ready? in) (eof-object? (read-char in))))
              (let-values (((in out) (tcp-connect THE-IP THE-PORT)))
                in)))))))

  ;;; TODO adjust-timeout!
  )
