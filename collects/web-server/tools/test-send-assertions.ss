(module test-send-assertions mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "servlet.ss" "web-server")
           "send-assertions.ss"
           )

  (provide test-send-assertions)

  (define test-send-assertions
    (make-test-suite
      "Test the test suite that tests send/*"

      (make-test-case
        "Test send/finish"
        (assert-output-response/suspended
          (lambda () (send/finish '(p "The output")))
          '()
          '(p () "The output")))

      (make-test-case
        "Test send/suspend and send/finish"
        (assert-output-response/suspended
          (lambda ()
            (let ((num (extract-binding/single
                         'num
                         (request-bindings
                           (send/suspend
                             (lambda (k-url)
                               `(form ((action ,k-url)) (input ((name "num"))))))))))
              (send/finish `(p ,num))))
          (list (cons (lambda (x) (cadr (car (cadr x))))
                      (list (cons 'num "5"))))
          '(p () "5")))

      (make-test-case
        "Test send/suspend/callback once"
        (assert-output-response/suspended
          (lambda ()
            (send/suspend/callback
              `(p () (a ((href ,(lambda (req) (send/finish '(p "Finished")))))
                        "Next"))))
          (list (cons (lambda (x) (cadr (car (cadr (caddr x)))))
                      '()))
          '(p () "Finished")))

      (make-test-case
        "Test mutual send/suspend/callbacks"
        (assert-output-response/suspended
          (lambda ()
            (letrec ((p1 `(p () (a ((href ,(lambda (req) (send/suspend/callback p2))))
                                   "Next")))
                     (p2 `(p () (a ((href ,(lambda (req) (send/suspend/callback p1))))
                                   "Previous"))))
              (send/suspend/callback p1)))
          (list (cons (lambda (x) (cadr (car (cadr (caddr x))))) '())
                (cons (lambda (x) (cadr (car (cadr (caddr x))))) '()))
          `(p () (a ((href ,(make-unknown))) "Next")))))

    )

  )
