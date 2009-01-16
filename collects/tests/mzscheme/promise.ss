
(load-relative "loadtest.ss")

(Section 'promise)

(require scheme/promise)

;; check that things are `promise?'s or not

(for ([v (list 1 '(1) (lambda () 1))])
  (test #f promise? v))
(for ([v (list (delay 1) (lazy 1) (delay (delay 1)) (lazy (lazy 1)))])
  (test #t promise? v))

(let ()
  (define thunk1 (lambda () 1))
  ;; test a few different values
  (define-syntax (t stx)
    (define _ (datum->syntax stx '_ stx))
    (syntax-case stx ()
      [(t (f x ...))
       (with-syntax ([_ _])
         #'(begin (let ([_ 1]) (test _ f x ...))
                  (let ([_ '()]) (test _ f x ...))
                  (let ([_ '(1)]) (test _ f x ...))
                  (let ([_ thunk1]) (test _ f x ...))))]))
  ;; `force' is identity for non-promises
  (t (force _))
  ;; basic checks that `delay' works as expected with all kinds of values
  (t (force (delay _)))
  (t (force (force (delay (delay _)))))
  (t (force (delay (force (delay _)))))
  ;; basic checks that `lazy' works as expected with all kinds of values
  (t (force (lazy _)))
  (t (force (lazy (lazy _))))
  (t (force (force (lazy (lazy _)))))
  (t (force (lazy (lazy (lazy (lazy _))))))
  ;; check that `lazy' combines as expected with `delay' in regards to `force'
  ;;   (generally, each `L*D?' sequence requires a force)
  (t (force (lazy (delay _))))
  (t (force (lazy (lazy (delay _)))))
  (t (force (lazy (lazy (lazy (delay _))))))
  ;; two delays = two forces
  (t (force (force (lazy (delay (delay _))))))
  (t (force (force (delay (lazy (delay _))))))
  (t (force (force (lazy (lazy (delay (delay _)))))))
  (t (force (force (lazy (delay (lazy (delay _)))))))
  (t (force (force (delay (lazy (lazy (delay _)))))))
  (t (force (force (lazy (lazy (lazy (delay (delay _))))))))
  (t (force (force (lazy (lazy (delay (lazy (delay _))))))))
  (t (force (force (lazy (delay (lazy (lazy (delay _))))))))
  (t (force (force (delay (lazy (lazy (lazy (delay _))))))))
  ;; now push the second force inside
  (t (force (lazy  (force (delay (delay _))))))
  (t (force (delay (force (lazy (delay _))))))
  (t (force (lazy  (force (lazy (delay (delay _)))))))
  (t (force (lazy  (force (delay (lazy (delay _)))))))
  (t (force (delay (force (lazy (lazy (delay _)))))))
  (t (force (lazy  (force (lazy (lazy (delay (delay _))))))))
  (t (force (lazy  (force (lazy (delay (lazy (delay _))))))))
  (t (force (lazy  (force (delay (lazy (lazy (delay _))))))))
  (t (force (delay (force (lazy (lazy (lazy (delay _))))))))
  (t (force (lazy  (delay (force (delay _))))))
  (t (force (lazy  (lazy  (force (delay (delay _)))))))
  (t (force (lazy  (delay (force (lazy (delay _)))))))
  (t (force (lazy  (lazy  (force (lazy (delay (delay _))))))))
  (t (force (lazy  (lazy  (force (delay (lazy (delay _))))))))
  (t (force (lazy  (delay (force (lazy (lazy (delay _))))))))
  (t (force (lazy  (lazy  (delay (force (delay _)))))))
  (t (force (lazy  (lazy  (lazy  (force (delay (delay _))))))))
  (t (force (lazy  (lazy  (delay (force (lazy (delay _)))))))))

;; more tests
(let ()
  (define (force+catch p)
    (with-handlers ([void (lambda (x) (cons 'catch x))]) (force p)))
  (define (forced+running? p) (list (promise-forced? p) (promise-running? p)))
  ;; results are cached
  (let ([p (delay (random 10000))])
    (test #t equal? (force p) (force p)))
  ;; errors are cached
  (let ([p (delay (error 'foo "blah"))])
    (test #t equal? (force+catch p) (force+catch p)))
  ;; other raised values are cached
  (let ([p (delay (raise (random 10000)))])
    (test #t equal? (force+catch p) (force+catch p)))
  ;; test the predicates
  (letrec ([p (delay (forced+running? p))])
    (test '(#f #f) forced+running? p)
    (test '(#f #t) force p)
    (test '(#t #f) forced+running? p))
  )

(report-errs)
