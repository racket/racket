(module force "mz-without-promises.ss"
  (require "promise.ss")
  (provide (all-defined-except do-!!))

  (define-syntax ~ (make-rename-transformer #'lazy))
  (define ! force)

  (define (!! x) (do-!! x #f))
  ;; Similar to the above, but wrap procedure values too
  (define (!!! x) (do-!! x #t))
  ;; Force just a top-level list structure, similar to the above.
  ;; (todo: this and the next assumes no cycles.)
  (define (!list x)
    (let loop ([x x])
      (let ([x (! x)]) (when (mpair? x) (set-mcdr! x (loop (mcdr x)))) x)))
  ;; Force a top-level list structure and the first level of values, again,
  ;; similar to the above.
  (define (!!list x)
    (let loop ([x x])
      (let ([x (! x)])
        (when (mpair? x)
          (set-mcar! x (! (mcar x)))
          (set-mcdr! x (loop (mcdr x)))) x)))
  ;; Force and split resulting values.
  (define (!values x)
    (split-values (! x)))
  ;; Similar, but forces the actual values too.
  (define (!!values x)
    (let ([x (! x)])
      (if (multiple-values? x)
        (apply values (map ! (multiple-values-values x)))
        x)))

  ;; Multiple values are problematic: MzScheme promises can use multiple
  ;; values, but to carry that out `call-with-values' should be used in all
  ;; places that deal with multiple values, which will make the whole thing
  ;; much slower (about twice in tight loops) -- but multiple values are rarely
  ;; used (spceifically, students never use them).  So `values' is redefined to
  ;; produce a first-class tuple-holding struct, and `split-values' turns that
  ;; into multiple values.
  (define-struct multiple-values (values))
  (define (split-values x)
    (let ([x (! x)])
      (if (multiple-values? x) (apply values (multiple-values-values x)) x)))

  ;; Force a nested structure -- we don't distinguish values from promises so
  ;; it's fine to destructively modify the structure.
  (define (do-!! x translate-procedures?)
    (define table (make-hash-table)) ; avoid loops due to sharing
    (split-values ; see below
     (let loop ([x x])
       (let ([x (! x)])
         (unless (hash-table-get table x (lambda () #f))
           (hash-table-put! table x #t)
           (cond [(mpair? x)
                  (set-mcar! x (loop (car x)))
                  (set-mcdr! x (loop (cdr x)))]
                 [(vector? x)
                  (let vloop ([i 0])
                    (when (< i (vector-length x))
                      (vector-set! x i (loop (vector-ref x i)))
                      (vloop (add1 i))))]
                 [(box? x) (set-box! x (loop (unbox x)))]
                 [(struct? x)
                  (let-values ([(type skipped?) (struct-info x)])
                    (if type
                      (let*-values ([(name initk autok ref set imms spr skp?)
                                     (struct-type-info type)]
                                    [(k) (+ initk autok)])
                        (let sloop ([i 0])
                          (unless (= i k)
                            (set x i (loop (ref x i)))
                            (sloop (add1 i)))))
                      x))]))
         (if (and (procedure? x) translate-procedures?)
           (lambda args (do-!! (apply x args) #t))
           x))))))
