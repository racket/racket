
(load-relative "loadtest.ss")

(Section 'zo-marshal)

(require compiler/zo-parse
         compiler/zo-marshal)

(define (check expr val #:wrap [wrap values])
  (let ([s (zo-marshal expr)])
    (test expr zo-parse (open-input-bytes s))
    (test val wrap (eval (parameterize ([read-accept-compiled #t])
                           (read (open-input-bytes s)))))))

(define (get-id id)
  (primval-id
   (compilation-top-code
    (zo-parse (let ([s (open-output-bytes)])
                (write (compile id) s)
                (open-input-bytes (get-output-bytes s)))))))

(define values-id (get-id #'values))
(define object-name-id (get-id #'object-name))

;; ----------------------------------------

(define (make-simple e)
  (make-compilation-top
   10
   (make-prefix 0 null null)
   e))

;; ----------------------------------------

(check (make-simple 5)
       5)

(let ([ck (lambda (cl? o-cls?)
            (check (make-simple (make-let-one 
                                 51
                                 (make-localref #f 0 cl? o-cls?)))
                   51))])
  (ck #f #f)
  (ck #t #f)
  (ck #f #t))


(check (make-simple (make-let-one 
                     15
                     (make-boxenv
                      0
                      (make-localref #t 0 #f #f))))
       15)

(check (make-simple (make-let-void
                     3
                     #f
                     (make-install-value
                      1
                      0
                      #f
                      503
                      (make-boxenv
                       0
                       (make-localref #t 0 #f #f)))))
       503)

(check (make-simple (make-let-void
                     3
                     #f
                     (make-install-value
                      2
                      1
                      #f
                      (make-application
                       (make-primval values-id)
                       (list 503
                             507))
                      (make-localref #f 2 #f #f))))
       507)

(check (make-simple (make-branch
                     #t
                     50
                     -50))
       50)

(check (make-simple (make-branch
                     #f
                     50
                     -50))
       -50)

;; ----------------------------------------

(define (make-ab body)
  (make-simple (make-let-void
                2
                #f
                (make-let-rec
                 (list
                  (make-lam 'a
                            null
                            1
                            '(val)
                            #f
                            #(1)
                            10
                            (make-branch
                             (make-localref #f 1 #f #f)
                             (make-localref #f 0 #f #f)
                             59))
                  (make-lam 'b
                            null
                            1
                            '(val)
                            #f
                            #(0)
                            10
                            (make-localref #f 0 #f #f)))
                 body))))

(check (make-ab 517)
       517)

(check (make-ab (make-application
                 (make-primval object-name-id)
                 (list (make-localref #f 1 #f #f))))
       'a)
(check (make-ab (make-application
                 (make-primval object-name-id)
                 (list (make-localref #f 2 #f #f))))
       'b)

(check (make-ab (make-application
                 (make-localref #f 1 #f #f)
                 (list #f)))
       59)
(check (make-ab (make-application
                 (make-primval object-name-id)
                 (list
                  (make-application
                   (make-localref #f 2 #f #f)
                   (list #t)))))
       'b)
(check (make-ab (make-application
                 (make-primval object-name-id)
                 (list
                  (make-application
                   (make-application
                    (make-localref #f 3 #f #f)
                    (list #t))
                   (list -5)))))
       'a)

;; ----------------------------------------


;; ----------------------------------------

(report-errs)
