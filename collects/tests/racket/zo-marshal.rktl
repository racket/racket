
(load-relative "loadtest.rktl")

(Section 'zo-marshal)

(require compiler/zo-parse
         compiler/zo-marshal)

(define-struct mpi (n b) #:transparent)

;; Exposes content of module-path indices, strip away
;; closure ids, and normalize `indirects' so we can compare them
;; with `equal?':
(define mpx
  (case-lambda
   [(v) (let ([it (make-hash)])
          (let loop ([v v])
            (cond
             [(pair? v) (cons (loop (car v)) (loop (cdr v)))]
             [(indirect? v)
              (or (hash-ref it v #f)
                  (let ([i (make-indirect #f)])
                    (hash-set! it v i)
                    (set-indirect-v! i
                                     (make-closure
                                      (loop (closure-code (indirect-v v)))
                                      'closure))
                    i))]
             [(closure? v) (make-indirect
                            (make-closure (loop (closure-code v)) 'closure))]
             [(struct? v) (let-values ([(st ?) (struct-info v)])
                            (if st
                                (let ([c (struct-type-make-constructor st)])
                                  (apply c
                                         (map loop
                                              (cdr 
                                               (vector->list
                                                (struct->vector v))))))
                                v))]
             [(module-path-index? v)
              (let-values ([(name base) (module-path-index-split v)])
                (make-mpi name base))]
             [else v])))]
   [(f v) (mpx (f v))]))

(define (check expr val #:wrap [wrap values])
  (let ([s (zo-marshal expr)])
    (test (mpx expr) mpx zo-parse (open-input-bytes s))
    (test val wrap (eval (parameterize ([read-accept-compiled #t])
                           (read (open-input-bytes s)))))))

(define (get-id id)
  (primval-id
   (compilation-top-code
    (zo-parse (let ([s (open-output-bytes)])
                (write (compile id) s)
                (open-input-bytes (get-output-bytes s)))))))

(define values-id (get-id #'values))
(define list-id (get-id #'list))
(define object-name-id (get-id #'object-name))

(define GLOBALV 78)
(module zo-m scheme/base
  (provide x)
  (define x 88))
(require 'zo-m)

;; ----------------------------------------

(define (make-simple e)
  (make-compilation-top
   10
   (make-prefix 0 null null)
   e))

(define (make-global e)
  (make-compilation-top
   10
   (make-prefix 0 (list (make-global-bucket 'GLOBALV)
                        (make-module-variable (module-path-index-join ''zo-m #f)
                                              'x
                                              -1
                                              0))
                null)
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

(check (make-simple 
        (make-let-one
         'v1
         (make-let-one
          'v0
          (make-let-one
           (make-lam 'proc
                     null
                     1
                     '(val)
                     #f
                     #(1 2)
                     20
                     (make-application
                      (make-primval list-id)
                      (list
                       (make-localref #f 2 #f #f)
                       (make-localref #f 3 #f #f))))
           (make-application
            (make-localref #f 1 #f #f)
            (list 5))))))
       '(v0 v1))

;; ----------------------------------------

(check (make-global
        (make-toplevel 0 0 #f #f))
       78)
(check (make-global
        (make-toplevel 0 1 #f #f))
       88)

;; ----------------------------------------

(check (make-simple
        (make-seq (list 1 56)))
       56)
(check (make-simple
        (make-splice (list 1 57)))
       57)
(check (make-global
        (make-splice (list (make-toplevel 0 0 #f #f) 57)))
       57)
(check (make-simple
        (make-beg0 (list 1 56)))
       1)
(check (make-global
        (make-beg0 (list 57 (make-toplevel 0 0 #f #f))))
       57)

;; ----------------------------------------

(check (make-simple
        (make-closure
         (make-lam 'proc
                   null
                   1
                   '(val)
                   #f
                   #()
                   10
                   (make-localref #f 0 #f #f))
         'proc))
       8
       #:wrap (lambda (f) (f 8)))

(define rec-proc
  (let ([self (make-indirect #f)])
    (set-indirect-v! self
                     (make-closure
                      (make-lam 'proc
                                null
                                1
                                '(val)
                                #f
                                #()
                                10
                                (make-branch
                                 (make-localref #f 0 #f #f)
                                 self
                                 17))
                      'proc))
    self))

(check (make-simple
        rec-proc)
       17
       #:wrap (lambda (f) (f #f)))
(check (make-simple
        rec-proc)
       'proc
       #:wrap (lambda (f) (object-name (f #t))))

;; ----------------------------------------

(define cl-proc
  (make-case-lam
   'cl-proc
   (list
    (make-lam 'proc
              null
              1
              '(val)
              #f
              #()
              10
              (make-localref #f 0 #f #f))
    (make-lam 'proc
              null
              2
              '(val val)
              #f
              #()
              10
              (make-application
               (make-primval list-id)
               (list
                (make-localref #f 2 #f #f)
                (make-localref #f 3 #f #f)))))))

(check (make-simple cl-proc)
       #:wrap (lambda (f) (f 3))
       3)
(check (make-simple cl-proc)
       #:wrap (lambda (f) (f 1 2))
       '(1 2))
(check (make-simple cl-proc)
       #:wrap object-name
       'cl-proc)

(define cl-proc2
  (make-let-one
   'cl1
   (make-let-one
    'cl2
    (make-case-lam
     'cl-proc
     (list
      (make-lam 'proc
                null
                0
                '()
                #f
                #(0)
                10
                (make-localref #f 0 #f #f))
      (make-lam 'proc
                null
                1
                '(val)
                #f
                #(1)
                10
                (make-application
                 (make-primval list-id)
                 (list
                  (make-localref #f 2 #f #f)
                  (make-localref #f 3 #f #f)))))))))
(check (make-simple cl-proc2)
       #:wrap (lambda (f) (f))
       'cl2)
(check (make-simple cl-proc2)
       #:wrap (lambda (f) (f 2))
       '(cl1 2))

;; ----------------------------------------

(check (make-global
        (make-varref (make-toplevel 0 0 #f #f)))
       #:wrap variable-reference?
       #t)

;; ----------------------------------------

(check (make-global
        (make-assign (make-toplevel 0 0 #f #f)
                     99
                     #f))
       (void))
(test 99 values GLOBALV)

;; ----------------------------------------

(check (make-global
        (make-apply-values
         (make-primval list-id)
         (make-application
          (make-primval values-id)
          (list 503
                507))))
       '(503 507))

;; ----------------------------------------

(report-errs)
