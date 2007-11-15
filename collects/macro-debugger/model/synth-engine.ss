
(module synth-engine mzscheme
  (require "deriv.ss"
           "deriv-util.ss"
           "stx-util.ss"
           (lib "plt-match.ss")
           (lib "contract.ss"))

  (provide recv
           >>P
           >>Pn
           >>Prim
           >>Seek
           macro-policy
           phase
           force-letrec-transformation
           subterms-table

           current-hiding-warning-handler
           warn

           (struct hiding-failure ())
           (struct nonlinearity (term paths))
           (struct localactions ())
           (struct hidden-lift-site ())
           
           SKunit
           SKzero
           SKseq
           SKmap
           SKmap2)

  ;; Parameters
  
  ;; macro-policy : parameter of (identifier -> boolean)
  (define macro-policy (make-parameter (lambda (id) #t)))

  ;; phase : parameter of number
  (define phase (make-parameter 0))

  ;; force-letrec-transformation : parameter of boolean
  (define force-letrec-transformation (make-parameter #f))

  ;; subterms-table : parameter of hashtable[syntax => (list-of Path)]
  (define subterms-table (make-parameter #f))

  ;; current-hiding-warning-handler : (parameter-of (symbol any -> void))
  (define current-hiding-warning-handler
    (make-parameter
     (lambda (tag args) (printf "hiding warning: ~a\n" tag))))
  
  (define (warn tag . args)
    ((current-hiding-warning-handler) tag args))
  

  ;; Machinery for reporting things that macro hiding can't handle

  (define-struct hiding-failure ())
  (define-struct (nonlinearity hiding-failure) (term paths))
  (define-struct (localactions hiding-failure) ())
  (define-struct (hidden-lift-site hiding-failure) ())


  ;; Macros
  
  (define-syntax recv
    (syntax-rules ()
      [(recv body)
       (begin body)]
      [(recv [(var ...) expr] . more)
       (let-values ([(var ...) expr]) (recv . more))]))

  (define (Hunit d s)
    (values d s #f))

  (define (Hfail d x)
    (values d #f x))

  (define-syntax H
    (syntax-rules ()
      [(H e1 e2 pr-expr . clauses)
       (let ([f e1])
         (H* f e2 _ pr-expr . clauses))]))
  
  (define-syntax H*
    (syntax-rules ()
      [(H* f e2 p pe)
       (let ([e2 f])
         (Hunit pe f))]
      
      [(H* f e2 p pe [! e] . clauses)
       (let ([x e])
         (if (not x)
             (H* f e2 p pe . clauses)
             (let ([e2 #f])
               (Hfail pe x))))]
      
      [(H* f e2 p pe [#:pattern p2] . clauses)
       (H* f e2 p2 pe . clauses)]

      [(H* f e2 p pe [#:hide-if expr seek-expr] . clauses)
       (if (with-syntax ([p f]) expr)
           seek-expr
           (H* f e2 p pe . clauses))]
      
      ;; Subterm
      [(H* f e2 p pe [#:sub process hole fill] . clauses)
       (let ([(fill s x) (process fill)])
         (if (not x)
             (let ([f2 (with-syntax ([p f]) (with-syntax ([hole s]) #'p))])
               (H* f2 e2 p pe . clauses))
             (let ([e2 #f]) (Hfail pe x))))]))


  (define-syntax (>>P stx)
    (syntax-case stx ()
      [(>>P pr (S var ...) pattern . clauses)
       #'(>>PrimI pr #t (S var ...) pattern pattern . clauses)]))

  ;; >>P with no restamping
  (define-syntax (>>Pn stx)
    (syntax-case stx ()
      [(>>Pn pr (S var ...) pattern . clauses)
       #'(>>PrimI pr #f (S var ...) pattern pattern . clauses)]))

  (define-syntax (>>PrimI stx)
    (syntax-case stx ()
      [(>>PrimI pr restamp? cons+vars inp outp . clauses)
       #'(let ([prvar pr])
           (>>Prim prvar (wderiv-e1 prvar) restamp? cons+vars inp outp . clauses))]))

  (define-syntax (>>Prim stx)
    (syntax-case stx ()
      [(>>Prim pr given-e1 restamp? (S var ...)
               in-pattern
               out-pattern
               ([recur hole fill/bind] ...))
       (let ([restamp? (syntax-e #'restamp?)])
         (with-syntax ([(s-tmp ...) (generate-temporaries #'(fill/bind ...))])
           #`(let ([prule-var pr])
               (let-values ([(fill/bind s-tmp)
                             (let ([fbvar fill/bind])
                               (if fbvar (recur fbvar) (values fbvar #f)))] ...)
                 (let ([new-e2
                        (if (interrupted-node? prule-var)
                            #f
                            (with-syntax ([in-pattern given-e1])
                              (with-syntax ([hole s-tmp] ...)
                                #,(if restamp?
                                      #'(syntax/restamp out-pattern #'out-pattern
                                                        (wderiv-e2 prule-var))
                                      #'#'out-pattern))))])
                   (let ([new-pr
                          (match prule-var
                            [(Wrap prule (e1 _ rs ?1))
                             (make S e1 new-e2 rs ?1 var ...)])])
                     (values new-pr new-e2)))))))]))


  ;; Seek

  ;; SK = (values (list-of SubItem) ?exn)

  (define subitem/c (or/c s:subterm? s:rename?))
  (define-syntax ->SK/c
    (syntax-rules ()
      [(->SK/c domain ...)
       (-> domain ... (values (listof subitem/c) (or/c exn? false/c)))]))
  
  (define/contract SKunit
    (->SK/c (listof subitem/c))
    (lambda (x)
      (values x #f)))

  (define/contract SKzero
    (->SK/c)
    (lambda () (values null #f)))
  
  (define/contract SKfail
    (->SK/c exn?)
    (lambda (exn)
      (values null exn)))
    
  (define/contract SKseq
    (->SK/c (->SK/c) (->SK/c))
    (lambda (c1 c2)
      (recv [(si1 exn1) (c1)]
            (if (not exn1)
                (recv [(si2 exn2) (c2)]
                      (values (append si1 si2) exn2))
                (values si1 exn1)))))

  (define (SKmap f xs)
    (if (pair? xs)
        (SKseq (lambda () (f (car xs)))
               (lambda () (SKmap f (cdr xs))))
        (SKzero)))

  (define (SKmap2 f xs ys)
    (if (pair? xs)
        (SKseq (lambda () (f (car xs) (car ys)))
               (lambda () (SKmap f (cdr xs) (cdr ys))))
        (SKzero)))

  (define-syntax >>Seek
    (syntax-rules (! =>)
      [(>>Seek) (SKunit null)]
      [(>>Seek [! exn] . more)
       (if (not exn)
           (>>Seek . more)
           (SKfail exn))]
      [(>>Seek [#:table t] . more)
       (parameterize ((subterms-table t))
         (>>Seek . more))]
      [(>>Seek [#:rename/no expr] . more)
       (let-values ([(subterms new-table) expr])
         (parameterize ((subterms-table new-table))
           (>>Seek . more)))]
      [(>>Seek [#:rename expr] . more)
       (let-values ([(subterms new-table) expr])
         (parameterize ((subterms-table new-table))
           (SKseq (lambda () (SKunit subterms))
                  (lambda () (>>Seek . more)))))]
      [(>>Seek expr . more)
       (SKseq (lambda () expr)
              (lambda () (>>Seek . more)))]))
  )
