
#lang scheme/base
(require (for-syntax scheme/base)
         scheme/match
         scheme/contract
         "deriv.ss"
         "deriv-util.ss"
         "stx-util.ss")

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

         handle-hiding-failure
         
         (struct-out hiding-failure)
         (struct-out nonlinearity)
         (struct-out localactions)
         (struct-out hidden-lift-site)

         DEBUG-LIFTS
         current-unvisited-lifts
         current-unhidden-lifts
         add-unhidden-lift
         extract/remove-unvisited-lift
         
         (struct-out SKtuple)
         SKlet
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

;; Warnings

(define (handle-hiding-failure d failure)
  (match failure
    [(struct nonlinearity (term paths))
     (warn 'nonlinearity term paths d)]
    [(struct localactions ())
     (warn 'localactions d)]
    [(struct hidden-lift-site ())
     (warn 'hidden-lift-site d)]))


;; Lift management

(define-syntax DEBUG-LIFTS
  (syntax-rules ()
    [(DEBUG-LIFTS . b)
     (void)]
    #;
    [(DEBUG-LIFTS . b)
     (begin . b)]))

;; current-unvisited-lifts : (paramter-of Derivation)
;; The derivs for the lifts yet to be seen in the processing 
;; of the first part of the current lift-deriv.
(define current-unvisited-lifts (make-parameter null))

;; current-unhidden-lifts : (parameter-of Derivation)
;; The derivs for those lifts that occur within unhidden macros.
;; Derivs are moved from the current-unvisited-lifts to this list.
(define current-unhidden-lifts (make-parameter null))

;; add-unhidden-lift : Derivation -> void
(define (add-unhidden-lift d)
  (when d
    (current-unhidden-lifts
     (cons d (current-unhidden-lifts)))))

;; extract/remove-unvisted-lift : identifier -> Derivation
(define (extract/remove-unvisited-lift id)
  (define (get-defined-id d)
    (match d
      [(Wrap deriv (e1 e2))
       (with-syntax ([(?define-values (?id) ?expr) e1])
         #'?id)]))
  ;; The Wrong Way
  (let ([unvisited (current-unvisited-lifts)])
    (if (null? unvisited)
        (begin (DEBUG-LIFTS
                (printf "hide:extract/remove-unvisited-lift: out of lifts!"))
               #f)
        (let ([lift (car unvisited)])
          (DEBUG-LIFTS
           (printf "extracting lift: ~s left\n" (length (cdr unvisited))))
          (current-unvisited-lifts (cdr unvisited))
          lift)))
  ;; The Right Way
  ;; FIXME: Doesn't work inside of modules. Why not?
  #;
  (let loop ([lifts (current-unvisited-lifts)]
             [prefix null])
    (cond [(null? lifts)
           (DEBUG-LIFTS
            (fprintf (current-error-port)
                     "hide:extract/remove-unvisited-lift: can't find lift for ~s~n"
                     id))
           (raise (make localactions))]
          [(bound-identifier=? id (get-defined-id (car lifts)))
           (let ([lift (car lifts)])
             (current-unvisited-lifts
              (let loop ([prefix prefix] [lifts (cdr lifts)])
                (if (null? prefix)
                    lifts
                    (loop (cdr prefix) (cons (car prefix) lifts)))))
             lift)]
          [else
           (loop (cdr lifts) (cons (car lifts) prefix))])))



;; Macros

(define-syntax recv
  (syntax-rules ()
    [(recv body)
     (begin body)]
    [(recv [(var ...) expr] . more)
     (let-values ([(var ...) expr]) (recv . more))]))

;; H data

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

;; OLD SK = (values (list-of SubItem) ?exn)

(define-struct SKtuple (subs exn))

(define subitem/c (or/c s:subterm? s:rename?))
(define SK/c (struct/c SKtuple (listof subitem/c) (or/c exn? false/c)))

(define-syntax-rule (SKlet ([x y] c) . body)
  (match-let ([(struct SKtuple (x y)) c]) . body))

(define/contract SKunit
  ((listof subitem/c) . -> . SK/c)
  (lambda (x) (make SKtuple x #f)))

(define/contract SKzero
  (-> SK/c)
  (lambda () (make SKtuple null #f)))

(define/contract SKfail
  (exn? . -> . SK/c)
  (lambda (exn) (make SKtuple null exn)))

(define/contract SKseq
  (SK/c SK/c . -> . SK/c)
  (lambda (c1 c2)
    (SKlet ((si1 exn1) c1)
           (if (not exn1)
               (SKlet ((si2 exn2) c2)
                      (make SKtuple (append si1 si2) exn2))
               (make SKtuple si1 exn1)))))

(define/contract SKmap
  ((any/c . -> . SK/c) (listof any/c) . -> . SK/c)
  (lambda (f xs)
    (if (pair? xs)
        (SKseq (f (car xs))
               (SKmap f (cdr xs)))
        (SKzero))))

(define/contract SKmap2
  ((any/c any/c . -> . SK/c) (listof any/c) (listof any/c) . -> . SK/c)
  (lambda (f xs ys)
    (if (pair? xs)
        (SKseq (f (car xs) (car ys))
               (SKmap2 f (cdr xs) (cdr ys)))
        (SKzero))))

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
         (SKseq (SKunit subterms)
                (>>Seek . more))))]
    [(>>Seek expr . more)
     (SKseq expr
            (>>Seek . more))]))
