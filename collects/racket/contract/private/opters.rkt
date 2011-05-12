#lang racket/base
(require "misc.rkt"
         "opt.rkt"
         "guts.rkt"
         "arrow.rkt"
         "blame.rkt"
         "misc.rkt"
         (for-syntax racket/base
                     syntax/stx
                     "opt-guts.rkt"))

(define/opter (or/c opt/i opt/info stx)
  ;; FIXME code duplication
  (define (opt/or-unknown uctc)
    (let* ((lift-var (car (generate-temporaries (syntax (lift)))))
           (partial-var (car (generate-temporaries (syntax (partial))))))
      (values
       (with-syntax ((partial-var partial-var)
                     (lift-var lift-var)
                     (uctc uctc)
                     (val (opt/info-val opt/info)))
         (syntax (partial-var val)))
       (list (cons lift-var 
                   ;; FIXME needs to get the contract name somehow
                   (with-syntax ((uctc uctc))
                     (syntax (coerce-contract 'opt/c uctc)))))
       null
       (list (cons
              partial-var
              (with-syntax ((lift-var lift-var)
                            (blame (opt/info-blame opt/info)))
                (syntax ((contract-projection lift-var) blame)))))
       #f
       lift-var
       (list #f)
       null)))
  
  (define (opt/or-ctc ps)
    (let ((lift-from-hos null)
          (superlift-from-hos null)
          (partial-from-hos null))
      (let-values ([(opt-ps lift-ps superlift-ps partial-ps stronger-ribs hos ho-ctc)
                    (let loop ([ps ps]
                               [next-ps null]
                               [lift-ps null]
                               [superlift-ps null]
                               [partial-ps null]
                               [stronger-ribs null]
                               [hos null]
                               [ho-ctc #f])
                      (cond
                        [(null? ps) (values next-ps
                                            lift-ps
                                            superlift-ps
                                            partial-ps
                                            stronger-ribs
                                            (reverse hos)
                                            ho-ctc)]
                        [else
                         (let-values ([(next lift superlift partial flat _ this-stronger-ribs)
                                       (opt/i opt/info (car ps))])
                           (if flat
                               (loop (cdr ps)
                                     (cons flat next-ps)
                                     (append lift-ps lift)
                                     (append superlift-ps superlift)
                                     (append partial-ps partial)
                                     (append this-stronger-ribs stronger-ribs)
                                     hos
                                     ho-ctc)
                               (if (< (length hos) 1)
                                   (loop (cdr ps)
                                         next-ps
                                         (append lift-ps lift)
                                         (append superlift-ps superlift)
                                         (append partial-ps partial)
                                         (append this-stronger-ribs stronger-ribs)
                                         (cons (car ps) hos)
                                         next)
                                   (loop (cdr ps)
                                         next-ps
                                         lift-ps
                                         superlift-ps
                                         partial-ps
                                         stronger-ribs
                                         (cons (car ps) hos)
                                         ho-ctc))))]))])
        (with-syntax ((next-ps
                       (with-syntax (((opt-p ...) (reverse opt-ps)))
                         (syntax (or opt-p ...)))))
          (values
           (cond
             [(null? hos) 
              (with-syntax ([val (opt/info-val opt/info)]
                            [blame (opt/info-blame opt/info)])
                (syntax
                 (if next-ps 
                     val
                     (raise-blame-error blame
                                        val
                                        "none of the branches of the or/c matched"))))]
             [(= (length hos) 1) (with-syntax ((ho-ctc ho-ctc))
                                   (syntax
                                    (if next-ps val ho-ctc)))]
             ;; FIXME something's not right with this case.
             [(> (length hos) 1)
              (let-values ([(next-hos lift-hos superlift-hos partial-hos _ __ stronger-hos stronger-vars-hos)
                            (opt/or-unknown stx)])
                (set! lift-from-hos lift-hos)
                (set! superlift-from-hos superlift-hos)
                (set! partial-from-hos partial-hos)
                (with-syntax ((next-hos next-hos))
                  (syntax
                   (if next-ps val next-hos))))])
           (append lift-ps lift-from-hos)
           (append superlift-ps superlift-from-hos)
           (append partial-ps partial-from-hos)
           (if (null? hos) (syntax next-ps) #f)
           #f
           stronger-ribs)))))
  
  (syntax-case stx (or/c)
    [(or/c p ...)
     (opt/or-ctc (syntax->list (syntax (p ...))))]))


;;
;; between/c opters
;;
;; note that the checkers are used by both optimized and normal contracts.
;;
(define/opter (between/c opt/i opt/info stx)
  (syntax-case stx (between/c)
    [(between/c low high) 
     (let*-values ([(lift-low lifts1) (lift/binding #'low 'between-low empty-lifts)]
                   [(lift-high lifts2) (lift/binding #'high 'between-high lifts1)])
       (with-syntax ([n lift-low]
                     [m lift-high])
         (let ([lifts3 (lift/effect #'(check-between/c n m) lifts2)])
           (with-syntax ((val (opt/info-val opt/info))
                         (ctc (opt/info-contract opt/info))
                         (blame (opt/info-blame opt/info))
                         (this (opt/info-this opt/info))
                         (that (opt/info-that opt/info)))
             (values
              (syntax (if (and (number? val) (<= n val m)) 
                          val
                          (raise-blame-error
                           blame
                           val
                           "expected <~s>, given: ~e"
                           (contract-name ctc)
                           val)))
              lifts3
              null
              null
              (syntax (and (number? val) (<= n val m)))
              #f
              (list (new-stronger-var
                     lift-low
                     (λ (this that)
                       (with-syntax ([this this]
                                     [that that])
                         (syntax (<= that this)))))
                    (new-stronger-var
                     lift-high
                     (λ (this that)
                       (with-syntax ([this this]
                                     [that that])
                         (syntax (<= this that)))))))))))]))

(define-for-syntax (single-comparison-opter opt/info stx check-arg comparison arg)
  (with-syntax ([comparison comparison])
    (let*-values ([(lift-low lifts2) (lift/binding arg 'single-comparison-val empty-lifts)])
      (with-syntax ([m lift-low])
        (let ([lifts3 (lift/effect (check-arg #'m) lifts2)])
          (with-syntax ((val (opt/info-val opt/info))
                        (ctc (opt/info-contract opt/info))
                        (blame (opt/info-blame opt/info))
                        (this (opt/info-this opt/info))
                        (that (opt/info-that opt/info)))
            (values
             (syntax 
              (if (and (real? val) (comparison val m)) 
                  val
                  (raise-blame-error
                   blame
                   val
                   "expected <~s>, given: ~e"
                   (contract-name ctc)
                   val)))
             lifts3
             null
             null
             (syntax (and (number? val) (comparison val m)))
             #f
             (list (new-stronger-var
                    lift-low
                    (λ (this that)
                      (with-syntax ([this this]
                                    [that that])
                        (syntax (comparison this that)))))))))))))

(define/opter (>=/c opt/i opt/info stx)
  (syntax-case stx (>=/c)
    [(>=/c low)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '>=/c m)))
      #'>=
      #'low)]))

(define/opter (<=/c opt/i opt/info stx)
  (syntax-case stx (<=/c)
    [(<=/c high)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '<=/c m)))
      #'<=
      #'high)]))

(define/opter (>/c opt/i opt/info stx)
  (syntax-case stx (>/c)
    [(>/c low)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '>/c m)))
      #'>
      #'low)]))

(define/opter (</c opt/i opt/info stx)
  (syntax-case stx (</c)
    [(</c high)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '</c m)))
      #'<
      #'high)]))

(define/opter (cons/c opt/i opt/info stx)
  (define (opt/cons-ctc hdp tlp)
    (let-values ([(next-hdp lifts-hdp superlifts-hdp partials-hdp flat-hdp unknown-hdp stronger-ribs-hd)
                  (opt/i opt/info hdp)]
                 [(next-tlp lifts-tlp superlifts-tlp partials-tlp flat-tlp unknown-tlp stronger-ribs-tl)
                  (opt/i opt/info tlp)]
                 [(error-check) (car (generate-temporaries (syntax (error-check))))])
      (with-syntax ((next (with-syntax ((flat-hdp flat-hdp)
                                        (flat-tlp flat-tlp)
                                        (val (opt/info-val opt/info)))
                            (syntax
                             (and (pair? val)
                                  (let ((val (car val))) flat-hdp)
                                  (let ((val (cdr val))) flat-tlp))))))
        (values
         (with-syntax ((val (opt/info-val opt/info))
                       (ctc (opt/info-contract opt/info))
                       (blame (opt/info-blame opt/info)))
           (syntax (if next
                       val
                       (raise-blame-error
                        blame
                        val
                        "expected <~s>, given: ~e"
                        (contract-name ctc)
                        val))))
         (append
          lifts-hdp lifts-tlp
          (list (cons error-check
                      (with-syntax ((hdp hdp)
                                    (tlp tlp)
                                    (check (with-syntax ((flat-hdp
                                                          (cond
                                                            [unknown-hdp
                                                             (with-syntax ((ctc unknown-hdp))
                                                               (syntax (flat-contract/predicate? ctc)))]
                                                            [else (if flat-hdp #'#t #'#f)]))
                                                         (flat-tlp
                                                          (cond
                                                            [unknown-tlp
                                                             (with-syntax ((ctc unknown-tlp))
                                                               (syntax (flat-contract/predicate? ctc)))]
                                                            [else (if flat-tlp #'#t #'#f)])))
                                             (syntax (and flat-hdp flat-tlp)))))
                        (syntax
                         (unless check
                           (error 'cons/c "expected two flat contracts or procedures of arity 1, got: ~e and ~e"
                                  hdp tlp)))))))
         (append superlifts-hdp superlifts-tlp)
         (append partials-hdp partials-tlp)
         (syntax (if next #t #f))
         #f
         (append stronger-ribs-hd stronger-ribs-tl)))))
  
  (syntax-case stx (cons/c)
    [(cons/c hdp tlp)
     (opt/cons-ctc #'hdp #'tlp)]))

;; only used by the opters
(define (flat-contract/predicate? pred)
  (or (flat-contract? pred)
      (and (procedure? pred)
           (procedure-arity-includes? pred 1))))

(define/opter (cons/c opt/i opt/info stx)
  (define (opt/cons-ctc hdp tlp)
    (let-values ([(next-hdp lifts-hdp superlifts-hdp partials-hdp flat-hdp unknown-hdp stronger-ribs-hd)
                  (opt/i opt/info hdp)]
                 [(next-tlp lifts-tlp superlifts-tlp partials-tlp flat-tlp unknown-tlp stronger-ribs-tl)
                  (opt/i opt/info tlp)])
      (with-syntax ((check (with-syntax ((val (opt/info-val opt/info)))
                             (syntax (pair? val)))))
        (values
         (with-syntax ((val (opt/info-val opt/info))
                       (ctc (opt/info-contract opt/info))
                       (blame (opt/info-blame opt/info))
                       (next-hdp next-hdp)
                       (next-tlp next-tlp))
           (syntax (if check
                       (cons (let ((val (car val))) next-hdp)
                             (let ((val (cdr val))) next-tlp))
                       (raise-blame-error
                        blame
                        val
                        "expected <~s>, given: ~e"
                        (contract-name ctc)
                        val))))        
         (append lifts-hdp lifts-tlp) 
         (append superlifts-hdp superlifts-tlp)
         (append partials-hdp partials-tlp)
         (if (and flat-hdp flat-tlp)
             (with-syntax ((val (opt/info-val opt/info))
                           (flat-hdp flat-hdp)
                           (flat-tlp flat-tlp))
               (syntax (if (and check
                                (let ((val (car val))) flat-hdp)
                                (let ((val (cdr val))) flat-tlp)) #t #f)))
             #f)
         #f
         (append stronger-ribs-hd stronger-ribs-tl)))))
  
  (syntax-case stx (cons/c)
    [(_ hdp tlp) (opt/cons-ctc #'hdp #'tlp)]))


;;
;; arrow opter
;;
(define/opter (-> opt/i opt/info stx)
  (define (opt/arrow-ctc doms rngs)
    (let*-values ([(dom-vars rng-vars) (values (generate-temporaries doms)
                                               (generate-temporaries rngs))]
                  [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom)
                   (let loop ([vars dom-vars]
                              [doms doms]
                              [next-doms null]
                              [lifts-doms null]
                              [superlifts-doms null]
                              [partials-doms null]
                              [stronger-ribs null])
                     (cond
                       [(null? doms) (values (reverse next-doms)
                                             lifts-doms
                                             superlifts-doms
                                             partials-doms
                                             stronger-ribs)]
                       [else
                        (let-values ([(next lift superlift partial _ __ this-stronger-ribs)
                                      (opt/i (opt/info-swap-blame opt/info) (car doms))])
                          (loop (cdr vars)
                                (cdr doms)
                                (cons (with-syntax ((next next)
                                                    (car-vars (car vars)))
                                        (syntax (let ((val car-vars)) next)))
                                      next-doms)
                                (append lifts-doms lift)
                                (append superlifts-doms superlift)
                                (append partials-doms partial)
                                (append this-stronger-ribs stronger-ribs)))]))]
                  [(next-rngs lifts-rngs superlifts-rngs partials-rngs stronger-ribs-rng)
                   (let loop ([vars rng-vars]
                              [rngs rngs]
                              [next-rngs null]
                              [lifts-rngs null]
                              [superlifts-rngs null]
                              [partials-rngs null]
                              [stronger-ribs null])
                     (cond
                       [(null? rngs) (values (reverse next-rngs)
                                             lifts-rngs
                                             superlifts-rngs
                                             partials-rngs
                                             stronger-ribs)]
                       [else
                        (let-values ([(next lift superlift partial _ __ this-stronger-ribs)
                                      (opt/i opt/info (car rngs))])
                          (loop (cdr vars)
                                (cdr rngs)
                                (cons (with-syntax ((next next)
                                                    (car-vars (car vars)))
                                        (syntax (let ((val car-vars)) next)))
                                      next-rngs)
                                (append lifts-rngs lift)
                                (append superlifts-rngs superlift)
                                (append partials-rngs partial)
                                (append this-stronger-ribs stronger-ribs)))]))])
      (values
       (with-syntax ((blame (opt/info-blame opt/info))
                     ((dom-arg ...) dom-vars)
                     ((rng-arg ...) rng-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars))
                     ((next-rng ...) next-rngs))
         (syntax (begin
                   (check-procedure val #f dom-len 0 '() '() #| keywords |# blame)
                   (λ (dom-arg ...)
                     (let-values ([(rng-arg ...) (val next-dom ...)])
                       (values next-rng ...))))))
       (append lifts-doms lifts-rngs)
       (append superlifts-doms superlifts-rngs)
       (append partials-doms partials-rngs)
       #f
       #f
       (append stronger-ribs-dom stronger-ribs-rng))))
  
  (define (opt/arrow-any-ctc doms)
    (let*-values ([(dom-vars) (generate-temporaries doms)]
                  [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom)
                   (let loop ([vars dom-vars]
                              [doms doms]
                              [next-doms null]
                              [lifts-doms null]
                              [superlifts-doms null]
                              [partials-doms null]
                              [stronger-ribs null])
                     (cond
                       [(null? doms) (values (reverse next-doms)
                                             lifts-doms
                                             superlifts-doms
                                             partials-doms
                                             stronger-ribs)]
                       [else
                        (let-values ([(next lift superlift partial flat _ this-stronger-ribs)
                                      (opt/i (opt/info-swap-blame opt/info) (car doms))])
                          (loop (cdr vars)
                                (cdr doms)
                                (cons (with-syntax ((next next)
                                                    (car-vars (car vars)))
                                        (syntax (let ((val car-vars)) next)))
                                      next-doms)
                                (append lifts-doms lift)
                                (append superlifts-doms superlift)
                                (append partials-doms partial)
                                (append this-stronger-ribs stronger-ribs)))]))])
      (values
       (with-syntax ((blame (opt/info-blame opt/info))
                     ((dom-arg ...) dom-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars)))
         (syntax (begin
                   (check-procedure val #f dom-len 0 '() '() #|keywords|# blame)
                   (λ (dom-arg ...)
                     (val next-dom ...)))))
       lifts-doms
       superlifts-doms
       partials-doms
       #f
       #f
       stronger-ribs-dom)))
  
  (syntax-case* stx (-> values any) module-or-top-identifier=?
    [(-> dom ... (values rng ...))
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                        (syntax->list (syntax (rng ...)))))]
    [(-> dom ... any)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-any-ctc (syntax->list (syntax (dom ...)))))]
    [(-> dom ... rng)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                        (list #'rng)))]))

