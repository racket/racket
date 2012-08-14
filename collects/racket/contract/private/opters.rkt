#lang racket/base
(require "misc.rkt"
         "opt.rkt"
         "guts.rkt"
         "arrow.rkt"
         "blame.rkt"
         "misc.rkt"
         "arrow.rkt"
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
                     (val (opt/info-val opt/info)))
         (syntax (partial-var val)))
       (list (cons lift-var 
                   ;; FIXME needs to get the contract name somehow
                   (with-syntax ((uctc uctc))
                     (syntax (coerce-contract 'opt/c uctc)))))
       '()
       (list (cons
              partial-var
              (with-syntax ((lift-var lift-var)
                            (blame (opt/info-blame opt/info)))
                (syntax ((contract-projection lift-var) blame))))))))
  
  (define (opt/or-ctc ps)
    (define lift-from-hos null)
    (define superlift-from-hos null)
    (define partial-from-hos null)
    (define-values (opt-ps lift-ps superlift-ps partial-ps stronger-ribs hos ho-ctc chaperone? no-negative-blame)
      (let loop ([ps ps]
                 [next-ps null]
                 [lift-ps null]
                 [superlift-ps null]
                 [partial-ps null]
                 [stronger-ribs null]
                 [hos null]
                 [ho-ctc #f]
                 [chaperone? #t]
                 [no-negative-blame #t])
        (cond
          [(null? ps) (values next-ps
                              lift-ps
                              superlift-ps
                              partial-ps
                              stronger-ribs
                              (reverse hos)
                              ho-ctc
                              chaperone?
                              no-negative-blame)]
          [else
           (define ps-optres (opt/i opt/info (car ps)))
           (if (optres-flat ps-optres)
               (loop (cdr ps)
                     (cons (optres-flat ps-optres) next-ps)
                     (append lift-ps (optres-lifts ps-optres))
                     (append superlift-ps (optres-superlifts ps-optres))
                     (append partial-ps (optres-partials ps-optres))
                     (append (optres-stronger-ribs ps-optres) stronger-ribs)
                     hos
                     ho-ctc
                     (combine-two-chaperone?s chaperone? (optres-chaperone ps-optres))
                     (combine-two-no-negative-blame no-negative-blame (optres-no-negative-blame? ps-optres)))
               (if (null? hos)
                   (loop (cdr ps)
                         next-ps
                         (append lift-ps (optres-lifts ps-optres))
                         (append superlift-ps (optres-superlifts ps-optres))
                         (append partial-ps (optres-partials ps-optres))
                         (append (optres-stronger-ribs ps-optres) stronger-ribs)
                         (cons (car ps) hos)
                         (optres-exp ps-optres)
                         (combine-two-chaperone?s chaperone? (optres-chaperone ps-optres))
                         (combine-two-no-negative-blame no-negative-blame (optres-no-negative-blame? ps-optres)))
                   (loop (cdr ps)
                         next-ps
                         lift-ps
                         superlift-ps
                         partial-ps
                         stronger-ribs
                         (cons (car ps) hos)
                         ho-ctc
                         chaperone?
                         no-negative-blame)))])))
    (with-syntax ((next-ps
                   (with-syntax (((opt-p ...) (reverse opt-ps)))
                     (syntax (or opt-p ...)))))
      (build-optres
       #:exp
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
         [(= (length hos) 1)
          (with-syntax ([ho-ctc ho-ctc]
                        [val (opt/info-val opt/info)])
            (syntax
             (if next-ps val ho-ctc)))]
         ;; FIXME something's not right with this case.
         [(> (length hos) 1)
          (define-values (exp new-lifts new-superlifts new-partials) (opt/or-unknown stx))
          (set! lift-from-hos new-lifts)
          (set! superlift-from-hos new-superlifts)
          (set! partial-from-hos new-partials)
          #`(if next-ps val #,exp)])
       #:lifts
       (append lift-ps lift-from-hos)
       #:superlifts
       (append superlift-ps superlift-from-hos)
       #:partials
       (append partial-ps partial-from-hos)
       #:flat
       (if (null? hos) (syntax next-ps) #f)
       #:opt #f
       #:stronger-ribs stronger-ribs
       #:chaperone chaperone?
       #:no-negative-blame? no-negative-blame)))
  
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
             (build-optres
              #:exp
              (syntax (if (and (number? val) (<= n val m)) 
                          val
                          (raise-opt-between/c-error
                           blame val n m)))
              #:lifts lifts3
              #:superlifts null
              #:partials null
              #:flat (syntax (and (number? val) (<= n val m)))
              #:opt #f
              #:stronger-ribs
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
                         (syntax (<= this that))))))
              #:chaperone
              #t)))))]))

(define (raise-opt-between/c-error blame val lo hi)
  (raise-blame-error
   blame
   val
   '(expected: "a number between ~a and ~a" given: "~e")
   lo hi val))

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
            (build-optres
             #:exp
             (syntax 
              (if (and (real? val) (comparison val m)) 
                  val
                  (raise-opt-single-comparison-opter-error blame val comparison m)))
             #:lifts lifts3
             #:superlifts null
             #:partials null
             #:flat (syntax (and (number? val) (comparison val m)))
             #:opt #f
             #:stronger-ribs
             (list (new-stronger-var
                    lift-low
                    (λ (this that)
                      (with-syntax ([this this]
                                    [that that])
                        (syntax (comparison this that))))))
             #:chaperone #t)))))))

(define (raise-opt-single-comparison-opter-error blame val comparison m)
  (raise-blame-error
   blame
   val
   '(expected: "a number ~a ~a" given: "~e")
   (object-name comparison) m val))


(define/opter (=/c opt/i opt/info stx)
  (syntax-case stx (=/c)
    [(=/c x)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '=/c m)))
      #'=
      #'x)]))

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

;; only used by the opters
(define (flat-contract/predicate? pred)
  (or (flat-contract? pred)
      (and (procedure? pred)
           (procedure-arity-includes? pred 1))))

(define/opter (cons/c opt/i opt/info stx)
  (define (opt/cons-ctc hdp tlp)
    (define optres-hd (opt/i opt/info hdp))
    (define optres-tl (opt/i opt/info tlp))
    (with-syntax ((check (with-syntax ((val (opt/info-val opt/info)))
                           (syntax (pair? val)))))
      (build-optres
       #:exp
       (with-syntax ((val (opt/info-val opt/info))
                     (ctc (opt/info-contract opt/info))
                     (blame (opt/info-blame opt/info))
                     (next-hdp (optres-exp optres-hd))
                     (next-tlp (optres-exp optres-tl)))
         (syntax (if check
                     (cons (let ((val (car val))) next-hdp)
                           (let ((val (cdr val))) next-tlp))
                     (raise-blame-error
                      blame
                      val
                      '(expected: "~s" given: "~e")
                      (contract-name ctc)
                      val))))
       #:lifts
       (append (optres-lifts optres-hd) (optres-lifts optres-tl))
       #:superlifts
       (append (optres-superlifts optres-hd) (optres-superlifts optres-tl))
       #:partials
       (append (optres-partials optres-hd) (optres-partials optres-tl))
       #:flat
       (if (and (optres-flat optres-hd) (optres-flat optres-tl))
           (with-syntax ((val (opt/info-val opt/info))
                         (flat-hdp (optres-flat optres-hd))
                         (flat-tlp (optres-flat optres-tl)))
             (syntax (and check
                          (let ((val (car val))) flat-hdp)
                          (let ((val (cdr val))) flat-tlp))))
           #f)
       #:opt #f
       #:stronger-ribs
       (append (optres-stronger-ribs optres-hd) (optres-stronger-ribs optres-tl))
       #:chaperone
       (combine-two-chaperone?s (optres-chaperone optres-hd) (optres-chaperone optres-tl)))))
  
  (syntax-case stx (cons/c)
    [(_ hdp tlp) (opt/cons-ctc #'hdp #'tlp)]))

(define-for-syntax (opt/listof-ctc content non-empty? opt/i opt/info)
  (define optres-ele (opt/i opt/info content))
  (with-syntax ([check (with-syntax ((val (opt/info-val opt/info)))
                         (if non-empty?
                             #'(and (list? val) (pair? val))
                             #'(list? val)))]
                [val (opt/info-val opt/info)])
    
    (build-optres
     #:exp
     (with-syntax ([blame (opt/info-blame opt/info)]
                   [next (optres-exp optres-ele)])
       (with-syntax ([(non-empty-check ...) (if non-empty?
                                                (list #'(pair? val))
                                                (list))])
         #`(if check
               (for/list ([val (in-list val)])
                 next)
               (raise-blame-error
                blame
                val
                #,(if non-empty?
                      #''(expected "a non-empty list")
                      #''(expected "a list"))))))
     #:lifts (optres-lifts optres-ele)
     #:superlifts (optres-superlifts optres-ele)
     #:partials (optres-partials optres-ele)
     #:flat
     (if (optres-flat optres-ele)
         (with-syntax ((val (opt/info-val opt/info))
                       (flat (optres-flat optres-ele)))
           #`(and check
                  #,@(if non-empty? (list #'(pair? val)) '())
                  (let loop ([lst val])
                    (cond
                      [(null? lst) #t]
                      [else 
                       (let ([val (car lst)])
                         (and flat
                              (loop (cdr lst))))]))))
         #f)
     #:opt #f
     #:stronger-ribs (optres-stronger-ribs optres-ele)
     #:chaperone (optres-chaperone optres-ele))))

(define/opter (listof opt/i opt/info stx)
  (syntax-case stx ()
    [(_ content) (opt/listof-ctc #'content #f opt/i opt/info)]))

(define/opter (non-empty-listof opt/i opt/info stx)
  (syntax-case stx ()
    [(_ content) (opt/listof-ctc #'content #t opt/i opt/info)]))


;;
;; arrow opter
;;
(define/opter (-> opt/i opt/info stx)
  (define (opt/arrow-ctc doms rngs)
    (let*-values ([(dom-vars rng-vars) (values (generate-temporaries doms)
                                               (generate-temporaries rngs))]
                  [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom dom-chaperone?)
                   (let loop ([vars dom-vars]
                              [doms doms]
                              [next-doms null]
                              [lifts-doms null]
                              [superlifts-doms null]
                              [partials-doms null]
                              [stronger-ribs null]
                              [chaperone? #t])
                     (cond
                       [(null? doms) (values (reverse next-doms)
                                             lifts-doms
                                             superlifts-doms
                                             partials-doms
                                             stronger-ribs 
                                             chaperone?)]
                       [else
                        (define optres-dom (opt/i (opt/info-swap-blame opt/info) (car doms)))
                        (loop (cdr vars)
                              (cdr doms)
                              (cons (with-syntax ((next (optres-exp optres-dom))
                                                  (car-vars (car vars))
                                                  (val (opt/info-val opt/info)))
                                      (syntax (let ((val car-vars)) next)))
                                    next-doms)
                              (append lifts-doms (optres-lifts optres-dom))
                              (append superlifts-doms (optres-superlifts optres-dom))
                              (append partials-doms (optres-partials optres-dom))
                              (append (optres-stronger-ribs optres-dom) stronger-ribs)
                              (combine-two-chaperone?s chaperone? (optres-chaperone optres-dom)))]))]
                  [(next-rngs lifts-rngs superlifts-rngs partials-rngs stronger-ribs-rng rng-chaperone?)
                   (let loop ([vars rng-vars]
                              [rngs rngs]
                              [next-rngs null]
                              [lifts-rngs null]
                              [superlifts-rngs null]
                              [partials-rngs null]
                              [stronger-ribs null]
                              [chaperone? #t])
                     (cond
                       [(null? rngs) (values (reverse next-rngs)
                                             lifts-rngs
                                             superlifts-rngs
                                             partials-rngs
                                             stronger-ribs
                                             chaperone?)]
                       [else
                        (define optres-rng (opt/i opt/info (car rngs)))
                        (loop (cdr vars)
                              (cdr rngs)
                              (cons (with-syntax ((next (optres-exp optres-rng))
                                                  (car-vars (car vars))
                                                  (val (opt/info-val opt/info)))
                                      (syntax (let ((val car-vars)) next)))
                                    next-rngs)
                              (append lifts-rngs (optres-lifts optres-rng))
                              (append superlifts-rngs (optres-superlifts optres-rng))
                              (append partials-rngs (optres-partials optres-rng))
                              (append (optres-stronger-ribs optres-rng) stronger-ribs)
                              (combine-two-chaperone?s chaperone? (optres-chaperone optres-rng)))]))])
      (values
       (with-syntax ((val (opt/info-val opt/info))
                     (blame (opt/info-blame opt/info))
                     ((dom-arg ...) dom-vars)
                     ((rng-arg ...) rng-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars))
                     (rng-len (length rng-vars))
                     ((next-rng ...) next-rngs))
         (syntax (begin
                   (check-procedure val #f dom-len 0 '() '() #| keywords |# blame)
                   (chaperone-procedure
                    val
                    (case-lambda
                      [(dom-arg ...)
                       (values 
                        (case-lambda
                          [(rng-arg ...)
                           (values next-rng ...)]
                          [args 
                           (bad-number-of-results blame val rng-len args)])
                        next-dom ...)]
                      [args
                       (bad-number-of-arguments blame val args dom-len)])))))
       (append lifts-doms lifts-rngs)
       (append superlifts-doms superlifts-rngs)
       (append partials-doms partials-rngs)
       #f
       #f
       (append stronger-ribs-dom stronger-ribs-rng)
       (combine-two-chaperone?s dom-chaperone? rng-chaperone?))))
  
  (define (opt/arrow-any-ctc doms)
    (let*-values ([(dom-vars) (generate-temporaries doms)]
                  [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom dom-chaperone?)
                   (let loop ([vars dom-vars]
                              [doms doms]
                              [next-doms null]
                              [lifts-doms null]
                              [superlifts-doms null]
                              [partials-doms null]
                              [stronger-ribs null]
                              [chaperone? #t])
                     (cond
                       [(null? doms) (values (reverse next-doms)
                                             lifts-doms
                                             superlifts-doms
                                             partials-doms
                                             stronger-ribs
                                             chaperone?)]
                       [else
                        (define optres-dom (opt/i (opt/info-swap-blame opt/info) (car doms)))
                        (loop (cdr vars)
                              (cdr doms)
                              (cons #`(let ([#,(opt/info-val opt/info) #,(car vars)]) #,(optres-exp optres-dom))
                                    next-doms)
                              (append lifts-doms (optres-lifts optres-dom))
                              (append superlifts-doms (optres-superlifts optres-dom))
                              (append partials-doms (optres-partials optres-dom))
                              (append (optres-stronger-ribs optres-dom) stronger-ribs)
                              (combine-two-chaperone?s chaperone? (optres-chaperone optres-dom)))]))])
      (values
       (with-syntax ((blame (opt/info-blame opt/info))
                     ((dom-arg ...) dom-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars)))
         (syntax (begin
                   (check-procedure val #f dom-len 0 '() '() #|keywords|# blame)
                   (chaperone-procedure
                    val
                    (case-lambda
                      [(dom-arg ...)  (values next-dom ...)]
                      [args 
                       (bad-number-of-arguments blame val args dom-len)])))))
       lifts-doms
       superlifts-doms
       partials-doms
       #f
       #f
       stronger-ribs-dom
       dom-chaperone?)))
  
  (syntax-case* stx (-> values any any/c) module-or-top-identifier=?
    [(-> any/c ... any)
     (with-syntax ([n (- (length (syntax->list stx)) 2)])
       (build-optres
        #:exp
        (with-syntax ((val (opt/info-val opt/info))
                      (ctc (opt/info-contract opt/info))
                      (blame (opt/info-blame opt/info)))
          (syntax (if (procedure-arity-includes? val n)
                      val
                      (raise-flat-arrow-err blame val n))))
        #:lifts null
        #:superlifts null
        #:partials null
        #:flat #'(procedure-arity-includes? val n)
        #:opt #f
        #:stronger-ribs null
        #:chaperone #t))]
    [(-> dom ... (values rng ...))
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (let-values ([(next lift superlift partial flat opt stronger-ribs chaperone?)
                       (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                                      (syntax->list (syntax (rng ...))))])
           (if (eq? chaperone? #t)
               (build-optres #:exp next #:lifts lift #:superlifts superlift #:partials partial
                             #:flat flat #:opt opt #:stronger-ribs stronger-ribs #:chaperone chaperone?)
               (opt/unknown opt/i opt/info stx))))]
    [(-> dom ... any)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (let-values ([(next lift superlift partial flat opt stronger-ribs chaperone?)
                       (opt/arrow-any-ctc (syntax->list (syntax (dom ...))))])
           (if (eq? chaperone? #t)
               (build-optres #:exp next #:lifts lift #:superlifts superlift #:partials partial
                             #:flat flat #:opt opt #:stronger-ribs stronger-ribs #:chaperone chaperone?)
               (opt/unknown opt/i opt/info stx))))]
    [(-> dom ... rng)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (let-values ([(next lift superlift partial flat opt stronger-ribs chaperone?)
                       (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                                      (list #'rng))])
           (if (eq? chaperone? #t)
               (build-optres #:exp next #:lifts lift #:superlifts superlift #:partials partial
                             #:flat flat #:opt opt #:stronger-ribs stronger-ribs #:chaperone chaperone?)
               (opt/unknown opt/i opt/info stx))))]))

(define (raise-flat-arrow-err blame val n)
  (raise-blame-error blame val
                     '(expected "a procedure matching the contract ~s")
                     `(-> ,@(build-list n (λ (x) 'any/c)) any)))

(define (bad-number-of-arguments blame val args dom-len)
  (define num-values (length args))
  (raise-blame-error (blame-swap blame) val 
                     '(expected: "~a argument~a" given: "~a argument~a")
                     dom-len (if (= dom-len 1) "" "s")
                     num-values (if (= num-values 1) "" "s")))
