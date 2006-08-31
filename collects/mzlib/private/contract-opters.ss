(module contract-opters mzscheme
  (require "contract.ss"
           "contract-guts.ss"
           "contract-arrow.ss"
           "contract-opt.ss")
  (require-for-syntax "contract-opt-guts.ss")
  
  ;; opt/between-ctc : syntax syntax syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
  (define-for-syntax (opt/between-ctc pos stx low high op)
    (let* ((lifted-vars (generate-temporaries (syntax (low high error-check))))
           (lifted-low (car lifted-vars))
           (lifted-high (cadr lifted-vars)))
      (with-syntax ((op op)
                    (n lifted-low)
                    (m lifted-high))
        (values
         (with-syntax ((pos pos))
           (syntax (if (and (number? val) (op n val m)) val
                       (raise-contract-error
                        val
                        src-info
                        pos
                        orig-str
                        "expected <~a>, given: ~e"
                        contract-name
                        val))))
         (append (interleave-lifted
                  lifted-vars
                  (list low
                        high
                        (syntax (unless (and (number? n) (number? m))
                                  (error 'between/c "expected two numbers for bounds, got ~e and ~e" n m)))))
                 (list (cons #'contract-name (syntax (cond
                                                       [(= n -inf.0) `(<=/c ,m)]
                                                       [(= m +inf.0) `(>=/c ,n)]
                                                       [(= n m) `(=/c ,n)]
                                                       [else `(between/c ,n ,m)])))))
         null
         (syntax (and (number? val) (op n val m)))
         (make-known #t stx)))))
  
  (define/opter (between/c opt/i pos neg stx)
    (syntax-case stx (between/c)
      [(between/c low high) (opt/between-ctc pos stx #'low #'high #'<=)]))
  
  (define/opter (>/c opt/i pos neg stx)
    (syntax-case stx (>/c)
      [(>/c low) (opt/between-ctc #'low #'+inf.0 #'<)]))
  
  (define/opter (>=/c opt/i pos neg stx)
    (syntax-case stx (>/c)
      [(>=/c low) (opt/between-ctc #'low #'+inf.0 #'<=)]))
  
  (define/opter (</c opt/i pos neg stx)
    (syntax-case stx (>/c)
      [(</c high) (opt/between-ctc #'-inf.0 #'high #'<)]))
  
  (define/opter (<=/c opt/i pos neg stx)
    (syntax-case stx (>/c)
      [(<=/c high) (opt/between-ctc #'-inf.0 #'high #'<=)]))
  
  (define/opter (cons-immutable/c opt/i pos neg stx)
    
    ;; opt/cons-immutable-ctc : syntax syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/cons-immutable-ctc hdp tlp)
      (let-values ([(next-hdp lifted-hdp partial-hdp flat?-hdp known?-hdp)
                    (opt/i pos neg hdp)]
                   [(next-tlp lifted-tlp partial-tlp flat?-tlp known?-tlp)
                    (opt/i pos neg tlp)]
                   [(error-check)
                    (car (generate-temporaries (syntax (error-check))))])
        (with-syntax ((check (syntax (and (immutable? val)
                                          (pair? val)))))
          (values
           (with-syntax ((pos pos)
                         (next-hdp next-hdp)
                         (next-tlp next-tlp))
             (syntax (if check
                         (cons-immutable (let ((val (car val))) next-hdp)
                                         (let ((val (cdr val))) next-tlp))
                         (raise-contract-error
                          val
                          src-info
                          pos
                          orig-str
                          "expected <~a>, given: ~e"
                          contract-name
                          val))))
           (append
            (append lifted-hdp lifted-tlp)
            ;; FIXME naming still broken
            (list (cons #'contract-name
                        #''cons-immutable/c)))
           (append partial-hdp partial-tlp)
           (if (and flat?-hdp flat?-tlp)
               (with-syntax ((flat-hdp flat?-hdp)
                             (flat-tlp flat?-tlp))
                 (syntax (if (and check
                                  (let ((val (car val))) flat-hdp)
                                  (let ((val (cdr val))) flat-tlp)) #t #f)))
               #f)
           (make-known #t stx)))))
    
    (syntax-case stx (cons-immutable/c)
      [(cons-immutable/c hdp tlp)
       (opt/cons-immutable-ctc #'hdp #'tlp)]))
  
  (define/opter (cons/c opt/i pos neg stx)
    
    ;; opt/cons-ctc : syntax syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/cons-ctc hdp tlp)
      (let-values ([(next-hdp lifted-hdp partial-hdp flat?-hdp known-hdp)
                    (opt/i pos neg hdp)]
                   [(next-tlp lifted-tlp partial-tlp flat?-tlp known-tlp)
                    (opt/i pos neg tlp)]
                   [(error-check)
                    (car (generate-temporaries (syntax (error-check))))])
        (with-syntax ((next 
                       (with-syntax ((flat?-hdp flat?-hdp)
                                     (flat?-tlp flat?-tlp))
                         (syntax
                          (and (pair? val)
                               (let ((val (car val))) flat?-hdp)
                               (let ((val (cdr val))) flat?-tlp))))))
          (values
           (with-syntax ((pos pos))
             (syntax (if next
                         val
                         (raise-contract-error
                          val
                          src-info
                          pos
                          orig-str
                          "expected <~a>, given: ~e"
                          contract-name
                          val))))
           (append
            lifted-hdp lifted-tlp
            (list (cons error-check
                        (with-syntax ((hdp (known-sexp known-hdp))
                                      (tlp (known-sexp known-tlp))
                                      (check (with-syntax ((flat-hdp
                                                            (cond
                                                              [(known-flag known-hdp)
                                                               (if flat?-hdp #'#t #'#f)]
                                                              [else (with-syntax ((ctc (known-sexp known-hdp)))
                                                                      (syntax (flat-contract? ctc)))]))
                                                           (flat-tlp
                                                            (cond
                                                              [(known-flag known-tlp)
                                                               (if flat?-tlp #'#t #'#f)]
                                                              [else (with-syntax ((ctct (known-sexp known-tlp)))
                                                                      (syntax (flat-contract? ctc)))])))
                                               (syntax (and flat-hdp flat-tlp)))))
                          (syntax
                           (unless check
                             (error 'cons/c "expected two flat contracts or procedures of arity 1, got: ~e and ~e"
                                    hdp tlp))))))
            ;; FIXME naming still broken
            (list (cons #'contract-name
                        #''cons/c)))
           (append partial-hdp partial-tlp)
           (syntax (if next #t #f))
           (make-known #t stx)))))
    
    (syntax-case stx (cons/c)
      [(cons/c hdp tlp)
       (opt/cons-ctc #'hdp #'tlp)]))
  
  ;; opt/pred-ctc : (any -> boolean) -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
  (define-for-syntax (opt/pred pos pred)
    (let* ((lifted-vars (generate-temporaries (syntax (pred))))
           (lifted-pred-var (car lifted-vars)))
      (with-syntax ((lifted-pred lifted-pred-var))
        (values
         (with-syntax ((pos pos))
           (syntax (if (lifted-pred val)
                       val
                       (raise-contract-error
                        val
                        src-info
                        pos
                        orig-str
                        "expected <~a>, given: ~e"
                        contract-name
                        val))))
         (append (list (cons lifted-pred-var pred))
                 #;(list (cons #'contract-name (syntax
                                                (if (object-name pred)
                                                    (object-name pred)
                                                    '???)))))
         null
         (syntax (lifted-pred val))
         (make-known #t pred)))))
  
  (define/opter (null? opt/i pos neg stx)
    (syntax-case stx (null?)
      [null? (opt/pred pos #'null?)]))
  
  (define/opter (boolean? opt/i pos neg stx)
    (syntax-case stx (boolean?)
      [boolean? (opt/pred pos #'boolean?)]))
  
  (define/opter (integer? opt/i pos neg stx)
    (syntax-case stx (integer?)
      [integer? (opt/pred pos #'integer?)]))
  
  (define/opter (number? opt/i pos neg stx)
    (syntax-case stx (number?)
      [number? (opt/pred pos #'number?)]))
  
  (define/opter (pair? opt/i pos neg stx)
    (syntax-case stx (pair?)
      [pair? (opt/pred pos #'pair?)]))
  
  (define/opter (any/c opt/i pos neg stx)
    
    ;; opt/any-ctc : -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define opt/any-ctc
      (values
       #'val
       (list (cons #'contract-name
                   #''any/c))
       null
       (syntax #t)
       (make-known #t stx)))
    
    (syntax-case stx (any/c)
      [any/c opt/any-ctc]))
  
  (define/opter (flat-contract opt/i pos neg stx)
    
    ;; opt/flat-ctc : (any -> boolean) -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/flat-ctc pred)
      (syntax-case pred (null? number? integer? boolean? pair?)
        ;; Better way of doing this?
        [null? (opt/pred pos pred)]
        [number? (opt/pred pos pred)]
        [integer? (opt/pred pos pred)]
        [boolean? (opt/pred pos pred)]
        [pair? (opt/pred pos pred)]
        [pred
         (let* ((lifted-vars (generate-temporaries (syntax (pred error-check))))
                (lifted-pred (car lifted-vars)))
           (with-syntax ((lifted-pred (car lifted-vars)))
             (values
              (with-syntax ((pos pos))
                (syntax (if (lifted-pred val)
                            val
                            (raise-contract-error
                             val
                             src-info
                             pos
                             orig-str
                             "expected <~a>, given: ~e"
                             contract-name
                             val))))
              (append (interleave-lifted
                       lifted-vars
                       (list #'pred
                             (syntax (unless (and (procedure? lifted-pred)
                                                  (procedure-arity-includes? lifted-pred 1))
                                       (error 'flat-named-contract
                                              "expected procedure of one argument, given: ~e" lifted-pred)))))
                      (list (cons #'contract-name (syntax
                                                   (if (object-name pred)
                                                       (object-name pred)
                                                       '???)))))
              null
              (syntax (lifted-pred val))
              (make-known #t stx))))]))
    
    (syntax-case stx (flat-contract)
      [(flat-contract pred)
       (opt/flat-ctc #'pred)]))
  
  (define/opter (-> opt/i pos neg stx)
    
    ;; opt/arrow-ctc : list-of-syntax list-of-syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/arrow-ctc doms rngs)
      (let*-values ([(dom-vars rng-vars) (values (generate-temporaries doms)
                                                 (generate-temporaries rngs))]
                    [(next-doms lifted-doms partial-doms)
                     (let loop ([vars dom-vars]
                                [doms doms]
                                [next-doms null]
                                [lifted-doms null]
                                [partial-doms null])
                       (cond
                         [(null? doms) (values (reverse next-doms) lifted-doms partial-doms)]
                         [else
                          (let-values ([(next lifted partial flat? _)
                                        (opt/i neg pos (car doms))])
                            (loop (cdr vars)
                                  (cdr doms)
                                  (cons (with-syntax ((next next)
                                                      (car-vars (car vars)))
                                          (syntax (let ((val car-vars)) next)))
                                        next-doms)
                                  (append lifted-doms lifted)
                                  (append partial-doms partial)))]))]
                    [(next-rngs lifted-rngs partial-rngs)
                     (let loop ([vars rng-vars]
                                [rngs rngs]
                                [next-rngs null]
                                [lifted-rngs null]
                                [partial-rngs null])
                       (cond
                         [(null? rngs) (values (reverse next-rngs) lifted-rngs partial-rngs)]
                         [else
                          (let-values ([(next lifted partial flat? _)
                                        (opt/i pos neg (car rngs))])
                            (loop (cdr vars)
                                  (cdr rngs)
                                  (cons (with-syntax ((next next)
                                                      (car-vars (car vars)))
                                          (syntax (let ((val car-vars)) next)))
                                        next-rngs)
                                  (append lifted-rngs lifted)
                                  (append partial-rngs partial)))]))])
        (values
         (with-syntax (((dom-arg ...) dom-vars)
                       ((rng-arg ...) rng-vars)
                       ((next-dom ...) next-doms)
                       (dom-len (length dom-vars))
                       ((next-rng ...) next-rngs))
           (syntax (if (and (procedure? val) (procedure-arity-includes? val dom-len))
                       (λ (dom-arg ...)
                         (let-values ([(rng-arg ...) (val next-dom ...)])
                           (values next-rng ...)))
                       (error '-> "expected a procedure of arity ~a, got ~e" dom-len val))))
         (append lifted-doms lifted-rngs
                 (list (cons #'contract-name
                             #''->/c)))
         (append partial-doms partial-rngs)
         #f
         (make-known #t stx))))
    
    ;; opt/arrow-any-ctc : list-of-syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/arrow-any-ctc doms)
      (let*-values ([(dom-vars) (generate-temporaries doms)]
                    [(next-doms lifted-doms partial-doms)
                     (let loop ([vars dom-vars]
                                [doms doms]
                                [next-doms null]
                                [lifted-doms null]
                                [partial-doms null])
                       (cond
                         [(null? doms) (values (reverse next-doms) lifted-doms partial-doms)]
                         [else
                          (let-values ([(next lifted partial flat? _)
                                        (opt/i pos neg (car doms))])
                            (loop (cdr vars)
                                  (cdr doms)
                                  (cons (with-syntax ((next next)
                                                      (car-vars (car vars)))
                                          (syntax (let ((val car-vars)) next)))
                                        next-doms)
                                  (append lifted-doms lifted)
                                  (append partial-doms partial)))]))])
        (values
         (with-syntax (((dom-arg ...) dom-vars)
                       ((next-dom ...) next-doms)
                       (dom-len (length dom-vars)))
           (syntax (if (and (procedure? val) (procedure-arity-includes? val dom-len))
                       (λ (dom-arg ...)
                         (val next-dom ...))
                       (error '-> "expected a procedure of arity ~a, got ~e" dom-len val))))
         (append lifted-doms
                 ;; FIXME naming still broken
                 (list (cons #'contract-name
                             #''->/c)))
         (append partial-doms)
         #f
         (make-known #t stx))))
    
    (syntax-case stx (-> values any)
      [(-> dom ... (values rng ...))
       (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                      (syntax->list (syntax (rng ...))))]
      [(-> dom ... any)
       (opt/arrow-any-ctc (syntax->list (syntax (dom ...))))]
      [(-> dom ... rng)
       (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                      (list #'rng))]))
  
  (define/opter (unknown opt/i pos neg stx)
    
    ;; opt/unknown-ctc : list-of-syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/unknown-ctc ctc)
      (let* ((lifted-vars (generate-temporaries (syntax (lifted error-check))))
             (lifted-var (car lifted-vars))
             (partial-var (car (generate-temporaries (syntax (partial))))))
        (values
         (with-syntax ((partial-var partial-var)
                       (lifted-var lifted-var)
                       (ctc ctc))
           (syntax (partial-var val)))
         (append
          (interleave-lifted
           lifted-vars
           (list ctc
                 (with-syntax ((lifted-var lifted-var))
                   (syntax
                    (unless (contract? lifted-var)
                      (error 'contract "expected contract, given ~e" lifted-var))))))
          (list
           (cons #'contract-name
                 #''opt/c)))
         (list (cons
                partial-var
                (with-syntax ((lifted-var lifted-var)
                              (pos pos)
                              (neg neg))
                  (syntax (((proj-get lifted-var) lifted-var) pos neg src-info orig-str)))))
         #f
         (make-known #f lifted-var))))
    
    (syntax-case stx ()
      [ctc
       (opt/unknown-ctc #'ctc)]))
  
  (define/opter (or/c opt/i pos neg stx)
    
    ;; opt/or-ctc : list-of-syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/or-ctc ps)  
      (let ((lifted-from-hos null)
            (partial-from-hos null))
        (let-values ([(opt-ps lifted-ps partial-ps hos ho-ctc)
                      (let loop ([ps ps]
                                 [next-ps null]
                                 [lifted-ps null]
                                 [partial-ps null]
                                 [hos null]
                                 [ho-ctc #f])
                        (cond
                          [(null? ps) (values next-ps lifted-ps partial-ps (reverse hos) ho-ctc)]
                          [else
                           (let-values ([(next lifted partial flat? _)
                                         (opt/i pos neg (car ps))])
                             (if flat?
                                 (loop (cdr ps)
                                       (cons flat? next-ps)
                                       (append lifted-ps lifted)
                                       (append partial-ps partial)
                                       hos
                                       ho-ctc)
                                 (if (< (length hos) 1)
                                     (loop (cdr ps)
                                           next-ps
                                           (append lifted-ps lifted)
                                           (append partial-ps partial)
                                           (cons (car ps) hos)
                                           next)
                                     (loop (cdr ps)
                                           next-ps
                                           lifted-ps
                                           partial-ps
                                           (cons (car ps) hos)
                                           ho-ctc))))]))])
          (with-syntax ((next-ps (with-syntax (((opt-p ...) opt-ps))
                                   (syntax (or #f opt-p ...)))))
            (values
             (cond
               [(null? hos) (with-syntax ((pos pos))
                              (syntax
                               (if next-ps val
                                   (raise-contract-error val src-info pos orig-str 
                                                         "none of the branches of the or/c matched"))))]
               [(= (length hos) 1) (with-syntax ((ho-ctc ho-ctc))
                                     (syntax
                                      (if next-ps val ho-ctc)))]
               [(> (length hos) 1) 
                (let-values ([(next-hos lifted-hos partial-hos _ __)
                              ((opter 'unknown) opt/i pos neg (cons #'or/c hos))])
                  (set! lifted-from-hos lifted-hos)
                  (set! partial-from-hos partial-hos)
                  (with-syntax ((next-hos next-hos))
                    (syntax
                     (if next-ps val next-hos))))])
             (append lifted-ps
                     lifted-from-hos
                     (list (cons #'contract-name
                                 #''or/c-placeholder)))
             (append partial-ps
                     partial-from-hos)
             (if (null? hos)
                 (syntax next-ps)
                 #f)
             (make-known #t stx))))))
    
    (syntax-case stx (or/c)
      [(or/c p ...)
       (opt/or-ctc (syntax->list (syntax (p ...))))])))
