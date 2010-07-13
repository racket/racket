#lang racket/base

(require (for-syntax racket/base
                     racket/struct-info
                     "helpers.rkt"
                     "opt-guts.rkt")
         racket/promise
         "opt.rkt"
         "guts.rkt")

(provide flat-rec-contract
         flat-murec-contract
         or/c 
         not/c
         =/c >=/c <=/c </c >/c between/c
         integer-in
         real-in
         natural-number/c
         string-len/c
         false/c
         printable/c
         symbols one-of/c
         listof non-empty-listof cons/c list/c
         vectorof vector-immutableof vector/c vector-immutable/c 
         box-immutable/c box/c
         promise/c
         struct/c
         syntax/c
         
         check-between/c
         check-unary-between/c
         parameter/c
         hash/c)

(define-syntax (flat-rec-contract stx)
  (syntax-case stx  ()
    [(_ name ctc ...)
     (identifier? (syntax name))
     (with-syntax ([(ctc-id ...) (generate-temporaries (syntax (ctc ...)))]
                   [(pred-id ...) (generate-temporaries (syntax (ctc ...)))])
       (syntax 
        (let* ([pred (λ (x) (error 'flat-rec-contract "applied too soon"))]
               [name (flat-contract (let ([name (λ (x) (pred x))]) name))])
          (let ([ctc-id (coerce-contract 'flat-rec-contract ctc)] ...)
            (unless (flat-contract? ctc-id)
              (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
            ...
            (set! pred
                  (let ([pred-id (flat-contract-predicate ctc-id)] ...)
                    (λ (x)
                      (or (pred-id x) ...))))
            name))))]
    [(_ name ctc ...)
     (raise-syntax-error 'flat-rec-contract "expected first argument to be an identifier" stx (syntax name))]))

(define-syntax (flat-murec-contract stx)
  (syntax-case stx  ()
    [(_ ([name ctc ...] ...) body1 body ...)
     (andmap identifier? (syntax->list (syntax (name ...))))
     (with-syntax ([((ctc-id ...) ...) (map generate-temporaries
                                            (syntax->list (syntax ((ctc ...) ...))))]
                   [(pred-id ...) (generate-temporaries (syntax (name ...)))]
                   [((pred-arm-id ...) ...) (map generate-temporaries
                                                 (syntax->list (syntax ((ctc ...) ...))))])
       (syntax 
        (let* ([pred-id (λ (x) (error 'flat-murec-contract "applied too soon"))] ...
               [name (flat-contract (let ([name (λ (x) (pred-id x))]) name))] ...)
          (let-values ([(ctc-id ...) (values (coerce-contract 'flat-rec-contract ctc) ...)] ...)
            (begin
              (void)
              (unless (flat-contract? ctc-id)
                (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
              ...) ...
            (set! pred-id
                  (let ([pred-arm-id (flat-contract-predicate ctc-id)] ...)
                    (λ (x)
                      (or (pred-arm-id x) ...)))) ...
            body1
            body ...))))]
    [(_ ([name ctc ...] ...) body1 body ...)
     (for-each (λ (name)
                 (unless (identifier? name)
                   (raise-syntax-error 'flat-rec-contract
                                       "expected an identifier" stx name)))
               (syntax->list (syntax (name ...))))]
    [(_ ([name ctc ...] ...))
     (raise-syntax-error 'flat-rec-contract "expected at least one body expression" stx)]))

(define/subexpression-pos-prop or/c
  (case-lambda 
    [() (make-none/c '(or/c))]
    [raw-args
     (let ([args (coerce-contracts 'or/c raw-args)])
       (let-values ([(ho-contracts flat-contracts)
                     (let loop ([ho-contracts '()]
                                [flat-contracts '()]
                                [args args])
                       (cond
                         [(null? args) (values ho-contracts (reverse flat-contracts))]
                         [else 
                          (let ([arg (car args)])
                            (cond
                              [(flat-contract? arg)
                               (loop ho-contracts (cons arg flat-contracts) (cdr args))]
                              [else
                               (loop (cons arg ho-contracts) flat-contracts (cdr args))]))]))])
         (let ([pred 
                (cond
                  [(null? flat-contracts) not]
                  [else
                   (let loop ([fst (car flat-contracts)]
                              [rst (cdr flat-contracts)])
                     (let ([fst-pred (flat-contract-predicate fst)])
                       (cond
                         [(null? rst) fst-pred]
                         [else 
                          (let ([r (loop (car rst) (cdr rst))])
                            (λ (x) (or (fst-pred x) (r x))))])))])])
           (cond
             [(null? ho-contracts)
              (make-flat-or/c pred flat-contracts)]
             [(null? (cdr ho-contracts))
              (make-or/c pred flat-contracts (car ho-contracts))]
             [else
              (make-multi-or/c flat-contracts ho-contracts)]))))]))

(define-struct or/c (pred flat-ctcs ho-ctc)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
      (let ([c-proc (contract-projection (or/c-ho-ctc ctc))]
            [pred (or/c-pred ctc)])
        (λ (blame)
           (let ([partial-contract (c-proc blame)])
             (λ (val)
                (cond
                 [(pred val) val]
                 [else
                  (partial-contract val)]))))))

   #:name
   (λ (ctc)
      (apply build-compound-type-name 
             'or/c 
             (or/c-ho-ctc ctc)
             (or/c-flat-ctcs ctc)))

   #:first-order
   (λ (ctc)
      (let ([pred (or/c-pred ctc)]
            [ho (contract-first-order (or/c-ho-ctc ctc))])
        (λ (x)
           (or (ho x)
               (pred x)))))

   #:stronger
   (λ (this that)
      (and (or/c? that)
           (contract-stronger? (or/c-ho-ctc this) (or/c-ho-ctc that))
           (let ([this-ctcs (or/c-flat-ctcs this)]
                 [that-ctcs (or/c-flat-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger?
                          this-ctcs
                          that-ctcs)))))))

(define (multi-or/c-proj ctc)
  (let* ([ho-contracts (multi-or/c-ho-ctcs ctc)]
         [c-procs (map (λ (x) (contract-projection x)) ho-contracts)]
         [first-order-checks (map (λ (x) (contract-first-order x)) ho-contracts)]
         [predicates (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))])
    (λ (blame)
      (let ([partial-contracts (map (λ (c-proc) (c-proc blame)) c-procs)])
        (λ (val)
          (cond
            [(ormap (λ (pred) (pred val)) predicates)
             val]
            [else
             (let loop ([checks first-order-checks]
                        [procs partial-contracts]
                        [contracts ho-contracts]
                        [candidate-proc #f]
                        [candidate-contract #f])
               (cond
                 [(null? checks)
                  (if candidate-proc
                      (candidate-proc val)
                      (raise-blame-error blame val 
                                         "none of the branches of the or/c matched, given ~e"
                                         val))]
                 [((car checks) val)
                  (if candidate-proc
                      (raise-blame-error blame val
                                         "two of the clauses in the or/c might both match: ~s and ~s, given ~e"
                                         (contract-name candidate-contract)
                                         (contract-name (car contracts))
                                         val)
                      (loop (cdr checks)
                            (cdr procs)
                            (cdr contracts)
                            (car procs)
                            (car contracts)))]
                 [else
                  (loop (cdr checks)
                        (cdr procs)
                        (cdr contracts)
                        candidate-proc
                        candidate-contract)]))]))))))

(define-struct multi-or/c (flat-ctcs ho-ctcs)
  #:property prop:contract
  (build-contract-property
   #:projection multi-or/c-proj
   #:name
   (λ (ctc)
      (apply build-compound-type-name 
             'or/c 
             (append
              (multi-or/c-flat-ctcs ctc)
              (reverse (multi-or/c-ho-ctcs ctc)))))

   #:first-order
   (λ (ctc)
      (let ([flats (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))]
            [hos (map (λ (x) (contract-first-order x)) (multi-or/c-ho-ctcs ctc))])
        (λ (x)
           (or (ormap (λ (f) (f x)) hos)
               (ormap (λ (f) (f x)) flats)))))

   #:stronger
   (λ (this that)
      (and (multi-or/c? that)
           (let ([this-ctcs (multi-or/c-ho-ctcs this)]
                 [that-ctcs (multi-or/c-ho-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger?
                          this-ctcs
                          that-ctcs)))
           (let ([this-ctcs (multi-or/c-flat-ctcs this)]
                 [that-ctcs (multi-or/c-flat-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger?
                          this-ctcs
                          that-ctcs)))))))

(define-struct flat-or/c (pred flat-ctcs)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
      (apply build-compound-type-name 
             'or/c 
             (flat-or/c-flat-ctcs ctc)))
   #:stronger
   (λ (this that)
      (and (flat-or/c? that)
           (let ([this-ctcs (flat-or/c-flat-ctcs this)]
                 [that-ctcs (flat-or/c-flat-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger?
                          this-ctcs
                          that-ctcs)))))

   #:first-order
   (λ (ctc) (flat-or/c-pred ctc))))

;;
;; or/c opter
;;
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

(define false/c #f)

(define/final-prop (string-len/c n)
  (unless (number? n)
    (error 'string-len/c "expected a number as argument, got ~e" n))
  (flat-named-contract 
   `(string-len/c ,n)
   (λ (x)
     (and (string? x)
          ((string-length x) . < . n)))))

(define/final-prop (symbols . ss)
  (unless ((length ss) . >= . 1)
    (error 'symbols "expected at least one argument"))
  (unless (andmap symbol? ss)
    (error 'symbols "expected symbols as arguments, given: ~a"
           (apply string-append (map (λ (x) (format "~e " x)) ss))))
  (make-one-of/c ss))

(define atomic-value? 
  (let ([undefined (letrec ([x x]) x)])
    (λ (x)
      (or (char? x) (symbol? x) (boolean? x)
          (null? x) (keyword? x) (number? x)
          (void? x) (eq? x undefined)))))

(define/final-prop (one-of/c . elems)
  (unless (andmap atomic-value? elems)
    (error 'one-of/c "expected chars, symbols, booleans, null, keywords, numbers, void, or undefined, got ~e"
           elems))
  (make-one-of/c elems))

(define (one-of-pc x)
  (cond
    [(symbol? x)
     `',x]
    [(null? x)
     ''()]
    [(void? x)
     '(void)]
    [(or (char? x) 
         (boolean? x)
         (keyword? x)
         (number? x))
     x]
    [(eq? x (letrec ([x x]) x))
     '(letrec ([x x]) x)]
    [else (error 'one-of-pc "undef ~s" x)]))


(define-struct one-of/c (elems)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc) 
      (let ([elems (one-of/c-elems ctc)])
        `(,(cond
            [(andmap symbol? elems)
             'symbols]
            [else
             'one-of/c])
          ,@(map one-of-pc elems))))

   #:stronger
   (λ (this that)
      (and (one-of/c? that)
           (let ([this-elems (one-of/c-elems this)]
                 [that-elems (one-of/c-elems that)])
             (and 
              (andmap (λ (this-elem) (memv this-elem that-elems))
                      this-elems)
              #t))))
   #:first-order 
   (λ (ctc) 
      (let ([elems (one-of/c-elems ctc)])
        (λ (x) (memv x elems))))))

(define printable/c
  (flat-named-contract
   'printable/c
   (λ (x)
     (let printable? ([x x])
       (or (symbol? x)
           (string? x)
           (bytes? x)
           (boolean? x)
           (char? x)
           (null? x)
           (number? x)
           (regexp? x)
           (prefab-struct-key x) ;; this cannot be last, since it doesn't return just #t
           (and (pair? x)
                (printable? (car x))
                (printable? (cdr x)))
           (and (vector? x)
                (andmap printable? (vector->list x)))
           (and (box? x)
                (printable? (unbox x))))))))

(define-struct between/c (low high)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc) 
      (let ([n (between/c-low ctc)]
            [m (between/c-high ctc)])
        (cond
         [(= n -inf.0) `(<=/c ,m)]
         [(= m +inf.0) `(>=/c ,n)]
         [(= n m) `(=/c ,n)]
         [else `(between/c ,n ,m)])))

   #:stronger
   (λ (this that)
      (and (between/c? that)
           (<= (between/c-low that) (between/c-low this))
           (<= (between/c-high this) (between/c-high that))))

   #:first-order
   (λ (ctc) 
      (let ([n (between/c-low ctc)]
            [m (between/c-high ctc)])
        (λ (x) 
           (and (real? x)
                (<= n x m)))))))

(define-syntax (check-unary-between/c stx)
  (syntax-case stx ()
    [(_ 'sym x-exp)
     (identifier? #'sym)
     #'(let ([x x-exp])
         (unless (real? x)
           (error 'sym "expected a real number, got ~e" x)))]))

(define/final-prop (=/c x) 
  (check-unary-between/c '=/c x)
  (make-between/c x x))
(define/final-prop (<=/c x) 
  (check-unary-between/c '<=/c x)
  (make-between/c -inf.0 x))
(define/final-prop (>=/c x)
  (check-unary-between/c '>=/c x)
  (make-between/c x +inf.0))
(define (check-between/c x y)
  (unless (real? x)
    (error 'between/c "expected a real number as first argument, got ~e, other arg ~e" x y))
  (unless (real? y)
    (error 'between/c "expected a real number as second argument, got ~e, other arg ~e" y x)))
(define/final-prop (between/c x y)
  (check-between/c x y)
  (make-between/c x y))

;;
;; between/c opter helper
;;



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
                           "expected <~a>, given: ~e"
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
                   "expected <~a>, given: ~e"
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

(define (</c x)
  (flat-named-contract
   `(</c ,x)
   (λ (y) (and (real? y) (< y x)))))
(define (>/c x)
  (flat-named-contract
   `(>/c ,x)
   (λ (y) (and (real? y) (> y x)))))

(define natural-number/c
  (flat-named-contract
   'natural-number/c
   (λ (x)
     (and (number? x)
          (integer? x)
          (exact? x)
          (x . >= . 0)))))

(define/final-prop (integer-in start end)
  (unless (and (integer? start)
               (exact? start)
               (integer? end)
               (exact? end))
    (error 'integer-in "expected two exact integers as arguments, got ~e and ~e" start end))
  (flat-named-contract 
   `(integer-in ,start ,end)
   (λ (x)
     (and (integer? x)
          (exact? x)
          (<= start x end)))))

(define/final-prop (real-in start end)
  (unless (and (real? start)
               (real? end))
    (error 'real-in "expected two real numbers as arguments, got ~e and ~e" start end))
  (flat-named-contract 
   `(real-in ,start ,end)
   (λ (x)
     (and (real? x)
          (<= start x end)))))

(define/final-prop (not/c f)
  (let* ([ctc (coerce-flat-contract 'not/c f)]
         [pred (flat-contract-predicate ctc)])
    (build-flat-contract
     (build-compound-type-name 'not/c ctc)
     (λ (x) (not (pred x))))))

(define-syntax (*-immutableof stx)
  (syntax-case stx ()
    [(_ predicate? app fill testmap type-name name)
     (identifier? (syntax predicate?))
     (syntax
      (let ([fill-name fill]
            [for-each-name app])
        (λ (input)
          (let* ([ctc (coerce-contract 'name input)]
                 [fo-check 
                  (λ (x)
                    (and (predicate? x) 
                         (testmap (λ (v) (contract-first-order-passes? ctc v)) x)))]
                 [proj (contract-projection ctc)])
            (if (flat-contract? ctc)
                (make-flat-contract
                 #:name (build-compound-type-name 'name ctc)
                 #:first-order fo-check
                 #:projection
                 (λ (blame)
                   (let ([p-app (proj blame)])
                     (λ (val)
                       (unless (predicate? val)
                         (raise-blame-error blame val
                                            "expected <~a>, given: ~e"
                                            'type-name val))
                       (for-each-name p-app val)
                       val))))
                (make-contract
                 #:name (build-compound-type-name 'name ctc)
                 #:first-order fo-check
                 #:projection
                 (λ (blame)
                   (let ([p-app (proj blame)])
                     (λ (val)
                       (unless (predicate? val)
                         (raise-blame-error blame val
                                            "expected <~a>, given: ~e"
                                            'type-name val))
                       (fill-name p-app val))))))))))]))

(define listof
  (*-immutableof list? for-each map andmap list listof))

(define (non-empty-list? x) (and (pair? x) (list? (cdr x))))
(define non-empty-listof
  (*-immutableof non-empty-list? for-each map andmap non-empty-list non-empty-listof))

(define/final-prop (immutable-vector? val) (and (immutable? val) (vector? val)))

(define vector-immutableof
  (*-immutableof immutable-vector?
                 (λ (f v) (for ([e (in-vector v)]) (f e)))
                 (λ (f v) (apply vector-immutable (for/list ([e (in-vector v)]) (f e))))
                 (λ (f v) (for/and ([e (in-vector v)]) (f e)))
                 immutable-vector
                 vector-immutableof))

(define/subexpression-pos-prop (vectorof p)
  (let* ([ctc (coerce-flat-contract 'vectorof p)]
         [pred (flat-contract-predicate ctc)])
    (build-flat-contract
     (build-compound-type-name 'vectorof ctc)
     (λ (v)
       (and (vector? v)
            (andmap pred (vector->list v)))))))

(define/subexpression-pos-prop (vector/c . args)
  (let* ([ctcs (coerce-flat-contracts 'vector/c args)]
         [largs (length args)]
         [procs (map flat-contract-predicate ctcs)])
    (build-flat-contract
     (apply build-compound-type-name 'vector/c ctcs)
     (λ (v)
       (and (vector? v)
            (= (vector-length v) largs)
            (andmap (λ (p? x) (p? x))
                    procs
                    (vector->list v)))))))

(define/final-prop (box/c pred)
  (let* ([ctc (coerce-flat-contract 'box/c pred)]
         [p? (flat-contract-predicate ctc)])
    (build-flat-contract
     (build-compound-type-name 'box/c ctc)
     (λ (x)
       (and (box? x)
            (p? (unbox x)))))))

;;
;; cons/c opter
;;
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
                        "expected <~a>, given: ~e"
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


(define-syntax (*-immutable/c stx)
  (syntax-case stx ()
    [(_ predicate? constructor (arb? selectors ...) type-name name)
     #'(*-immutable/c predicate? constructor (arb? selectors ...) type-name name #t)]
    [(_ predicate? constructor (arb? selectors ...) type-name name test-immutable?)
     (and (eq? #f (syntax->datum (syntax arb?)))
          (boolean? (syntax->datum #'test-immutable?)))
     (let ([test-immutable? (syntax->datum #'test-immutable?)])
       (with-syntax ([pred?
                      (if test-immutable?
                          #'(λ (v) (and (predicate?-name v) (immutable? v)))
                          #'predicate?-name)]
                     [pred-fail-text
                      (if test-immutable?
                          "expected immutable <~a>, given: ~e"
                          "expected <~a>, given: ~e")]
                     [(params ...) (generate-temporaries (syntax (selectors ...)))]
                     [(p-apps ...) (generate-temporaries (syntax (selectors ...)))]
                     [(ctc-x ...) (generate-temporaries (syntax (selectors ...)))]
                     [(procs ...) (generate-temporaries (syntax (selectors ...)))]
                     [(selector-names ...) (generate-temporaries (syntax (selectors ...)))])
         #`(let ([predicate?-name predicate?]
                 [constructor-name constructor]
                 [selector-names selectors] ...)
             (λ (params ...)
               (let* ([ctc-x (coerce-contract 'name params)] ...
                      [procs (contract-projection ctc-x)] ...
                      [fo-check
                       (λ (v)
                         (and (pred? v)
                              (contract-first-order-passes? ctc-x (selector-names v)) ...))])
                 (if (and (flat-contract? ctc-x) ...)
                     (make-flat-contract
                      #:name (build-compound-type-name 'name ctc-x ...)
                      #:first-order fo-check
                      #:projection
                      (λ (blame)
                        (let ([p-apps (procs blame)] ...)
                          (λ (v)
                            (unless (pred? v)
                              (raise-blame-error blame v
                                                   pred-fail-text
                                                   'type-name v))
                            (void (p-apps (selector-names v)) ...)
                            v))))
                     (make-contract
                      #:name (build-compound-type-name 'name ctc-x ...)
                      #:first-order fo-check
                      #:projection
                      (λ (blame)
                        (let ([p-apps (procs blame)] ...)
                          (λ (v)
                            (unless (pred? v)
                              (raise-blame-error blame v
                                                 pred-fail-text
                                                 'type-name v))
                            (constructor-name (p-apps (selector-names v)) ...)))))))))))]
    [(_ predicate? constructor (arb? selector) correct-size type-name name)
     (eq? #t (syntax->datum (syntax arb?)))
     (syntax
      (let ([predicate?-name predicate?]
            [constructor-name constructor]
            [selector-name selector])
        (λ params
          (let* ([count (length params)]
                 [pred? (λ (v)
                          (and (immutable? v)
                               (predicate?-name v) 
                               (correct-size count v)))]
                 [ctcs (map (λ (param) (coerce-contract 'name param)) params)]
                 [procs (map contract-projection ctcs)]
                 [fo-check
                  (λ (v)
                    (and (pred? v)
                         (for/and ([c (in-list ctcs)]
                                   [i (in-naturals)])
                           (contract-first-order-passes? c (selector-name v i)))))])
            (if (andmap flat-contract? ctcs)
                (make-flat-contract
                 #:name (apply build-compound-type-name 'name ctcs)
                 #:first-order fo-check
                 #:projection
                 (λ (blame)
                   (let ([p-apps (map (λ (proc) (proc blame)) procs)])
                     (λ (v)
                       (unless (pred? v)
                         (raise-blame-error blame v
                                            "expected <~a>, given: ~e"
                                            'type-name v))
                       (for ([p (in-list p-apps)]
                             [i (in-naturals)])
                         (p (selector-name v i)))
                       v))))
                (make-contract
                 #:name (apply build-compound-type-name 'name ctcs)
                 #:first-order fo-check
                 #:projection
                 (λ (blame)
                   (let ([p-apps (map (λ (proc) (proc blame)) procs)])
                     (λ (v)
                       (unless (pred? v)
                         (raise-blame-error blame v
                                            "expected <~a>, given: ~e"
                                            'type-name v))
                       (apply constructor-name 
                              (for/list ([p (in-list p-apps)]
                                         [i (in-naturals)])
                                (p (selector-name v i)))))))))))))]))

(define cons/c (*-immutable/c pair? cons (#f car cdr) cons cons/c #f))
(define box-immutable/c (*-immutable/c box? box-immutable (#f unbox) immutable-box box-immutable/c))
(define vector-immutable/c (*-immutable/c vector?
                                          vector-immutable
                                          (#t (λ (v i) (vector-ref v i)))
                                          (λ (n v) (= n (vector-length v)))
                                          immutable-vector
                                          vector-immutable/c))

;;
;; cons/c opter
;;
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
                        "expected <~a>, given: ~e"
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

(define/subexpression-pos-prop (list/c . args)
  (let* ([args (coerce-contracts 'list/c args)])
    (if (andmap flat-contract? args)
      (flat-list/c args)
      (higher-order-list/c args))))

(struct flat-list/c [args]
        #:property prop:flat-contract
        (build-flat-contract-property
         #:name
         (lambda (c)
           (apply build-compound-type-name
             'list/c (flat-list/c-args c)))
         #:first-order
         (lambda (c)
           (lambda (x)
             (and (list? x)
                  (= (length x) (length (flat-list/c-args c)))
                  (for/and ([arg/c (in-list (flat-list/c-args c))]
                            [v (in-list x)])
                    (arg/c v)))))
         #:projection
         (lambda (c)
           (lambda (b)
             (lambda (x)
               (unless (list? x)
                 (raise-blame-error b x "expected a list, got: ~e" x))
               (let* ([args (flat-list/c-args c)]
                      [expected (length args)]
                      [actual (length x)])
                 (unless (= actual expected)
                   (raise-blame-error
                    b x
                    "expected a list of ~a elements, but got ~a elements in: ~e"
                    expected actual x))
                 (for ([arg/c (in-list args)] [v (in-list x)])
                   (((contract-projection arg/c) b) v))
                 x))))))

(struct higher-order-list/c [args]
        #:property prop:contract
        (build-contract-property
         #:name
         (lambda (c)
           (apply build-compound-type-name
             'list/c (higher-order-list/c-args c)))
         #:first-order
         (lambda (c)
           (lambda (x)
             (and (list? x)
                  (= (length x) (length (higher-order-list/c-args c)))
                  (for/and ([arg/c (in-list (higher-order-list/c-args c))]
                            [v (in-list x)])
                    (contract-first-order-passes? arg/c v)))))
         #:projection
         (lambda (c)
           (lambda (b)
             (lambda (x)
               (unless (list? x)
                 (raise-blame-error b x "expected a list, got: ~e" x))
               (let* ([args (higher-order-list/c-args c)]
                      [expected (length args)]
                      [actual (length x)])
                 (unless (= actual expected)
                   (raise-blame-error
                    b x
                    "expected a list of ~a elements, but got ~a elements in: ~e"
                    expected actual x))
                 (for/list ([arg/c (in-list args)] [v (in-list x)])
                   (((contract-projection arg/c) b) v))))))))

(define/subexpression-pos-prop (syntax/c ctc-in)
  (let ([ctc (coerce-contract 'syntax/c ctc-in)])
    (build-flat-contract
     (build-compound-type-name 'syntax/c ctc)
     (let ([pred (flat-contract-predicate ctc)])
       (λ (val)
         (and (syntax? val)
              (pred (syntax-e val))))))))

(define/subexpression-pos-prop promise/c
  (λ (ctc-in)
    (let* ([ctc (coerce-contract 'promise/c ctc-in)]
           [ctc-proc (contract-projection ctc)])
      (make-contract
       #:name (build-compound-type-name 'promise/c ctc)
       #:projection
       (λ (blame)
         (let ([p-app (ctc-proc blame)])
           (λ (val)
             (unless (promise? val)
               (raise-blame-error
                blame
                val
                "expected <promise>, given: ~e"
                val))
             (delay (p-app (force val))))))
       #:first-order promise?))))

#|
   as with copy-struct in struct.rkt, this first begin0
   expansion "declares" that struct/c is an expression.
   It prevents further expansion until the internal definition
   context is sorted out.
  |#
(define-syntax (struct/c stx)
  (syntax-case stx ()
    [(_ . args) 
     (with-syntax ([x (syntax/loc stx (do-struct/c . args))])
       (syntax/loc stx (begin0 x)))]))

(define-syntax (do-struct/c stx)
  (syntax-case stx ()
    [(_ struct-name args ...)
     (and (identifier? (syntax struct-name))
          (struct-info? (syntax-local-value (syntax struct-name) (λ () #f))))
     (with-syntax ([(ctc-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-name-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-pred-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-app-x ...) (generate-temporaries (syntax (args ...)))]
                   [(field-numbers ...)
                    (let loop ([i 0]
                               [l (syntax->list (syntax (args ...)))])
                      (cond
                        [(null? l) '()]
                        [else (cons i (loop (+ i 1) (cdr l)))]))]
                   [(type-desc-id 
                     constructor-id 
                     predicate-id 
                     (rev-selector-id ...)
                     (mutator-id ...)
                     super-id)
                    (lookup-struct-info (syntax struct-name) stx)])
       (unless (= (length (syntax->list (syntax (rev-selector-id ...))))
                  (length (syntax->list (syntax (args ...)))))
         (raise-syntax-error 'struct/c 
                             (format "expected ~a contracts because struct ~a has ~a fields"
                                     (length (syntax->list (syntax (rev-selector-id ...))))
                                     (syntax-e #'struct-name)
                                     (length (syntax->list (syntax (rev-selector-id ...)))))
                             stx))
       (with-syntax ([(selector-id ...) (reverse (syntax->list (syntax (rev-selector-id ...))))])
         (syntax
          (let ([ctc-x (coerce-contract 'struct/c args)] ...)
            
            (unless predicate-id
              (error 'struct/c "could not determine predicate for ~s" 'struct-name))
            (unless (and selector-id ...)
              (error 'struct/c "could not determine selectors for ~s" 'struct-name))
            
            (unless (flat-contract? ctc-x)
              (error 'struct/c "expected flat contracts as arguments, got ~e" args))
            ...
            
            (let ([ctc-pred-x (flat-contract-predicate ctc-x)] 
                  ...
                  [ctc-name-x (contract-name ctc-x)]
                  ...)
              (build-flat-contract
               (build-compound-type-name 'struct/c 'struct-name ctc-x ...)
               (λ (val)
                 (and (predicate-id val)
                      (ctc-pred-x (selector-id val)) ...))))))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))


(define/subexpression-pos-prop (parameter/c x)
  (make-parameter/c (coerce-contract 'parameter/c x)))

(define-struct parameter/c (ctc)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
      (let ([c-proc (contract-projection (parameter/c-ctc ctc))])
        (λ (blame)
           (let ([partial-neg-contract (c-proc (blame-swap blame))]
                 [partial-pos-contract (c-proc blame)])
             (λ (val)
                (cond
                 [(parameter? val)
                  (make-derived-parameter 
                   val 
                   partial-neg-contract
                   partial-pos-contract)]
                 [else
                  (raise-blame-error blame val "expected a parameter")]))))))

   #:name
   (λ (ctc) (build-compound-type-name 'parameter/c (parameter/c-ctc ctc)))
   #:first-order
   (λ (ctc)
      (let ([tst (contract-first-order (parameter/c-ctc ctc))])
        (λ (x)
           (and (parameter? x)
                (tst (x))))))

   #:stronger
   (λ (this that)
      ;; must be invariant (because the library doesn't currently split out pos/neg contracts
      ;; which could be tested individually ....)
      (and (parameter/c? that)
           (contract-stronger? (parameter/c-ctc this) 
                               (parameter/c-ctc that))
           (contract-stronger? (parameter/c-ctc that) 
                               (parameter/c-ctc this))))))

(define (hash/c dom rng #:immutable [immutable 'dont-care])
  (unless (memq immutable '(#t #f dont-care))
    (error 'hash/c "expected #:immutable argument to be either #t, #f, or 'dont-care, got ~s" immutable))
  (cond
    [(eq? immutable #t) 
     (make-immutable-hash/c (coerce-contract 'hash/c dom) 
                            (coerce-contract 'hash/c rng))]
    [else
     (make-hash/c (coerce-flat-contract 'hash/c dom) 
                  (coerce-flat-contract 'hash/c rng)
                  immutable)]))

;; hash-test : hash/c -> any -> bool
(define (hash-test ctc)
  (let ([dom-proc (flat-contract-predicate (hash/c-dom ctc))]
        [rng-proc (flat-contract-predicate (hash/c-rng ctc))]
        [immutable (hash/c-immutable ctc)])
    (λ (val)
      (and (hash? val)
           (case immutable
             [(#t) (immutable? val)]
             [(#f) (not (immutable? val))]
             [(dont-care) #t])
           (let/ec k
             (hash-for-each
              val
              (λ (dom rng)
                (unless (dom-proc dom) (k #f))
                (unless (rng-proc rng) (k #f))))
             #t)))))

(define-struct hash/c (dom rng immutable)
  #:omit-define-syntaxes

  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order hash-test
   #:projection
   (λ (ctc)
      (let ([dom-proc (contract-projection (hash/c-dom ctc))]
            [rng-proc (contract-projection (hash/c-rng ctc))]
            [immutable (hash/c-immutable ctc)])
        (λ (blame)
           (let ([partial-dom-contract (dom-proc blame)]
                 [partial-rng-contract (rng-proc blame)])
             (λ (val)
                (unless (hash? val)
                  (raise-blame-error blame val "expected a hash, got ~a" val))
                (case immutable
                  [(#t) (unless (immutable? val)
                          (raise-blame-error blame val
                                             "expected an immutable hash, got ~a" val))]
                  [(#f) (when (immutable? val)
                          (raise-blame-error blame val
                                             "expected a mutable hash, got ~a" val))]
                  [(dont-care) (void)])
                
                (hash-for-each
                 val
                 (λ (key val)
                    (partial-dom-contract key)
                    (partial-rng-contract val)))
                
                val)))))

   #:name
   (λ (ctc) (apply 
             build-compound-type-name
             'hash/c (hash/c-dom ctc) (hash/c-rng ctc)
             (if (eq? 'dont-care (hash/c-immutable ctc))
               '()
               (list '#:immutable (hash/c-immutable ctc)))))))

(define-struct immutable-hash/c (dom rng)
  #:omit-define-syntaxes

  #:property prop:contract
  (build-contract-property
   #:first-order (λ (ctc) (λ (val) (and (hash? val) (immutable? val))))
   #:projection
   (λ (ctc)
      (let ([dom-proc (contract-projection (immutable-hash/c-dom ctc))]
            [rng-proc (contract-projection (immutable-hash/c-rng ctc))])
        (λ (blame)
           (let ([partial-dom-contract (dom-proc blame)]
                 [partial-rng-contract (rng-proc blame)])
             (λ (val)
                (unless (and (hash? val)
                             (immutable? val))
                  (raise-blame-error blame val
                                     "expected an immutable hash"))
                (make-immutable-hash
                 (hash-map
                  val
                  (λ (k v)
                     (cons (partial-dom-contract k)
                           (partial-rng-contract v))))))))))

   #:name
   (λ (ctc) (build-compound-type-name
             'hash/c (immutable-hash/c-dom ctc) (immutable-hash/c-rng ctc)
             '#:immutable #t))))
