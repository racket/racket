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
         promise/c
         struct/c
         syntax/c
         
         check-between/c
         check-unary-between/c
         parameter/c)

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
              (if (chaperone-contract? (car ho-contracts))
                  (make-chaperone-single-or/c pred flat-contracts (car ho-contracts))
                  (make-proxy-single-or/c pred flat-contracts (car ho-contracts)))]
             [else
              (if (andmap chaperone-contract? ho-contracts)
                  (make-chaperone-multi-or/c flat-contracts ho-contracts)
                  (make-proxy-multi-or/c flat-contracts ho-contracts))]))))]))

(define (single-or/c-projection ctc)
  (let ([c-proc (contract-projection (single-or/c-ho-ctc ctc))]
        [pred (single-or/c-pred ctc)])
    (λ (blame)
      (let ([partial-contract (c-proc blame)])
        (λ (val)
          (cond
            [(pred val) val]
            [else (partial-contract val)]))))))

(define (single-or/c-name ctc)
  (apply build-compound-type-name 
         'or/c 
         (single-or/c-ho-ctc ctc)
         (single-or/c-flat-ctcs ctc)))

(define (single-or/c-first-order ctc)
  (let ([pred (single-or/c-pred ctc)]
        [ho (contract-first-order (single-or/c-ho-ctc ctc))])
    (λ (x) (or (ho x) (pred x)))))

(define (single-or/c-stronger? this that)
  (and (single-or/c? that)
       (contract-stronger? (single-or/c-ho-ctc this)
                           (single-or/c-ho-ctc that))
       (let ([this-ctcs (single-or/c-flat-ctcs this)]
             [that-ctcs (single-or/c-flat-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger?
                      this-ctcs
                      that-ctcs)))))

(define-struct single-or/c (pred flat-ctcs ho-ctc))

(define-struct (chaperone-single-or/c single-or/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection single-or/c-projection
   #:name single-or/c-name
   #:first-order single-or/c-first-order
   #:stronger single-or/c-stronger?))

(define-struct (proxy-single-or/c single-or/c) ()
  #:property prop:contract
  (build-contract-property
   #:projection single-or/c-projection
   #:name single-or/c-name
   #:first-order single-or/c-first-order
   #:stronger single-or/c-stronger?))

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

(define (multi-or/c-name ctc)
  (apply build-compound-type-name 
         'or/c 
         (append
          (multi-or/c-flat-ctcs ctc)
          (reverse (multi-or/c-ho-ctcs ctc)))))

(define (multi-or/c-first-order ctc)
  (let ([flats (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))]
        [hos (map (λ (x) (contract-first-order x)) (multi-or/c-ho-ctcs ctc))])
    (λ (x)
      (or (ormap (λ (f) (f x)) hos)
          (ormap (λ (f) (f x)) flats)))))

(define (multi-or/c-stronger? this that)
  (and (multi-or/c? that)
       (let ([this-ctcs (multi-or/c-ho-ctcs this)]
             [that-ctcs (multi-or/c-ho-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger? this-ctcs that-ctcs)))
       (let ([this-ctcs (multi-or/c-flat-ctcs this)]
             [that-ctcs (multi-or/c-flat-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger? this-ctcs that-ctcs)))))

(define-struct multi-or/c (flat-ctcs ho-ctcs))

(define-struct (chaperone-multi-or/c multi-or/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection multi-or/c-proj
   #:name multi-or/c-name
   #:first-order multi-or/c-first-order
   #:stronger multi-or/c-stronger?))

(define-struct (proxy-multi-or/c multi-or/c) ()
  #:property prop:contract
  (build-contract-property
   #:projection multi-or/c-proj
   #:name multi-or/c-name
   #:first-order multi-or/c-first-order
   #:stronger multi-or/c-stronger?))

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
          [(and (= n -inf.0) (= m +inf.0))
           `(between/c ,n ,m)]
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

(define-syntax (*-listof stx)
  (syntax-case stx ()
    [(_ predicate? type-name name)
     (identifier? (syntax predicate?))
     (syntax
      (λ (input)
        (let* ([ctc (coerce-contract 'name input)]
               [ctc-name (build-compound-type-name 'name ctc)]
               [proj (contract-projection ctc)])
          (define (fo-check x)
            (and (predicate? x) 
                 (for/and ([v (in-list x)])
                   (contract-first-order-passes? ctc v))))
          (define ((ho-check check-all) blame)
            (let ([p-app (proj blame)])
              (λ (val)
                (unless (predicate? val)
                  (raise-blame-error blame val
                                     "expected <~a>, given: ~e"
                                     'type-name val))
                (check-all p-app val))))
          (cond
            [(flat-contract? ctc)
             (make-flat-contract
              #:name ctc-name
              #:first-order fo-check
              #:projection (ho-check (λ (p v) (for-each p v) v)))]
            [(chaperone-contract? ctc)
             (make-chaperone-contract
              #:name ctc-name
              #:first-order fo-check
              #:projection (ho-check (λ (p v) (map p v))))]
            [else
             (make-contract
              #:name ctc-name
              #:first-order fo-check
              #:projection (ho-check (λ (p v) (map p v))))]))))]))

(define listof-func (*-listof list? list listof))
(define/subexpression-pos-prop (listof x) (listof-func x))

(define (non-empty-list? x) (and (pair? x) (list? (cdr x))))
(define non-empty-listof-func (*-listof non-empty-list? non-empty-list non-empty-listof))
(define/subexpression-pos-prop (non-empty-listof a) (non-empty-listof-func a))

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


(define cons/c-main-function
  (λ (car-c cdr-c)
    (let* ([ctc-car (coerce-contract 'cons/c car-c)]
           [ctc-cdr (coerce-contract 'cons/c cdr-c)]
           [ctc-name (build-compound-type-name 'cons/c ctc-car ctc-cdr)]
           [car-proj (contract-projection ctc-car)]
           [cdr-proj (contract-projection ctc-cdr)])
      (define (fo-check v)
        (and (pair? v)
             (contract-first-order-passes? ctc-car (car v))
             (contract-first-order-passes? ctc-cdr (cdr v))))
      (define ((ho-check combine) blame)
        (let ([car-p (car-proj blame)]
              [cdr-p (cdr-proj blame)])
          (λ (v)
            (unless (pair? v)
              (raise-blame-error blame v "expected <~a>, given: ~e" 'cons v))
            (combine v (car-p (car v)) (cdr-p (cdr v))))))
      (cond
        [(and (flat-contract? ctc-car) (flat-contract? ctc-cdr))
         (make-flat-contract
          #:name ctc-name
          #:first-order fo-check
          #:projection (ho-check (λ (v a d) v)))]
        [(and (chaperone-contract? ctc-car) (chaperone-contract? ctc-cdr))
         (make-chaperone-contract
          #:name ctc-name
          #:first-order fo-check
          #:projection (ho-check (λ (v a d) (cons a d))))]
        [else
         (make-contract
           #:name ctc-name
           #:first-order fo-check
           #:projection (ho-check (λ (v a d) (cons a d))))]))))

(define/subexpression-pos-prop (cons/c a b) (cons/c-main-function a b))

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
