#lang racket/base

(require (for-template racket/base)
         syntax/boundmap
         racket/struct-info
         ;macro-debugger/emit
         "patterns.rkt")

(provide ddk? parse-literal all-vars pattern-var? match:syntax-err
         match-expander-transform trans-match parse-struct
         dd-parse parse-quote parse-id in-splicing?)

(define in-splicing? (make-parameter #f))

;; parse x as a match variable
;; x : identifier
(define (parse-id x)
  (cond [(eq? '_ (syntax-e x))
         (make-Dummy x)]
        [(ddk? x) (raise-syntax-error 'match "incorrect use of ... in pattern"
                                      x)]
        [else (make-Var x)]))

;; stx : syntax of pattern, starting with quote
;; parse : the parse function
(define (parse-quote stx parse)
  (syntax-case stx (quote)
    [(quote ())
     (make-Null (make-Dummy stx))]
    [(quote (a . b))
     (make-Pair (parse (syntax/loc stx (quote a)))
                (parse (syntax/loc stx (quote b))))]
    [(quote vec)
     (vector? (syntax-e #'vec))
     (make-Vector (for/list ([e (syntax-e #'vec)])
                    (parse (quasisyntax/loc stx (quote #,e)))))]
    [(quote bx)
     (box? (syntax-e #'bx))
     (make-Box (parse (quasisyntax/loc 
                       stx 
                       (quote #,(unbox (syntax-e #'bx))))))]
    [(quote v)
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "non-literal in quote pattern" stx #'v))]
    [_ (raise-syntax-error 'match "syntax error in quote pattern" stx)]))

;; parse : the parse fn
;; p : the repeated pattern
;; dd : the ... stx
;; rest : the syntax for the rest
;; pred? : recognizer for the parsed data structure (such as list?)
;; to-list: function to convert the value to a list
(define (dd-parse parse p dd rest pred? #:to-list [to-list #'values] #:mutable [mutable? #f])
  (define count (ddk? dd))
  (define min (and (number? count) count))
  (define pat (parameterize ([match-...-nesting (add1 (match-...-nesting))])
                (parse p)))
  (define rest-pat (parse rest))
  (cond [(and (not (in-splicing?)) ;; when we're inside splicing, rest-pat isn't the rest
              (not min) ;; if we have a count, better generate general code
              (Null? rest-pat)
              (or (Var? pat) (Dummy? pat)))
         (make-And (list (make-Pred pred?) (make-App to-list (list pat))))]
        [else (make-GSeq (list (list pat))
                         (list min)
                         ;; no upper bound
                         (list #f)
                         ;; patterns in p get bound to lists
                         (list #f)
                         rest-pat
                         mutable?)]))

;; stx : the syntax object for the whole pattern
;; parse : the pattern parser
;; struct-name : identifier
;; mode : #f or one of '#:first '#:last '#:full
;; pats : syntax representing the member patterns
;; returns a pattern
(define (parse-struct stx parse struct-name mode pats)
  (let* ([fail (lambda ()
                 (raise-syntax-error
                  'match (format "~a does not refer to a structure definition"
                                 (syntax->datum struct-name))
                  stx struct-name))]
         [v (syntax-local-value struct-name fail)])
    (unless (struct-info? v) (fail))
    (let-values ([(id _1 pred acc _2 super fields)
                  (apply values
                         (append (extract-struct-info v)
                                 (list (extract-struct-field-info v))))])
      ;; this produces a list of all the super-types of this struct
      ;; ending when it reaches the top of the hierarchy, or a struct that we
      ;; can't access
      (define (get-lineage struct-name)
        (let ([super (list-ref (extract-struct-info (syntax-local-value
                                                     struct-name))
                               5)])
          (cond [(equal? super #t) (values #t '())] ;; no super type exists
                [(equal? super #f) (values #f '())] ;; super type is unknown
                [else 
                 (let-values ([(complete? lineage) (get-lineage super)])
                   (values complete?
                           (cons super lineage)))])))
      (unless pred
        (raise-syntax-error 'match (format "structure ~a does not have an associated predicate"
                                           (syntax->datum struct-name))
                            stx struct-name))
      (let-values ([(complete? lineage) (get-lineage struct-name)])
        (let* ( ;; the accessors come in reverse order
               [acc* (reverse acc)]
               ;; remove the first element, if it's #f
               [acc* (cond [(null? acc*) acc*]
                           [(not (car acc*)) (cdr acc*)]
                           [else acc*])])
          (make-Struct pred
                       (syntax-property 
                        pred 
                        'disappeared-use (list struct-name))
                       lineage (and (checked-struct-info? v) complete?)
                       acc*
                       (cond [(eq? '_ (syntax-e pats))                            
                              (map make-Dummy acc*)]
                             [(syntax->list pats)
                              =>
                              (parse-struct-pats stx parse struct-name mode acc acc* fields)]
                             [else (raise-syntax-error
                                    'match
                                    "improper syntax for struct pattern"
                                    stx pats)])))))))

(define ((parse-struct-pats stx parse struct-name mode rev-acc accessors rev-fields) pats)
  ;; The grammar of a struct form when not fully dummied is
  ;; (struct-id kw/pats ...) 
  ;; (struct struct-id mode-op (kw/pats ...))
  ;; mode not available for (s . kw/pats) because un-quoted keywords are already patterns.
  ;; mode-op ::=
  ;;           | #:first
  ;;           | #:last
  ;;           | #:full
  ;; kw/pats := [f-kw pat]
  ;;          | pat
  ;; where f-kw is a field name as a keyword.
  ;; If no first/last is given and some kw/pats are just pats,
  ;; there must be exactly the same number of patterns as accessors.
  ;; If all kw/pats are named fields, then there only needs to be
  ;; the full amount of patterns if the mode is #:full.
  (define acclen (length rev-acc))
  (define (bad-arity pats)
    (raise-syntax-error
     'match
     (format "~a structure ~a: expected ~a but got ~a"
             "wrong number of fields for"
             (syntax->datum struct-name) acclen
             (length pats))
     stx pats))
  (cond
   ;; do a fast-path check
   [(and (not mode)
         (for/and ([p (in-list pats)])
           (syntax-case p ()
             [[kw _] (keyword? (syntax-e #'kw)) #f]
             [_ #t])))
    (map parse pats)]
   ;; named patterns, but no field info
   [(not rev-fields)
    (raise-syntax-error
     'match
     (format 
      "struct identifier ~a not bound to extended struct info. Cannot use named patterns."
      (syntax->datum struct-name))
     stx)]
   [else
    ;; Named and non-named patterns may be interleaved, but
    ;; a name following its position is an error.
    ;; e.g. with fields (x y z), we can't have (0 [#:x 1] [#:z 2])
    ;; but we can have ([#:x 1] 0 [#:z 2]) and ([#:z 2] [#:x 1] 0).

    ;; Build a table of field name to a pair of the associated
    ;; pattern syntax and the count of unnamed patterns preceding it.
    ;; Parse while we're at it.
    (define named (make-hasheq))
    ;; build a table to check that the names given are actual field names.
    (define actual-fields (make-hasheq))
    (for ([f (in-list rev-fields)]) (hash-set! actual-fields f #t))
    (define (bad-field? f)
      (unless (hash-has-key? actual-fields f)
        (raise-syntax-error 'match
                            (format "field name (~a) not associated with structure ~a"
                                    f (syntax->datum struct-name))
                            stx)))
    ;; We collect the layout in a list for #:last to compute the analogous count
    ;; of unnamed patterns that /follow/ the named pattern.
    (define-values (rev-parsed-unnamed rev-layout num-unnamed)
      (for/fold ([parsed '()] [layout '()] [count 0]) ([p (in-list pats)])
        (define (named-pat name pat)
          (bad-field? name)
          (when (hash-has-key? named name)
            (raise-syntax-error
             'match
             "field name appears twice"
             stx name))
          (hash-set! named name (cons (parse pat) count))
          (values parsed (cons name layout) count))
        (syntax-case p ()
          [[kw pat]
           (keyword? (syntax-e #'kw))
           (named-pat (string->symbol (keyword->string (syntax-e #'kw))) #'pat)]
          [_ (values (cons (parse p) parsed) (cons #f layout) (add1 count))])))

    ;; Bearings made. Now we check sanity.
    (define total (+ num-unnamed (hash-count named)))
    (unless (if (or (eq? mode '#:full)
                    ;; default behavior: expect all fields if
                    ;; any unnamed pattern is given.
                    (and (not mode) (< 0 num-unnamed)))
                (= total acclen)
                (<= total acclen))
      (bad-arity pats))

    ;; the following raise conflict errors with (struct Foo (x y z w))
    ;; (match (Foo 0 2 1 3) [(Foo 0 1 [#:y 2] 3) #t])
    ;; (match (Foo 'dropped 0 3 2) [(Foo #:last 0 [#:w 2] 3) #t])

    (define used-unnamed (box #f))
    (define (use! acc)
      (define lst (unbox used-unnamed))
      (cond
       [(null? lst) (make-Dummy acc)]
       [else (set-box! used-unnamed (cdr lst))
             (car lst)]))
    (cond
     [(eq? mode '#:last)
      (set-box! used-unnamed rev-parsed-unnamed)
      ;; The count in named isn't useful for #:last mode.
      ;; We need the number of unnamed patterns that happen /after/ a named pattern.
      (define count-after (make-hasheq))
      (for/fold ([count 0]) ([op-field (in-list rev-layout)])
        (cond [op-field
               (hash-set! count-after op-field count)
               count]
              [else (add1 count)]))

      ;; Layout backwards.
      (for/fold ([rev-layout '()])
          ([acc (in-list rev-acc)]
           [field (in-list rev-fields)]
           [i (in-range (sub1 acclen) -1 -1)]
           ;; don't count the "missing super info" #f
           #:when (and acc field))
        (define info (hash-ref named field #f))
        (cond
         [(pair? info)
          (define num (hash-ref count-after field))
          (unless (< num (- acclen i))
            ;; Give position in terms of pattern number, not field number.
            (raise-syntax-error
             'match
             (format "named field pattern (~a) given before ~a~a~a~a~a in #:last mode"
                     field num
                     (format " unnamed pattern~a but the field is "
                             (if (= num 1) "" "s"))
                     (- (sub1 acclen) i)
                     " from last for structure "
                     (syntax->datum struct-name))
             stx pats))
          (cons (car info) rev-layout)]
         [else (cons (use! acc) rev-layout)]))]
     [else ;;(or (eq? mode '#:first) (not mode))
      (define fields (reverse rev-fields))
      (set-box! used-unnamed (reverse rev-parsed-unnamed))
      (for/list ([acc (in-list accessors)]
                 [field (in-list fields)]
                 [i (in-naturals)])
        (define info (hash-ref named field #f))
        (cond
         [(pair? info)
          (define num (cdr info))
          (unless (<= num i)
            (raise-syntax-error
             'match
             (format "named field pattern (~a) given after ~a~a~a for structure ~a"
                     field num
                     (format " unnamed pattern~a but the field is in position "
                             (if (= num 1) "" "s"))
                     i (syntax->datum struct-name))
             stx pats))
          (car info)]
         [else (use! acc)]))])]))

(define (trans-match pred transformer pat)
  (make-And (list (make-Pred pred) (make-App transformer (list pat)))))

;; transform a match-expander application
;; parse : stx -> pattern
;; expander : identifier
;; stx : the syntax of the match-expander application (armed)
;; accessor : match-expander -> syntax transformer/#f
;; error-msg : string
;; produces a parsed pattern
(define (match-expander-transform parse expander stx accessor
                                  error-msg)
  (let* ([expander* (syntax-local-value expander)]
         [transformer (accessor expander*)]
         ;; this transformer might have been defined w/ `syntax-id-rules'
         [transformer (if (set!-transformer? transformer)
                          (set!-transformer-procedure transformer)
                          transformer)])
    (unless transformer (raise-syntax-error #f error-msg expander*))
    (let* ([introducer (make-syntax-introducer)]
           [mstx (introducer (syntax-local-introduce stx))]
           [mresult (if (procedure-arity-includes? transformer 2) 
                        (transformer expander* mstx)
                        (transformer mstx))]
           [result (syntax-local-introduce (introducer mresult))])
      ;(emit-local-step stx result #:id expander)
      (parse result))))

;; raise an error, blaming stx
(define (match:syntax-err stx msg)
  (raise-syntax-error #f msg stx))

;; pattern-var? : syntax -> bool
;; is p an identifier representing a pattern variable?
(define (pattern-var? p)
  (and (identifier? p) (not (ddk? p))))

;; ddk? : syntax -> number or boolean
;; if #f is returned, was not a ddk identifier
;; if #t is returned, no minimum
;; if a number is returned, that's the minimum
(define (ddk? s*)
  (define (./_ c) (or (equal? c #\.) (equal? c #\_)))
  (let ([s (syntax->datum s*)])
    (and (symbol? s)
         (if (memq s '(... ___))
           #t
           (let* ([m (regexp-match #rx"^(?:\\.\\.|__)([0-9]+)$"
                                   (symbol->string s))]
                  [n (and m (string->number (cadr m)))])
             (cond [(not n) #f]
                   [(zero? n) #t]
                   [(exact-nonnegative-integer? n) n]
                   [else (raise-syntax-error
                          'match "invalid number for ..k pattern"
                          s*)]))))))

;; parse-literal : racket-val -> pat option
;; is v is a literal, return a pattern matching it
;; otherwise, return #f
(define (parse-literal v)
  (if (or (number? v) (string? v) (keyword? v) (symbol? v) (bytes? v)
          (regexp? v) (boolean? v) (char? v))
    (make-Exact v)
    #f))

;; (listof pat) syntax -> void
;; ps is never null
;; check that all the ps bind the same set of variables
(define (all-vars ps stx)  
  (let* ([first-vars (bound-vars (car ps))]
         [l (length ps)]
         [ht (make-free-identifier-mapping)])
    (for ([v first-vars]) (free-identifier-mapping-put! ht v 1))
    (for* ([p (cdr ps)]
           [v (bound-vars p)])
      (cond [(free-identifier-mapping-get ht v (lambda () #f))
             => (lambda (n)
                  (free-identifier-mapping-put! ht v (add1 n)))]
            [else (raise-syntax-error 'match
                                      "variable not bound in all or patterns"
                                      stx v)]))
    (free-identifier-mapping-for-each
     ht
     (lambda (v n)
       (unless (= n l)
         (raise-syntax-error 'match "variable not bound in all or patterns"
                             stx v))))))
