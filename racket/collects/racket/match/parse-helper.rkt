#lang racket/base

(require (for-template racket/base)
         syntax/boundmap
         racket/struct-info
         ;macro-debugger/emit
         "patterns.rkt"
         "syntax-local-match-introduce.rkt")

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
(define (dd-parse parse p dd rest pred? #:to-list [to-list #f] #:mutable [mutable? #f])
  (define count (ddk? dd))
  (define min (and (number? count) count))
  (define pat (parameterize ([match-...-nesting (add1 (match-...-nesting))])
                (parse p)))
  (define rest-pat (parse rest))
  (cond [(and (not (in-splicing?)) ;; when we're inside splicing, rest-pat isn't the rest
              (not min) ;; if we have a count, better generate general code
              (Null? rest-pat)
              (or (Var? pat) (Dummy? pat)))
         (make-OrderedAnd (list (make-Pred pred?)
                                (if to-list
                                    (make-App to-list (list pat))
                                    pat)))]
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
;; pats : syntax representing the member patterns
;; returns a pattern
(define (parse-struct stx parse struct-name pats)
  (let* ([fail (lambda ()
                 (raise-syntax-error
                  'match (format "~a does not refer to a structure definition"
                                 (syntax->datum struct-name))
                  stx struct-name))]
         [v (syntax-local-value struct-name fail)])
    (unless (struct-info? v) (fail))
    (let-values ([(id _1 pred acc _2 super)
                  (apply values (extract-struct-info v))])
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
        (let* (;; the accessors come in reverse order
               [acc (reverse acc)]
               ;; remove the first element, if it's #f
               [acc (cond [(null? acc) acc]
                          [(not (car acc)) (cdr acc)]
                          [else acc])])
          (make-Struct pred
                       (syntax-property
                        pred
                        'disappeared-use (list struct-name))
                       lineage (and (checked-struct-info? v) complete?)
                       acc
                       (cond [(eq? '_ (syntax-e pats))
                              (map make-Dummy acc)]
                             [(syntax->list pats)
                              =>
                              (lambda (ps)
                                (unless (= (length ps) (length acc))
                                  (raise-syntax-error
                                   'match
                                   (format "~a structure ~a: expected ~a but got ~a"
                                           "wrong number for fields for"
                                           (syntax->datum struct-name) (length acc)
                                           (length ps))
                                   stx pats))
                                (map parse ps))]
                             [else (raise-syntax-error
                                    'match
                                    "improper syntax for struct pattern"
                                    stx pats)])))))))

(define (trans-match pred transformer pat)
  (make-OrderedAnd (list (make-Pred pred) (make-App transformer (list pat)))))

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
    (define introducer (make-syntax-introducer))
    (parameterize ([current-match-introducer introducer])
      (let* ([mstx (introducer (syntax-local-introduce stx))]
             [mresult (if (procedure-arity-includes? transformer 2)
                          (transformer expander* mstx)
                          (transformer mstx))]
             [result (syntax-local-introduce (introducer mresult))])
        ;(emit-local-step stx result #:id expander)
        (parse result)))))

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
