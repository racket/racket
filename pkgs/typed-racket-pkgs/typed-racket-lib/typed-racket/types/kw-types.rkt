#lang racket/base

(require "abbrev.rkt" "../rep/type-rep.rkt"
         "../utils/tc-utils.rkt"
         "../base-env/annotate-classes.rkt"
         "tc-result.rkt"
         racket/list racket/set racket/dict racket/match
         racket/format racket/string
         syntax/parse)

;; convert : [Listof Keyword] [Listof Type] [Listof Type] [Option Type]
;;           [Option Type] [Option (Pair Type symbol)] boolean -> Type
(define (convert kw-t plain-t opt-t rng rest drest split?)
  (define-values (mand-kw-t opt-kw-t) (partition (match-lambda [(Keyword: _ _ m) m]) kw-t))

  (when drest
    (int-err "drest passed to kw-convert"))

  (define arities
    (for/list ([i (in-range (length opt-t))])
      (make-arr* (append plain-t (take opt-t i))
                 rng
                 #:kws kw-t
                 #:rest rest
                 #:drest drest)))
  ;; the kw function protocol passes rest args as an explicit list
  (define rest-type (if rest (-lst rest) empty))
  (define ts 
    (flatten
     (list
      (for/list ([k (in-list mand-kw-t)])
        (match k
          [(Keyword: _ t _) t]))
      (for/list ([k (in-list opt-kw-t)])
        (match k
          [(Keyword: _ t _) (list (-opt t) -Boolean)]))
      plain-t
      (for/list ([t (in-list opt-t)]) (-opt t))
      (for/list ([t (in-list opt-t)]) -Boolean)
      rest-type)))
  ;; the kw protocol puts the arguments in keyword-sorted order in the
  ;; function header, so we need to sort the types to match
  (define sorted-kws
    (sort kw-t keyword<? #:key (match-lambda [(Keyword: kw _ _) kw])))
  (define ts/true
    (flatten
     (list
      (for/list ([k (in-list sorted-kws)])
        (match k
          [(Keyword: _ t #t) t]
          [(Keyword: _ t #f) (list t (-val #t))]))
      plain-t
      (for/list ([t (in-list opt-t)]) t)
      (for/list ([t (in-list opt-t)]) (-val #t))
      rest-type)))
  (define ts/false
    (flatten
     (list
      (for/list ([k (in-list sorted-kws)])
        (match k
          [(Keyword: _ t #t) t]
          [(Keyword: _ t #f) (list (-val #f) (-val #f))]))
      plain-t
      (for/list ([t (in-list opt-t)]) (-val #f))
      (for/list ([t (in-list opt-t)]) (-val #f))
      rest-type)))
  (make-Function
    (if split?
        (remove-duplicates
          (list (make-arr* ts/true rng #:drest drest)
                (make-arr* ts/false rng #:drest drest)))
        (list (make-arr* ts rng #:rest rest #:drest drest)))))


;; This is used to fix the filters of keyword types.
;; TODO: This should also explore deeper into the actual types and remove filters in there as well.
;; TODO: This should not remove the filters but instead make them refer to the actual arguments after
;; keyword conversion.
(define (erase-filter/Values values)
  (match values
    [(AnyValues: _) ManyUniv]
    [(Results: ts fs os)
     (-values ts)]
    [(Results: ts fs os dty dbound)
     (-values-dots ts dty dbound)]))


(define (prefix-of a b)
  (define (rest-equal? a b)
    (match* (a b)
      [(#f #f) #t]
      [(#f _) #f]
      [(_ #f) #f]
      [(a b) (type-equal? a b)]))
  (define (drest-equal? a b)
    (match* (a b)
      [((list t b) (list t* b*)) (and (type-equal? t t*) (equal? b b*))]
      [(#f #f) #t]
      [(_ _) #f]))
  (define (kw-equal? a b)
    (and (equal? (length a) (length b))
         (for/and ([k1 (in-list a)] [k2 (in-list b)])
           (type-equal? k1 k2))))
  (match* (a b)
    [((arr: args result rest drest kws)
      (arr: args* result* rest* drest* kws*))
     (and (< (length args) (length args*))
          (rest-equal? rest rest*)
          (drest-equal? drest drest*)
          (type-equal? result result*)
          (kw-equal? kws kws*)
          (for/and ([p (in-list args)] [p* (in-list args*)])
            (type-equal? p p*)))]))

(define (arity-length a)
  (match a
    [(arr: args result rest drest kws) (length args)]))


(define (arg-diff a1 a2)
  (match a2
    [(arr: args _ _ _ _) (drop args (arity-length a1))]))

(define (find-prefixes l)
  (define l* (sort l < #:key arity-length))
  (for/fold ([d (list)]) ([e (in-list l*)])
    (define prefix (for/or ([p (in-dict-keys d)])
                     (and (prefix-of p e) p)))
    (if prefix
        (dict-set d prefix (arg-diff prefix e))
        (dict-set d e empty))))

;; handle-extra-or-missing-kws : (Listof Keyword) LambdaKeywords
;;                               -> (Listof Keyword)
;; Remove keywords in the given list that aren't in the actual lambda
;; keywords list. This allows the typechecker to check some branches of the
;; type that match the actual kws. Add extra actual keywords with Bottom type.
(define (handle-extra-or-missing-kws kws actual-kws)
  (match-define (lambda-kws actual-mands actual-opts) actual-kws)
  (define expected-kws (map Keyword-kw kws))
  (define missing-removed
    (filter
     (λ (kw) (or (member (Keyword-kw kw) actual-mands)
                 (member (Keyword-kw kw) actual-opts)))
     kws))
  (append missing-removed
          (for/list ([kw (in-list (set-subtract actual-mands expected-kws))])
            (make-Keyword kw -Bottom #t))
          (for/list ([kw (in-list (set-subtract actual-opts expected-kws))])
            (make-Keyword kw -Bottom #f))))

;; inner-kw-convert : (Listof arr) (Option LambdaKeywords) Boolean -> Type
;; Helper function that converts each arr to a Function type and then
;; merges them into a single Function type.
(define (inner-kw-convert arrs actual-kws split?)
  (define table (find-prefixes arrs))
  (define fns
    (for/set ([(k v) (in-dict table)])
      (match k
        [(arr: mand rng rest drest kws)
         (define kws* (if actual-kws
                          (handle-extra-or-missing-kws kws actual-kws)
                          kws))
         (convert kws* mand v rng rest drest split?)])))
  (apply cl->* (set->list fns)))

;; kw-convert : Type (Option LambdaKeywords) [Boolean] -> Type
;; Given an ordinary function type, convert it to a type that matches the keyword
;; function protocol
(define (kw-convert ft actual-kws #:split [split? #f])
  (match ft
    [(Function: arrs)
     (inner-kw-convert arrs actual-kws split?)]
    [(Poly-names: names f)
     (make-Poly names (kw-convert f actual-kws #:split split?))]
    [(PolyDots-names: names f)
     (make-PolyDots names (kw-convert f actual-kws #:split split?))]))

;; kw-unconvert : Type (Listof Syntax) (Listof Keyword) (Listof Keyword) -> Type
;; Given a type for a core keyword function, unconvert it to a
;; normal function type.
;;
;; precondition: only call this for functions with no type annotations,
;;               which means they will never have polymorphic types.
(define (kw-unconvert ft formalss mand-keywords keywords)
  (define (lengthish formals)
    (define lst (syntax->list formals))
    (if lst (length lst) +inf.0))
  ;; only need longest formals and smallest formals to determine
  ;; which arguments are optional non-kw arguments
  (define max-formals (argmax lengthish formalss))
  (define min-formals (argmin lengthish formalss))
  (define-values (raw-non-kw-argc rest?)
    (syntax-parse max-formals
      [(_ _ non-kw:id ...)
       (values (length (syntax->list #'(non-kw ...))) #f)]
      [(_ _ non-kw:id ... . rst:id)
       (values (length (syntax->list #'(non-kw ...))) #t)]))
  (define opt-non-kw-argc
    (syntax-parse min-formals
      [(_ _ non-kw:id ...)
       (- raw-non-kw-argc (length (syntax->list #'(non-kw ...))))]
      ;; if min and max both have rest args, then there cannot
      ;; have been any optional arguments
      [(_ _ non-kw:id ... . rst:id) 0]))
  ;; counted twice since optionals expand to two arguments
  (define non-kw-argc (+ raw-non-kw-argc opt-non-kw-argc))
  (define mand-non-kw-argc (- non-kw-argc (* 2 opt-non-kw-argc)))
  (match ft
    [(Function: arrs)
     (cond [(= (length arrs) 1) ; no optional args (either kw or not)
            (match-define (arr: doms rng _ _ _) (car arrs))
            (define kw-length
              (- (length doms) (+ non-kw-argc (if rest? 1 0))))
            (define-values (kw-args other-args) (split-at doms kw-length))
            (define actual-kws
              (for/list ([kw (in-list keywords)]
                         [kw-type (in-list kw-args)])
                (make-Keyword kw kw-type #t)))
            (define rest-type
              (and rest? (last other-args)))
            (make-Function
             (list (make-arr* (take other-args non-kw-argc)
                              (erase-filter/Values rng)
                              #:kws actual-kws
                              #:rest rest-type)))]
           [(and (even? (length arrs)) ; had optional args
                 (>= (length arrs) 2))
            ;; assumption: only one arr is needed, since the types for
            ;; the actual domain are the same (the difference is in the
            ;; second type for the optional keyword protocol)
            (match-define (arr: doms rng _ _ _) (car arrs))
            (define kw-length
              (- (length doms) (+ non-kw-argc (if rest? 1 0))))
            (define kw-args (take doms kw-length))
            (define actual-kws (compute-kws mand-keywords keywords kw-args))
            (define other-args (drop doms kw-length))
            (define-values (mand-args opt-and-rest-args)
              (split-at other-args mand-non-kw-argc))
            (define rest-type
              (and rest? (last opt-and-rest-args)))
            (define opt-types (take opt-and-rest-args opt-non-kw-argc))
            (make-Function
             (for/list ([to-take (in-range (add1 (length opt-types)))])
               (make-arr* (append mand-args (take opt-types to-take))
                          (erase-filter/Values rng)
                          #:kws actual-kws
                          #:rest rest-type)))]
           [else (int-err "unsupported arrs in keyword function type")])]
    [_ (int-err "unsupported keyword function type")]))

;; check-kw-arity : LambdaKeywords Type -> Void
;;
;; Check if the TR lambda property listing the keywords matches up with
;; the type that we've given. Allows for better error messages than just
;; relying on tc-expr. Return #f if the function shouldn't be checked.
(define (check-kw-arity kw-prop f-type)
  (match-define (lambda-kws actual-mands actual-opts) kw-prop)
  (define arrs
    (match f-type
      [(AnyPoly-names: _ _ (Function: arrs)) arrs]))
  (for/and ([arr (in-list arrs)])
    (match-define (arr: _ _ _ _ kws) arr)
    (define-values (mand-kw-types opt-kw-types) (partition-kws kws))
    (define mand-kws (map Keyword-kw mand-kw-types))
    (define opt-kws (map Keyword-kw opt-kw-types))
    (define missing-opts (set-subtract opt-kws actual-opts))
    (define missing-mands (set-subtract mand-kws actual-mands))
    ;; extra optional keywords are okay
    (define extra-kws (set-subtract actual-mands mand-kws))
    (unless (and (set-empty? missing-mands)
                 (set-empty? missing-opts))
      (tc-error/fields
       #:delayed? #t
       "type mismatch"
       #:more "function is missing keyword arguments"
       "missing mandatory keywords"
       (string-join (map ~a missing-mands))
       "missing optional keywords"
       (string-join (map ~a missing-opts))
       "expected type" f-type))
    (unless (set-empty? extra-kws)
      (tc-error/fields
       #:delayed? #t
       "type mismatch"
       #:more "function has too many mandatory keyword arguments"
       "extra keywords"
       (string-join (map ~a extra-kws))
       "expected type" f-type))))

;; compute-kws : (Listof Keyword) (Listof Keyword) (Listof Type)
;;               -> (Listof make-Keyword)
;; Computes the keyword types for an arr in kw-unconvert
;;
;; assumptions: (1) in kw-args, there are two types per optional kw
;;                  and the first type is the argument type (which is
;;                  the same in every `arr` in the function type)
;;              (2) kw-args and keywords are sorted by keyword<? order
(define (compute-kws mand-keywords keywords kw-args)
  (let loop ([kw-args kw-args]
             [keywords keywords]
             [kw-types '()])
    (cond [(empty? kw-args) (reverse kw-types)]
          [(memq (car keywords) mand-keywords)
           (loop (cdr kw-args) (cdr keywords)
                 (cons (make-Keyword (car keywords) (car kw-args) #t)
                       kw-types))]
          [else ; optional, so there are two arg types
           (loop (cddr kw-args) (cdr keywords)
                 (cons (make-Keyword (car keywords) (car kw-args) #f)
                       kw-types))])))

(define ((opt-convert-arr required-pos optional-pos) arr)
  (match arr
    [(arr: args result #f #f '())
     (define num-args (length args))
     (and (>= num-args required-pos)
          (<= num-args (+ required-pos optional-pos))
          (let* ([required-args (take args required-pos)]
                 [opt-args (drop args required-pos)]
                 [missing-opt-args (- (+ required-pos optional-pos) num-args)]
                 [present-flags (map (λ (t) (-val #t)) opt-args)]
                 [missing-args (make-list missing-opt-args (-val #f))])
            (make-arr (append required-args
                              opt-args
                              missing-args
                              present-flags
                              missing-args)
                      result
                      #f
                      #f
                      '())))]
    [(arr: args result _ _ _) #f]))

(define (opt-convert ft required-pos optional-pos)
  (let/ec exit
    (let loop ((ft ft))
      (match ft
        [(Function: arrs)
         (let ((arrs (map (opt-convert-arr required-pos optional-pos) arrs)))
           (if (andmap values arrs)
               (make-Function arrs)
               (exit #f)))]
        [(Poly-names: names f)
         (make-Poly names (loop f))]
        [(PolyDots-names: names f)
         (make-PolyDots names (loop f))]
        [t t]))))

;; opt-unconvert : Type (Listof Syntax) -> Type
;; Given a type for a core optional arg function, unconvert it to a
;; normal function type. See `kw-unconvert` above.
(define (opt-unconvert ft formalss)
  (define (lengthish formals)
    (define lst (syntax->list formals))
    (if lst (length lst) +inf.0))
  (define max-formals (argmax lengthish formalss))
  (define min-formals (argmin lengthish formalss))
  (define-values (raw-argc rest?)
    (syntax-parse max-formals
      [(arg:id ...)
       (values (length (syntax->list #'(arg ...))) #f)]
      [(arg:id ... . rst:id)
       (values (length (syntax->list #'(arg ...))) #t)]))
  (define opt-argc
    (syntax-parse min-formals
      [(arg:id ...)
       (- raw-argc (length (syntax->list #'(arg ...))))]
      ;; if min and max both have rest args, then there cannot
      ;; have been any optional arguments
      [(arg:id ... . rst:id) 0]))
  ;; counted twice since optionals expand to two arguments
  (define argc (+ raw-argc opt-argc))
  (define mand-argc (- argc (* 2 opt-argc)))
  (match ft
    [(Function: arrs)
     (cond [(and (even? (length arrs)) (>= (length arrs) 2))
            (match-define (arr: doms rng _ _ _) (car arrs))
            (define-values (mand-args opt-and-rest-args)
              (split-at doms mand-argc))
            (define rest-type
              (and rest? (last opt-and-rest-args)))
            (define opt-types (take opt-and-rest-args opt-argc))
            (make-Function
             (for/list ([to-take (in-range (add1 (length opt-types)))])
               (make-arr* (append mand-args (take opt-types to-take))
                          rng
                          #:rest rest-type)))]
           [else (int-err "unsupported arrs in keyword function type")])]
    [_ (int-err "unsupported keyword function type")]))

;; partition-kws : (Listof Keyword) -> (values (Listof Keyword) (Listof Keyword))
;; Partition keywords by whether they are mandatory or not
(define (partition-kws kws)
  (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws))

(provide kw-convert kw-unconvert opt-convert opt-unconvert partition-kws
         check-kw-arity)
