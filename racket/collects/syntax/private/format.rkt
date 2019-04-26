#lang racket/base

(require racket/list
         syntax/srcloc)

(provide ~id ~id/1 ~symbol)

;; ---------------------------------------------------------------------------------------------------

(define (~id #:context [context #f]
             #:source [source context]
             #:props [props 'infer]
             #:track [track #t]
             #:binder? [binder? #t]
             . pieces)
  ; Convert all the pieces to strings; hold onto the identifiers for attaching sub-range-binders.
  (define-values [symbol id-pieces] (pieces->symbol pieces #:keep-id-pieces? track))

  ; Build the new identifier.
  (define new-id (datum->syntax context
                                symbol
                                (build-source-location-vector source)
                                (if (eq? props 'infer)
                                    ; If we’re attaching 'sub-range-binders, then we don’t want to
                                    ; copy properties, since 'sub-range-binders doesn’t care about
                                    ; originalness, and in fact it will probably do more harm than
                                    ; good. But if context is provided and we’re not attaching
                                    ; 'sub-range-binders, then we probably want to copy originalness,
                                    ; after all.
                                    (if track #f context)
                                    props)))
  (cond
    ; Attach 'sub-range-binders if relevant.
    [(and track (not (empty? id-pieces)))
     (define track-introduce (and track (if (procedure? track)
                                            track
                                            (if (syntax-transforming?)
                                                syntax-local-introduce
                                                values))))
     (define new-id-introduced (track-introduce new-id))

     ; Builds a 'sub-range-binders leaf from a given input identifier.
     (define (build-prop-leaf piece)
       (define old-id (track-introduce (id-piece-id piece)))
       (define old-id-length (id-piece-length piece))
       (define new-id-start (id-piece-position piece))

       ; If the identifier already has a 'sub-range-binders property on it, then in all likelihood
       ; that means it was just created in the dynamic extent of the same macro transformer that
       ; added it in the first place. This means that the identifier may be being built in several
       ; steps, using multiple calls to ~id in sequence. Therefore, we want to copy over old
       ; 'sub-range-binders values that are already present if one side of the binding arrow points
       ; to the old identifier.
       ;
       ; Conservatively, we use bound-identifier=? to check if the identifier is, in fact, the
       ; “same”, since if we’re really in the same macro transformer, it’s unlikely other scopes
       ; have been added in between, anyway.
       (define (relocate-old-leaf val)
         (define-values [binder-id binder-id-start binder-id-range binder-id-x binder-id-y
                                   use-id use-id-start use-id-range use-id-x use-id-y]
           (vector->values val))
         (cond
           [(bound-identifier=? binder-id old-id)
            (vector-immutable new-id-introduced (+ binder-id-start new-id-start) binder-id-range
                              binder-id-x binder-id-y
                              use-id use-id-start use-id-range use-id-x use-id-y)]
           [(bound-identifier=? use-id old-id)
            (vector-immutable binder-id binder-id-start binder-id-range binder-id-x binder-id-y
                              new-id-introduced (+ use-id-start new-id-start) use-id-range
                              use-id-x use-id-y)]
           [else #f]))
       (define old-leaves (jumble->list (syntax-property old-id 'sub-range-binders)
                                        sub-range-binder-leaf?))
       (define relocated-old-leaves (filter-map relocate-old-leaf old-leaves))

       (define new-value
         (if binder?
             (vector-immutable new-id-introduced new-id-start old-id-length 0.5 0.5
                               old-id 0 old-id-length 0.5 0.5)
             (vector-immutable old-id 0 old-id-length 0.5 0.5
                               new-id-introduced new-id-start old-id-length 0.5 0.5)))
       (if (empty? relocated-old-leaves) new-value (cons new-value relocated-old-leaves)))

     (syntax-property new-id 'sub-range-binders (map build-prop-leaf id-pieces))]

    [else
     new-id]))

(define (~id/1 #:context [context 'infer]
               #:source [source 'infer]
               #:props [props 'infer]
               #:track [track #t]
               #:binder? [binder? #t]
               . pieces)
  (define the-id (first (filter identifier? pieces)))
  (define (infer-or x) (if (eq? x 'infer) the-id x))
  (apply ~id pieces
         #:context (infer-or context)
         #:source (infer-or source)
         #:props props
         #:track track
         #:binder? binder?))

(define (~symbol . pieces)
  (define-values [symbol id-pieces] (pieces->symbol pieces #:keep-id-pieces? #f))
  symbol)

;; ---------------------------------------------------------------------------------------------------

(struct id-piece (id length position) #:transparent)

(define (pieces->symbol pieces #:keep-id-pieces? keep-id-pieces?)
  (for/fold ([string-pieces '()]
             [position 0]
             [id-pieces (and keep-id-pieces? '())]
             #:result (values (string->symbol (apply string-append (reverse string-pieces)))
                              (and keep-id-pieces? (reverse id-pieces))))
            ([piece (in-list pieces)])
    (define string-piece (cond
                           [(string? piece)
                            piece]
                           [(identifier? piece)
                            (symbol->string (syntax-e piece))]
                           [(symbol? piece)
                            (symbol->string piece)]
                           [(char? piece)
                            (string piece)]
                           [(number? piece)
                            (number->string piece)]
                           [(keyword? piece)
                            (keyword->string piece)]))
    (define piece-length (string-length string-piece))
    (values (cons string-piece string-pieces)
            (+ position piece-length)
            (if (and keep-id-pieces? (identifier? piece))
                (cons (id-piece piece piece-length position) id-pieces)
                id-pieces))))

;; ---------------------------------------------------------------------------------------------------

(define (jumble->list v leaf?)
  (let recur ([v v])
    (cond
      [(leaf? v) (list v)]
      [(list? v) (append-map recur v)]
      [(pair? v) (append (recur (car v)) (recur (cdr v)))]
      [else '()])))

(define (sub-range-binder-leaf? v)
  (define (offset? n)
    (and (real? n) (<= 0 n 1)))
  (define (id-spec? a b c d e)
    (and (identifier? a)
         (exact-nonnegative-integer? b)
         (exact-nonnegative-integer? c)
         (offset? d)
         (offset? e)))
  (and (vector? v)
       (= (vector-length v) 10)
       (call-with-values (λ () (vector->values v 0 5)) id-spec?)
       (call-with-values (λ () (vector->values v 5 10)) id-spec?)))

(module+ for-test (provide sub-range-binder-leaf?))
