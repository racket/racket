#lang racket/base
(require "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/set.rkt"
         "../common/inline.rkt"
         "preserved.rkt"
         "tamper.rkt"
         "datum-map.rkt")

(provide
 (struct-out syntax) ; includes `syntax?`
 syntax-tamper
 empty-syntax
 identifier?
 syntax-identifier?
 
 syntax->datum
 datum->syntax
 
 syntax-map
 non-syntax-map
 
 prop:propagation
 prop:propagation-tamper
 prop:propagation-set-tamper
 propagation-set-tamper?
 propagation-set-tamper-ref
 
 deserialize-syntax
 deserialize-datum->syntax
 current-arm-inspectors)

(struct syntax ([content #:mutable] ; datum and nested syntax objects; mutated for lazy propagation
                scopes  ; scopes that apply at all phases
                shifted-multi-scopes ; scopes with a distinct identity at each phase; maybe a fallback search
                [scope-propagations+tamper #:mutable] ; lazy propagation info and/or tamper state
                mpi-shifts ; chain of module-path-index substitutions
                srcloc  ; source location
                props   ; properties
                inspector) ; inspector for access to protected bindings
  #:authentic
  ;; Custom printer:
  #:property prop:custom-write
  (lambda (s port mode)
    (write-string "#<syntax" port)
    (define srcloc (syntax-srcloc s))
    (when srcloc
      (define srcloc-str (srcloc->string srcloc))
      (when srcloc-str
        (fprintf port ":~a" srcloc-str)))
    (fprintf port " ~.s" (syntax->datum s))
    (write-string ">" port))
  #:property prop:serialize
  (lambda (s ser-push! state)
    (define prop (syntax-scope-propagations+tamper s))
    (define content
      (if (propagation? prop)
          ((propagation-ref prop) s)
          (syntax-content s)))
    (define properties
      (intern-properties
       (syntax-props s)
       (lambda ()
         (for/hasheq ([(k v) (in-hash (syntax-props s))]
                      #:when (preserved-property-value? v))
           (values k (check-value-to-preserve (plain-property-value v) syntax?))))
       state))
    (define tamper
      (serialize-tamper (syntax-tamper s)))
    (define context-triple
      (intern-context-triple (intern-scopes (syntax-scopes s) state)
                             (intern-shifted-multi-scopes (syntax-shifted-multi-scopes s) state)
                             (intern-mpi-shifts (syntax-mpi-shifts s) state)
                             state))
    (define stx-state (get-syntax-context state))
    (cond
      [(or properties tamper)
       (ser-push! 'tag '#:syntax+props)
       (push-syntax-context! state #f)
       (ser-push! content)
       (pop-syntax-context! state)
       (ser-push! 'reference context-triple)
       (ser-push! 'reference (syntax-srcloc s))
       (ser-push! properties)
       (ser-push! tamper)
       (when stx-state (set-syntax-state-all-sharing?! stx-state #f))]
      [else
       ;; We rely on two passes to reach a fixpoint on sharing:
       (define sharing-mode (hash-ref (serialize-state-sharing-syntaxes state) s 'unknown))
       (cond
         [(eq? sharing-mode 'share)
          (ser-push! 'tag '#:datum->syntax)
          (ser-push! (syntax->datum s))]
         [(eq? sharing-mode 'unknown)
          (ser-push! 'tag '#:syntax)
          ;; Communicate to nested syntax objects the info that they might share
          (define this-state (and (no-pair-syntax-in-cdr? content)
                                  (syntax-state #t context-triple (syntax-srcloc s))))
          (push-syntax-context! state this-state)
          ;; Serialize content
          (ser-push! content)
          ;; Check whether we're sharing for all nested syntax objects
          (pop-syntax-context! state)
          (define new-sharing-mode
            (if (and this-state
                     (syntax-state-all-sharing? this-state))
                'share
                'none))
          (hash-set! (serialize-state-sharing-syntaxes state)
                     s
                     ;; If the syntax object has only simple content,
                     ;; it doesn't need any sharing support by itself
                     (if (datum-has-elements? content)
                         new-sharing-mode
                         'none))
          (when (and stx-state (eq? new-sharing-mode 'none))
            (set-syntax-state-all-sharing?! stx-state #f))]
         [else
          (ser-push! 'tag '#:syntax)
          (push-syntax-context! state #f)
          (ser-push! content)
          (pop-syntax-context! state)])
       ;; Finish up
       (ser-push! 'reference context-triple)
       (ser-push! 'reference (syntax-srcloc s))
       (when stx-state
         (unless (and (eq? context-triple (syntax-state-context-triple stx-state))
                      (equal? (syntax-srcloc s) (syntax-state-srcloc stx-state)))
           (set-syntax-state-all-sharing?! stx-state #f)))]))
  #:property prop:reach-scopes
  (lambda (s reach)
    (define prop (syntax-scope-propagations+tamper s))
    (reach (if (propagation? prop)
               ((propagation-ref prop) s)
               (syntax-content s)))
    (reach (syntax-scopes s))
    (reach (syntax-shifted-multi-scopes s))
    (for ([(k v) (in-immutable-hash (syntax-props s))]
          #:when (preserved-property-value? v))
      (reach (plain-property-value v)))
    (reach (syntax-srcloc s))))

;; Property to abstract over handling of propagation for
;; serialization; property value takes a syntax object and
;; returns its content
(define-values (prop:propagation propagation? propagation-ref)
  (make-struct-type-property 'propagation))

;; Property to abstract over extraction of tamper from propagation
(define-values (prop:propagation-tamper propagation-tamper? propagation-tamper-ref)
  (make-struct-type-property 'propagation-tamper))
(define-values (prop:propagation-set-tamper propagation-set-tamper? propagation-set-tamper-ref)
  (make-struct-type-property 'propagation-set-tamper))

(define (syntax-tamper s)
  (define v (syntax-scope-propagations+tamper s))
  (if (tamper? v)
      v
      ((propagation-tamper-ref v) v)))

;; ----------------------------------------

(define empty-scopes (seteq))
(define empty-shifted-multi-scopes (seteq))
(define empty-mpi-shifts null)
(define empty-props #hasheq())

(define empty-syntax
  (syntax #f
          empty-scopes
          empty-shifted-multi-scopes
          #f   ; scope-propogations+tamper (clean)
          empty-mpi-shifts
          #f   ; srcloc
          empty-props
          #f)) ; inspector

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-content s))))

(define (syntax-identifier? s) ; assumes that `s` is syntax
  (symbol? (syntax-content s)))

(define (syntax->datum s)
  (syntax-map s (lambda (tail? x) x) (lambda (s d) d) syntax-content))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f])
  (cond
   [(syntax? s) s]
   [else
    (define (wrap content)
      (syntax content
              (if stx-c
                  (syntax-scopes stx-c)
                  empty-scopes)
              (if stx-c
                  (syntax-shifted-multi-scopes stx-c)
                  empty-shifted-multi-scopes)
              (and stx-c
                   (syntax-tamper stx-c)
                   (tamper-tainted-for-content content))
              (if stx-c
                  (syntax-mpi-shifts stx-c)
                  empty-mpi-shifts)
              (and stx-l (syntax-srcloc stx-l))
              empty-props
              (and stx-c
                   (syntax-inspector stx-c))))
    (define result-s
      (non-syntax-map s
                      (lambda (tail? x) (if tail? x (wrap x)))
                      (lambda (s) s)
                      disallow-cycles))
    (if (and stx-p (not (eq? (syntax-props stx-p) empty-props)))
        (struct-copy syntax result-s
                     [props (syntax-props stx-p)])
        result-s)]))

;; `(syntax-map s f d->s)` walks over `s`:
;; 
;;  * `(f tail? d)` is called to each datum `d`, where `tail?`
;;    indicates that the value is a pair/null in a `cdr` --- so that it
;;    doesn't need to be wrapped for `datum->syntax`, for example
;;
;;  * `(d->s orig-s d)` is called for each syntax object,
;;    and the second argument is result of traversing its datum
;; 
;;  * the `s-e` function extracts content of a syntax object
;;
;; The optional `seen` argument is an `eq?`-based immutable hash table
;; to detect and reject cycles. See `datum-map`.

(define-inline (syntax-map s f d->s s-e [seen #f])
  (let loop ([s s])
    (datum-map s
               f
               (lambda (tail? v)
                 (cond
                  [(syntax? v) (d->s v (loop (s-e v)))]
                  [else (f tail? v)]))
               seen)))

;; `(non-syntax-map s f s->)` is like `(syntax-map s f d->s)`, except that
;;  when a syntax object is found, it is just passed to `s->` --- so there's
;;  no `d->s` or `s-e`, since they would not be called

(define-inline (non-syntax-map s f [s-> (lambda (x) x)] [seen #f])
  (datum-map s
             f
             (lambda (tail? v)
               (cond
                [(syntax? v) (s-> v)]
                [else (f tail? v)]))
             seen))

(define disallow-cycles
  (hasheq 'cycle-fail
          (lambda (s)
            (raise-arguments-error 'datum->syntax
                                   "cannot create syntax from cyclic datum"
                                   "datum" s))))

;; ----------------------------------------

;; When serializing syntax objects, let nested objects know the
;; content of an enclosing syntax object, so sharing is enabled if the
;; nested syntax objects have the same context and source location.
(struct syntax-state ([all-sharing? #:mutable] context-triple srcloc))

;; When sharing syntax information in serialization, we have to be
;; careful not to lose syntax objects that wrap a pair in a `cdr` (and
;; therefore would not be restored by `datum->syntax`).
(define (no-pair-syntax-in-cdr? content)
  (cond
   [(pair? content) (let loop ([content (cdr content)])
                      (cond
                       [(and (syntax? content)
                             (pair? (syntax-content content)))
                        #f]
                       [(pair? content) (loop (cdr content))]
                       [else #t]))]
   [else #t]))

;; ----------------------------------------

;; Called by the deserializer

(define (deserialize-syntax content context-triple srcloc props tamper inspector)
  (syntax content
          (vector*-ref context-triple 0)
          (vector*-ref context-triple 1)
          (deserialize-tamper tamper)
          (vector*-ref context-triple 2)
          srcloc
          (if props
              (for/hasheq ([(k v) (in-immutable-hash props)])
                (values k (preserved-property-value v)))
              empty-props)
          inspector))

(define (deserialize-datum->syntax content context-triple srcloc inspector)
  (define s (deserialize-syntax #f context-triple srcloc #f #f inspector))
  (datum->syntax s content s s))
