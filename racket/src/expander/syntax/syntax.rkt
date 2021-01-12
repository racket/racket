#lang racket/base
(require racket/private/place-local
         racket/fixnum
         (only-in racket/unsafe/ops unsafe-struct*-cas!)
         "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/set.rkt"
         "../common/inline.rkt"
         "../namespace/inspector.rkt"
         "preserved.rkt"
         "tamper.rkt"
         "datum-map.rkt"
         "weaker-inspector.rkt")

(provide
 (struct-out syntax) ; includes `syntax?`
 syntax-content
 syntax-tamper
 empty-syntax
 identifier?
 syntax-identifier?

 (struct-out modified-content)
 re-modify-content
 syntax-content*-cas!
 
 syntax->datum
 datum->syntax

 immediate-datum->syntax
 empty-props

 syntax-map
 non-syntax-map
 
 prop:propagation
 prop:propagation-tamper
 prop:propagation-set-tamper
 propagation-set-tamper?
 propagation-set-tamper-ref
 
 deserialize-syntax
 deserialize-datum->syntax
 current-arm-inspectors

 syntax-place-init!)

;; Used for content wrapped with scope propagations and/or a tamper,
;; so a `content*` is either a `modified-content` or plain content
(struct modified-content (content scope-propagations+tamper)
  #:authentic)

(struct syntax ([content* #:mutable] ; datum and nested syntax objects; mutated for lazy propagation
                scopes  ; scopes that apply at all phases
                shifted-multi-scopes ; scopes with a distinct identity at each phase; maybe a fallback search
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
    (define content* (syntax-content* s))
    (define content
      (if (modified-content? content*)
          (let ([prop (modified-content-scope-propagations+tamper content*)])
            (if (propagation? prop)
                ((propagation-ref prop) s)
                (modified-content-content content*)))
          content*))
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
    (define content* (syntax-content* s))
    (reach
     (if (modified-content? content*)
         (let ([prop (modified-content-scope-propagations+tamper content*)])
           (if (propagation? prop)
               ((propagation-ref prop) s)
               (modified-content-content content*)))
         content*))
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

(define (syntax-content s)
  (define content* (syntax-content* s))
  (if (modified-content? content*)
      (modified-content-content content*)
      content*))
  
(define (syntax-tamper s)
  (define content* (syntax-content* s))
  (cond
    [(modified-content? content*)
     (define v (modified-content-scope-propagations+tamper content*))
     (if (tamper? v)
         v
         ((propagation-tamper-ref v) v))]
    [else #f]))

(define (syntax-content*-cas! stx old new)
  (unsafe-struct*-cas! stx 0 old new))

(define (re-modify-content s d)
  (define content* (syntax-content* s))
  (if (modified-content? content*)
      (modified-content d (modified-content-scope-propagations+tamper content*))
      d))

;; ----------------------------------------

(define empty-scopes (seteq))
(define empty-shifted-multi-scopes (seteq))
(define empty-mpi-shifts null)
(define empty-props #hasheq())

(define empty-syntax
  (syntax #f
          empty-scopes
          empty-shifted-multi-scopes
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

(define-place-local known-syntax-pairs (make-weak-hasheq))

(define (immediate-datum->syntax stx-c content stx-l props insp)
  (syntax (if (and stx-c
                   (syntax-tamper stx-c))
              (modified-content content
                                (tamper-tainted-for-content content))
              content)
          (if stx-c
              (syntax-scopes stx-c)
              empty-scopes)
          (if stx-c
              (syntax-shifted-multi-scopes stx-c)
              empty-shifted-multi-scopes)
          (if stx-c
              (syntax-mpi-shifts stx-c)
              empty-mpi-shifts)
          (and stx-l (syntax-srcloc stx-l))
          props
          (and insp
               stx-c
               (weaker-inspector insp (syntax-inspector stx-c)))))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f])
  (cond
   [(syntax? s) s]
   [else
    (define insp (if (syntax? s) 'not-needed (current-module-code-inspector)))
    (define (wrap content)
      (let ([content (datum-intern-literal content)])
        (immediate-datum->syntax stx-c content stx-l empty-props insp)))
    (define result-s
      (non-syntax-map s
                      (lambda (tail? x) (cond
                                          [tail?
                                           (when (and (fx> tail? 32)
                                                      (fx= 0 (fxand tail? (fx- tail? 1))))
                                             (hash-set! known-syntax-pairs x #t))
                                           x]
                                          [else (wrap x)]))
                      (lambda (s) s)
                      disallow-cycles
                      known-syntax-pairs))
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

(define-inline (non-syntax-map s f [s-> (lambda (x) x)] [seen #f] [known-pairs #f])
  (datum-map s
             f
             (lambda (tail? v)
               (cond
                [(syntax? v) (s-> v)]
                [else (f tail? v)]))
             seen
             known-pairs))

(define disallow-cycles
  (hasheq 'cycle-fail
          (lambda (s)
            (raise-arguments-error 'datum->syntax
                                   "cannot create syntax from cyclic datum"
                                   "datum" s))))

(define (syntax-place-init!)
  (set! known-syntax-pairs (make-weak-hasheq)))

;; ----------------------------------------

;; When serializing syntax objects, let nested objects know the
;; content of an enclosing syntax object, so sharing is enabled if the
;; nested syntax objects have the same context and source location.
(struct syntax-state ([all-sharing? #:mutable] context-triple srcloc)
  #:authentic)

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
  (syntax (let ([t (deserialize-tamper tamper)])
            (if t
                (modified-content content t)
                content))
          (vector*-ref context-triple 0)
          (vector*-ref context-triple 1)
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
