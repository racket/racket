(define correlated->annotation
  (case-lambda
   [(v serializable? sfd-cache)
    (let-values ([(e stripped-e) (correlated->annotation* v serializable? sfd-cache)])
      e)]
   [(v) (correlated->annotation v #f (get-nonserializable-sfd-cache))]))

(define (correlated->annotation* v serializable? sfd-cache)
  (cond
   [(pair? v) (let-values ([(a stripped-a) (correlated->annotation* (car v) serializable? sfd-cache)]
                           [(d stripped-d) (correlated->annotation* (cdr v) serializable? sfd-cache)])
                (cond
                 [(and (eq? a (car v))
                       (eq? d (cdr v)))
                  (values v v)]
                 [else (values (cons a d)
                               (cons stripped-a stripped-d))]))]
   [(correlated? v) (let-values ([(e stripped-e) (correlated->annotation* (correlated-e v) serializable? sfd-cache)])
                      (let ([name (correlated-property v 'inferred-name)]
                            [method-arity-error (correlated-property v 'method-arity-error)])
                        (define (add-name e)
                          (if (and name (not (void? name)))
                              `(|#%name| ,name ,e)
                              e))
                        (define (add-method-arity-error e)
                          (if method-arity-error
                              `(|#%method-arity| ,e)
                              e))
                        (values (add-method-arity-error
                                 (add-name (transfer-srcloc v e stripped-e serializable? sfd-cache)))
                                (add-method-arity-error
                                 (add-name stripped-e)))))]
   ;; correlated will be nested only in pairs with current expander
   [else (values v v)]))

(define (extract-inferred-name expr default-name)
  (let ([name (and (correlated? expr)
                   (correlated-property expr 'inferred-name))])
    (cond
     [(void? name) #f]
     [(correlated? name) (correlated-e name)]
     [(symbol? name) name]
     [else default-name])))

(define (transfer-srcloc v e stripped-e serializable? sfd-cache)
  (let ([src (correlated-source v)]
        [pos (correlated-position v)]
        [line (correlated-line v)]
        [column (correlated-column v)]
        [span (correlated-span v)])
    (if (and pos span (or (path? src) (string? src) (symbol? src)))
        (let ([pos (sub1 pos)]) ; Racket positions are 1-based; host Scheme positions are 0-based
          (make-annotation e
                           (if (and line column)
                               ;; Racket columns are 0-based; host-Scheme columns are 1-based
                               (make-source-object (source->sfd src serializable? sfd-cache) pos (+ pos span) line (add1 column))
                               (make-source-object (source->sfd src serializable? sfd-cache) pos (+ pos span)))
                           stripped-e))
        e)))

(define sfd-cache-box (unsafe-make-place-local #f))

(define (get-nonserializable-sfd-cache)
  (or (unsafe-place-local-ref sfd-cache-box)
      ;; There's a race here at the level of Racket threads,
      ;; but that seems ok for setting up a cache
      (let ([cache (make-weak-hash)])
        (unsafe-place-local-set! sfd-cache-box cache)
        ;; Use this cache only with interrupts disabled, otherwise
        ;; it's not kill-safe
        cache)))

(define (source->sfd src serializable? sfd-cache)
  (or (with-interrupts-disabled
       (hash-ref sfd-cache src #f))
      ;; We'll use a file-position object in source objects, so
      ;; the sfd checksum doesn't matter
      (let ([sfd (source-file-descriptor
                  ;; Wrap path as a srcloc so that absolute paths are just
                  ;; dropped when serializing the path (while paths relative
                  ;; to the containing source can be preserved):
                  (if (path? src)
                      (srcloc src #f #f #f #f)
                      src)
                  0)])
        (with-interrupts-disabled
         (hash-set! sfd-cache src sfd))
        sfd)))

;; --------------------------------------------------

(define (strip-nested-annotations s)
  (cond
   [(annotation? s) (annotation-stripped s)]
   [(pair? s)
    (let ([a (strip-nested-annotations (car s))]
          [d (strip-nested-annotations (cdr s))])
      (if (and (eq? a (car s)) (eq? d (cdr s)))
          s
          (cons a d)))]
   [else s]))

;; --------------------------------------------------

;; A correlated might have a srcloc that has a source that cannot be
;; marshaled; handle that at the last minute by discarding the source
(define (fixup-correlated-srclocs v)
  (cond
    [(srcloc? v)
     (srcloc #f
             (srcloc-line v)
             (srcloc-column v)
             (srcloc-position v)
             (srcloc-span v))]
    [else
     (raise-arguments-error 'fixup-correlated-srclocs
                            "cannot fixup value"
                            "value" v)]))
