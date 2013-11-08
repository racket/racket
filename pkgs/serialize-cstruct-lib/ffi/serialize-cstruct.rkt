#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/serialize
                     syntax/parse
                     syntax/stx)
         racket/promise
         racket/serialize
         racket/runtime-path
         ffi/unsafe
         (only-in '#%foreign ctype-c->scheme))

(provide (rename-out [prop:serialize-cstruct? serializable-cstruct?])
         define-serializable-cstruct)


(define-values (prop:serialize-cstruct prop:serialize-cstruct? prop:serialize-cstruct-ref)
  (make-struct-type-property 'serialize-cstruct))


(define cpointer-mapping (make-weak-hash))


(define-syntax (define-serializable-cstruct stx)
  (syntax-parse stx
    [(_ _ID:id ([field-id:id type-expr:expr] ...)
        (~or (~optional (~seq #:malloc-mode malloc-mode:expr)
                        #:name "#:malloc-mode" #:defaults ([malloc-mode #'(quote atomic)]))
             (~optional (~seq (~and #:serialize-inplace serialize-inplace-kw))
                        #:name "#:serialize-inplace")
             (~optional (~seq (~and #:deserialize-inplace deserialize-inplace-kw))
                        #:name "#:deserialize-inplace")
             (~optional (~seq #:alignment align-expr:expr)
                        #:name "#:alignment")
             (~seq #:property prop-expr:expr propval-expr:expr))
        ...)

     (unless (eq? (syntax-local-context) 'module)
       (raise-syntax-error #f "only allowed in module context" stx))
     (unless (stx-pair? #'(type-expr ...))
       (raise-syntax-error #f "expected [field-id ctype] ..." stx))

     (define id (string->symbol (cadr (or (regexp-match #rx"^_(.*)$" (symbol->string (syntax-e #'_ID)))
                                          (raise-syntax-error #f "id must start with '_'" stx #'_ID)))))

     (with-syntax ([_ID-pointer (format-id #'_ID "~a-pointer" #'_ID)]

                   [(align ...)
                    (if (attribute align-expr)
                        (list '#:alignment (attribute align-expr))
                        null)]
                   [((props ...) ...)
                    (map (lambda (p v)
                           (when (free-identifier=? #'prop:serializable p)
                             (raise-syntax-error #f "#:property prop:serializable not allowed" stx p))
                           (list '#:property p v))
                         (attribute prop-expr) (attribute propval-expr))]

                   [(acc-list ...) (stx-map (lambda (e) (format-id #'_ID "~a-~a" id e))
                                            #'(field-id ...))]
                   [(mod-list ...) (stx-map (lambda (e) (format-id #'_ID "set-~a-~a!" id e))
                                            #'(field-id ...))]

                   [deser-ID (format-id #'_ID "deserialize:cstruct:~a" id)]
                   [make-ID/mode (format-id #'_ID "make-~a/mode" id)]

                   [serialize-inplace (and (attribute serialize-inplace-kw) #t)]
                   [deserialize-inplace (and (attribute deserialize-inplace-kw) #t)])

       (quasisyntax/loc stx
         (begin
           ;; the wrapped cstruct
           (define-cstruct _ID ([field-id type-expr] ...)
             align ...
             props ... ...
             #:property prop:serializable
             (make-serialize-info
              (lambda (s)
                (force check-all-serializable)
                (hash-set! cpointer-mapping s s)
                (define inplace-bs (make-sized-byte-string s (ctype-sizeof _ID)))
                (define bs
                  (if serialize-inplace
                      inplace-bs
                      (let ([mem (malloc _ID 'atomic)])
                        (memcpy mem inplace-bs 1 _ID)
                        (make-sized-byte-string mem (ctype-sizeof _ID)))))
                (vector bs (serialize-cstruct-pointers s)))
              (quote-syntax deser-ID)
              #t
              (or (current-load-relative-directory) (current-directory)))
             #:property prop:serialize-cstruct
             (lambda () (values _ID (list acc-list ...) (list mod-list ...))))

           ;; malloc according to malloc-mode
           (define (malloc-ID)
             (if (procedure? malloc-mode)
                 (malloc-mode (ctype-sizeof _ID))
                 (malloc _ID malloc-mode)))

           ;; deserialization proc
           #,@(if (eq? (syntax-local-context) 'module)
                  #`((runtime-require (submod "." deserialize-info))
                     (module+ deserialize-info (provide deser-ID)))
                  null)
           (define deser-ID (id->deserialize-info _ID _ID-pointer deserialize-inplace malloc-ID))

           ;; mode-aware make-ID
           (define (make-ID/mode field-id ...)
             (define s (ptr-ref (malloc-ID) _ID))
             (mod-list s field-id) ...
             s)

           ;; ctype serializable check (must be delayed to handle cyclic structs)
           (define check-all-serializable
             (delay
               (let ([ctypes (list type-expr ...)])
                 (for ([ct (in-list ctypes)]
                       [t (in-list (ctype->layout _ID))]
                       [n '(field-id ...)])
                   (define base (ctype-layout-base-type t))

                   ;; fpointer never possible
                   (when (eq? base 'fpointer)
                     (error 'serialize-cstruct "~a::~a of type ~a is not serializable" '_ID n t))

                   ;; struct (ptr, embedded), maybe in array
                   (when (or (list? base)
                             (eq? base 'pointer))

                     (define c->s (ctype-c->scheme (array-base-type ct)))
                     (unless (and c->s (prop:serialize-cstruct? (c->s (malloc _pointer))))
                       (error 'serialize-cstruct "~a::~a of type ~a is not serializable" '_ID n t)))))))  )))]))


(define (array-base-type ct)
  (if (vector? (ctype->layout ct))
      (array-base-type (array-type ((ctype-c->scheme ct) #f)))
      ct))


(define (ctype-layout-base-type v)
  (if (vector? v)
      (ctype-layout-base-type (vector-ref v 0))
      v))


(define (id->deserialize-info _ID _ID-pointer deserialize-inplace malloc-ID)
  (make-deserialize-info
   (lambda (bs ptrs)
     (define s
       (if deserialize-inplace
           (cast bs _bytes _ID-pointer)
           (let ([mem (malloc-ID)])
             (memcpy mem bs 1 _ID)
             (cast mem _pointer _ID-pointer))))
     (deserialize-cstruct-pointers s ptrs)
     s)

   (lambda ()
     (define s (malloc-ID))
     (values (cast s _pointer _ID-pointer)
             (lambda (s0)
               (memcpy s s0 1 _ID))))))


(define ptr-types '(bytes string/ucs-4 string/utf-16 pointer))


(define (serialize-cstruct-pointers o)
  (define who 'serialize-cstruct-pointers)
  (unless (prop:serialize-cstruct? o)
    (raise-argument-error who "serializable-cstruct?" o))

  (define-values (_ID accs mods) ((prop:serialize-cstruct-ref o)))

  (define (serialize-basic t o)
    (cond
     [(list? t)
      (and o (serialize-cstruct-pointers o))]

     [(memq t ptr-types)
      (unless (serializable? o)
        (raise-argument-error who "serializable?" o))
      (hash-ref! cpointer-mapping o o)]))

  (for/vector ([t (in-list (ctype->layout _ID))]
               [acc (in-list accs)]
               #:when (let ([base (ctype-layout-base-type t)])
                        (when (eq? base 'fpointer)
                          (raise-argument-error who "serializable?" 'fpointer))
                        (or (list? base)
                            (memq base ptr-types))))

    (define base (ctype-layout-base-type t))

    (cond
     [(vector? t)
      (let loop ([ar (acc o)])
        (define len (array-length ar))
        (for/vector #:length len ([i (in-range len)])
          (define v (array-ref ar i))
          (if (array? v)
              (loop v)
              (serialize-basic base v))))]

     [else
      (serialize-basic t (acc o))])))


(define (deserialize-cstruct-pointers o ptrs)
  (unless (prop:serialize-cstruct? o)
    (raise-argument-error 'deserialize-cstruct-pointers "serializable-cstruct?" o))

  (define-values (_ID accs mods) ((prop:serialize-cstruct-ref o)))

  (for ([acc (in-list accs)]
        [mod (in-list mods)]
        [t (in-list (ctype->layout _ID))]
        [p (in-vector ptrs)])

    (define base (ctype-layout-base-type t))
    (cond
     [(and (vector? t)
           (or (memq base ptr-types)
               (list? base)))

      (let loop ([ar (acc o)]
                 [pvec p]
                 [sub-t t])
        (define len (array-length ar))
        (if (vector? (vector-ref sub-t 0))
            (for ([i (in-range len)]
                  [pv (in-vector pvec)])
              (loop (array-ref ar i) pv (vector-ref sub-t 0)))
            (for ([i (in-range len)]
                  [pv (in-vector pvec)])
              (if (list? base)
                  (deserialize-cstruct-pointers (array-ref ar i) pv)
                  (array-set! ar i pv)))))]


     [(list? t)
      (deserialize-cstruct-pointers (acc o) p)]

     [(and p (memq t ptr-types))
      (mod o p)])))
