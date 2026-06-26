#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         setup/cross-system
         (for-syntax racket/base
                     syntax/parse/pre
                     setup/cross-system))

(provide define-runtime-lib)

(begin-for-syntax
  (define-syntax-class :system-spec
    #:description "platform specification"
    #:attributes (predicate)
    #:datum-literals (windows macosx unix)
    #:literals (and or)
    (pattern (and system::system-spec ...)
             #:attr predicate #'(lambda (os os* arch platform word)
                                  (and (system.predicate os os* arch platform word)
                                       ...)))
    (pattern (or system::system-spec ...)
             #:attr predicate #'(lambda (os os* arch platform word)
                                  (or (system.predicate os os* arch platform word)
                                      ...)))
    (pattern (~and (~or windows macosx unix) sys) ; ad hoc optimization
             #:attr predicate #'(lambda (os os* arch platform word) (eq? os 'sys)))
    (pattern name:id
             #:when (not (free-identifier=? #'name #'else))
             #:attr predicate #'(lambda (os os* arch platform word)
                                  (or (eq? 'name os)
                                      (eq? 'name os*)
                                      (eq? 'name arch))))
    (pattern platform-str:string
             #:attr predicate #'(lambda (os os* arch platform word)
                                  (equal? platform 'platform-str)))
    (pattern (~and (~or 32 64) size)
             #:attr predicate #'(lambda (os os* arch platform word) (eqv? word 'size))))

  (define-syntax-class :lib-spec
    #:description "library specification using `so`"
    #:attributes (quoted)
    #:datum-literals (so)
    (pattern (so lib-str:string)
             #:attr quoted #'(so lib-str))
    (pattern (so lib-str:string vers:string)
             #:attr quoted #'(so lib-str vers))
    (pattern (so lib-str:string (~and vers ((~or _:string #f) ...)))
             #:attr quoted #'(so lib-str vers))))

(define-syntax (define-runtime-lib stx)
  (syntax-parse stx
    #:literals (else)
    #:datum-literals (unix)
    [(_ lib-id:id
        (~optional (~seq #:ffi-lib-args (~and ((~seq _:keyword _:expr) ...)
                                              (ffi-lib-arg ...)))
                   #:defaults ([(ffi-lib-arg 1) '()]))
        [system::system-spec
         ~!
         system-lib::lib-spec
         ...]
        ...
        (~optional
         [else
          else-lib-expr
          ...+]))
     (unless (attribute else-lib-expr)
       (raise-syntax-error #f "missing an `else` case" stx))
     #'(begin
         (define-runtime-path-list libs
           #:runtime?-id runtime?
           (let* ([os (if runtime? (system-type) (cross-system-type))]
                  [os* (if runtime? (system-type 'os*) (cross-system-type 'os*))]
                  [arch (if runtime? (system-type 'arch) (cross-system-type 'arch))]
                  [word (if runtime? (system-type 'word) (cross-system-type 'word))]
                  [platform (if runtime? (system-type 'platform) (cross-system-type 'platform))])
             (cond
               [(system.predicate os os* arch platform word)
                '(system-lib.quoted ...)]
               ...
               [else null])))
         (define lib-id
           (if (null? libs)
               (let ()
                 (begin
                   else-lib-expr
                   ...))
               (for/fold ([v #f]) ([lib (in-list libs)])
                 (ffi-lib lib ffi-lib-arg ...)))))]))
