#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in '#%foreign
                  ffi-lib?
                  [ffi2-lib-ref ffi2-lib-ref*])
         racket/symbol
         setup/dirs
         ffi/unsafe/private/ffi-lib)

(provide (protect-out
          ffi2-lib
          ffi2-lib?
          ffi2-lib-ref))

(define (ffi2-lib name [version/s ""]
                  #:fail [fail #f]
                  #:get-lib-dirs [get-lib-dirs get-lib-search-dirs]
                  #:global? [global? (eq? (system-type 'so-mode) 'global)]
                  #:custodian [custodian #f])
  (get-ffi-lib* name version/s
                #:who 'ffi2-lib
                #:fail fail
                #:get-lib-dirs get-lib-dirs
                #:global? global?
                #:custodian custodian))

(define (ffi2-lib? v)
  (ffi-lib? v))

(define (ffi2-lib-ref lib name-in #:fail [failure #f])
  (define who 'ffi2-lib-ref)
  (unless (or (not lib) (ffi-lib? lib)) (raise-argument-error who "(or/c ffi2-lib? #f)" lib))
  (when (and failure (not (and (procedure? failure) (procedure-arity-includes? failure 0))))
    (raise-argument-error who "(procedure-arity-includes/c 0)" failure))
  (cond
    [(not lib)
     (if failure
         (failure)
         (raise-arguments-error who
                                "cannot find exported name due to missing library"
                                "library" lib
                                "name to find" name-in))]
    [else
     (define name
       (cond
         [(bytes? name-in) name-in]
         [(string? name-in) (string->bytes/utf-8 name-in)]
         [(symbol? name-in) (string->bytes/utf-8 (symbol->immutable-string name-in))]
         [else (raise-argument-error who "(or/c bytes? string? symbol?)" name-in)]))
     (if failure
         (with-handlers ([exn:fail:filesystem? (lambda (e) (failure))])
           (ffi2-lib-ref* lib name))
         (ffi2-lib-ref* lib name))]))
