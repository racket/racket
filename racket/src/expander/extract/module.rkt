#lang racket/base
(require "../host/linklet.rkt"
         "../run/cache.rkt"
         "../compile/serialize.rkt"
         "../compile/module-use.rkt"
         "../compile/linklet.rkt"
         (prefix-in new: "../common/module-path.rkt"))

(provide (struct-out compiled-module)
         get-compiled-module
         module-path-index->module-name)

;; We locate each module's declation and phase-specific
;; linklets once:
(struct compiled-module (declaration        ; linklet instance
                         phase-to-linklet)) ; phase -> linklet


;; Get (possibly already-loaded) representation of a compiled module
;; from the cache
(define (get-compiled-module name root-name
                             #:compiled-modules compiled-modules
                             #:cache cache)
  (or (hash-ref compiled-modules name #f)
      (let ([local-name name])
        ;: Seeing this module for the first time
        (define cd (get-cached-compiled cache root-name void))
        (unless cd
          (error "unavailable in cache:" name))
        ;; For submodules, recur into the compilation directory:
        (define h (let loop ([cd cd] [name name])
                    (cond
                     [(linklet-bundle? cd)
                      (linklet-bundle->hash cd)]
                     [else
                      (define h (linklet-directory->hash cd))
                      (if (or (not (pair? name))
                              (null? (cdr name)))
                          (linklet-bundle->hash (hash-ref h #f))
                          (loop (hash-ref h (cadr name))
                                (cdr name)))])))
        ;; Instantiate the declaration linklet
        (define data-instance (instantiate-linklet (hash-ref h 'data)
                                                   (list deserialize-instance)))
        (define decl (instantiate-linklet (hash-ref h 'decl)
                                          (list deserialize-instance
                                                data-instance)))
        ;; Make a `compiled-module` structure to represent the compiled module
        ;; and all its linklets (but not its submodules, although they're in `h`)
        (define comp-mod (compiled-module decl h))
        (hash-set! compiled-modules name comp-mod)
        comp-mod)))

;; Convert a module path index implemented by our compiler to
;; a module path index in the host Racket:
(define (build-native-module-path-index mpi wrt-name)
  (define-values (mod-path base) (new:module-path-index-split mpi))
  (cond
   [(not mod-path) (make-resolved-module-path wrt-name)]
   [else
    (module-path-index-join mod-path
                            (and base
                                 (build-native-module-path-index base wrt-name)))]))

;; Convert one of our module path indexes and a name to
;; the referenced name
(define (module-path-index->module-name mod name)
  (define p (build-native-module-path-index mod name))
  (resolved-module-path-name
   (if (resolved-module-path? p)
       p
       (module-path-index-resolve p))))

