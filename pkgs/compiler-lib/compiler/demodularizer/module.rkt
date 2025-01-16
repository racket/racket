#lang racket/base
(require racket/set
         racket/path
         racket/match
         racket/string
         compiler/zo-parse
         syntax/modcode
         racket/linklet
          setup/collects
         (only-in '#%kernel [syntax-deserialize kernel:syntax-deserialize])
         "one-mod.rkt"
         "../private/deserialize.rkt"
         "path-submod.rkt"
         "linklet.rkt"
         "module-path.rkt"
         "run.rkt"
         "syntax.rkt"
         "binding.rkt"
         "log.rkt")

(provide find-modules)

;; A file-based module with its submodules
(struct mod (compiled ; loaded using Racket; useful to query
             zo))     ; loaded via `compiler/zo-parse`; useful to rewrite

(define (find-modules orig-top-path
                      #:includes given-includes 
                      #:excludes given-excludes
                      #:include-submods include-submods
                      #:exclude-submods exclude-submods
                      #:keep-syntax? keep-syntax?)
  (define top-path (simple-form-path orig-top-path))
  (define top-path/submod (path/submod-join top-path '()))

  (define mods (make-hash))                 ; path -> mod
  (define one-mods (make-hash))             ; path+submod -> one-mod
  (define excluded-module-mpis (make-hash)) ; path/submod -> (cons mpi phase)
  (define symbol-module-paths (make-hasheq))

  (define collects-cache (make-hash))

  (define-values (pre-explicitly-included-modules
                  explicitly-included-dirs
                  explicitly-included-collects)
    (if given-includes
        (normalize-modules-and-collects given-includes)
        (values #f #f #f)))
  (define explicitly-included-modules
    (and pre-explicitly-included-modules
         (set-add pre-explicitly-included-modules top-path)))
  (define-values (explicitly-excluded-modules
                  explicitly-excluded-dirs
                  explicitly-excluded-collects)
    (normalize-modules-and-collects given-excludes))

  ;; deserialization of syntax objects is too tedious to re-implement, so
  ;; we access the implementation directly from `#%kernel`
  (define-values (real-deserialize-instance bulk-binding-registry register!
                                            syntax-shift-module-path-index)
    (kernel:syntax-deserialize))

  (define (find-submod compiled submod raise-no-submod #:submod-list? submod-list?)
    (let loop ([compiled compiled] [submod submod])
      (cond
        [(linklet-bundle? compiled)
         (unless (null? submod) (raise-no-submod))
         (if submod-list?
             (values null null)
             compiled)]
        [else
         (cond
           [(null? submod)
            (define ht (linklet-directory->hash compiled))
            (define m-compiled (or (hash-ref ht #f #f)
                                   (raise-no-submod)))
            (if submod-list?
                (let ([ht (linklet-bundle->hash m-compiled)])
                  (values (hash-ref ht 'pre null)
                          (hash-ref ht 'post null)))
                m-compiled)]
           [else
            (loop (or (hash-ref (linklet-directory->hash compiled) (car submod) #f)
                      (raise-no-submod))
                  (cdr submod))])])))

  ;; returns (values min-phase mx-phase)
  (define (find-modules! path/submod rel-mpi exclude? exclude-root)
    (define path (path/submod-path path/submod))
    (define submod (path/submod-submod path/submod))

    (when exclude?
      (unless (hash-ref excluded-module-mpis path/submod #f)
        (hash-set! excluded-module-mpis path/submod (cons rel-mpi 0))))

    (unless (hash-ref mods path #f) 
      (define-values (zo-path kind) (get-module-path path))
      (unless (eq? kind 'zo)
        (error 'demodularize "not available in bytecode form\n  path: ~a" path))
      (define zo (call-with-input-file zo-path zo-parse))
      (define compiled (parameterize ([read-accept-compiled #t]
                                      [current-load-relative-directory
                                       (let-values ([(dir file-name dir?) (split-path path)])
                                         dir)])
                         (call-with-input-file zo-path read)))
      (hash-set! mods path (mod compiled zo)))

    (define (find-transitive decl min-phase max-phase)
      (define reqs (instance-variable-value decl 'requires))

      (for/fold ([min-phase min-phase]
                 [max-phase max-phase]
                 [rev-reqs (hash)]) ; phase-shift -> reverse (list path/submod ...)
                ([phase+reqs (in-list reqs)]
                 #:do [(define req-phase (car phase+reqs))]
                 #:when req-phase
                 [req (in-list (cdr phase+reqs))])
        (define req-path/submod (module-path-index->path/submod req path/submod))
        (define req-path (path/submod-path req-path/submod))
        (when (symbol? req-path) (hash-set! symbol-module-paths req-path #t))
        (define exclude-req?
          ;; Even if this module is excluded, traverse it to get all
          ;; modules that it requires, so that we don't duplicate those
          ;; modules by accessing them directly                         
          (or exclude?
              (symbol? req-path)
              (and explicitly-included-modules
                   (not (or (set-member? explicitly-included-modules req-path)
                            (dir-set-member? explicitly-included-dirs req-path)
                            (collect-set-member? explicitly-included-collects
                                                 (path->collect req-path #:cache collects-cache)))))
              (set-member? explicitly-excluded-modules req-path)
              (dir-set-member? explicitly-excluded-dirs req-path)
              (collect-set-member? explicitly-excluded-collects
                                   (path->collect req-path #:cache collects-cache))))
        (define-values (req-min-phase req-max-phase)
          (if (symbol? req-path)
              (values 0 0)
              (find-modules! req-path/submod (module-path-index-reroot req rel-mpi) exclude-req? (and exclude?
                                                                                                      (or exclude-root
                                                                                                          path/submod)))))
        (values (min min-phase (+ req-phase req-min-phase))
                (max max-phase (+ req-phase req-max-phase))
                (hash-update rev-reqs req-phase
                             (lambda (path/submods) (cons req-path/submod path/submods))
                             null))))

    (define (get-provides decl self-mpi)
      (define orig-provides (instance-variable-value decl 'provides))
      (and orig-provides
           ((hash-count orig-provides) . > . 0)
           (let ([path-mpi (module-path-index-join
                            (path/submod->module-path path/submod)
                            #f)])
             (for/hasheqv ([(phase+space provs) (in-hash orig-provides)])
               (values phase+space
                       (for/hasheq ([(name bind) (in-hash provs)])
                         (values name
                                 (binding-module-path-index-shift bind self-mpi path-mpi))))))))

    (define (report-excluded)
      (log-demodularizer-debug " Exclude ~s" path/submod)
      (when exclude-root
        (log-demodularizer-debug "   via ~s" exclude-root)))

    (define done-m (hash-ref one-mods path/submod #f))

    ;; We might reach a module first as non-excluded and then later as
    ;; excluded, in which case we need to re-traverse dependencies as
    ;; also excluded
    (when (and exclude?
               done-m
               (not (one-mod-excluded? done-m)))
      (report-excluded)
      (hash-set! one-mods path/submod (struct-copy one-mod done-m
                                                   [excluded? #t]))
      (find-transitive (one-mod-decl done-m)
                       (one-mod-min-phase done-m)
                       (one-mod-max-phase done-m)))
                                         
    (unless done-m
      (define m (hash-ref mods path))
      (define compiled (mod-compiled m))
      (define zo (mod-zo m))

      (define (raise-no-submod)
        (error 'demodularize "no such submodule\n  path: ~a\n  submod: ~a"
               path submod))
      (define one-compiled
        (find-submod compiled submod raise-no-submod #:submod-list? #f))
      (define one-zo
        (cond
          [(not zo) #f]
          [(linkl-bundle? zo)
           (unless (null? submod) (raise-no-submod))
           zo]
          [else
           (or (hash-ref (linkl-directory-table zo) submod #f)
               (raise-no-submod))]))

      (define h (linklet-bundle->hash one-compiled))
      (define min-phase (hash-ref h 'min-phase 0))
      (define max-phase (hash-ref h 'max-phase 0))
      (define data-linklet (hash-ref h 'data #f))
      (define decl-linklet (hash-ref h 'decl #f))
      (define stx-data-linklet (and keep-syntax? 
                                    (hash-ref h 'stx-data #f)))
      (unless data-linklet
        (error 'demodularize "could not find module path metadata\n  path: ~a\n  submod: ~a"
               path submod))
      (unless decl-linklet
        (error 'demodularize "could not find module metadata\n  path: ~a\n  submod: ~a"
               path submod))

      (define data-instance (instantiate-linklet data-linklet
                                                 (list deserialize-instance)))
      (define decl (instantiate-linklet decl-linklet
                                        (list deserialize-instance
                                              data-instance)))

      (when keep-syntax?
        (register-provides-for-syntax register! bulk-binding-registry
                                      path/submod
                                      decl
                                      ;; use the real deserializer to get the internal form of provides
                                      (instantiate-linklet decl-linklet
                                                           (list real-deserialize-instance
                                                                 data-instance))))

      (define self-mpi (instance-variable-value decl 'self-mpi))

      ;; Transitive requires
      (define-values (trans-min-phase trans-max-phase rev-reqs)
        (find-transitive decl min-phase max-phase))

      ;; Deserialize syntax objects last, because we may need requires to be registered
      ;; in `bulk-binding-registry`
      (define-values (stx-vec stx-mpi)
        (if keep-syntax?
            (deserialize-syntax real-deserialize-instance stx-data-linklet data-instance
                                bulk-binding-registry
                                syntax-shift-module-path-index
                                path submod self-mpi)
            (values #f #f)))

      (define provides? (equal? top-path path))

      (define provides (or (and provides?
                                (get-provides decl self-mpi))
                           #hasheqv()))

      (define portal-stxes (instance-variable-value decl 'portal-stxes))

      (define phase-uses ; phase-level -> (list (cons path/submod phase-level) ...) for linklet imports
        (for/hasheqv ([(phase-level linkl) (in-hash (linkl-bundle-table one-zo))])
          (values
           phase-level
           (list*
            (cons '#%syntax-literals 0)
            (cons '#%transformer-register 0)
            (for/list ([u (hash-ref (instance-variable-value decl 'phase-to-link-modules)
                                    phase-level
                                    null)])
              (define use-path/submod (module-path-index->path/submod (module-use-module u) path/submod))
              (cons use-path/submod (module-use-phase u)))))))

      (define exports ; phase-level -> ext-name -> int-name
        (for/hasheqv ([(phase linkl) (in-hash (linkl-bundle-table one-zo))]
                      #:when (exact-integer? phase))
          (values phase
                  (for/hasheq ([ext-name (in-list (linklet*-exports linkl))]
                               [int-name (in-list (linklet*-internal-exports linkl))])
                    (values ext-name int-name)))))

      (define-values (pre-submodules post-submodules)
        (find-submod compiled submod void #:submod-list? #t))

      (when exclude? (report-excluded))

      (hash-set! one-mods path/submod (one-mod (hash-count one-mods)
                                               exclude?
                                               rel-mpi
                                               one-zo decl
                                               phase-uses
                                               (for/hasheqv ([(phase rev-path/submods) (in-hash rev-reqs)])
                                                 (values phase (reverse rev-path/submods)))
                                               exports
                                               trans-min-phase trans-max-phase
                                               provides
                                               stx-vec stx-mpi
                                               portal-stxes
                                               pre-submodules
                                               post-submodules)))

    (let ([m (hash-ref one-mods path/submod #f)])
      (values (one-mod-min-phase m)
              (one-mod-max-phase m))))

  (define self-mpi (module-path-index-join #f #f))

  (define-values (reachable-min-phase reachable-max-phase)
    (find-modules! top-path/submod self-mpi #f #f))

  (define submods
    (let ([top-m (hash-ref mods top-path)])
      (define compiled (mod-compiled top-m))
      (let loop ([compiled compiled] [prefix '()] [accum null])
        (cond
          [(linklet-bundle? compiled) accum]
          [else
           (for/fold ([accum accum])
                     ([(k v) (in-hash (linklet-directory->hash compiled))])
             (if (symbol? k)
                 (loop v (cons k prefix)
                       (cons (reverse (cons k prefix)) accum))
                 accum))]))))

  (define (preserve-for-embedding? submod)
    ;; Keep a submodule that is tagged by the existence of a
    ;; `declare-preserve-for-embedding` sub-submodule, and keep that
    ;; tag, too.
    (define rev-submod (reverse submod))
    (or (eq? 'declare-preserve-for-embedding (car rev-submod))
        (member (reverse (cons 'declare-preserve-for-embedding rev-submod))
                submods)))

  (define kept-submods
    (for/list ([submod (in-list submods)]
               #:when (and (or (not include-submods)
                               (member submod include-submods)
                               (preserve-for-embedding? submod))
                           (not (member submod exclude-submods))))
      (find-modules! (path/submod-join top-path submod) (module-path-index-join `(submod "." ,@submod) self-mpi) #f #f)
      submod))

  (when explicitly-included-modules
    (for ([incl (in-set explicitly-included-modules)])
      (unless (hash-ref mods incl #f)
        (error 'demodularize "explicitly included module is not a dependency: ~a" incl))))
  (when explicitly-excluded-modules
    (for ([excl (in-set explicitly-excluded-modules)])
      (unless (hash-ref mods excl #f)
        (error 'demodularize "explicitly excluded module is not a dependency: ~a" excl))))

  (values one-mods
          kept-submods
          (for/hash ([(path/submod mpi+phase) (in-hash excluded-module-mpis)])
            (values path/submod mpi+phase))
          symbol-module-paths))

(define (normalize-modules-and-collects elems)
  (for/fold ([modules (set)]
             [dirs (set)]
             [collects (set)])
            ([elem (in-list elems)])
    (match elem
      [`(module ,path)
       (values (set-add modules (simple-form-path path))
               dirs
               collects)]
      [`(dir ,path)
       (values modules
               (set-add dirs (path->directory-path (simple-form-path path)))
               collects)]
      [`(collect ,collect)
       (values modules
               dirs
               (set-add collects
                        (reverse
                         (map string->bytes/utf-8
                              (string-split collect #rx"/")))))])))

(define (path->collect path #:cache collects-cache)
  (define r (path->collects-relative path #:cache collects-cache))  
  (and (pair? r) (cdr (reverse (cdr r)))))

(define (collect-set-member? collects collect)
  (and collect
       (or (set-member? collects collect)
           (and (pair? (cdr collect))
                (collect-set-member? collects (cdr collect))))))

(define (dir-set-member? dirs path)
  (let-values ([(base name dir?) (split-path path)])
    (and (path? base)
         (or (set-member? dirs base)
             (dir-set-member? dirs base)))))
