#lang racket/base
(require racket/set
         compiler/zo-parse
         syntax/modcode
         racket/linklet
         "../private/deserialize.rkt"
         "module-path.rkt"
         "run.rkt")

(provide find-modules
         current-excluded-modules)

(struct mod (compiled zo))          ; includes submodules; `zo` is #f for excluded
(struct one-mod (compiled zo decl)) ; module without submodules

(define current-excluded-modules (make-parameter (set)))

(define (find-modules orig-path #:submodule [submod '()])
  (define mods (make-hash))      ; path -> mod 
  (define one-mods (make-hash))  ; path+submod -> one-mod
  (define runs-done (make-hash)) ; path+submod+phase -> #t
  (define runs null)             ; list of `run`
  (define excluded-module-mpis (make-hash)) ; path -> mpi

  (define (find-modules! orig-path+submod exclude?)
    (define orig-path (if (pair? orig-path+submod) (car orig-path+submod) orig-path+submod))
    (define submod (if (pair? orig-path+submod) (cdr orig-path+submod) '()))
    (define path (normal-case-path (simplify-path (path->complete-path orig-path))))

    (unless (hash-ref mods path #f) 
      (define-values (zo-path kind) (get-module-path path))
      (unless (eq? kind 'zo)
        (error 'demodularize "not available in bytecode form\n  path: ~a" path))
      (define zo (and (not exclude?)
                      (call-with-input-file zo-path zo-parse)))
      (define compiled (parameterize ([read-accept-compiled #t]
                                      [current-load-relative-directory
                                       (let-values ([(dir file-name dir?) (split-path path)])
                                         dir)])
                         (call-with-input-file zo-path read)))
      (hash-set! mods path (mod compiled zo)))

    (unless (hash-ref one-mods (cons path submod) #f)
      (define m (hash-ref mods path))
      (define compiled (mod-compiled m))
      (define zo (mod-zo m))

      (define (raise-no-submod)
        (error 'demodularize "no such submodule\n  path: ~a\n  submod: ~a"
               path submod))
      (define one-compiled
        (let loop ([compiled compiled] [submod submod])
          (cond
            [(linklet-bundle? compiled)
             (unless (null? submod) (raise-no-submod))
             compiled]
            [else
             (cond
               [(null? submod)
                (or (hash-ref (linklet-directory->hash compiled) #f #f)
                    (raise-no-submod))]
               [else
                (loop (or (hash-ref (linklet-directory->hash compiled) (car submod) #f)
                          (raise-no-submod))
                      (cdr submod))])])))
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
      (define data-linklet (hash-ref h 'data #f))
      (define decl-linklet (hash-ref h 'decl #f))
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

      (hash-set! one-mods (cons path submod) (one-mod one-compiled one-zo decl))

      ;; Transitive requires
      
      (define reqs (instance-variable-value decl 'requires))

      (for ([phase+reqs (in-list reqs)]
            #:when (car phase+reqs)
            [req (in-list (cdr phase+reqs))])
        (define path/submod (module-path-index->path req path submod))
        (define req-path (if (pair? path/submod) (car path/submod) path/submod))
        (unless (symbol? req-path)
          (find-modules! path/submod
                         ;; Even if this module is excluded, traverse it to get all
                         ;; modules that it requires, so that we don't duplicate those
                         ;; modules by accessing them directly
                         (or exclude? (set-member? (current-excluded-modules) req-path)))))))

  (define (find-phase-runs! orig-path+submod orig-mpi #:phase [phase 0])
    (define orig-path (if (pair? orig-path+submod) (car orig-path+submod) orig-path+submod))
    (define submod (if (pair? orig-path+submod) (cdr orig-path+submod) '()))
    (define path (normal-case-path (simplify-path (path->complete-path orig-path))))
    (define path/submod (if (pair? submod) (cons path submod) path))

    (unless (hash-ref runs-done (cons (cons path submod) phase) #f)
      (define one-m (hash-ref one-mods (cons path submod) #f))
      (when (one-mod-zo one-m) ; not excluded
        (define decl (one-mod-decl one-m))

        (define linkl (hash-ref (linkl-bundle-table (one-mod-zo one-m)) phase #f))
        (define uses
          (list*
           ;; The first implicit import might get used for syntax literals;
           ;; recognize it with a 'syntax-literals "phase"
           (cons path/submod 'syntax-literals)
           ;; The second implicit import might get used to register a macro;
           ;; we'll map those registrations to the same implicit import:
           '(#%transformer-register . transformer-register)
           (for/list ([u (hash-ref (instance-variable-value decl 'phase-to-link-modules)
                                   phase
                                   null)])
             (define path/submod (module-path-index->path (module-use-module u) path submod))

             ;; In case the import turns out to stay imported:
             (define req-path (if (pair? path/submod) (car path/submod) path/submod))
             (hash-set! excluded-module-mpis req-path (module-path-index-reroot (module-use-module u) orig-mpi))

             (cons path/submod (module-use-phase u)))))

        (define r (run (if (null? submod) path (cons path submod)) phase linkl uses))
        (hash-set! runs-done (cons (cons path submod) phase) #t)

        (define reqs (instance-variable-value decl 'requires))
        (for* ([phase+reqs (in-list reqs)]
               #:when (car phase+reqs)
               [req (in-list (cdr phase+reqs))])
          (define at-phase (- phase (car phase+reqs)))
          (define path/submod (module-path-index->path req path submod))
          (define full-mpi (module-path-index-reroot req orig-mpi))
          (define req-path (if (pair? path/submod) (car path/submod) path/submod))
          (unless (or (symbol? req-path)
                      (set-member? (current-excluded-modules) req-path))
            (find-phase-runs! path/submod full-mpi #:phase at-phase)))

        ;; Adding after requires, so that `runs` ends up in the
        ;; reverse order that we want to emit code
        (when linkl (set! runs (cons r runs))))))

  (find-modules! (cons orig-path submod) #f)
  (find-phase-runs! (cons orig-path submod) (module-path-index-join #f #f))

  (values (reverse runs)
          excluded-module-mpis))
