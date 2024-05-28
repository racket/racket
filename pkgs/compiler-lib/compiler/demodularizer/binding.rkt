#lang racket/base
(require racket/match
         "../private/deserialize.rkt")

(provide binding-module-path-index-shift
         binding-mpis
         serialize-binding)

(define (binding-module-path-index-shift bind from-mpi to-mpi)
  (cond
    [(provided? bind) (struct-copy provided bind
                                   [binding (binding-module-path-index-shift (provided-binding bind)
                                                                             from-mpi
                                                                             to-mpi)])]
    [else
     (define (shift mpi)
       (cond
         [(eq? mpi from-mpi) to-mpi]
         [else
          (define-values (name base) (module-path-index-split mpi))
          (define new-base (and base (shift base)))
          (if (eq? new-base base)
              mpi
              (module-path-index-join name new-base))]))
     (define content (binding-content bind))
     (define new-content
       (match content
         [`(,mod ,sym ,phase ,nom-mod) (list (shift mod) sym phase (shift nom-mod))]
         [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
          ;; Currently dropping free-id=? and extra nominals
          (list (shift mod) sym phase (shift nom-mod) nom-phase nom-sym req-phase #f insp null)]))
     (struct-copy binding bind
                  [content new-content])]))

(define (binding-mpis bind)
  (cond
    [(provided? bind) (binding-mpis (provided-binding bind))]
    [else
     (match (binding-content bind)
       [`(,mod ,sym ,phase ,nom-mod) (list mod nom-mod)]
       [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
        (list mod nom-mod)])]))

(define (serialize-binding bind external-path-pos excluded-module-mpis names)
  (let loop ([bind bind])
    (cond
      [(provided? bind)
       `(#:provided
         ,@(loop (provided-binding bind))
         ,(provided-protected? bind)
         ,(provided-syntax? bind))]
      [else
       (define (lookup mpi)
         (define r (module-path-index-resolve mpi))
         (or (hash-ref external-path-pos (resolved-module-path-name r) #f)
             ;; self-mpi:
             0))
       (define (lookup-sym mpi phase sym)
         (define r (module-path-index-resolve mpi))
         (define path/submod (resolved-module-path-name r))
         (if (or (symbol? path/submod)
                 (hash-ref excluded-module-mpis path/submod #f))
             sym
             (or (hash-ref names (cons (cons path/submod phase) sym) #f)
                 (error 'provides
                        "cannot find name for provided identifier: ~s ~s" sym mpi))))
       (match (binding-content bind)
         [`(,mod ,sym ,phase ,nom-mod)
          `(#:simple-module-binding
            #:mpi ,(lookup mod)
            ,(lookup-sym mod phase sym)
            ,phase
            #:mpi ,(lookup nom-mod))]
         [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
          ;; Currently dropping free-id=? and extra nominals
          `(#:module-binding
            #:mpi ,(lookup mod)
            ,(lookup-sym mod phase sym)
            ,phase
            #:mpi ,(lookup nom-mod)
            ,nom-phase
            ,nom-sym
            ,req-phase
            ,#f
            ,insp
            ,null)])])))
