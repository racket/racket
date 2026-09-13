#lang racket/base
(require racket/match
         "../private/deserialize.rkt"
         "import.rkt"
         "binding-lookup.rkt"
         "path-submod.rkt")

(provide binding-module-path-index-shift
         binding-mpi+phases
         binding-sym
         binding-syntax?
         binding-sym-path/submod-phase
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

(define (binding-mpi+phases bind)
  (cond
    [(provided? bind) (binding-mpi+phases (provided-binding bind))]
    [else
     (match (binding-content bind)
       [`(,mod ,sym ,phase ,nom-mod) (list (cons mod phase) (cons nom-mod phase))]
       [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
        (list (cons mod phase) (cons nom-mod nom-phase))])]))

(define (binding-sym bind)
  (cond
    [(provided? bind) (binding-sym (provided-binding bind))]
    [else
     (match (binding-content bind)
       [`(,mod ,sym ,phase ,nom-mod) sym]
       [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
        sym])]))

(define (binding-syntax? bind)
  (cond
    [(provided? bind) (provided-syntax? bind)]
    [else #f]))

(define (binding-sym-path/submod-phase bind)
  (define (resolve mpi)
    (define r (module-path-index-resolve mpi))
    (resolved-module-path->path/submod r))
  (cond
    [(provided? bind) (binding-sym-path/submod-phase (provided-binding bind))]
    [else
     (match (binding-content bind)
       [`(,mod ,sym ,phase ,nom-mod) (values sym (resolve mod) phase)]
       [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
        (values sym (resolve mod) phase)])]))

(define (serialize-binding bind root-phase
                           external-path-pos excluded-module-mpis included-module-phases
                           names transformer-names one-mods
                           mpi-count)
  (let loop ([bind bind])
    (cond
      [(provided? bind)
       `(#:provided
         ,@(loop (provided-binding bind))
         ,(provided-protected? bind)
         ,(provided-syntax? bind))]
      [else
       (define (lookup mpi phase)
         (define r (module-path-index-resolve mpi))
         (define pos
           (or (hash-ref external-path-pos (cons (resolved-module-path->path/submod r) phase) #f)
               ;; self-mpi:
               0))
         (when (pos . >= . mpi-count)
           (error 'bundle-binding "nonsense pos: ~a for ~s" pos (resolved-module-path-name r)))
         pos)
       (define (lookup-sym mpi phase sym)
         (define r (module-path-index-resolve mpi))
         (define path/submod (resolved-module-path->path/submod r))
         (binding-lookup path/submod phase sym
                         names transformer-names
                         one-mods
                         excluded-module-mpis included-module-phases))
       (match (binding-content bind)
         [`(,mod ,sym ,phase ,nom-mod)
          (define-values (new-sym new-phase) (lookup-sym mod phase sym))
          `(#:simple-module-binding
            #:mpi ,(lookup mod phase)
            ,new-sym
            ,new-phase
            #:mpi ,(lookup nom-mod phase))]
         [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
          ;; Currently dropping free-id=? and extra nominals
          (define-values (new-sym new-phase) (lookup-sym mod phase sym))
          `(#:module-binding
            #:mpi ,(lookup mod phase)
            ,new-sym
            ,new-phase
            #:mpi ,(lookup nom-mod nom-phase)
            ,nom-phase
            ,nom-sym
            ,req-phase
            ,#f
            ,insp
            ,null)])])))
