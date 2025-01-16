#lang racket/base
(require compiler/zo-structs
         racket/match
         racket/pretty
         "run.rkt"
         "name.rkt"
         "import.rkt"
         "remap.rkt"
         "linklet.rkt"
         "merged.rkt"
         "log.rkt")

(provide merge-linklets)

(define (merge-linklets phase-runs names transformer-names
                        #:prune-definitions? prune-definitions?)
  ;; Accumulate syntax objects, which span phases. If would be nice if we didn't
  ;; keep syntax objects in expressions that are later pruned,
  ;; but we'll leave that as a future improvement.
  (define stx-objs (make-hasheq))
  (define stx-obj-map (make-hasheqv))
  (define (remap-syntax-object! r i)
    (define stx-vec (run-stx-vec r))
    (cond
      [stx-vec
       (define stx (vector-ref (run-stx-vec r) i))
       (or (hash-ref stx-objs stx #f)
           (let ([j (hash-count stx-objs)])
             (hash-set! stx-objs stx j)
             (hash-set! stx-obj-map j stx)
             j))]
      [else 0]))

  (define phase-merged
    (for/hasheqv ([(root-phase runs) (in-hash phase-runs)])
      (define used-import-names (make-hasheq))
      (define any-syntax-literals? #f)
      (define any-transformer-registers? #f)
      (define defined-names (make-hasheq))

      (define new-body
        (apply
         append
         (for/list ([r (in-list runs)])
           (define linkl (run-linkl r))
           (define import-map (run-import-map r))

           (define body (linklet*-body linkl))

           (define (remap-name name)
             (cond
               [(hash-ref import-map name #f)
                => (lambda (i)
                     (cond
                       [(import? i)
                        (define new-name (import-name i))
                        (hash-set! used-import-names new-name #t)
                        new-name]
                       [else i]))]
               [else
                (or (maybe-find-name names (cons (run-path/submod r) (run-phase r)) name)
                    ;; fall-through for builtins
                    name)]))

           (define (remap-defined-name name)
             (define new-name (remap-name name))
             (hash-set! defined-names new-name #t)
             new-name)

           (remap-names body
                        remap-name
                        #:unsafe? (run-unsafe? r)
                        #:remap-defined-name remap-defined-name
                        #:application-hook
                        (lambda (rator rands remap)
                          ;; Check for a `(.get-syntax-literal! '<pos>)` call
                          ;; or a `(.set-transformer! '<sym> <expr>)` call
                          (cond
                            [(eq? rator '.get-syntax-literal!)
                             (unless (and (= 1 (length rands))
                                          (exact-nonnegative-integer? (car rands)))
                               (error "unrecognized syntax-literal access"))
                             (set! any-syntax-literals? #t)
                             `(,(remap rator)
                               ,(remap-syntax-object! r (car rands)))]
                            [(eq? rator '.set-transformer!)
                             (cond
                               [(eqv? root-phase 0)
                                `(void)]
                               [else
                                (set! any-transformer-registers? #t)
                                (match rands
                                  [`((quote ,name) ,rhs)
                                   (define path/submod+phase (cons (run-path/submod r) (sub1 (run-phase r))))
                                   (define new-name
                                     (or (hash-ref transformer-names (cons path/submod+phase name) #f)
                                         (error 'merge "cannot find name for transformer definition: ~s"
                                                name)))
                                   `(,(remap rator)
                                     ',new-name
                                     ,(remap rhs))])])]
                            [else #f]))))))

      (values root-phase (merged new-body
                                 used-import-names
                                 any-syntax-literals?
                                 any-transformer-registers?
                                 defined-names))))
  
  (define name-imports
    (for*/hasheq ([(root-phase runs) (in-hash phase-runs)]
                  [r (in-list runs)]
                  [i (in-hash-values (run-import-map r))]
                  #:when (import? i))
      (values (import-name i) i)))

  (define portal-stxes
    (for/hasheqv ([(root-phase runs) (in-hash phase-runs)])
      (values root-phase
              (for/fold ([ht #hasheq()]) ([r (in-list runs)])
                (define path/submod+phase (cons (run-path/submod r) (run-phase r)))
                (for/fold ([ht ht]) ([(name pos) (in-hash (run-portal-stxes r))])
                  (define new-name (hash-ref transformer-names (cons path/submod+phase name)))
                  (hash-set ht new-name (remap-syntax-object! r pos)))))))

  (define new-stx-vec
    (for/vector ([i (in-range (hash-count stx-obj-map))])
      (hash-ref stx-obj-map i)))

  (when (log-level? (current-logger) 'debug 'demodularizer)
    (log-demodularizer-debug " Merged:")
    (for ([(phase mgd) (in-hash phase-merged)])
      (define-values (i o) (make-pipe))
      (pretty-print (merged-body mgd) o)
      (close-output-port o)
      (for ([line (in-lines i)])
        (log-demodularizer-debug "    ~a" line))))

  (values phase-merged
          name-imports
          new-stx-vec
          portal-stxes))
