#lang racket/base
(require (for-syntax racket/base
                     syntax/for-body)
         racket/hash-code
         "treelist.rkt"
         (submod "treelist.rkt" unsafe)
         "private/sort.rkt")

(provide make-mutable-treelist
         (rename-out [build-mutable-treelist mutable-treelist])
         mutable-treelist?
         mutable-treelist-ref
         mutable-treelist-first
         mutable-treelist-last
         mutable-treelist-set!
         mutable-treelist-add!
         mutable-treelist-cons!
         mutable-treelist-append!
         mutable-treelist-insert!
         mutable-treelist-delete!
         mutable-treelist-take!
         mutable-treelist-drop!
         mutable-treelist-take-right!
         mutable-treelist-drop-right!
         mutable-treelist-sublist!
         mutable-treelist-reverse!
         mutable-treelist-length
         mutable-treelist-empty?
         treelist-copy
         mutable-treelist-copy
         mutable-treelist-snapshot
         mutable-treelist->vector
         vector->mutable-treelist
         mutable-treelist->list
         list->mutable-treelist
         mutable-treelist-map!
         mutable-treelist-for-each
         mutable-treelist-member?
         mutable-treelist-find
         mutable-treelist-sort!
         in-mutable-treelist
         for/mutable-treelist
         for*/mutable-treelist
         chaperone-mutable-treelist
         impersonate-mutable-treelist)

(struct mutable-treelist ([tl #:mutable])
  #:sealed
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (lambda (mtl port mode)
                                 (treelist-print "mutable-treelist" (mutable-treelist-tl mtl) port mode))
  #:property prop:equal+hash (list
                              (lambda (mtl other-mtl recur)
                                (treelist-equal? (mutable-treelist-tl mtl)
                                                 (mutable-treelist-tl other-mtl)
                                                 recur))
                              (lambda (v recur)
                                (treelist-hash-code (mutable-treelist-tl v) recur))
                              (lambda (v recur)
                                (treelist-length (mutable-treelist-tl v))))
  #:property prop:sequence (lambda (mtl)
                             (in-mutable-treelist/proc mtl)))

(define (make-mutable-treelist n [v #f])
  (cond
    [(eqv? n 0) (mutable-treelist empty-treelist)]
    [else
     (unless (exact-nonnegative-integer? n)
       (raise-argument-error* 'make-mutable-treelist 'racket/primitive "exact-nonnegative-integer?" n))
     (mutable-treelist (vector->treelist (make-vector n v)))]))

(define build-mutable-treelist
  (let ([mutable-treelist
         (lambda ds
           (mutable-treelist (list->treelist ds)))])
    mutable-treelist))

(define (treelist-copy tl)
  (unless (treelist? tl)
    (raise-argument-error* 'treelist-copy 'racket/primitive "treelist?" tl))
  (mutable-treelist (treelist-copy-for-mutable tl)))

(define (mutable-treelist-copy mtl)
  (check-mutable-treelist 'mutable-treelist-copy mtl)
  (mutable-treelist (treelist-copy-for-mutable (mutable-treelist-tl mtl))))

(define (check-mutable-treelist who tl)
  (unless (mutable-treelist? tl)
    (raise-argument-error* who 'racket/primitive "mutable-treelist?" tl)))

(define (mutable-treelist-snapshot mtl [pos 0] [end-pos #f])
  (check-mutable-treelist 'mutable-treelist-snapshot mtl)
  (define tl (mutable-treelist-tl mtl))
  (cond
    [(and (eqv? pos 0) (not end-pos))
     (treelist-copy-for-mutable tl)]
    [else
     (define size (treelist-length tl))
     (check-treelist-bound-index 'mutable-treelist-snapshot tl size pos 0 "starting ")
     (when end-pos
       (check-treelist-bound-index 'mutable-treelist-snapshot tl size end-pos pos "ending "))
     (treelist-copy-for-mutable (treelist-sublist tl pos (or end-pos size)))]))

(define (mutable-treelist-length mtl)
  (check-mutable-treelist 'mutable-treelist-length mtl)
  (treelist-length (mutable-treelist-tl mtl)))

(define (mutable-treelist-empty? mtl)
  (check-mutable-treelist 'mutable-treelist-empty? mtl)
  (treelist-empty? (mutable-treelist-tl mtl)))

(define (mutable-treelist-ref mtl index)
  (check-mutable-treelist 'mutable-treelist-ref mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-index 'mutable-treelist-ref tl (treelist-length tl) index)
  (treelist-ref tl index))

(define (mutable-treelist-first mtl)
  (check-mutable-treelist 'mutable-treelist-first mtl)
  (define tl (mutable-treelist-tl mtl))
  (when (treelist-empty? tl)
    (raise-arguments-error 'mutable-treelist-first "mutable treelist is empty"))
  (treelist-first tl))

(define (mutable-treelist-last mtl)
  (check-mutable-treelist 'mutable-treelist-last mtl)
  (define tl (mutable-treelist-tl mtl))
  (when (treelist-empty? tl)
    (raise-arguments-error 'mutable-treelist-last "mutable treelist is empty"))
  (treelist-last tl))

(define (mutable-treelist-set! mtl index val)
  (check-mutable-treelist 'mutable-treelist-set! mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-index 'mutable-treelist-set! tl (treelist-length tl) index)
  (treelist-set! tl index val))

(define (mutable-treelist-add! mtl val)
  (check-mutable-treelist 'mutable-treelist-add! mtl)
  (define tl (mutable-treelist-tl mtl))
  (set-mutable-treelist-tl! mtl (treelist-add tl val)))

(define (mutable-treelist-cons! mtl val)
  (check-mutable-treelist 'mutable-treelist-cons! mtl)
  (define tl (mutable-treelist-tl mtl))
  (set-mutable-treelist-tl! mtl (treelist-cons tl val)))

(define (mutable-treelist-append! mtl m/tl)
  (check-mutable-treelist 'mutable-treelist-append! mtl)
  (define tl (mutable-treelist-tl mtl))
  (define tl2
    (cond
      [(treelist? m/tl) m/tl]
      [(mutable-treelist? m/tl) (mutable-treelist-snapshot m/tl)]
      [else (raise-argument-error* 'mutable-treelist-append! 'racket/primitive "mutable-treelist?" m/tl)]))
  (set-mutable-treelist-tl! mtl (treelist-append tl tl2)))

(define (mutable-treelist-insert! mtl index val)
  (check-mutable-treelist 'mutable-treelist-insert! mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-end-index 'mutable-treelist-insert! tl (treelist-length tl) index)
  (set-mutable-treelist-tl! mtl (treelist-insert tl index val)))

(define (mutable-treelist-delete! mtl index)
  (check-mutable-treelist 'mutable-treelist-delete! mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-index 'mutable-treelist-delete! tl (treelist-length tl) index)
  (set-mutable-treelist-tl! mtl (treelist-delete tl index)))

(define (mutable-treelist-take! mtl n)
  (check-mutable-treelist 'mutable-treelist-take! mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-end-index 'mutable-treelist-take! tl (treelist-length tl) n)
  (set-mutable-treelist-tl! mtl (treelist-take tl n)))

(define (mutable-treelist-drop! mtl n)
  (check-mutable-treelist 'mutable-treelist-drop! mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-end-index 'mutable-treelist-drop! tl (treelist-length tl) n)
  (set-mutable-treelist-tl! mtl (treelist-drop tl n)))

(define (mutable-treelist-take-right! mtl n)
  (check-mutable-treelist 'mutable-treelist-take-right! mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-end-index 'mutable-treelist-take-right! tl (treelist-length tl) n)
  (set-mutable-treelist-tl! mtl (treelist-take-right tl n)))

(define (mutable-treelist-drop-right! mtl n)
  (check-mutable-treelist 'mutable-treelist-drop-right! mtl)
  (define tl (mutable-treelist-tl mtl))
  (check-treelist-end-index 'mutable-treelist-drop-right! tl (treelist-length tl) n)
  (set-mutable-treelist-tl! mtl (treelist-drop-right tl n)))

(define (mutable-treelist-sublist! mtl pos end-pos)
  (check-mutable-treelist 'mutable-treelist-sublist! mtl)
  (define tl (mutable-treelist-tl mtl))
  (define size (treelist-length tl))
  (check-treelist-bound-index 'mutable-treelist-sublist! tl size pos 0 "starting ")
  (check-treelist-bound-index 'mutable-treelist-sublist! tl size end-pos pos "ending ")
  (set-mutable-treelist-tl! mtl (treelist-sublist tl pos end-pos)))

(define (mutable-treelist-reverse! mtl)
  (check-mutable-treelist 'mutable-treelist-reverse! mtl)
  (define tl (mutable-treelist-tl mtl))
  (set-mutable-treelist-tl! mtl (treelist-reverse tl)))

(define (mutable-treelist->vector mtl)
  (check-mutable-treelist 'mutable-treelist->vector mtl)
  (treelist->vector (mutable-treelist-tl mtl)))

(define (mutable-treelist->list mtl)
  (check-mutable-treelist 'mutable-treelist->list mtl)
  (treelist->list (mutable-treelist-tl mtl)))

(define (vector->mutable-treelist vec)
  (unless (vector? vec)
    (raise-argument-error 'vector->mutable-treelist "vector?" vec))
  (mutable-treelist (vector->treelist vec)))

(define (list->mutable-treelist lst)
  (unless (list? lst)
    (raise-argument-error 'list->mutable-treelist "list?" lst))
  (mutable-treelist (list->treelist lst)))

(define (mutable-treelist-map! mtl proc)
  (check-mutable-treelist 'mutable-treelist-map! mtl)
  (unless (and (procedure? proc) (procedure-arity-includes? proc 1))
    (raise-argument-error 'mutable-treelist-map! "(procedure-arity-includes/c 1)" proc))
  (define tl (mutable-treelist-tl mtl))
  (for ([i (in-range (treelist-length tl))])
    (treelist-set! tl i (proc (treelist-ref tl i)))))

(define (mutable-treelist-for-each mtl proc)
  (check-mutable-treelist 'mutable-treelist-for-each mtl)
  (unless (and (procedure? proc) (procedure-arity-includes? proc 1))
    (raise-argument-error 'mutable-treelist-for-each "(procedure-arity-includes/c 1)" proc))
  (treelist-for-each (mutable-treelist-tl mtl) proc))

(define (mutable-treelist-member? mtl v [eql? equal?])
  (check-mutable-treelist 'mutable-treelist-member? mtl)
  (unless (and (procedure? eql?) (procedure-arity-includes? eql? 2))
    (raise-argument-error 'mutable-treelist-member? "(procedure-arity-includes/c 2)" eql?))
  (treelist-member? (mutable-treelist-tl mtl) v eql?))

(define (mutable-treelist-find mtl match?)
  (check-mutable-treelist 'mutable-treelist-find mtl)
  (unless (and (procedure? match?) (procedure-arity-includes? match? 1))
    (raise-argument-error 'mutable-treelist-find "(procedure-arity-includes/c 1)" match?))
  (treelist-find (mutable-treelist-tl mtl) match?))

(define (mutable-treelist-sort! mtl less-than?
                                #:key [get-key #f]
                                #:cache-keys? [cache-keys? #f])
  (check-mutable-treelist 'mutable-treelist-sort! mtl)
  (check-sort-arguments 'mutable-treelist-sort! less-than? get-key)
  (define vec (treelist->vector (mutable-treelist-tl mtl)))
  (vector-sort! vec less-than? 0 (vector*-length vec) get-key cache-keys?)
  (for/treelist ([i (in-range 0 (vector-length vec))])
    (mutable-treelist-set! mtl i (vector*-ref vec i))))

(define-sequence-syntax in-mutable-treelist
  (lambda () #'in-mutable-treelist/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ mtl-expr)]
       #'[(d) (in-treelist
               (let ([mtl mtl-expr])
                 (check-mutable-treelist 'in-mutable-terelist mtl)
                 (mutable-treelist-tl mtl)))]])))

(define (in-mutable-treelist/proc mtl)
  (check-mutable-treelist 'in-mutable-terelist mtl)
  (in-treelist (mutable-treelist-tl mtl)))

(define-syntax for/mutable-treelist
  (make-for/treelist #'for/fold/derived #'treelist-copy))
(define-syntax for*/mutable-treelist
  (make-for/treelist #'for*/fold/derived #'treelist-copy))

(define (chaperone-mutable-treelist mtl
                                    ref-proc
                                    set-proc
                                    insert-proc
                                    append-proc
                                    . props)
  (check-mutable-treelist 'chaperone-mutable-treelist mtl)
  (check-chaperone-arguments 'chaperone-mutable-treelist
                             ref-proc
                             set-proc
                             insert-proc
                             append-proc
                             props)
  (chaperone-or-impersonate chaperone-struct chaperone-treelist
                            mtl
                            ref-proc set-proc insert-proc append-proc
                            props))

(define (impersonate-mutable-treelist mtl
                                      ref-proc
                                      set-proc
                                      insert-proc
                                      append-proc
                                      . props)
  (check-mutable-treelist 'impersonate-mutable-treelist mtl)
  (check-chaperone-arguments 'impersonate-mutable-treelist
                             ref-proc
                             set-proc
                             insert-proc
                             append-proc
                             props)
  (chaperone-or-impersonate impersonate-struct impersonate-treelist
                            mtl
                            ref-proc set-proc insert-proc append-proc
                            props))

(define (chaperone-or-impersonate chaperone-struct chaperone-treelist
                                  mtl
                                  ref-proc set-proc insert-proc append-proc
                                  props)
  (define (tl-ref-proc tl index v) (ref-proc mtl index v))
  (define (tl-set-proc tl index v) (set-proc mtl index v))
  (define (tl-insert-proc tl index v) (insert-proc mtl index v))
  (define (tl-append-proc tl rhs) (append-proc mtl rhs))
  (chaperone-struct mtl
                    mutable-treelist-tl
                    (lambda (mtl tl)
                      (apply chaperone-treelist
                             tl
                             (and ref-proc tl-ref-proc)
                             tl-set-proc
                             tl-insert-proc
                             tl-append-proc
                             props))
                    set-mutable-treelist-tl!
                    (lambda (mtl tl)
                      ;; strip chaperone or impersonator away, and we'll reapply
                      ;; as appropriate when reading via a mutable wrapper
                      (unimpersonate-treelist tl))))
