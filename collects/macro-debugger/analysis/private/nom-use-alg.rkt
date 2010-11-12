#lang racket/base
(require racket/dict
         racket/match
         "reftable.rkt"
         "moduledb.rkt"
         "util.rkt")
(provide nom-use-alg)

;; sMPI = S-expr form of mpi (see mpi->key)
;; Using MPIs doesn't work. I conjecture that the final module shift means that
;; all during-expansion MPIs are different from all compiled-expr MPIs.

;; A UsedTable = hash[(list int sMPI) => list]

;; calculate-used-approximations : RefTable -> (values UsedTable UsedTable)
(define (calculate-used-approximations refs)
  (let ([NOM-USES (make-hash)]
        [DEF-USES (make-hash)])
    (for* ([(use-phase id-table) (in-hash refs)]
           [id (in-dict-keys id-table)])
      ;; Only look at identifiers written in module being examined.
      ;; (Otherwise, nom-mod & nom-phase aren't enough info (???)
      (when (here-mpi? (syntax-source-module id)) ;; REDUNDANT
        (let ([b (identifier-binding id use-phase)])
          (match b
            [(list def-mod def-sym nom-mod nom-sym
                   def-phase nom-imp-phase nom-exp-phase)
             ;; use-phase = def-phase + required-phase
             ;; thus required-phase = use-phase - def-phase
             (let* ([required-phase (- use-phase def-phase)]
                    [key (list required-phase (mpi->key def-mod))])
               (hash-set! DEF-USES key
                          (cons id (hash-ref DEF-USES key null))))
             ;; use-phase = nom-imp-phase + nom-exp-phase ?????
             ;; We just care about nom-imp-phase, since importing into *here*
             #|
             ;; FIXME: This check goes wrong on defined-for-syntax ids
             (unless (equal? use-phase (+ nom-imp-phase nom-exp-phase))
               (error 'calculate
                      "internal error: phases wrong in ~s @ ~s, binding = ~s"
                      id use-phase b))
             |#
             (let ([key (list nom-imp-phase (mpi->key nom-mod))])
               (hash-set! NOM-USES key
                          (cons id (hash-ref NOM-USES key null))))]
            [_
             (void)]))))
    (values NOM-USES DEF-USES)))

;; ========

;; get-requires : compiled-module-expr -> (listof (list int MPI))
(define (get-requires compiled)
  (let ([phase+mods-list (module-compiled-imports compiled)])
    (for*/list ([phase+mods (in-list phase+mods-list)]
                #:when (car phase+mods) ;; Skip for-label requires
                [mod (cdr phase+mods)])
      (list (car phase+mods) mod))))

;; add-provides! : compiled-module-expr UsedTable UsedTable -> void
(define (add-provides! compiled NOM-USES DEF-USES)
  (define (add! mpi phase)
    (let ([key (list phase (mpi->key mpi))])
      (hash-set! NOM-USES key (cons 'export (hash-ref NOM-USES key null)))
      (hash-set! DEF-USES key (cons 'export (hash-ref DEF-USES key null)))))
  (let-values ([(vprov sprov) (module-compiled-exports compiled)])
    (for* ([phase+exps (in-list (append vprov sprov))]
           #:when (car phase+exps) ;; Skip for-label provides
           [name+srcs (in-list (cdr phase+exps))]
           [src (in-list (cadr name+srcs))])
      (let ([name (car name+srcs)])
        (match src
          [(? module-path-index?)
           (add! src 0)]
          [(list imp-mod imp-phase-shift imp-name imp-phase-???)
           (add! imp-mod imp-phase-shift)])))))

;; ========

;; report : UseTable UseTable (listof (list int mpi)) -> (listof recommendation)
(define (report NOM-USES DEF-USES phase+mod-list)
  (for/list ([phase+mod (in-list phase+mod-list)])
    (let* ([key (list (car phase+mod) (mpi->key (cadr phase+mod)))]
           [db-config
            (hash-ref module-db
                      (resolved-module-path-name
                       (module-path-index-resolve (cadr phase+mod)))
                      #f)]
           [nom-ids (hash-ref NOM-USES key null)]
           [def-ids (hash-ref DEF-USES key null)]
           [phase (car phase+mod)]
           [mod (cadr phase+mod)]
           [name (format "~s at ~s" (mpi->key mod) phase)])
      (cond [(and (pair? nom-ids) (pair? def-ids))
             (list 'keep mod phase (if (ormap identifier? nom-ids) #f "for exports"))]
            [(pair? nom-ids)
             (if (memq db-config '(no-bypass no-drop))
                 (list 'keep mod phase "db says no-bypass")
                 (list 'bypass mod phase))]
            [else
             (if (memq db-config '(no-drop))
                 (list 'keep mod phase "db says no-drop")
                 (list 'drop mod phase))]))))

;; nom-use-alg : RefTable compiled -> (listof recommendation)
(define (nom-use-alg refs compiled)
  (let-values ([(NOM-USES DEF-USES) (calculate-used-approximations refs)])
    (add-provides! compiled NOM-USES DEF-USES)
    (report NOM-USES DEF-USES (get-requires compiled))))
