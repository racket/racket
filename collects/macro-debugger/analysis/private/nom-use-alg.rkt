#lang racket/base
(require racket/match
         "moduledb.rkt"
         "util.rkt")
(provide nom-use-alg)

;; nom-use-alg : Refs compiled -> (listof recommendation)
(define (nom-use-alg refs0 compiled)
  (let ([refs (append (provides->refs compiled) refs0)])
    (let-values ([(NOM-USES DEF-USES) (calculate-used-approximations refs)])
      (report NOM-USES DEF-USES (get-requires compiled)))))

;; ========

;; sMPI = S-expr form of mpi (see mpi->key)
;; Using MPIs doesn't work. I conjecture that the final module shift means that
;; all during-expansion MPIs are different from all compiled-expr MPIs.

;; A UsedTable = hash[(list int sMPI) => Refs]

;; calculate-used-approximations : Refs -> (values UsedTable UsedTable)
(define (calculate-used-approximations refs)
  (let ([NOM-USES (make-hash)]
        [DEF-USES (make-hash)])
    (for ([ref (in-list refs)])
      (when (relevant? ref)
        (match (ref-binding ref)
          [(list def-mod def-sym nom-mod nom-sym
                 def-phase nom-imp-phase nom-exp-phase)
           (define use-phase (ref-phase ref))
           (when def-mod
             ;; use-phase = def-phase + required-phase
             ;; thus required-phase = use-phase - def-phase
             (let* ([required-phase (- use-phase def-phase)]
                    [key (list required-phase (mpi->key def-mod))])
               (hash-set! DEF-USES key
                          (cons ref (hash-ref DEF-USES key null)))))
           ;; We just care about nom-imp-phase, since importing into *here*
           (let* ([key (list nom-imp-phase (mpi->key nom-mod))])
             (hash-set! NOM-USES key
                        (cons ref (hash-ref NOM-USES key null))))]
          [_ (void)])))
    (values NOM-USES DEF-USES)))

;; relevant? : Ref -> boolean
;; Only want identifiers actually originating from module being analyzed,
;; not identifiers from other modules inserted by macro expansion.
;; - Actually, want identifiers with lexical context of module, which includes
;;   some identifiers not originating from module (eg, inserted by unit-from-context).
;; - Also, if ref represents a re-export, no identifier but still relevant.
;; So, use syntax-source-module conservatively: only to disqualify refs.
(define (relevant? ref)
  (let* ([phase (ref-phase ref)]
         [id (ref-id ref)]
         [binding (ref-binding ref)]
         [srcmod (and id (syntax-source-module id))])
    (cond [(and srcmod (not (here-mpi? srcmod))) #f]
          [else #t])))

;; ========

;; get-requires : compiled-module-expr -> (listof (list int MPI))
(define (get-requires compiled)
  (let ([phase+mods-list (module-compiled-imports compiled)])
    (for*/list ([phase+mods (in-list phase+mods-list)]
                #:when (car phase+mods) ;; Skip for-label requires
                [mod (cdr phase+mods)])
      (list (car phase+mods) mod))))

;; provides->refs : compiled-module-expr -> Refs
(define (provides->refs compiled)
  (let-values ([(vprov sprov) (module-compiled-exports compiled)])
    (for*/list ([phase+exps (in-list (append vprov sprov))]
                #:when (car phase+exps) ;; Skip for-label provides
                [name+srcs (in-list (cdr phase+exps))]
                [src (in-list (cadr name+srcs))])
      (let ([phase (car phase+exps)]
            [name (car name+srcs)])

        (define (->ref nom-mod exp-sym phase-shift sym orig-phase)
          ;; We don't have the DEF information, so put #f
          (let ([b (list #f #f nom-mod sym #f phase-shift orig-phase)])
            (ref phase #f 'provide b)))

        (match src
          [(? module-path-index?)
           (->ref src name 0 name phase)]
          [(list imp-mod imp-phase-shift imp-name imp-orig-phase)
           (->ref imp-mod name imp-phase-shift imp-name imp-orig-phase)])))))

;; ========

;; A RefineTable is hash[(cons mpi phase) => (or RefineTable Imps)]
;; preserve nesting because inner MPIs need to be resolved wrt outer MPIs

;; try-bypass : mpi phase Refs -> RefineTable or #f
(define (try-bypass mod reqphase refs)
  ;; refs are all nominally from mod
  (let* ([imps (map ref->imp refs)])
    (refine-imps/one-require mod reqphase imps)))

;; refine-imps/one-require : mod phase Imps -> RefineTable or #f
;; where all imps come from mod at phase
;; the result table contains new (refined) imps
(define (refine-imps/one-require mod reqphase imps)
  (let ([use-table (make-hash)] ;; RefineTable
        [bytable (mod->bypass-table mod)])
    (and (for/and ([i (in-list imps)])
           (match i
             [(imp _m _rp sym exp-phase r)
              (let* ([bykey (cons sym exp-phase)]
                     [src (hash-ref bytable bykey #f)])
                (match src
                  [(renm srcmod phase-shift srcsym srcphase)
                   (let ([use-key (cons srcmod (+ reqphase phase-shift))]
                         [imp* (imp srcmod (+ reqphase phase-shift) srcsym srcphase r)])
                     (hash-set! use-table use-key (cons imp* (hash-ref use-table use-key null))))
                   #t]
                  [else #f]))]))
         (refine-imps* use-table))))

(define (refine-imps* partitions)
  (for/hash ([(mod+reqphase imps) (in-hash partitions)])
    (values mod+reqphase
            (let ([mod (car mod+reqphase)]
                  [reqphase (cdr mod+reqphase)])
              (or (and (allow-bypass? mod)
                       (refine-imps/one-require mod reqphase imps))
                  imps)))))

;; ========

;; A BypassTable is hash[(cons sym phase) => Renm
;; Contains only approved modules (no private, etc).

;; A Renm is (renm srcmod reqphase srcsym)
(struct renm (srcmod phase-shift srcsym srcphase))

;; mod->bypass-table : mpi -> BypassTable
;; FIXME: cache tables
(define (mod->bypass-table mod)
  (define table (make-hash))
  (let ([prov (get-module-all-exports mod)])
    (for* ([phase+exps (in-list prov)]
           #:when (car phase+exps) ;; Skip for-label provides
           [name+srcs (in-list (cdr phase+exps))]
           [src (in-list (cadr name+srcs))])
      (let ([phase (car phase+exps)]
            [name (car name+srcs)])

        (define (add-source! src-mod phase-offset src-sym)
          (when (bypass-ok-mpi? src-mod)
            (let ([key (cons name phase)]
                  ;; src-phase + phase-shift = phase
                  [src-phase (- phase phase-offset)])
              (hash-ref! table key (renm src-mod phase-offset src-sym src-phase)))))

        (match src
          [(? module-path-index?)
           (add-source! src 0 name)]
          [(list imp-mod imp-phase-shift imp-name imp-orig-phase)
           (add-source! imp-mod imp-phase-shift imp-name)]))))
  table)

;; ========

;; report : UseTable UseTable (listof (list int mpi)) -> (listof recommendation)
(define (report NOM-USES DEF-USES phase+mod-list)
  (for/list ([phase+mod (in-list phase+mod-list)])
    (let* ([phase (car phase+mod)]
           [mod (cadr phase+mod)]
           [key (list phase (mpi->key mod))]
           [nom-refs (hash-ref NOM-USES key null)]
           [def-refs (hash-ref DEF-USES key null)])
      (cond [(and (pair? nom-refs) (pair? def-refs))
             ;; We use refs defined in the module (and we got them from the module)
             (list 'keep mod phase (map ref->imp nom-refs))]
            [(pair? nom-refs)
             ;; We use refs gotten from the module (but defined elsewhere)
             (let ([bypass
                    (and (allow-bypass? mod)
                         (try-bypass mod phase nom-refs))])
               (if bypass
                   (list 'bypass mod phase bypass)
                   (list 'keep mod phase (map ref->imp nom-refs))))]
            [else
             ;; We don't have any refs gotten from the module
             ;; (although we may---possibly---have refs defined in it, but gotten elsewhere)
             (if (allow-drop? mod)
                 (list 'drop mod phase)
                 (list 'keep mod phase null))]))))
