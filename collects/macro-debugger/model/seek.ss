
#lang scheme/base
(require scheme/match
         scheme/list
         "deriv.ss"
         "deriv-util.ss"
         "synth-engine.ss"
         "synth-derivs.ss"
         "stx-util.ss"
         "context.ss")

(provide seek/deriv/on-fail
         seek/deriv
         current-seek-processor)

(define current-seek-processor (make-parameter values))

(define (process-node d)
  ((current-seek-processor) d))

;                                      
;                              ;;      
;                              ;;      
;                               ;      
;                               ;      
;    ;;;;;     ;;;;     ;;;;    ;  ;;; 
;   ;;   ;    ;   ;    ;   ;    ;  ;   
;   ;;       ;;   ;;  ;;   ;;   ; ;    
;    ;;;     ;;;;;;;  ;;;;;;;   ;;;    
;      ;;;;  ;        ;         ;;;    
;   ;    ;;  ;;       ;;        ; ;;   
;   ;    ;;   ;;       ;;       ;  ;;  
;   ;;;;;;     ;;;;     ;;;;   ;;;  ;;;
;                                      


;; Seek:
;; The derivation is "inactive" or "hidden" by default,
;; but pieces of it can become visible if they correspond to subterms
;; of the hidden syntax.

;; seek/deriv/on-fail : WDeriv (-> (values WDeriv syntax)) -> (values WDeriv syntax)
(define (seek/deriv/on-fail d fail-k)
  (with-handlers ([hiding-failure?
                   (lambda (failure)
                     (handle-hiding-failure d failure)
                     (fail-k))])
    (seek/deriv d)))

;; seek/deriv : WDeriv -> (values WDeriv syntax)
;; Seeks for derivations of all proper subterms of the derivation's
;; initial syntax.
(define (seek/deriv d)
  (match d
    [(Wrap deriv (e1 e2))
     (let ([subterms (gather-proper-subterms e1)])
       (parameterize ((subterms-table subterms))
         (let ([sd (seek d)])
           (values sd (wderiv-e2 sd)))))]))

;; seek : WDeriv -> WDeriv
;; Expects macro-policy, subterms-table to be set up already
(define (seek d)
  (match d
    [(Wrap deriv (e1 e2))
     (SKlet ((subterms hidden-exn) (subterm-derivations d))
            (begin (check-nonlinear-subterms subterms)
                   ;; Now subterm substitution is safe, because they don't overlap
                   (create-synth-deriv e1 subterms hidden-exn)))]))

;; create-synth-deriv : syntax (list-of Subterm) ?exn -> WDeriv
(define (create-synth-deriv e1 subterms hidden-exn)
  (let ([e2 (if hidden-exn #f (substitute-subterms e1 subterms))])
    (make p:synth e1 e2 null #f subterms hidden-exn)))

;; subterm-derivations : Derivation -> SK
(define (subterm-derivations d)
  (subterms-of-deriv d))

;; subterms-of-deriv : Derivation -> SK
(define (subterms-of-deriv d)
  (let ([path (check-visible d)])
    (if path
        (let ([d (process-node d)])
          (SKunit (list (make s:subterm path d))))
        (subterms-of-unlucky-deriv d))))

;; subterms-of-deriv/phase-up : Derivation -> SK
(define (subterms-of-deriv/phase-up d)
  (parameterize ((phase (add1 (phase))))
    (subterms-of-deriv d)))

;; check-visible : Derivation -> Path/#f
(define (check-visible d)
  (match d
    [(Wrap deriv (e1 e2))
     (let ([paths (table-get (subterms-table) e1)])
       (cond [(null? paths) #f]
             [(null? (cdr paths))
              (car paths)]
             [else
              ;; More than one path to the same(eq?) syntax object
              ;; Not good.
              ;; FIXME: Better to delay check to here, or check whole table first?
              ;; FIXME
              (raise
               (make nonlinearity e1 paths))]))]
    [#f #f]))

;; subterms-of-unlucky-deriv : Derivation -> SK
;; Guarantee: (wderiv-e1 deriv) is not in subterms table
(define (subterms-of-unlucky-deriv d)
  (match d
    ;; Primitives
    [(Wrap p:module (e1 e2 rs ?1 ?2 tag rename check tag2 ?3 body shift))
     (match (normalize-module d)
       [(Wrap p:module (e1 e2 rs ?1 ?2 tag rename check tag2 ?3 body shift))
        (>>Seek [! ?1]
                [! ?2]
                [#:rename
                 (do-rename (if tag
                                tag
                                (with-syntax ([(?module ?name ?lang ?body)
                                               e1])
                                  #'?body))
                            rename)]
                (subterms-of-deriv check)
                ;; FIXME: tag
                [! ?3]
                (subterms-of-deriv body))])]
    [(Wrap p:#%module-begin (e1 e2 rs ?1 me pass1 pass2 ?2))
     (>>Seek [! ?1]
             (subterms-of-lderiv (module-begin->lderiv d))
             [! ?2])]
    [(Wrap p:variable (e1 e2 rs ?1))
     (>>Seek)]
    [(Wrap p:define-syntaxes (e1 e2 rs ?1 rhs ?2))
     (>>Seek [! ?1]
             (subterms-of-deriv/phase-up rhs)
             [! ?2])]
    [(Wrap p:define-values (e1 e2 rs ?1 rhs))
     (>>Seek [! ?1]
             (subterms-of-deriv rhs))]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner))
     (>>Seek [! ?1]
             (subterms-of-deriv inner))]
    [(Wrap p:if (e1 e2 rs ?1 test then else))
     (>>Seek [! ?1]
             (subterms-of-deriv test)
             (subterms-of-deriv then)
             (subterms-of-deriv else))]
    [(Wrap p:wcm (e1 e2 rs ?1 key value body))
     (>>Seek [! ?1]
             (subterms-of-deriv key)
             (subterms-of-deriv value)
             (subterms-of-deriv body))]
    [(Wrap p:set! (e1 e2 rs ?1 id-resolves rhs))
     (>>Seek [! ?1]
             (subterms-of-deriv rhs))]
    [(Wrap p:set!-macro (e1 e2 rs ?1 deriv))
     (>>Seek [! ?1]
             (subterms-of-deriv deriv))]
    [(Wrap p:begin (e1 e2 rs ?1 lderiv))
     (>>Seek [! ?1]
             (subterms-of-lderiv lderiv))]
    [(Wrap p:begin0 (e1 e2 rs ?1 head lderiv))
     (>>Seek [! ?1]
             (subterms-of-deriv head)
             (subterms-of-lderiv lderiv))]
    [(Wrap p:#%app (e1 e2 rs ?1 lderiv))
     (>>Seek [! ?1]
             (subterms-of-lderiv lderiv))]
    [(Wrap p:lambda (e1 e2 rs ?1 renames body))
     (>>Seek [! ?1]
             [#:rename (do-rename/lambda e1 renames)]
             (subterms-of-bderiv body))]
    [(Wrap p:case-lambda (e1 e2 rs ?1 clauses))
     (>>Seek [! ?1]
             (SKmap2 subterms-of-case-lambda-clause
                     clauses
                     (stx->list (stx-cdr e1))))]
    [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
     (>>Seek [! ?1]
             [#:rename (do-rename/let e1 renames)]
             (SKmap subterms-of-deriv rhss)
             (subterms-of-bderiv body))]
    [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
     (>>Seek [! ?1]
             [#:rename (do-rename/let e1 renames)]
             (SKmap subterms-of-deriv rhss)
             (subterms-of-bderiv body))]
    [(Wrap p:letrec-syntaxes+values (e1 e2 rs ?1 srenames srhss vrenames vrhss body))
     (>>Seek [! ?1]
             [#:rename (do-rename/lsv1 e1 srenames)]
             (SKmap subterms-of-bind-syntaxes srhss)
             [#:rename (do-rename/lsv2 srenames vrenames)]
             (SKmap subterms-of-deriv vrhss)
             (subterms-of-bderiv body))]
    [(Wrap p::STOP (e1 e2 rs ?1))
     (>>Seek)]
    ;; synth (should synth be idempotent?... heh, no point for now)
    [(Wrap p:rename (e1 e2 rs ?1 rename inner))
     (>>Seek [! ?1]
             [#:rename (do-rename (car rename) (cdr rename))]
             (subterms-of-deriv inner))]
    
    ;; Macros
    
    [(Wrap mrule (e1 e2 tx next))
     (recv [(sk1 table) (subterms-of-transformation tx)]
           (parameterize ((subterms-table table))
             (SKseq sk1
                    (subterms-of-deriv next))))]

    [(Wrap tagrule (e1 e2 tagged-stx next))
     (subterms-of-deriv next)]

    [(Wrap lift-deriv (e1 e2 first lifted-stx next))
     (raise (make hidden-lift-site))]
    
    [(Wrap lift/let-deriv (e1 e2 first lifted-stx next))
     (raise (make hidden-lift-site))]
    
    ;; Errors
    
    [#f (SKzero)]
    ))

;; subterms-of-transformation : Transformation -> SK Table
(define (subterms-of-transformation tx)
  (match tx
    [(Wrap transformation (e1 e2 rs ?1 me1 locals me2 ?2 _seq))
     ;; FIXME: We'll need to use e1/e2/me1/me2 to synth locals, perhaps
     ;; FIXME: and we'll also need to account for *that* marking, too...
     (let ([end-table #f])
       (let ([sk1
              (>>Seek [! ?1]
                      [#:rename/no (do-rename e1 me1)]
                      (SKmap subterms-of-local-action locals)
                      [! ?2]
                      [#:rename/no (do-rename me2 e2)]
                      (begin (set! end-table (subterms-table))
                             (SKzero)))])
         (values sk1 end-table)))]))

;; subterms-of-local-action : LocalAction -> SK
(define (subterms-of-local-action local)
  (match local
    [(struct local-expansion (e1 e2 me1 me2 deriv for-stx? lifted opaque))
     (>>Seek [#:rename/no (do-rename me1 e1)]  ;; FIXME: right order?
             (let ([sk1 (subterms-of-deriv deriv)])
               (SKlet ((subterms exn) sk1)
                      (if (pair? (filter s:subterm? subterms))
                          (raise (make localactions))
                          sk1))))]
    [(struct local-lift (expr id))
     ;; FIXME: seek in the lifted deriv, transplant subterm expansions *here*
     (let ([d (extract/remove-unvisited-lift id)])
       (subterms-of-deriv d))]
    [(struct local-lift-end (decl))
     ;; FIXME
     (>>Seek)]
    [(struct local-bind (names bindrhs))
     ;; FIXME: learn names
     (let ([sk1 (subterms-of-bind-syntaxes bindrhs)])
       (SKlet ((subterms exn) sk1)
              (if (pair? (filter s:subterm? subterms))
                  (raise (make localactions))
                  sk1)))]))

;; subterms-of-lderiv : ListDerivation -> SK
(define (subterms-of-lderiv ld)
  (match ld
    [(Wrap lderiv (es1 es2 ?1 derivs))
     (>>Seek [! ?1]
             (SKmap subterms-of-deriv derivs))]
    [#f (SKzero)]))

;; subterms-of-bderiv : BlockDerivation -> SK
(define (subterms-of-bderiv bd)
  (subterms-of-lderiv (bderiv->lderiv bd)))

;; subterms-of-case-lambda-clause : CaseLambdaClause Syntax -> SK
(define (subterms-of-case-lambda-clause clause stx)
  (match clause
    [(Wrap clc (?1 renames body))
     (>>Seek [! ?1]
             [#:rename (do-rename/case-lambda stx renames)]
             (subterms-of-bderiv body))]))

;; subterms-of-bind-syntaxes : BindSyntaxes -> SK
(define (subterms-of-bind-syntaxes bindrhs)
  (match bindrhs
    [(Wrap bind-syntaxes (rhs ?1))
     (>>Seek (subterms-of-deriv rhs)
             [! ?1])]))

;                                                                 
;                     ;;;;                                        
;   ;;                   ;                                        
;    ;                   ;                                        
;    ;                   ;                                        
;    ; ;;;     ;;;;      ;     ;; ;;;     ;;;;   ;;; ;;;   ;;;;;  
;    ;;  ;    ;   ;      ;     ;;;  ;;   ;   ;    ;;;  ;  ;;   ;  
;    ;   ;   ;;   ;;     ;      ;   ;;  ;;   ;;    ;   ;  ;;      
;    ;   ;   ;;;;;;;     ;      ;   ;;  ;;;;;;;    ;       ;;;    
;    ;   ;   ;           ;      ;   ;;  ;          ;         ;;;; 
;    ;   ;   ;;          ;      ;   ;;  ;;         ;      ;    ;; 
;    ;   ;    ;;         ;      ;   ;    ;;        ;      ;    ;; 
;   ;;; ;;;    ;;;;   ;;;;;;;   ;;;;      ;;;;   ;;;;;;   ;;;;;;  
;                               ;                                 
;                               ;                                 
;                              ;;;;                               
;                                                                 

;; gather-one-subterm : syntax syntax -> SubtermTable
(define (gather-one-subterm whole part)
  (let ([table (make-hasheq)])
    (let ([paths (find-subterm-paths part whole)])
      (for-each (lambda (p) (table-add! table part p)) paths))
    table))

;; gather-proper-subterms : Syntax -> SubtermTable
;; FIXME: Eventually, need to descend into vectors, boxes, etc.
(define (gather-proper-subterms stx0)
  (let ([table (make-hasheq)])
    ;; loop : Syntax Path -> void
    (define (loop stx rpath)
      (unless (eq? stx0 stx)
        (table-add! table stx (reverse rpath)))
      (let ([p (syntax-e stx)])
        (when (pair? p)
          (loop-cons p rpath 0))))
    ;; loop-cons : (cons Syntax ?) Path number -> void
    (define (loop-cons p rpath pos)
      (loop (car p) (cons (make ref pos) rpath))
      (let ([t (cdr p)])
        (cond [(syntax? t)
               (let ([te (syntax-e t)])
                 (if (pair? te)
                     (begin
                       (table-add! table t (reverse (cons (make tail pos) rpath)))
                       (loop-cons te rpath (add1 pos)))
                     (loop t (cons (make tail pos) rpath))))]
              [(pair? t)
               (loop-cons t rpath (add1 pos))]
              [(null? t)
               (void)])))
    (loop stx0 null)
    table))


;                                               
;                              ;;;;             
;                     ;;          ;             
;     ;                ;          ;             
;     ;                ;          ;             
;   ;;;;;;    ;;;;;    ; ;;;      ;       ;;;;  
;     ;       ;   ;    ;;  ;;     ;      ;   ;  
;     ;           ;    ;   ;;     ;     ;;   ;; 
;     ;        ;;;;    ;   ;;     ;     ;;;;;;; 
;     ;      ;;   ;    ;   ;;     ;     ;       
;     ;      ;;   ;    ;   ;;     ;     ;;      
;     ;;     ;;  ;;    ;   ;      ;      ;;     
;      ;;;    ;;; ;;   ;;;;    ;;;;;;;    ;;;;  
;                                               
;                                               
;                                               


;; A Table is a hashtable[syntax => (list-of Path)
(define (table-add! table stx v)
  (hash-set! table stx (cons v (table-get table stx))))
(define (table-add-if-absent! table stx v)
  (unless (memq v (table-get table stx))
    (table-add! table stx v)))
(define (table-get table stx)
  (hash-ref table stx (lambda () null)))

;; do-rename : syntax syntax -> (values (list-of Subterm) Table)
(define (do-rename stx rename)
  (let ([t (make-hasheq)]
        [old (subterms-table)])
    ;; loop : syntax syntax -> (list-of Subterm)
    ;; Puts things into the new table, too
    ;; If active? is #f, always returns null
    (define (loop stx rename active?)
      (cond [(and (syntax? stx) (syntax? rename))
             (let ([paths (table-get old stx)])
               (if (pair? paths)
                   (begin (hash-set! t rename paths)
                          (loop (syntax-e stx) (syntax-e rename) #f)
                          (if active?
                              (map (lambda (p) (make s:rename p stx rename))
                                   paths)
                              null))
                   (loop (syntax-e stx) (syntax-e rename) active?)))]
            [(syntax? rename)
             (loop stx (syntax-e rename) active?)]
            [(syntax? stx)
             (loop (syntax-e stx) rename active?)]
            [(and (pair? stx) (pair? rename))
             (append
              (loop (car stx) (car rename) active?)
              (loop (cdr stx) (cdr rename) active?))]
            [else
             null]))
    (let ([subterms (loop stx rename #t)])
      (values subterms t))))

(define (do-rename/lambda stx rename)
  (if rename
      (with-syntax ([(?lambda ?formals . ?body) stx])
        (do-rename (cons #'?formals #'?body) rename))
      (values null (subterms-table))))

(define (do-rename/let stx rename)
  (if rename 
      (with-syntax ([(?let ?bindings . ?body) stx])
        (do-rename (cons #'?bindings #'?body) rename))
      (values null (subterms-table))))

(define (do-rename/case-lambda stx rename)
  (if rename
      (with-syntax ([(?formals . ?body) stx])
        (do-rename (cons #'?formals #'?body) rename))
      (values null (subterms-table))))

(define (do-rename/lsv1 stx rename) 
  (if rename
      (with-syntax ([(?lsv ?sbindings ?vbindings . ?body) stx])
        (do-rename (cons #'?sbindings (cons #'?vbindings #'?body)) rename))
      (values null (subterms-table))))

(define (do-rename/lsv2 old-rename rename)
  (if rename
      (with-syntax ([(?sbindings ?vbindings . ?body) old-rename])
        (do-rename (cons #'?vbindings #'?body) rename))
      (values null (subterms-table))))
