#lang unstable/2d racket/base

(require racket/contract
         racket/function
         racket/list
         racket/match
         racket/promise
         racket/set
         "match-a-pattern.rkt"
         "matcher.rkt"
         "lang-struct.rkt"
         "extract-conditions.rkt"
         "enum.rkt"
         "error.rkt"
         (for-syntax "rewrite-side-conditions.rkt"
                     racket/base)
         unstable/2d/match)

(provide unify
         unify*
         (struct-out p*e)
         (struct-out env)
         (struct-out bound)
         (struct-out lvar)
         check-and-resimplify
         disunify*
         disunify
         empty-env
         pat*-clause-p?s
         bind-names
         remove-empty-dqs
         and/fail
         (struct-out unif-fail)
         not-failed?
         dq
         dq-dq
         predef-pat?
         unique-name-nums
         fresh-pat-vars
         make-uid
         p*e-eqs
         unsupported-pat-err-name
         unsupported-pat-err)


;;
;; atom := `any | `number | `string | `integer | `boolean | `real | `variable | `variable-not-otherwise-mentioned
;; var  := symbol?
;; nt   := symbol?
;; pat  := `(nt ,var) | `(list ,pat ...)  | atom | `(name ,var ,pat) | `(mismatch-name ,name ,pat)
;; pat* := `(nt ,var) | `(list ,pat* ...) | atom | `(name ,var ,(bound)) |
;;         `(cstr (,nt ...) ,pat*)  -- no nested cstrs | `(mismatch-name ,name ,pat*)
;;
;; Unification takes a language, two pats, and an environment,
;; and produces a pat* and and environment (p*e) or fails.
;; Unify calls bind-names to convert a pat into a pat*, and unify*
;; does the actual work of unifying two pat*'s.
;; A pat* doesn't have any meaning unless it is paired with an environment - all
;; names in a pat* are bound, and a pat* may have constraints (cstrs) on subpatterns.


(struct lvar (id) #:prefab)

(struct uninstantiated () #:prefab)

(struct bound () #:prefab)

(struct p*e (p e) #:transparent)

(struct env (eqs dqs) #:transparent)
(struct dq (params dq) #:transparent)
(define empty-env (env (hash) '()))

(struct unif-fail () #:transparent)

(define (not-failed? maybe-failed)
  (not (unif-fail? maybe-failed)))

(define-syntax (and/fail stx)
  (syntax-case stx ()
    [(_ conds ... res)
     #'(if (and conds ...)
           res
           (unif-fail))]))

(define predef-pats (set 'any 'number 'string 'integer 'boolean 'real 'variable 'natural 'variable-not-otherwise-mentioned))
(define (predef-pat? a)
  (or (set-member? predef-pats a)
      (match a
        [`(variable-except ,_ ...) #t]
        [`(variable-prefix ,_) #t]
        [_ #f])))
(define (var? s)
  (symbol? s))
(define (n-t? s)
  (symbol? s))
(define (pat-or-pat*? is-pat*? p)
  (let loop ([p p])
    (extract-clauses
     (match-a-pattern #:allow-else p
       [`any #t]
       [`number #t]
       [`string #t]
       [`natural #t]
       [`integer #t]
       [`real #t]
       [`boolean #t]
       [`variable #t]
       [`(variable-except ,vars ...) #t]
       [`(variable-prefix ,pfx) #t]
       [`variable-not-otherwise-mentioned #t]
       [`hole #f]
       [`(nt ,(? n-t? n))
        #t]
       [`(name ,(? var? name) ,p)
        (if is-pat*?
            (bound? p)
            (loop p))]
       [`(mismatch-name ,name ,p)
        (loop p)]
       [`(in-hole ,p1 ,p2) #f]
       [`(hide-hole ,p) #f]
       [`(side-condition ,pat ,condition ,srcloc-expr) #f]
       [`(cross ,var) #f]
       [`(list ,ps ...)
        (for/and ([p ps]) (loop p))]
       [(? (compose not pair?))
        (or (symbol? p) (number? p) (string? p) (boolean? p))]
       [_ 
        (and is-pat*?
             (match p
               [`(cstr (,nts ...) ,p*)
                (and (for/and ([n nts]) (n-t? n))
                     (loop p*))]
               [_ #f]))]))))

(define (pat? p) (pat-or-pat*? #f p))
(define (pat*? p) (pat-or-pat*? #t p))
(define pat*-clause-p?s (append (list
                                 (let ([bound-name? (λ (p) (match p [`(name ,id ,(bound)) #t] [_ #f]))])
                                   bound-name?)
                                 (let ([cstr? (λ (p) (match p [`(cstr (,nts ...) ,pat) #t] [_ #f]))])
                                   cstr?))
                                (extracted-clauses->fns)))

;; used to track new equations for disunification
(define new-eqs (make-parameter (make-hash)))

;; (hash/c id (listof pat*))
;; disqequations generated (by mismatch-pattern) during a given unification pass
(define dqs-found (make-parameter (make-hash)))

(define-syntax-rule
  (maybe-let ([x e]) body ...)
  (let ([val e])
    (match val
      [(unif-fail) val]
      [_ (let ([x val]) body ...)])))

(define-syntax maybe-let*
  (syntax-rules ()
    [(maybe-let* () body ...) (let () body ...)]
    [(maybe-let* ([x x-e] [y e] ...) body ...)
     (maybe-let ([x x-e])
       (maybe-let* ([y e] ...) body ...))]))

(define-syntax-rule
  (p*e-eqs x)
  (env-eqs (p*e-e x)))
      
;; pat pat env lang -> (or/c p*e? unif-fail?)
(define (unify t u e L)
  (parameterize ([dqs-found (make-hash)])
    (maybe-let* ([t* (bind-names t e L)]
                 [u* (bind-names u (p*e-e t*) L)]
                 [res (unify* (p*e-p t*) (p*e-p u*) (p*e-e u*) L)])
                (define eqs (p*e-eqs res))
                (define res-pat (p*e-p res))
                (define found-pre-dqs (apply set-union (set) 
                                             (for/list ([dq-sides/id (hash-values (dqs-found))])
                                               (list->dq-pairs dq-sides/id))))
                (maybe-let* ([found-dqs (for/fold ([fdqs '()])
                                          ([pdq (in-set found-pre-dqs)])
                                          (maybe-let* ([fdqs fdqs])
                                                      (define new-dq (disunify* '() (first pdq) (second pdq) eqs L))
                                                      (and/fail new-dq
                                                                (cons new-dq fdqs))))])
                            (define real-dqs (filter (λ (dq) (not (boolean? dq))) found-dqs))
                            (define new-dqs (check-and-resimplify eqs (append real-dqs (env-dqs e)) L))
                            (and/fail new-dqs
                                      (p*e res-pat (env eqs new-dqs)))))))

(define (list->dq-pairs dq-sides)
  (cond
    [(or (empty? dq-sides) 
         (equal? '() (cdr dq-sides)))
     (set)]
    [else
     (set-union (for/set ([rhs (cdr dq-sides)])
                         (list (car dq-sides) rhs))
                (list->dq-pairs (cdr dq-sides)))]))

;; pat pat env lang -> (or/c env boolean?)
(define (disunify params t u e L)
  (parameterize ([new-eqs (make-hash)])
    (define t*e (bind-names t e L))
    (cond 
      [(unif-fail? t*e) e]
      [else
       (define u*e (bind-names u (p*e-e t*e) L))
       (cond
         [(unif-fail? u*e) e]
         [else
          (define new-dq (disunify* params (p*e-p t*e) (p*e-p u*e) (env-eqs (p*e-e u*e)) L))
          (match new-dq
            [#f #f]
            [#t
             (env (env-eqs (p*e-e u*e))
                  (env-dqs e))]
            [_
             (env (env-eqs (p*e-e u*e))
                  (cons new-dq
                        (env-dqs e)))])])])))

(define base-dq `((list) (list)))

(define (remove-empty-dqs dqs)
  (filter (λ (dq) (not (equal? base-dq dq)))
          dqs))

(define (extend-dq new-eqs ineq0 eqs)
  (for/fold ([ineq ineq0])
    ([(k v) (in-hash new-eqs)])
    (match ineq
      [`((list ,vars ...) (list ,terms ...))
       (match* (k v)
         [((lvar id-l) (lvar id-r))
          `((list ,@vars (name ,id-l ,(bound))) 
            (list ,@terms ,(resolve-no-nts/var v eqs)))]
         [((lvar id-l) pat*-r)
          `((list ,@vars (name ,id-l ,(bound))) 
            (list ,@terms ,(resolve-no-nts/pat v eqs)))])])))

(define (resolve-no-nts/var lv eqs)
  (define-values (rep pat _) (lookup (lvar-id lv) eqs))
  (if (not (groundable? pat))
      `(name ,(lvar-id rep) ,(bound))
      (resolve-no-nts/pat pat eqs)))

(define (resolve-no-nts/pat pat eqs)
  (let recur ([p pat])
        (match p
          [`(name ,id ,(bound))
           (resolve-no-nts/var (lvar id) eqs)]
          [`(list ,ps ...)
           `(list ,@(for/list ([p ps]) (recur p)))]
          [`(cstr (,cs ...) p)
           (recur p)]
          [_
           (unless (groundable? p)
             (error resolve-no-nts/pat 
                    "non-groundable pat at internal pattern position: ~s" p))
           p])))


(define (groundable? p)
  (match p
    [`(nt ,_) #f]
    [(? predef-pat? _) #f]
    [`(cstr ,_ ,p)
     (groundable? p)]
    [_ #t]))                                   
                                           

;; eqs dqs -> dqs or #f
;; simplified - first element in lhs of all inequations is a var not occuring in lhs of eqns
(define (check-and-resimplify eqs dqs L)
  (define-values (dqs-notok dqs-ok) 
    (partition (λ (a-dq)
                 (hash-has-key? 
                  eqs
                  (lvar (match a-dq
                          [(dq ps `((list (name ,v1 ,(bound)) ,vs ...) (list ,t1 ,ts ...)))
                           v1]))))
               (remove-empty-dqs dqs)))
  (let loop ([ok dqs-ok]
             [notok dqs-notok])
    (cond
      [(empty? notok)
       ok]
      [else
       (match notok
         [`(,(dq ps `(,vars-p* ,term-p*)) ,rest ...)
          (let ([new-dq (disunify* ps vars-p* term-p* eqs L)])
            (and new-dq
                 (match new-dq
                   [#t (loop ok rest)]
                   [_ (loop (cons new-dq ok) rest)])))])])))

;; params pat* pat* eqs lang -> boolean or dq
(define (disunify* params u* t* eqs L)
  (parameterize ([new-eqs (make-hash)])
    (define res (unify* u* t* (env eqs '()) L))
    (cond
      [(unif-fail? res) #t]
      [(empty? (hash-keys (new-eqs))) #f]
      [else
       (define-values (new-ps new-dq)
         (param-elim params (extend-dq (new-eqs) base-dq (env-eqs (p*e-e res)))))
       (match new-dq
         [`((list) (list))
          #f]
         [`((list (name, var-ids ,(bound)) ...) (list ,pats ...))
          ;; check to see if parameter eliminations exposed some
          ;; equivalences...
          (and
           (or (equal? (length params) (length new-ps))
               (for/and ([v (in-list var-ids)]
                         [p (in-list pats)])
                 (or (not (hash-has-key? eqs (lvar v)))
                     (not (equal? (resolve-no-nts/pat `(name ,v ,(bound)) eqs)
                                  p)))))
           (dq new-ps new-dq))])])))


(define (param-elim params unquantified-dq)             
  (let loop ([dq-rest unquantified-dq]                                             
             [ps params]             
             [new-dq-l '()]         
             [new-dq-r '()]
             [lhs-ps (hash)])
    (match dq-rest                   
      ['((list) (list))
       (define-values
         (ndql ndqr nps)
         (for/fold ([ndql new-dq-l] [ndqr new-dq-r] [nps ps])
           ([(p lhss) (in-hash lhs-ps)])
           (if ((length lhss) . > . 1)
               (values (foldr cons ndql lhss)
                       (foldr cons ndqr (build-list (length lhss) 
                                                    (λ (_) p)))
                       nps)
               (values ndql 
                       ndqr 
                       (remove (list-ref p 1) nps)))))
       (values nps `((list ,@ndql) (list ,@ndqr)))]       
      [`((list (name ,v1,(bound)) ,vs ...) (list ,t1 ,ts ...))    
       (cond                         
         [(member v1 params)
          (loop `((list ,@vs) (list ,@ts))          
                (remove v1 ps)                      
                new-dq-l                            
                new-dq-r
                lhs-ps)]
         [(match t1
            [`(name ,tn ,(bound)) (member tn ps)]
            [_ #f])
          (loop `((list ,@vs) (list ,@ts))
                ps
                new-dq-l
                new-dq-r
                (hash-set lhs-ps t1 (cons `(name ,v1 ,(bound)) 
                                          (hash-ref lhs-ps t1 '()))))]
         [else                       
          (loop `((list ,@vs) (list ,@ts))          
                ps                                  
                (cons `(name ,v1 ,(bound)) new-dq-l)
                (cons t1 new-dq-r)
                lhs-ps)])]))) 


;; refactoring bind-names to remove set!'s (with the idea of enabling optimizations)
;; seems to have no effect on performance

;; pat env lang -> p*e or (unif-fail)
(define (bind-names pat e L)
  (define eqs (env-eqs e))
  (define dqs (env-dqs e))
  (define res-pat
    (let loop ([pat pat])
      (match pat
        [`(name ,name ,(bound))
         (error 'bind-names "pat*, not a pat: ~s" pat)]
        [`(name ,name ,pat)
         (maybe-let ([b-pat (loop pat)])
                    (let recur ([id name])
                      (define res (hash-ref eqs (lvar id) (uninstantiated)))
                      (match res
                        [(uninstantiated)
                         (when (equal? b-pat (bound))
                           (error 'bind-names "tried to set something to bound"))
                         (and/fail (not (occurs?* id b-pat eqs L))
                                   (set! eqs (hash-set eqs (lvar id) b-pat))
                                   ;; here we only bind to things in the local pattern
                                   ;; so don't update new-eqs
                                   `(name ,name ,(bound)))]
                        [(lvar new-id)
                         (define next (recur new-id))
                         (match next
                           [`(name ,id-new ,(bound))
                            (unless (eq? id id-new)
                              ;; path compression: don't update new-eqs here
                              (set! eqs (hash-set eqs (lvar id) (lvar id-new))))]
                           [_ (void)])
                         next]
                        [_
                         (maybe-let ([u-res (unify-update* id b-pat res (env eqs #f) L)])
                                    (set! eqs (p*e-eqs u-res))
                                    `(name ,id ,(bound)))])))]
        [`(list ,pats ...)
         (let/ec fail
           `(list ,@(for/list ([p pats])
                      (define res (loop p))
                      (if (not-failed? res)
                          res
                          (fail (unif-fail))))))]
        [`(variable-not-in ,p ,s)
         (maybe-let* ([pat (loop p)]
                      [s-pat (loop s)])
                     `(variable-not-in ,pat ,s-pat))]
        [`(mismatch-name ,name ,p)
         (maybe-let ([b-pat (loop p)])
                    `(mismatch-name ,name ,b-pat))]
        [_ pat])))
  (maybe-let ([res-pat res-pat])
             (p*e res-pat (env eqs dqs))))


;; unify* : pat* pat* env lang -> p*e? or unif-fail?
(define (unify* t0 u0 e0 L)
  (define t*e (resolve t0 e0))
  (define u*e (resolve u0 (p*e-e t*e)))
  (define t (p*e-p t*e))
  (define u (p*e-p u*e))
  (define e (p*e-e u*e))
  #2dmatch
  ╔═════════════════╦═════════════════╦═════════════╦═══════════════╦═══════════╦══════╦════════════╦══════════════╦═══════════════════╦═════════╦══════════╦══════════════╦═════════════╗
  ║            u    ║ `(mismatch-name ║ `(name      ║ `(cstr        ║`(nt ,n-u) ║`any  ║ (? num-ty?)║`(list        ║ (? pvar?)         ║ `string ║ `boolean ║ (? base-ty?) ║(? not-pair?)║
  ║                 ║   ,u-name       ║   ,name-u   ║   (,nts1 ...) ║           ║      ║            ║  ,us ...)    ║                   ║         ║          ║              ║             ║
  ║  t              ║   ,u-pat)       ║   ,(bound)) ║   ,p1)        ║           ║      ║            ║              ║                   ║         ║          ║              ║             ║
  ╠═════════════════╬═════════════════╩═════════════╩═══════════════╩═══════════╩══════╩════════════╩══════════════╩═══════════════════╩═════════╩══════════╩══════════════╩═════════════╣
  ║`(mismatch-name  ║ (hash-set! (dqs-found) t-name (cons u (hash-ref (dqs-found) t-name (λ () '()))))                                                                                   ║
  ║  ,t-name        ║ (unify* t-pat u e L)                                                                                                                                               ║
  ║  ,t-pat)        ║                                                                                                                                                                    ║
  ╠═════════════════╬═════════════════╦══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║`(name ,name-t   ║                 ║ (instantiate* name-t u e L)                                                                                                                      ║
  ║       ,(bound)) ║                 ║                                                                                                                                                  ║
  ╠═════════════════╣                 ╚═════════════╦═══════════════╦════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║`(cstr           ║                               ║(u*-2cstrs     ║ (u*-1cstr nts2 p2 u e L)                                                                                           ║
  ║  (,nts2 ...)    ║                               ║  nts1 p1      ║                                                                                                                    ║
  ║  ,p2)           ║                               ║  nts2 p2 e L) ║                                                                                                                    ║
  ╠═════════════════╣                               ╚═══════════════╬═══════════╦════════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║ `(nt ,n-t)      ║                                               ║(u*-2nts   ║ (u*-1nt n-t u e L)                                                                                     ║
  ║                 ║                                               ║ n-t n-u   ║                                                                                                        ║
  ║                 ║                                               ║ e L)      ║                                                                                                        ║
  ╠═════════════════╣                                               ╚═══════════╬════════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║  `any           ║                                                           ║ (p*e u e)                                                                                              ║
  ║                 ║                                                           ║                                                                                                        ║
  ╠═════════════════╣                                                           ╚══════╦════════════╦══════════════════════════════════════════════════════════════════════╦═════════════╣
  ║  (? num-ty?)    ║                                                                  ║(u*-2nums   ║                                                                      ║             ║
  ║                 ║                                                                  ║  t u e)    ║                                                                      ║             ║
  ╠═════════════════╣                                                                  ╚════════════╬══════════════╗                                                       ║             ║
  ║ `(list ,ts ...) ║                                                                               ║(u*-2lsts     ║                                (unif-fail)            ║             ║
  ║                 ║                                                                               ║ ts us e L)   ║                                                       ║(u*-matches? ║
  ╠═════════════════╣                                                                               ╚══════════════╬═══════════════════╗                                   ║  t u        ║
  ║ (? pvar?)       ║                                                                                              ║(u*-2pvars u t e L)║                                   ║  e L)       ║
  ╠═════════════════╣                                                                                              ╚═══════════════════╬═════════╗                         ║             ║
  ║ `string         ║                   (unify* u t e L)                                                                               ║(p*e t e)║                         ║             ║
  ╠═════════════════╣                                                                                                                  ╚═════════╬══════════╗              ║             ║
  ║ `boolean        ║                                                                                                                            ║(p*e t e) ║              ║             ║
  ╠═════════════════╣                                                                                                                            ╚══════════╬══════════════╣             ║
  ║ (? base-ty?)    ║                                                                                                                                       ║   (p*e t e)  ║             ║
  ╠═════════════════╣                                                                                                                                       ╚══════════════╬═════════════╣
  ║ (? not-pair?)   ║                                                                                                                                                      ║(and/fail    ║
  ║                 ║                                                                                                                                                      ║ (equal? t u)║
  ║                 ║                                                                                                                                                      ║ (p*e t e))  ║
  ╚═════════════════╩══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╩═════════════╝)

(define (pvar? x) (or (vnom? x)
                      (var-pref? x)
                      (var-exc? x)
                      (vni? x)
                      (equal? 'variable x)))
(define (vnom? x) (equal? x 'variable-not-otherwise-mentioned))
(define (var-pref? x) (match x
                        [`(variable-prefix ,p) #t]
                        [_ #f]))
(define (var-exc? x) (match x
                       [`(variable-except ,p ...) #t]
                       [_ #f]))
(define (not-pair? x) (not (pair? x)))
(define vni?
  (match-lambda [`(variable-not-in ,e ,s) #t]
                [_ #f]))

;; pvar? pvar? env lang -> (or/c p*e? unif-fail?)
(define (u*-2pvars v1 v2 e L)
  (define res
    (begin
    #2dmatch
    ╔══════════════════════════╦══════════════════════════╦════════════════════════════════╦═══════════════════════════════╦════════════════╦══════════════════════════════════════╗
    ║                   v1     ║`(variable-prefix ,p1)    ║ `(variable-except ,e1 ... )    ║    (? vnom?)                  ║   `variable    ║`(variable-not-in ,e1 ,s)             ║       
    ║ v2                       ║                          ║                                ║                               ║                ║                                      ║
    ╠══════════════════════════╬══════════════════════════╬════════════════════════════════╬═══════════════════════════════╬════════════════╬══════════════════════════════════════╣
    ║                          ║ (cond                    ║ (and/fail                      ║(u*-2pvars                     ║                ║(and/fail                             ║
    ║ `(variable-prefix ,p2)   ║  [(sym-pref? p1 p2)      ║  (not (ormap                   ║ v2                            ║                ║  (sym-pref? p2 s)                    ║
    ║                          ║   `(variable-prefix ,p2)]║        (curry sym-pref? p2)    ║ `(variable-except             ║                ║  v1)                                 ║
    ║                          ║  [(sym-pref? p2 p1)      ║        e1))                    ║ ,@(compiled-lang-literals L)) ║                ║                                      ║
    ║                          ║   `(variable-prefix ,p1)]║  v2)                           ║ e L)                          ║                ║                                      ║
    ║                          ║  [else (unif-fail)])     ║                                ║                               ║                ║                                      ║
    ╠══════════════════════════╬══════════════════════════╬════════════════════════════════╣                               ║                ╠══════════════════════════════════════╣
    ║                          ║                          ║ `(variable-except              ║                               ║                ║`(variable-not-in                     ║
    ║  `(variable-except       ║                          ║   ,@(de-dupe/sorted            ║                               ║       v2       ║   (list ,e1 ,@e2)                    ║
    ║     ,e2 ...)             ║                          ║      (merge/sorted e1 e2)))    ║                               ║                ║   ,s)                                ║
    ║                          ║                          ║                                ║                               ║                ║                                      ║
    ╠══════════════════════════╣                          ╚════════════════════════════════╬═══════════════════════════════╣                ╠══════════════════════════════════════╣
    ║                          ║                                                           ║                               ║                ║(u*-2pvars v1                         ║
    ║  (? vnom?)               ║                                                           ║              v1               ║                ║`(variable-except                     ║
    ║                          ║                                                           ║                               ║                ║  ,@(compiled-lang-literals L))       ║
    ║                          ║                                                           ║                               ║                ║ e L)                                 ║
    ╠══════════════════════════╣               (u*-2pvars v2 v1 e L)                       ╚═══════════════════════════════╣                ╠══════════════════════════════════════╣
    ║                          ║                                                                                           ║                ║                                      ║
    ║ `variable                ║                                                                                           ║                ║              v1                      ║  
    ║                          ║                                                                                           ║                ║                                      ║
    ╠══════════════════════════╣                                                                                           ╚════════════════╬══════════════════════════════════════╣
    ║                          ║                                                                                                            ║                                      ║
    ║ `(variable-not-in ,e2 ,t)║                                                                                                            ║     (2-vnis v1 v2 e L)               ║  
    ║                          ║                                                                                                            ║                                      ║
    ╚══════════════════════════╩════════════════════════════════════════════════════════════════════════════════════════════════════════════╩══════════════════════════════════════╝))
  (match res
    [(unif-fail) res]
    [(p*e _ _) res]
    [_ (p*e res e)]))
  

(define (2-vnis v1 v2 e L)
  (match-define `(variable-not-in ,e1 ,s1) v1)
  (match-define `(variable-not-in ,e2 ,s2) v2)
  (cond
    [(not (and (symbol? s1) (symbol? s2)))
     (maybe-let ([s-res (unify* s1 s2 e L)])
       (p*e `(variable-not-in (list ,e1 ,e2) ,(p*e-p s-res))
            (p*e-e s-res)))]
    [(sym-pref? s1 s2)
     `(variable-not-in (list ,e1 ,e2) s2)]
    [(sym-pref? s2 s1)
     `(variable-not-in (list ,e1 ,e2) s1)]
    [else (unif-fail)])) 

(define (sym-pref? sp s)
  (regexp-match
   (string-append "^" (symbol->string sp) ".*$")
   (symbol->string s)))

(define (u*-2cstrs nts1 p1 nts2 p2 e L)
  (maybe-let ([res (unify* p1 p2 e L)])
    (define res-p (p*e-p res))
    (define new-nts (merge-ids/sorted nts1 nts2 L))
    (and/fail new-nts
              (when (lvar? res-p)
                (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" p1 p2 e))
              (p*e `(cstr ,new-nts ,res-p) (p*e-e res)))))

(define (u*-1cstr nts p u e L)
  (maybe-let ([res (unify* p u e L)])
             (define res-p (p*e-p res))
             (define res-e (p*e-e res))
             (match res-p
               [(lvar id)
                (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" p u e)]
               [`(nt ,nt)
                (define new-nts (merge-ids/sorted (list nt) nts L))
                (and/fail new-nts
                          (p*e `(cstr ,new-nts ,p) res-e))]
               [`(cstr ,nts2 ,new-p)
                (define new-nts (merge-ids/sorted nts nts2 L))
                (and/fail new-nts
                          (p*e `(cstr ,new-nts ,new-p) res-e))]
               [_
                (and/fail (for/and ([n nts]) (check-nt n L res-p e))
                          (p*e `(cstr ,nts ,res-p) res-e))])))
  
(define (u*-2nts n-t n-u e L)
  (if (equal? n-t n-u)
      (p*e `(nt ,n-t) e)
      (u*-1nt n-t `(nt ,n-u) e L)))

(define (u*-1nt p u e L)
  (and/fail
   (check-nt p L u e)
   (if (hash-has-key? (compiled-lang-collapsible-nts L) p)
       (maybe-let ([bn-res (bind-names (fresh-pat-vars (hash-ref (compiled-lang-collapsible-nts L) p) (make-hash)) e L)])
         (unify* (p*e-p bn-res) u (p*e-e bn-res) L))
       ;; removed a unification of u with itself here
       ;; (the reason for which was mysterious)
       (p*e `(cstr (,p) ,u) e))))
  
(define (u*-2lsts ts us e L)
  (and/fail (= (length ts) (length us))
            (maybe-let ([res-ps-e
                         (let loop ([ts ts]
                                    [us us]
                                    [e e]
                                    [rs '()])
                           (if (empty? ts)
                               (cons rs e)
                               (maybe-let ([res (unify* (car ts) (car us) e L)])
                                          (loop (cdr ts)
                                                (cdr us)
                                                (p*e-e res)
                                                (cons (p*e-p res) rs)))))])
                 (p*e `(list ,@(reverse (car res-ps-e)))
                      (cdr res-ps-e)))))

(define (u*-2nums t u e)
  (p*e
   (cond 
     [(number-superset? t u) u]
     [(number-superset? u t) t])
   e))

(define (u*-matches? t u e L)
  (maybe-let ([pat-res
               (match* (t u)
                 [((? num-ty? t) _)
                  (and/fail ((number-pred t) u)
                            u)]
                 [(`variable-not-otherwise-mentioned (? symbol? s))
                  (and/fail (not (memq s (compiled-lang-literals L)))
                            (not (base-ty? s))
                            s)]
                 [(`(variable-except ,ss ...) (? symbol? s))
                  (and/fail (not (memq s ss))
                            u)]
                 [(`(variable-prefix ,pref) (? symbol? s))
                  (and/fail (regexp-match 
                             (regexp (string-append "^" (symbol->string pref) ".*$"))
                             (symbol->string s))
                            u)]           
                 [(`variable (? symbol? s))
                  (and/fail (not (base-ty? s))
                            s)]
                 [(`string (? string? s))
                  s]
                 [(`boolean (? boolean? b))
                  b]
                 [(_ _) (unif-fail)])])
             (p*e pat-res e)))
    
(define (resolve pat e)
  (define e-eqs (env-eqs e))
  (define-values (res-pat eqz)
    (let loop ([pat pat]
               [eqs e-eqs])
      (match pat
        [`(name ,id ,(bound))
         (define-values (rep eqs2) (lookup-rep id eqs))
         (define id-rep (lvar-id rep))
         (match (hash-ref eqs2 (lvar id-rep))
           [`(name ,next-id ,(bound))
            (loop `(name ,next-id ,(bound)) (hash-set eqs2 (lvar id) (lvar next-id)))]
           [_
            (values `(name ,id-rep ,(bound)) eqs2)])]
        [_ (values pat eqs)])))
  (p*e res-pat (struct-copy env e [eqs eqz])))

;; id pat* pat* env lang -> p*e? or unif-fail?
(define (unify-update* id pat-1 pat-2 e L)
  (maybe-let ([u-res (unify* pat-1 pat-2 e L)])
   (define res-e (p*e-e u-res))
   (define res-eqs (env-eqs res-e))
   (define res-p (p*e-p u-res))
   (and/fail (not (occurs?* id pat-1 res-eqs L))
             (not (occurs?* id pat-2 res-eqs L))
             (when (equal? u-res (bound)) 
               (error 'update "tried to set something to bound"))
             (unless (equal? u-res (hash-ref res-eqs (lvar id) (uninstantiated)))
               (unless (or (nt-only-pat? res-p)
                           (ground-pat-eq? pat-1 pat-2))
                 (hash-set! (new-eqs) (lvar id) res-p)))
             (p*e res-p (struct-copy env res-e [eqs (hash-set res-eqs (lvar id) res-p)])))))

(define (nt-only-pat? p*)
  (match p*
    [`(nt ,_)
      #t]
    [`(cstr (,_ ...) ,p2)
     (nt-only-pat? p2)]
    ;; only put things in disequations that are constraints between lvars
    ;; or actual values for lvars
    [(? (λ (s) (predef-pat? s)))
     p*]
    [_
     #f]))

;; occurs* : name (pat* or lvar] -> bool
;; pat* --> path compression has been added by bind-names, so
;; we only need to look at the id in name, and the pattern it is bound to
;; TODO: replace name in p*'s with lvar - this is the most obvious of many 
;; functions that would be improved by this
(define (occurs?* name p eqs L)
  (match p
    [`(name ,name-p ,(bound))
     (or (eq? name name-p)
         (occurs?* name (hash-ref eqs (lvar name-p) (uninstantiated)) eqs L))]
    [`(list ,ps ...)
     (for/or ([p ps])
       (occurs?* name p eqs L))]
    [`(cstr (,nts ...) ,pat)
     (occurs?* name pat eqs L)]
    [(lvar id)
     (or (eq? name id)
         (occurs?* name (hash-ref eqs (lvar id) (uninstantiated)) eqs L))]
    [`(cstr ,(lvar _))
     (error 'occurs?* "rogue lvar: ~s\n" p)]
    [_ #f]))

(define (instantiate* id pat e L)
  (define-values (p eqs2) (lookup-pat id (env-eqs e)))
  (define id-res (resolve p (struct-copy env e [eqs eqs2])))
  (define e-res (p*e-e id-res))
  (define id-pat (p*e-p id-res))
  (define e-eqs (p*e-eqs id-res))
  (match* (id-pat pat)
    [(`(name ,next-id ,(bound)) _)
     (maybe-let ([n-res (instantiate* next-id pat e-res L)])
        (define n-eqs (p*e-eqs n-res))
        (and/fail (not (occurs?* id (lvar next-id) n-eqs L))
                  (p*e `(name ,next-id ,(bound))
                       (struct-copy env e
                                    [eqs (hash-set n-eqs (lvar id) (lvar next-id))]))))]
    [(_ `(name ,id-2 ,(bound)))
     (cond 
       [(eq? id id-2) (p*e pat e)]
       [else
        (define-values (p2 eqsp2) (lookup-pat id-2 e-eqs))
        (define id-2-res (resolve p2 (struct-copy env e-res [eqs eqsp2])))
        (define id-2-pat (p*e-p id-2-res))
        (maybe-let ([res (unify-update* id id-pat id-2-pat (p*e-e id-2-res) L)])
          (define e-res (p*e-e res))
          (and/fail (not (occurs?* id-2 (lvar id) (env-eqs e-res) L))
                    (unless (ground-pat-eq? id-pat id-2-pat)
                      (hash-set! (new-eqs) (lvar id-2) (lvar id)))
                    (p*e `(name ,id ,(bound))
                         (struct-copy env e
                                      [eqs (hash-set (env-eqs e-res) (lvar id-2) (lvar id))]))))])]
    [(_ _)
     (maybe-let ([res (unify-update* id id-pat pat e L)])
       (p*e `(name ,id ,(bound)) (p*e-e res)))]))

;; we want to consider ground pats that are equal
;; modulo constraints as equal when disunifying (see uses)
(define (ground-pat-eq? p* u*)
  (match* (p* u*)
    [(`(cstr (,pnts ...) ,p-next) u)
     (ground-pat-eq? p-next u)]
    [(p `(cstr (,unts ...) ,u-next))
     (ground-pat-eq? p u-next)]
    [(`(list ,ps ...) `(list ,us ...))
     (and (equal? (length ps) (length us))
          (for/and ([p ps] [u us]) 
            (ground-pat-eq? p u)))]
    [(_ `(name ,id ,(bound)))
     #f]
    [(`(name ,id ,(bound)) _)
     #f]
    [(_ `(nt ,u-nt))
     #f]
    [(`(nt ,p-nt) _)
     #f]
    [(_ (? predef-pat? u))
     #f]
    [((? predef-pat? p) _)
     #f]
    [(_ _)
     (equal? p* u*)]))

(define (merge-ids/sorted l1 l2 lang)
  (and (for*/and ([nt1 l1] [nt2 l2])
         (check-nt nt1 lang `(nt ,nt2) empty-env))
       (de-dupe/sorted (merge/sorted l1 l2))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (merge/sorted ids1 ids2)
  (cond [(empty? ids1) ids2]
        [(empty? ids2) ids1]
        [(symbol<? (car ids1) (car ids2))
         (cons (car ids1) (merge/sorted (cdr ids1) ids2))]
        [else
         (cons (car ids2) (merge/sorted ids1 (cdr ids2)))]))

(define (de-dupe/sorted lst)
  (match lst
    ['() '()]
    [`(,a) `(,a)]
    [`(,a ,a ,rest ...)
     (de-dupe/sorted (cons a rest))]
    [`(,a ,b ,rest ...)
     (cons a (de-dupe/sorted (cons b rest)))]))

(define (num-ty? symbol)
  (member symbol
          '(number real integer natural)))

(define npreds (hash 'number number?
                     'real real?
                     'integer integer?
                     'natural (λ (n) (and (integer? n)
                                           (>= n 0)))))
(define (number-pred symbol)
  (hash-ref npreds symbol))

(define (number-superset? super sub)
  (define nums '(any number real integer natural))
  (>= (length (member super nums))
      (length (member sub nums))))

(define (base-ty? symbol)
  (member symbol
          '(any number string natural integer real boolean
                variable variable-not-otherwise-mentioned)))

(define (lookup-pat id eqs)
  (define-values (_ pat new-eqs) (lookup id eqs))
  (values pat new-eqs))

(define (lookup-rep id eqs)
  (define-values (rep _ new-eqs) (lookup id eqs))
  (values rep new-eqs))

(define (lookup id eqs)
  (let loop ([id id])
    (define res (hash-ref eqs (lvar id) 
                          (λ () (error 'lookup "unbound var: ~a" id))))
    (match res
      [(lvar new-id)
       (define-values (rep pat new-eqs) (loop new-id))
       (values rep pat (hash-set new-eqs (lvar id) rep))]
      [_
       (values (lvar id) res eqs)])))


(define unsupported-pat-err-name (make-parameter #f))

(define (unsupported-pat-err pat)
  (unless (unsupported-pat-err-name)
    (redex-error 'unsupported-pat-err-name "not set before derivation generation"))
  (redex-error (unsupported-pat-err-name)
               (string-append "generation failed at unsupported pattern;\n"
                              " (#:satisfying keyword does not support ellipses, contexts, side-conditions, or unquote)\n"
                              "  pattern: ~a")
               pat))

(define normalizing-nt (make-parameter #f))

(define check-nt
  (let ([memo (make-hash)])
    (λ (nt clang pat e)
      (parameterize ([normalizing-nt nt])
        (define npat (normalize-pat clang e pat))
        (hash-ref memo (list nt clang npat)
                  (λ ()
                    (define pat-ok? 
                      (for/or ([ntp (in-list (map (((curry normalize-pat) clang) e) (nt-pats nt clang)))])
                        (not-failed? (unify* npat ntp empty-env clang))))
                    (hash-set! memo (list nt clang npat) pat-ok?)
                    pat-ok?))))))

(define (normalize-pat lang e pat)
  (define err unsupported-pat-err)
  (let loop ([pat pat]
             [depth 5])
    (cond
      [(depth . <= . 0) 'any]
      [else
       (match-a-pattern #:allow-else pat
                        [`any pat]
                        [`number pat]
                        [`string pat]
                        [`natural pat]
                        [`integer pat]
                        [`real pat]
                        [`boolean pat]
                        [`variable pat]
                        [`(variable-except ,s ...) pat]
                        [`(variable-prefix ,s) pat]
                        [`variable-not-otherwise-mentioned pat]
                        [`hole (err pat)]
                        [`(nt ,id)
                         (cond 
                           [(and (normalizing-nt)
                                 (eq? (normalizing-nt) id)) 'any]
                           [(hash-has-key? (compiled-lang-collapsible-nts lang) id)
                            (loop (hash-ref (compiled-lang-collapsible-nts lang) id) (sub1 depth))]
                           [else pat])]
                        [`(name ,name ,npat)
                         (if (bound? npat)
                             (match-let ([(p*e `(name ,n ,(bound)) e-new) (resolve pat e)])
                               (loop (hash-ref (env-eqs e) (lvar n)) depth))
                             `(name ,name ,(loop npat (sub1 depth))))]
                        [`(mismatch-name ,name ,pat) (loop pat (sub1 depth))]
                        [`(in-hole ,p1 ,p2) (err pat)]
                        [`(hide-hole ,p) (loop p (sub1 depth))]
                        [`(side-condition ,p ,g ,e)
                         (err pat)]
                        [`(cross ,s) (err pat)]
                        [`(list ,sub-pats ...)
                         `(list ,@(for/list ([sub-pat (in-list sub-pats)])
                                    (match sub-pat
                                      [`(repeat ,pat ,name ,mismatch)
                                       (err sub-pat)]
                                      [_
                                       (loop sub-pat (sub1 depth))])))]
                        [(? (compose not pair?)) 
                         pat]
                        [_
                         (match pat
                           [`(cstr ,cs ,p)
                            (loop p (sub1 depth))]
                           [`(variable-not-in ,p ,s)
                            `variable])])])))

(define (nt-pats nt lang)
  (define this-rhs
    (nt-rhs
     (let ([the-nt (findf (λ (lang-nt)
                            (equal? nt (nt-name lang-nt)))
                          (compiled-lang-lang lang))])
       (unless the-nt
         (error 'unify "nonterminal ~a not found for provided language... nts found: ~a" 
                nt (hash-keys (compiled-lang-nt-map lang))))
       the-nt)))
  (match this-rhs
    [(list (rhs `(nt ,alias)))
     (nt-pats alias lang)]
    [_
     (map rhs-pattern this-rhs)]))
       
(define empty-lang
  (compiled-lang
   #f #f #f #f #f #f #f #f #f #f '() #f (hash)
   (lang-enumerators '() (delay '()))))

(define unique-name-nums (make-parameter 0))

;; TODO: compare with free-identifier=? so renaming is safe
;; w/r/t macro expansion
;; (use free-id-table)
(define (fresh-pat-vars pre-pat instantiations)
  (match pre-pat
    [`(name ,id ,pat)
     (define new-id (hash-ref instantiations id
                              (λ ()
                                (define unique-id (make-uid id))
                                (hash-set! instantiations id unique-id)
                                unique-id)))
     `(name ,new-id ,(fresh-pat-vars pat instantiations))]
    [`(list ,pats ...)
     `(list ,@(for/list ([p pats]) (fresh-pat-vars p instantiations)))]
    [`(variable-not-in ,pat ,s)
     `(variable-not-in ,(fresh-pat-vars pat instantiations) ,s)]
    [_ pre-pat]))

(define (make-uid id)
  (let ([uid-num (unique-name-nums)])
    (unique-name-nums (add1 uid-num))
    (string->symbol (string-append (symbol->string id) "_" (number->string uid-num)))))
