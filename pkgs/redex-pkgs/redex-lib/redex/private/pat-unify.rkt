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
         make-uid)



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
  (set-member? predef-pats a))
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

(define (bound-var? b)
  (match b
    [`(name ,(? var? name) ,(bound))
     #t]
    [_ 
     #f]))

(define eqs/c
  (hash/c lvar? (or/c lvar? pat*?)))
(define dq/c
  (list/c (listof (or/c 'list bound-var?)) (listof (or/c 'list pat*?))))
(define dqs/c
  (listof dq/c))
(define env/c
  (struct/c env eqs/c dqs/c))
(define p*e/c
  (struct/c p*e pat*? env/c))

;; used to track new equations for disunification
(define new-eqs (make-parameter (make-hash)))

;; (hash/c id (listof pat*))
;; disqequations generated (by mismatch-pattern) during a given unification pass
(define dqs-found (make-parameter (make-hash)))


;; pat pat env -> (or/c p*e #f)
(define (unify t u e L)
  (parameterize ([dqs-found (make-hash)])
    (define eqs (hash-copy (env-eqs e)))
    (define t* (bind-names t eqs L))
    (define u* (bind-names u eqs L))
    (define res (and/fail (not-failed? t*) 
                          (not-failed? u*)
                          (unify* t* u* eqs L)))
    (and/fail (not-failed? res)
              (let* ([static-eqs (hash/mut->imm eqs)]
                     [found-pre-dqs 
                      (apply set-union (set) 
                             (for/list ([dq-sides/id (hash-values (dqs-found))])
                               (list->dq-pairs dq-sides/id)))]
                     [found-dqs
                      (for/list ([pdq found-pre-dqs])
                        (disunify* '() (first pdq) (second pdq) static-eqs L))])
                (and/fail (for/and ([d found-dqs]) d)
                          (let* ([real-dqs (filter (λ (dq) (not (boolean? dq))) found-dqs)]
                                 [new-dqs (check-and-resimplify static-eqs (append real-dqs (env-dqs e)) L)])
                            (and/fail new-dqs
                                      (p*e res
                                           (env static-eqs new-dqs)))))))))

(define (list->dq-pairs dq-sides)
  (cond
    [(or (empty? dq-sides) 
         (equal? '() (cdr dq-sides)))
     (set)]
    [else
     (set-union (for/set ([rhs (cdr dq-sides)])
                         (list (car dq-sides) rhs))
                (list->dq-pairs (cdr dq-sides)))]))


;; pat pat env lang -> (or/c env #f)
(define (disunify params t u e L)
  (parameterize ([new-eqs (make-hash)])
    (define eqs (hash-copy (env-eqs e)))
    (define t* (bind-names t eqs L))
    (define u* (bind-names u eqs L))
    (define bn-eqs (hash/mut->imm eqs))
    (cond 
      [(or (unif-fail? t*) (unif-fail? u*))
       e]
      [else
       (define new-dq (disunify* params t* u* bn-eqs L)) 
       (match new-dq
         [#f #f]
         [#t 
          (env bn-eqs
               (env-dqs e))]
         [_
          (env bn-eqs
               (cons new-dq 
                     (env-dqs e)))])])))

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
  (define-values (rep pat) (lookup (lvar-id lv) eqs))
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
           
                
(define (hash/mut->imm h0)
  (for/fold ([h (hash)]) 
    ([(k v) (in-hash h0)])
    (hash-set h k v)))                                    
                                           

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

;; disunfy* pat* pat* eqs lang -> dq or boolean (dq is a pat*)
(define (disunify* params u* t* static-eqs L)
  (define eqs (hash-copy static-eqs))
  (parameterize ([new-eqs (make-hash)])
    (let ([res (unify* u* t* eqs L)])
      (cond
        [(unif-fail? res) #t]
        [(empty? (hash-keys (new-eqs))) #f]
        [else
         (define-values (new-ps new-dq)
           (param-elim params (extend-dq (new-eqs) base-dq eqs)))
         (match new-dq
           [`((list) (list))
            #f]
           [`((list (name ,var-ids ,(bound)) ...) (list ,pats ...))
            ;; check to see if parameter elimination exposed some
            ;; equivalences...
            (and
             (or (equal? (length params) (length new-ps))
                 (for/and ([v (in-list var-ids)] [p (in-list pats)])
                   (or (not (hash-has-key? static-eqs (lvar v)))
                       (not (equal? (resolve-no-nts/pat `(name ,v ,(bound)) static-eqs)
                                    p)))))
             (dq new-ps new-dq))])]))))

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


;; the "root" pats will be pats without names,
;; which match both pat and pat*...
;; (those are the ones bind-names does nothing with)

;; bind-names : pat env lang -> pat* or unif-fail
(define (bind-names pat e L)
  (match pat
    [`(name ,name ,(bound))
     (error 'bind-names "pat*, not a pat: ~s\n" pat)]
    [`(name ,name ,pat)
     (define b-pat (bind-names pat e L))
     (and/fail (not-failed? b-pat)
               (let recur ([id name])
                 (define res (hash-ref e (lvar id) (uninstantiated)))
                 (match res
                   [(uninstantiated)
                    (when (equal? b-pat (bound)) 
                      (error 'bind-names "tried to set something to bound"))
                    (and/fail (not (occurs?* id b-pat e L))
                              (hash-set! e (lvar id) b-pat)
                              ;; here we only bind to things in the LOCAL pattern
                              ;; so don't update new-eqs
                              `(name ,name ,(bound)))]
                   [(lvar id′)
                    (define next (recur id′))
                    (match next
                      [`(name ,id-new ,(bound))
                       (unless (eq? id id-new)
                         ;; path compression: don't update new-eqs here
                         (hash-set! e (lvar id) (lvar id-new)))]
                      [_ (void)])
                    next]
                   [_ ;; some pat* (res is already bound)
                    (and/fail (not-failed? (unify-update* id b-pat res e L))
                              `(name ,id ,(bound)))])))]
    [`(list ,pats ...)
     (let/ec fail
       `(list ,@(for/list ([p pats])
                  (define res (bind-names p e L))
                  (if (not-failed? res)
                      res
                      (fail (unif-fail))))))]
    [`(mismatch-name ,name ,p)
     (define b-pat (bind-names p e L))
     (and/fail (not-failed? b-pat)
               `(mismatch-name ,name ,(bind-names p e L)))]
    [_ pat]))


;; unify* : pat* pat* env lang -> pat* or unif-fail
(define (unify* t0 u0 e L)
  (define t (resolve t0 e))
  (define u (resolve u0 e))
  #2dmatch
  ╔═════════════════╦═════════════════╦═════════════╦═══════════════╦═══════════╦══════╦════════════╦══════════════╦═════════════════╦═════════╦══════════╦══════════════╦═════════════╗
  ║            u    ║ `(mismatch-name ║ `(name      ║ `(cstr        ║`(nt ,n-u) ║`any  ║ (? num-ty?)║`(list        ║ (? pvar?)       ║ `string ║ `boolean ║ (? base-ty?) ║(? not-pair?)║
  ║                 ║   ,u-name       ║   ,name-u   ║   (,nts1 ...) ║           ║      ║            ║  ,us ...)    ║                 ║         ║          ║              ║             ║
  ║  t              ║   ,u-pat)       ║   ,(bound)) ║   ,p1)        ║           ║      ║            ║              ║                 ║         ║          ║              ║             ║
  ╠═════════════════╬═════════════════╩═════════════╩═══════════════╩═══════════╩══════╩════════════╩══════════════╩═════════════════╩═════════╩══════════╩══════════════╩═════════════╣
  ║`(mismatch-name  ║ (hash-set! (dqs-found) t-name (cons u (hash-ref (dqs-found) t-name (λ () '()))))                                                                                 ║
  ║  ,t-name        ║ (unify* t-pat u e L)                                                                                                                                             ║
  ║  ,t-pat)        ║                                                                                                                                                                  ║
  ╠═════════════════╬═════════════════╦════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║`(name ,name-t   ║                 ║ (instantiate* name-t u e L)                                                                                                                    ║
  ║       ,(bound)) ║                 ║                                                                                                                                                ║
  ╠═════════════════╣                 ╚═════════════╦═══════════════╦══════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║`(cstr           ║                               ║(u*-2cstrs     ║ (u*-1cstr nts2 p2 u e L)                                                                                         ║
  ║  (,nts2 ...)    ║                               ║  nts1 p1      ║                                                                                                                  ║
  ║  ,p2)           ║                               ║  nts2 p2 e L) ║                                                                                                                  ║
  ╠═════════════════╣                               ╚═══════════════╬═══════════╦══════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║ `(nt ,n-t)      ║                                               ║(u*-2nts   ║ (u*-1nt n-t u e L)                                                                                   ║
  ║                 ║                                               ║ n-t n-u   ║                                                                                                      ║
  ║                 ║                                               ║ e L)      ║                                                                                                      ║
  ╠═════════════════╣                                               ╚═══════════╬══════════════════════════════════════════════════════════════════════════════════════════════════════╣
  ║  `any           ║                                                           ║ u                                                                                                    ║
  ║                 ║                                                           ║                                                                                                      ║
  ╠═════════════════╣                                                           ╚══════╦════════════╦════════════════════════════════════════════════════════════════════╦═════════════╣
  ║  (? num-ty?)    ║                                                                  ║(u*-2nums   ║                                                                    ║             ║
  ║                 ║                                                                  ║  t u)      ║                                                                    ║             ║
  ╠═════════════════╣                                                                  ╚════════════╬══════════════╗                                                     ║             ║
  ║ `(list ,ts ...) ║                                                                               ║(u*-2lsts     ║                              (unif-fail)            ║             ║
  ║                 ║                                                                               ║ ts us e L)   ║                                                     ║(u*-matches? ║
  ╠═════════════════╣                                                                               ╚══════════════╬═════════════════╗                                   ║  t u        ║
  ║ (? pvar?)       ║                                                                                              ║(u*-2pvars u t L)║                                   ║  e L)       ║
  ╠═════════════════╣                                                                                              ╚═════════════════╬═════════╗                         ║             ║
  ║ `string         ║                   (unify* u t e L)                                                                             ║    t    ║                         ║             ║
  ╠═════════════════╣                                                                                                                ╚═════════╬══════════╗              ║             ║
  ║ `boolean        ║                                                                                                                          ║    t     ║              ║             ║
  ╠═════════════════╣                                                                                                                          ╚══════════╬══════════════╣             ║
  ║ (? base-ty?)    ║                                                                                                                                     ║      t       ║             ║
  ╠═════════════════╣                                                                                                                                     ╚══════════════╬═════════════╣
  ║ (? not-pair?)   ║                                                                                                                                                    ║(and/fail    ║
  ║                 ║                                                                                                                                                    ║ (equal? t u)║
  ║                 ║                                                                                                                                                    ║ t)          ║
  ╚═════════════════╩════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╩═════════════╝)

(define (pvar? x) (or (vnom? x)
                      (var-pref? x)
                      (var-exc? x)
                      (equal? 'variable x)))
(define (vnom? x) (equal? x 'variable-not-otherwise-mentioned))
(define (var-pref? x) (match x
                        [`(variable-prefix ,p) #t]
                        [_ #f]))
(define (var-exc? x) (match x
                       [`(variable-except ,p ...) #t]
                       [_ #f]))
(define (not-pair? x) (not (pair? x)))

(define (u*-2pvars v1 v2 L)
  #2dmatch
  ╔════════════════════════╦══════════════════════════╦════════════════════════════════╦═══════════════════════════════╦════════════════╗
  ║                   v1   ║`(variable-prefix ,p1)    ║ `(variable-except ,e1 ... )    ║    (? vnom?)                  ║   `variable    ║
  ║ v2                     ║                          ║                                ║                               ║                ║
  ╠════════════════════════╬══════════════════════════╬════════════════════════════════╬═══════════════════════════════╬════════════════╣
  ║                        ║ (cond                    ║ (and/fail                      ║(u*-2pvars                     ║                ║
  ║ `(variable-prefix ,p2) ║  [(sym-pref? p1 p2)      ║  (not (ormap                   ║ v2                            ║                ║
  ║                        ║   `(variable-prefix ,p2)]║        (curry sym-pref? p2)    ║ `(variable-except             ║                ║
  ║                        ║  [(sym-pref? p2 p1)      ║        e1))                    ║ ,@(compiled-lang-literals L)) ║                ║
  ║                        ║   `(variable-prefix ,p1)]║  v2)                           ║ L)                            ║                ║
  ║                        ║  [else (unif-fail)])     ║                                ║                               ║                ║
  ╠════════════════════════╬══════════════════════════╬════════════════════════════════╣                               ║                ║
  ║                        ║                          ║ `(variable-except              ║                               ║                ║
  ║  `(variable-except     ║                          ║   ,@(de-dupe/sorted            ║                               ║       v2       ║
  ║     ,e2 ...)           ║                          ║      (merge/sorted e1 e2)))    ║                               ║                ║
  ║                        ║                          ║                                ║                               ║                ║
  ╠════════════════════════╣                          ╚════════════════════════════════╬═══════════════════════════════╣                ║
  ║                        ║                                                           ║                               ║                ║
  ║  (? vnom?)             ║                                                           ║              v1               ║                ║
  ║                        ║                                                           ║                               ║                ║
  ╠════════════════════════╣               (u*-2pvars v2 v1 L)                         ╚═══════════════════════════════╣                ║
  ║                        ║                                                                                           ║                ║
  ║ `variable              ║                                                                                           ║                ║
  ║                        ║                                                                                           ║                ║
  ╚════════════════════════╩═══════════════════════════════════════════════════════════════════════════════════════════╩════════════════╝)

(define (sym-pref? sp s)
  (regexp-match
   (string-append "^" (symbol->string sp) ".*$")
   (symbol->string s)))

(define (u*-2cstrs nts1 p1 nts2 p2 e L)
  (let ([res (unify* p1 p2 e L)]
        [new-nts (merge-ids/sorted nts1 nts2 L)])
    (and/fail (not-failed? res)
              new-nts
              (when (lvar? res)
                (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" p1 p2 e))
              `(cstr ,new-nts ,res))))

(define (u*-1cstr nts p u e L)
    (let ([res (unify* p u e L)])
      (and/fail (not-failed? res)
                (match res
                  [(lvar id)
                   (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" p u e)]
                  [`(nt ,nt)
                   (define new-nts (merge-ids/sorted (list nt) nts L))
                   (and/fail new-nts
                             `(cstr ,new-nts ,p))]
                  [`(cstr ,nts2 ,new-p)
                   (define new-nts (merge-ids/sorted nts nts2 L))
                   (and/fail new-nts
                             `(cstr ,new-nts ,new-p))]
                  [_
                   (and/fail (for/and ([n nts]) (check-nt n L res))
                             `(cstr ,nts ,res))]))))
  
(define (u*-2nts n-t n-u e L)
  (if (equal? n-t n-u)
      `(nt ,n-t)
      (u*-1nt n-t `(nt ,n-u) e L)))

(define (u*-1nt p u e L)
  (and/fail
   (check-nt p L u)
   (if (hash-has-key? (compiled-lang-collapsible-nts L) p)
       (let ([p-bn (bind-names (fresh-pat-vars (hash-ref (compiled-lang-collapsible-nts L) p) (make-hash)) e L)])
         (and/fail
          (not-failed? p-bn)
          (unify* p-bn u e L)))
       (let ([res (unify* u u e L)]) ;; look at structure of nt here?
         (and/fail (not-failed? res)
                   (when (lvar? res)
                     (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" u u e))
                   `(cstr (,p) ,res))))))
  
(define (u*-2lsts ts us e L)
  (and/fail (= (length ts) (length us))
            (let/ec fail
              `(list ,@(for/list ([t ts] [u us])
                         (let ([res (unify* t u e L)])
                           (if (not-failed? res)
                               res
                               (fail (unif-fail)))))))))

(define (u*-2nums t u)
  (cond 
    [(number-superset? t u) u]
    [(number-superset? u t) t]))

(define (u*-matches? t u e L) 
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
    [(_ _) (unif-fail)]))
    
(define (resolve pat env)
  (match pat
    [`(name ,id ,(bound))
     (define id-rep (lvar-id (lookup-rep id env)))
     (match (hash-ref env (lvar id-rep))
       [`(name ,next-id ,(bound))
        (hash-set! env (lvar id) (lvar next-id))
        (resolve `(name ,next-id ,(bound)) env)]
       [_
        `(name ,id-rep ,(bound))])]
    [_ pat]))

;; unify-update* : id pat* pat* env lang -> pat* or #f
(define (unify-update* id pat-1 pat-2 e L)
  (let ([u-res (unify* pat-1 pat-2 e L)])
    (and/fail (not (occurs?* id pat-1 e L))
              (not (occurs?* id pat-2 e L))
              (when (not-failed? u-res)
                (when (equal? u-res (bound)) (error 'update "tried to set something to bound"))
                (unless (equal? u-res (hash-ref e (lvar id) (uninstantiated)))
                  (hash-set! e (lvar id) u-res)
                  (unless (or (nt-only-pat? u-res)
                              (ground-pat-eq? pat-1 pat-2))
                    (hash-set! (new-eqs) (lvar id) u-res))))
              u-res)))

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
(define (occurs?* name p e L)
  (match p
    [`(name ,name-p ,(bound))
     (or (eq? name name-p)
         (occurs?* name (hash-ref e (lvar name-p) (uninstantiated)) e L))]
    [`(list ,ps ...)
     (for/or ([p ps])
       (occurs?* name p e L))]
    [`(cstr (,nts ...) ,pat)
     (occurs?* name pat e L)]
    [(lvar id)
     (or (eq? name id)
         (occurs?* name (hash-ref e (lvar id) (uninstantiated)) e L))]
    [`(cstr ,(lvar _))
     (error 'occurs?* "rogue lvar: ~s\n" p)]
    [_ #f]))


(define (instantiate* id pat e L)
  (define id-pat (resolve (lookup-pat id e) e))
  (match id-pat
    [`(name ,next-id ,(bound))
     (and/fail (not-failed? (instantiate* next-id pat e L))
               (not (occurs?* id (lvar next-id) e L))
               (hash-set! e (lvar id) (lvar next-id))
               `(name ,next-id ,(bound)))]
    [_
     (match pat
       [`(name ,id-2 ,(bound))
        (cond
          [(eq? id id-2)
           pat]
          [else
           (define id-2-pat (resolve (lookup-pat id-2 e) e))
           (define res (unify-update* id id-pat id-2-pat e L))
           (and/fail (not-failed? res)
                     (not (occurs?* id-2 (lvar id) e L))
                     (hash-set! e (lvar id-2) (lvar id))
                     (unless (ground-pat-eq? id-pat id-2-pat)
                       (hash-set! (new-eqs) (lvar id-2) (lvar id)))
                     `(name ,id ,(bound)))])]
       [_
        (and/fail (not-failed? (unify-update* id id-pat pat e L))
                  `(name ,id ,(bound)))])]))

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
         (check-nt nt1 lang `(nt ,l2)))
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

(define (lookup-pat id env)
  (define-values (_ pat) (lookup id env))
  pat)

(define (lookup-rep id env)
  (define-values (rep _) (lookup id env))
  rep)

(define (lookup id env)
  (define res (hash-ref env (lvar id) (λ () #f)))
  (match res
    [(lvar new-id)
     (define-values (rep pat) (lookup new-id env))
     (hash-set! env (lvar id) rep)
     (values rep pat)]
    [_
     (values (lvar id) res)]))

(provide check-nt)

(define check-nt
  (let ([memo (hash)])
    (λ (nt clang pat)
      (define npat (normalize-pat pat))
      (hash-ref memo (list nt clang npat)
                (λ ()
                  (define pat-ok? 
                    (for/or ([ntp (in-list (map normalize-pat (nt-pats nt clang)))])
                      (not-failed? (unify* npat ntp #f empty-lang))))
                  (set! memo
                        (hash-set memo (list nt clang npat) pat-ok?))
                  pat-ok?)))))

(define (normalize-pat pat)
  (let loop ([pat pat])
    (match-a-pattern pat
                     [`any pat]
                     [`number pat]
                     [`string pat]
                     [`natural pat]
                     [`integer pat]
                     [`real pat]
                     [`boolean pat]
                     [`variable pat]
                     [`(variable-except ,s ...) `variable]
                     [`(variable-prefix ,s) `variable]
                     [`variable-not-otherwise-mentioned pat]
                     [`hole (error "can't normalize pattern: ~s" pat)]
                     [`(nt ,id) `any]
                     [`(name ,name ,npat)
                      (if (bound? npat)
                          `any
                          `(name ,name ,(loop npat)))]
                     [`(mismatch-name ,name ,pat) (loop pat)]
                     [`(in-hole ,p1 ,p2) (error "can't normalize pattern: ~s" pat)]
                     [`(hide-hole ,p) (loop p)]
                     [`(side-condition ,p ,g ,e)
                      (error "can't normalize pattern: ~s" pat)]
                     [`(cross ,s) (error "can't normalize pattern: ~s" pat)]
                     [`(list ,sub-pats ...)
                      `(list ,@(for/list ([sub-pat (in-list sub-pats)])
                                 (match sub-pat
                                   [`(repeat ,pat ,name ,mismatch)
                                    (error "can't normalize pattern: ~s" pat)]
                                   [_
                                    (loop sub-pat)])))]
                     [(? (compose not pair?)) 
                      pat])))

(define (nt-pats nt lang)
  (define this-rhs
    (nt-rhs
     (let ([the-nt (findf (λ (lang-nt)
                        (equal? nt (nt-name lang-nt)))
                      (compiled-lang-lang lang))])
       (unless the-nt
         (error 'unify "nonterminal ~s not found for provided language... nts found: ~s" 
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
    [_ pre-pat]))

(define (make-uid id)
  (let ([uid-num (unique-name-nums)])
    (unique-name-nums (add1 uid-num))
    (string->symbol (string-append (symbol->string id) "_" (number->string uid-num)))))
