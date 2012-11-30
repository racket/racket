#lang racket/base

(require racket/list
         racket/contract
         racket/set
         racket/match
         (for-syntax "rewrite-side-conditions.rkt")
         "match-a-pattern.rkt"
         "matcher.rkt"
         "extract-conditions.rkt")

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
         remove-empty-dqs)

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
(define empty-env (env (hash) '()))

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
       [`(variable-except ,vars ...) #f]
       [`(variable-prefix ,pfx) #f]
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
       [else 
        (and is-pat*?
             (match p
               [`(cstr (,nts ...) ,p*)
                (and (for/and ([n nts]) (n-t? n))
                     (loop p*))]
               [else #f]))]))))

(define (pat? p) (pat-or-pat*? #f p))
(define (pat*? p) (pat-or-pat*? #t p))
(define pat*-clause-p?s (append (list
                                 (let ([bound-name? (λ (p) (match p [`(name ,id ,(bound)) #t] [else #f]))])
                                   bound-name?)
                                 (let ([cstr? (λ (p) (match p [`(cstr (,nts ...) ,pat) #t] [else #f]))])
                                   cstr?))
                                (extracted-clauses->fns)))

(define (bound-var? b)
  (match b
    [`(name ,(? var? name) ,(bound))
     #t]
    [else 
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
  ;(-> pat? pat? env/c compiled-lang? (or/c p*e/c #f))
  ;(printf "u: ~s ~s ~s\n\n" t u e)
  (parameterize ([dqs-found (make-hash)])
    (define eqs (hash-copy (env-eqs e)))
    (define t* (bind-names t eqs L))
    (define u* (bind-names u eqs L))
    (define res (and t* u* (unify* t* u* eqs L)))
    (and res
         (let* ([static-eqs (hash/mut->imm eqs)]
                [found-pre-dqs 
                 (apply set-union (set) 
                        (for/list ([dq-sides/id (hash-values (dqs-found))])
                          (list->dq-pairs dq-sides/id)))]
                [found-dqs
                 (for/list ([pdq found-pre-dqs])
                   (disunify* (first pdq) (second pdq) (hash-copy static-eqs) L))])
           (and (for/and ([d found-dqs]) d)
                (let* ([real-dqs (filter (λ (dq) (not (boolean? dq))) found-dqs)]
                       [new-dqs (check-and-resimplify static-eqs (append real-dqs (env-dqs e)) L)])
                  (and new-dqs
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
(define (disunify t u e L)
  ;(-> pat? pat? env/c any/c (or/c env/c #f))
  ;(printf "du: ~s ~s\n\n" t u)
  (parameterize ([new-eqs (make-hash)])
    (define eqs (hash-copy (env-eqs e)))
    (define t* (bind-names t eqs L))
    (define u* (bind-names u eqs L))
    (cond 
      [(or (not t*) (not u*))
       e]
      [else
       (define bn-eqs (hash-copy eqs))
       (define new-dq (disunify* t* u* eqs L)) 
       (match new-dq
         [#f #f]
         [#t 
          (env (hash/mut->imm bn-eqs)
               (env-dqs e))]
         [else
          (env (hash/mut->imm bn-eqs)
               (cons new-dq 
                     (env-dqs e)))])])))

(define base-dq `((list) (list)))

(define (remove-empty-dqs dqs)
  (filter (λ (dq) (not (equal? base-dq dq)))
          dqs))

(define (extend-dq eqs ineq0)
  (for/fold ([ineq ineq0])
    ([(k v) (in-hash eqs)])
    (match ineq
      [`((list ,vars ...) (list ,terms ...))
       (match* (k v)
         [((lvar id-l) (lvar id-r))
          `((list ,@vars (name ,id-l ,(bound))) (list ,@terms (name ,id-r ,(bound))))]
         [((lvar id-l) pat*-r)
          `((list ,@vars (name ,id-l ,(bound))) (list ,@terms ,pat*-r))])])))
                
(define (hash/mut->imm h0)
  (for/fold ([h (hash)])
    ([(k v) (in-hash h0)])
    (hash-set h k v)))                                    
                                           

;; eqs dqs -> dqs or #f
;; simplified - first element in lhs of all inequations is a var not occuring in lhs of eqns
(define (check-and-resimplify eqs dqs L)
  ;(printf "c-a-r: ~s\n~s\n" dqs eqs)
  (define-values (dqs-notok dqs-ok) 
    (partition (λ (dq)
                 (hash-has-key? 
                  eqs
                  (lvar (match dq
                          [`((list (name ,v1 ,(bound)) ,vs ...) (list ,t1 ,ts ...))
                           v1]))))
               (remove-empty-dqs dqs)))
  (let loop ([ok dqs-ok]
             [notok dqs-notok])
    (cond
      [(empty? notok)
       ok]
      [else
       (match notok
         [`((,vars-p* ,term-p*) ,rest ...)
          (let ([new-dq (disunify* vars-p* term-p* (hash-copy eqs) L)])
            (and new-dq
                 (match new-dq
                   [#t (loop ok rest)]
                   [`((list)(list)) (loop ok rest)]
                   [else (loop (cons new-dq ok) rest)])))])])))

;; disunfy* pat* pat* eqs lang -> dq or boolean (dq is a pat*)
(define (disunify* u* t* eqs L)
  ;(printf "du*: ~s ~s ~s\n" t* u* eqs)
  (parameterize ([new-eqs (make-hash)])
    (let ([res (unify* u* t* eqs L)])
      (cond
        [(not res) #t]
        [(empty? (hash-keys (new-eqs))) #f]
        [else
         (extend-dq (new-eqs) base-dq)]))))

(define (update-env e new-eqs the-dqs)
  (env (for/fold ([eqs (env-eqs e)])
           ([(k v) (in-hash new-eqs)])
         (hash-set eqs k v))
       the-dqs))

(define (update-ineqs e new-es)
  (struct-copy env e
               [dqs (cons (for/fold ([dq '((list)(list))])
                            ([(l r) (in-hash new-es)])
                            (match dq
                              [`((,vars ...) (,rhss ...))
                               `((,vars ... (name ,l ,(bound))) (,rhss ... ,r))]))
                          (env-dqs e))]))


;; the "root" pats will be pats without names,
;; which match both pat and pat*...
;; (those are the ones bind-names does nothing with)

;; bind-names : pat env lang -> pat* or #f
(define (bind-names pat e L)
  (match pat
    [`(name ,name ,(bound))
     (error 'bind-names "pat*, not a pat: ~s\n" pat)]
    [`(name ,name ,pat)
     (define b-pat (bind-names pat e L))
     (and b-pat
          (let recur ([id name])
            (define res (hash-ref e (lvar id) (uninstantiated)))
            (match res
              [(uninstantiated)
               (when (equal? b-pat (bound)) 
                 (error 'bind-names "tried to set something to bound"))
               (and (not (occurs?* id b-pat e L))
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
              [else ;; some pat* (res is already bound)
               (and (unify-update* id b-pat res e L)
                    `(name ,id ,(bound)))])))]
    [`(list ,pats ...)
     (let/ec fail
       `(list ,@(for/list ([p pats])
                  (or (bind-names p e L) (fail #f)))))]
    [`(mismatch-name ,name ,p)
     (define b-pat (bind-names p e L))
     (and b-pat
          `(mismatch-name ,name ,(bind-names p e L)))]
    [_ pat]))


;; unify* : pat* pat* env lang -> pat* or #f
(define (unify* t0 u0 e L)
  (define t (resolve t0 e))
  (define u (resolve u0 e))
  ;(printf "unify*: ~s ~s\n" t u)
  (match* (t u)
    ;; mismatch patterns
    [(`(mismatch-name ,name ,t-pat) u)
     (hash-set! (dqs-found) name 
                (cons u (hash-ref (dqs-found) name (λ () '()))))
     (unify* t-pat u e L)]
    [(t `(mismatch-name ,name ,u-pat))
     (hash-set! (dqs-found) name 
                (cons t (hash-ref (dqs-found) name (λ () '()))))
     (unify* t u-pat e L)]
    ;; named pats always pre-bound here
    [(`(name ,name ,(bound)) _)
     (instantiate* name u e L)]
    [(_ `(name ,name ,(bound)))
     (unify* u t e L)]
    ;; cstrs
    #;[(`(nt ,n) `(cstr (,nts ...) ,p)) ;; remove ?? put back?
     `(cstr ,(sort (remove-duplicates (cons n nts))
                   symbol<?) ,p)]
    [(`(cstr (,nts1 ...) ,p1) `(cstr (,nts2 ...) ,p2))
     (let ([res (unify* p1 p2 e L)])
       (and res
            (when (lvar? res)
              (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" p1 p2 e))
            `(cstr ,(merge-ids/sorted nts1 nts2) ,res)))]
    [(`(cstr ,nts ,p) _)
     (let ([res (unify* p u e L)])
       (and res
            (match res
              [(lvar id)
               (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" p u e)]
              [`(nt ,nt)
               `(cstr ,(merge-ids/sorted (list nt) nts)
                      ,p)]
              [`(cstr ,nts2 ,new-p)
               `(cstr ,(merge-ids/sorted nts nts2) ,new-p)]
              [else
               `(cstr ,nts ,res)])))]
    [(_ `(cstr ,nts ,p))
     (unify* `(cstr ,nts ,p) t e L)]
    ;; nts
    [(`(nt ,n) `(nt ,n))
     (if (hash-has-key? (compiled-lang-collapsible-nts L) n)
         (hash-ref (compiled-lang-collapsible-nts L) n)
         `(nt ,n))]
    [(`(nt ,p) u)
     (if (hash-has-key? (compiled-lang-collapsible-nts L) p)
         (unify* (hash-ref (compiled-lang-collapsible-nts L) p) u e L)
         (let ([res (unify* u u e L)])
           (and res
                (when (lvar? res)
                  (error 'unify* "unify* returned lvar as result: ~s\n~s\n~s\n" u u e))
                `(cstr (,p) ,res))))]
    [(_ `(nt ,p))
     (unify* `(nt ,p) t e L)]
    ;; other pat stuff
    [(`(list ,ts ...) `(list ,us ...))
     (and (= (length ts) (length us))
          (let/ec fail
            `(list ,@(for/list ([t ts] [u us])
                       (or (unify* t u e L) (fail #f))))))]
    [((? number-type? t) (? number-type? u))
     (cond 
       [(number-superset? t u) u]
       [(number-superset? u t) t])]
    [((? number-type? t) _)
     (and ((number-pred t) u)
          u)]
    [(_ (? number-type? u))
     (unify* u t e L)]
    [(`variable-not-otherwise-mentioned `variable-not-otherwise-mentioned)
     `variable-not-otherwise-mentioned]
    [(_ `variable-not-otherwise-mentioned)
     (unify* u t e L)]
    [(`variable-not-otherwise-mentioned `variable)
     `variable-not-otherwise-mentioned]
    [(`variable-not-otherwise-mentioned (? symbol? s))
     (and (not (memq s (compiled-lang-literals L)))
          (not (base-type? s))
          s)]
    [(`variable `variable)
     `variable]
    [(_ `variable)
     (unify* u t e L)]
    [(`variable (? symbol? s))
     (and (not (base-type? s))
          s)]
    ;; string stuff
    [(`string `string)
     `string]
    [(_ `string)
     (unify* u t e L)]
    [(`string (? string? s))
     s]
    
    ;; booleans
    [(`boolean `boolean)
     `boolean]
    [(`string `boolean)
     #f]
    [(_ `boolean)
     (unify* u t e L)]
    [(`boolean (? boolean? b))
     b]
    
    ;; other
    [((? base-type? t) (? base-type? u))
     (and (equal? t u)
          t)]
    [((? (compose not pair?) t) (? (compose not pair?) u))
     (and (equal? t u)
          t)]
    [(_ _) #f]))

(define (resolve pat env)
  (match pat
    [`(name ,id ,(bound))
     (define id-rep (lvar-id (lookup-rep id env)))
     (match (hash-ref env (lvar id-rep))
       [`(name ,next-id ,(bound))
        (hash-set! env (lvar id) (lvar next-id))
        (resolve `(name ,next-id ,(bound)) env)]
       [else
        `(name ,id-rep ,(bound))])]
    [_ pat]))

;; unify-update* : id pat* pat* env lang -> pat* or #f
(define (unify-update* id pat-1 pat-2 e L)
  ;(printf "unify-update ~s ~s ~s\n" id pat-1 pat-2)
  (let ([u-res (unify* pat-1 pat-2 e L)])
    (and (not (occurs?* id pat-1 e L))
         (not (occurs?* id pat-2 e L))
         (when u-res
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
    [else
     #f]))

;; occurs* : name (pat* or lvar] -> bool
;; pat* --> path compression has been added by bind-names, so
;; we only need to look at the id in name, and the pattern it is bound to
;; TODO: replace name in p*'s with lvar - this is the most obvious of many 
;; functions that would be improved by this
(define (occurs?* name p e L)
  ;(printf "occurs: ~s ~s\n" name p)
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
    [else #f]))


(define (instantiate* id pat e L)
  ;(printf "inst*: ~s ~s\n" id pat)
  (define id-pat (resolve (lookup-pat id e) e))
  (match id-pat
    [`(name ,next-id ,(bound))
     (and (instantiate* next-id pat e L)
          (not (occurs?* id (lvar next-id) e L))
          (hash-set! e (lvar id) (lvar next-id))
          `(name ,next-id ,(bound)))]
    [else
     (match pat
       [`(name ,id-2 ,(bound))
        (cond
          [(eq? id id-2)
           pat]
          [else
           (define id-2-pat (resolve (lookup-pat id-2 e) e))
           ;(printf "id: ~s id-pat: ~s id-2: ~s id-2-pat: ~s\n" id id-pat id-2 id-2-pat)
           (define res (unify-update* id id-pat id-2-pat e L))
           (and res
                (not (occurs?* id-2 (lvar id) e L))
                (hash-set! e (lvar id-2) (lvar id))
                (unless (ground-pat-eq? id-pat id-2-pat)
                  (hash-set! (new-eqs) (lvar id-2) (lvar id)))
                `(name ,id ,(bound)))])]
       [else
        (and (unify-update* id id-pat pat e L)
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

(define (merge-ids/sorted l1 l2)
  (de-dupe/sorted (merge/sorted l1 l2)))

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

(define (number-type? symbol)
  (member symbol
          '(any number real integer natural)))

(define npreds (hash 'number number?
                     'real real?
                     'integer integer?
                     'natural (λ (n) (and (integer? n)
                                           (>= n 0)))
                     'any (λ (n) #t)))
(define (number-pred symbol)
  (hash-ref npreds symbol))

(define (number-superset? super sub)
  (define nums '(any number real integer natural))
  (>= (length (member super nums))
      (length (member sub nums))))

(define (base-type? symbol)
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
  (define res (hash-ref env (lvar id) (λ ()
                                        #;(hash-set! env (lvar id) 'any)
                                        #;'any
                                        #f)))
  (match res
    [(lvar new-id)
     (lookup new-id env)]
    [else
     (values (lvar id) res)]))






