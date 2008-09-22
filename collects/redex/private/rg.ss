#|

iteratively grow the set of numbers & variables during generation.

redex: disallow non-terminals on rhs of rules unless they are actually bound(?)

need support for: 
 - collecting statistics
 - simplifying test cases

To do a better job of not generating programs with free variables, 
  keep track of which forms introduce binders 
  and prefer to generate that before generating any variables
  (also get rid of kludge, as below)

|#

#lang scheme

(require "matcher.ss"
         "reduction-semantics.ss"
         "underscore-allowed.ss"
         "term.ss"
         (for-syntax "rewrite-side-conditions.ss")
         mrlib/tex-table)

(define random-numbers '(0 1 -1 17 8))
(define (allow-free-var? [random random]) (= 0 (random 30)))
(define (exotic-char? [random random]) (= 0 (random 10)))
(define (use-lang-literal? [random random]) (= 0 (random 20)))
(define (try-to-introduce-binder?) (= 0 (random 2)) #f)

;; unique-chars : (listof string) -> (listof char)
(define (unique-chars strings)
  (let ([uniq (make-hasheq)])
    (for ([lit strings])
         (for ([char lit])
              (hash-set! uniq char #t)))
    (hash-map uniq (λ (k v) k))))

(define generation-retries 100)
(define ascii-chars-threshold 50)
(define tex-chars-threshold 500)
(define chinese-chars-threshold 2000)

;; E(pick-length) = 4/5(1 + E(pick-length)) = 4
;; P(pick-length >= 50) = 4/5^50 ≈ 0.00143%
(define (pick-length [random random]) 
  (cond
    [(zero? (random 5)) 0]
    [else (+ 1 (pick-length random))]))

;; pick-length averages about 4, has a max of about 50 and likes the small numbers:
#;
(let ([l (build-list 100000 (λ (x) (pick-length)))])
  (values (/ (apply + l) (length l))
          (apply max l)
          (let ([ht (make-hash)])
            (for-each
             (λ (n) (hash-set! ht n (+ 1  (hash-ref ht n 0))))
             l)
            (sort (hash-map ht (λ (x y) (list x (/ y (length l) 1.0))))
                  (λ (x y) (> (cadr x) (cadr y)))))))

(define (pick-var lang-chars lang-lits bound-vars attempt [random random])
  ;; E(length) = 4/5 + 1/5(1 + E(length)) = 5/4
  ;; P(length=c) = 4/(5^c)
  (define (length) (if (not (zero? (random 5))) 1 (add1 (length))))
  (if (or (null? bound-vars) (allow-free-var? random))
      (string->symbol (random-string lang-chars lang-lits (length) attempt random))
      (pick-from-list bound-vars random)))

(define (pick-char attempt lang-chars [random random])
  (if (and (not (null? lang-chars)) 
           (or (< attempt ascii-chars-threshold)
               (not (exotic-char? random))))
      (pick-from-list lang-chars random)
      (if (or (< attempt tex-chars-threshold) (not (exotic-char? random)))
          (let ([i (random (- #x7E #x20 1))]
                [_ (- (char->integer #\_) #x20)])
            (integer->char (+ #x20 (if (= i _) (add1 i) i))))
          (if (or (< attempt chinese-chars-threshold) (not (exotic-char? random)))
              (car (string->list (pick-from-list (map cadr tex-shortcut-table) random)))
              (integer->char (+ #x4E00 (random (- #x9FCF #x4E00))))))))

(define (random-string lang-chars lang-lits length attempt [random random])
  (if (and (not (null? lang-lits)) (use-lang-literal? random))
      (pick-from-list lang-lits random)
      (list->string (build-list length (λ (_) (pick-char attempt lang-chars random))))))

(define (pick-any lang [random random]) 
  (if (zero? (random 5))
      (values lang (pick-from-list (map nt-name (compiled-lang-lang lang)) random))
      (values sexp (nt-name (car (compiled-lang-lang sexp))))))

(define (pick-string lang-chars lang-lits attempt [random random])
  (random-string lang-chars lang-lits (pick-length random) attempt random))

(define (pick-nt prods bound-vars size)
  (let* ([binders (filter (λ (x) (not (null? (rhs-var-info x)))) prods)]
         [do-intro-binder? (and (not (zero? size)) (null? bound-vars)
                                (not (null? binders)) (try-to-introduce-binder?))])
    (pick-from-list (if do-intro-binder? binders prods))))

(define (pick-from-list l [random random]) (list-ref l (random (length l))))

(define (min-prods nt base-table)
  (let* ([sizes (hash-ref base-table (nt-name nt))]
         [min-size (apply min/f sizes)]
         [zip (λ (l m) (map cons l m))])
    (map cdr (filter (λ (x) (equal? min-size (car x))) (zip sizes (nt-rhs nt))))))

(define (generation-failure pat)
  (error 'generate "unable to generate pattern ~s in ~s attempts" 
         (unparse-pattern pat) generation-retries))

(define (generate* lang pat size [decisions@ random-decisions@])
  (define-values/invoke-unit decisions@
    (import) (export decisions^))
  
  (define lang-lits (map symbol->string (compiled-lang-literals lang)))
  (define lang-chars (unique-chars lang-lits))
  (define base-table (find-base-cases lang))
  
  (define (generate-nt name fvt-id bound-vars size attempt in-hole state)
    (let*-values 
        ([(nt) (findf (λ (nt) (eq? name (nt-name nt))) 
                      (append (compiled-lang-lang lang)
                              (compiled-lang-cclang lang)))]
         [(rhs) 
          ((next-non-terminal-decision) 
           (if (zero? size) (min-prods nt base-table) (nt-rhs nt))
           bound-vars size)]
         [(bound-vars) (append (extract-bound-vars fvt-id state) bound-vars)]
         [(nt-state) (make-state (map fvt-entry (rhs-var-info rhs)) #hash())]
         [(term _)
          (generate/pred 
           (rhs-pattern rhs)
           (λ (pat) (((generate-pat bound-vars (max 0 (sub1 size)) attempt) pat in-hole) nt-state))
           (λ (_ env) (mismatches-satisfied? env)))])
      (values term (extend-found-vars fvt-id term state))))
  
  (define (generate-sequence ellipsis generate state length)
    (define (split-environment env)
      (foldl (λ (var seq-envs)
               (let ([vals (hash-ref env var #f)])
                 (if vals
                     (map (λ (seq-env val) (hash-set seq-env var val)) seq-envs vals)
                     seq-envs)))
             (build-list length (λ (_) #hash())) (ellipsis-vars ellipsis)))
    (define (merge-environments seq-envs)
      (foldl (λ (var env)
               (hash-set env var (map (λ (seq-env) (hash-ref seq-env var)) seq-envs)))
             (state-env state) (ellipsis-vars ellipsis)))
    (let-values
        ([(seq envs fvt)
          (let recur ([fvt (state-fvt state)]
                      [envs (split-environment (state-env state))])
            (if (null? envs)
                (values null null fvt)
                (let*-values 
                    ([(term state) ((generate (ellipsis-pattern ellipsis) the-hole) 
                                    (make-state fvt (car envs)))]
                     [(terms envs fvt) (recur (state-fvt state) (cdr envs))])
                  (values (cons term terms) (cons (state-env state) envs) fvt))))])
      (values seq (make-state fvt (merge-environments envs)))))
  
  (define (generate/pred pat gen pred)
    (let retry ([remaining generation-retries])
      (if (zero? remaining)
          (generation-failure pat)
          (let-values ([(term state) (gen pat)])
            (if (pred term (state-env state))
                (values term state)
                (retry (sub1 remaining)))))))
  
  (define (generate/prior name state generate)
    (let* ([none (gensym)]
           [prior (hash-ref (state-env state) name none)])
      (if (eq? prior none)
          (let-values ([(term state) (generate)])
            (values term (set-env state name term)))
          (values prior state))))
  
  (define (mismatches-satisfied? env)
    (let ([groups (make-hasheq)])
      (define (get-group group)
        (hash-ref groups group
                  (λ ()
                    (let ([vals (make-hash)])
                      (hash-set! groups group vals)
                      vals))))
      (for/and ([(name val) env])
        (or (not (mismatch? name))
            (let ([prior (get-group (mismatch-group name))])
              (and (not (hash-ref prior val #f))
                   (hash-set! prior val #t)))))))
  
  (define-struct state (fvt env))
  (define (set-env state name value)
    (make-state (state-fvt state) (hash-set (state-env state) name value)))
  
  (define (bindings env)
    (make-bindings
     (for/fold ([bindings null]) ([(key val) env])
               (if (binder? key) 
                   (cons (make-bind (binder-name key) val) bindings)
                   bindings))))
  
  (define-struct found-vars (nt source bound-vars found-nt?))
  (define (fvt-entry binds)
    (make-found-vars (binds-binds binds) (binds-source binds) '() #f))
  
  (define (((generate-pat bound-vars size attempt) pat in-hole) state)
    (define recur (generate-pat bound-vars size attempt))
    (define (recur/pat pat) ((recur pat in-hole) state))
    
    (match pat
      [`number (values ((next-number-decision) random-numbers) state)]
      [`(variable-except ,vars ...)
       (generate/pred 'variable recur/pat (λ (var _) (not (memq var vars))))]
      [`variable (values ((next-variable-decision) lang-chars lang-lits bound-vars attempt) state)]
      [`variable-not-otherwise-mentioned
       (generate/pred 'variable recur/pat (λ (var _) (not (memq var (compiled-lang-literals lang)))))]
      [`(variable-prefix ,prefix) 
       (define (symbol-append prefix suffix)
         (string->symbol (string-append (symbol->string prefix) (symbol->string suffix))))
       (let-values ([(term state) (recur/pat 'variable)])
         (values (symbol-append prefix term) state))]
      [`string (values ((next-string-decision) lang-chars lang-lits attempt) state)]
      [`(side-condition ,pat ,(? procedure? condition))
       (generate/pred pat recur/pat (λ (_ env) (condition (bindings env))))]
      [`(name ,(? symbol? id) ,p)
       (let-values ([(term state) (recur/pat p)])
         (values term (set-env state (make-binder id) term)))]
      [`hole (values in-hole state)]
      [`(in-hole ,context ,contractum)
       (let-values ([(term state) (recur/pat contractum)])
         ((recur context term) state))]
      [`(hide-hole ,pattern) ((recur pattern the-hole) state)]
      [`any
       (let*-values ([(lang nt) ((next-any-decision) lang)]
                     [(term _) ((generate* lang nt size decisions@) attempt)])
         (values term state))]
      [(? (is-nt? lang))
       (generate-nt pat pat bound-vars size attempt in-hole state)]
      [(struct binder ((and name (or (? (is-nt? lang) nt) (app (symbol-match named-nt-rx) (? (is-nt? lang) nt))))))
       (generate/prior pat state (λ () (generate-nt nt name bound-vars size attempt in-hole state)))]
      [(struct binder ((or (? built-in? b) (app (symbol-match named-nt-rx) (? built-in? b)))))
       (generate/prior pat state (λ () (recur/pat b)))]
      [(struct mismatch (name (app (symbol-match mismatch-nt-rx) (? symbol? (? (is-nt? lang) nt)))))
       (let-values ([(term state) (generate-nt nt pat bound-vars size attempt in-hole state)])
         (values term (set-env state pat term)))]
      [(struct mismatch (name (app (symbol-match mismatch-nt-rx) (? symbol? (? built-in? b)))))
       (let-values ([(term state) (recur/pat b)])
         (values term (set-env state pat term)))]
      [`(cross ,(? symbol? cross-nt))
       (generate-nt cross-nt #f bound-vars size attempt in-hole state)]
      [(or (? symbol?) (? number?) (? string?) (? boolean?) (? null?)) (values pat state)]
      [(list-rest (and (struct ellipsis (name sub-pat class vars)) ellipsis) rest)
       (let*-values ([(length) (let ([prior (hash-ref (state-env state) class #f)])
                                 (if prior prior ((next-sequence-decision))))]
                     [(seq state) (generate-sequence ellipsis recur state length)]
                     [(rest state) ((recur rest in-hole) 
                                    (set-env (set-env state class length) name length))])
         (values (append seq rest) state))]
      [(list-rest pat rest)
       (let*-values 
           ([(pat-term state) (recur/pat pat)]
            [(rest-term state) ((recur rest in-hole) state)])
         (values (cons pat-term rest-term) state))]
      [else
       (error 'generate "unknown pattern ~s\n" pat)]))
  
  (define (extract-bound-vars pat state)
    (let loop ([found-vars-table (state-fvt state)])
      (cond
        [(null? found-vars-table) '()]
        [else (let ([found-vars (car found-vars-table)])
                (if (eq? pat (found-vars-nt found-vars))
                    (found-vars-bound-vars found-vars)
                    (loop (cdr found-vars-table))))])))
  
  (define (extend-found-vars pat res state)
    (make-state
     (map
      (λ (found-vars)
        (cond
          [(eq? (found-vars-source found-vars) pat)
           (let ([new-found-vars
                  (make-found-vars (found-vars-nt found-vars)
                                   (found-vars-source found-vars)
                                   (cons res (found-vars-bound-vars found-vars))
                                   #f)])
             (when (found-vars-found-nt? found-vars)
               (error 'generate "kludge in #:binds was exposed! #:binds ~s ~s" 
                      (found-vars-nt found-vars)
                      (found-vars-source found-vars)))
             new-found-vars)]
          [(eq? (found-vars-nt found-vars) pat)
           (make-found-vars (found-vars-nt found-vars)
                            (found-vars-source found-vars)
                            (found-vars-bound-vars found-vars)
                            #t)]
          [else found-vars]))
      (state-fvt state))
     (state-env state)))
  
  (λ (attempt)
    (let-values ([(term state)
                  (generate/pred 
                   pat
                   (λ (pat) 
                     (((generate-pat null size attempt) pat the-hole)
                      (make-state null #hash())))
                   (λ (_ env) (mismatches-satisfied? env)))])
      (values term (bindings (state-env state))))))

;; find-base-cases : compiled-language -> hash-table
(define (find-base-cases lang)
  (define nt-table (make-hasheq))
  (define changed? #f)
  (define (nt-get nt) (hash-ref nt-table nt 'inf))
  (define (nt-set nt new) 
    (let ([old (nt-get nt)])
      (unless (equal? new old)
        (set! changed? #t)
        (hash-set! nt-table nt new))))
  
  (define (process-nt nt)
    (nt-set (nt-name nt) (apply min/f (map process-rhs (nt-rhs nt)))))
  
  (define (process-rhs rhs)
    (let ([nts (rhs->nts (rhs-pattern rhs))])
      (if (null? nts) 
          0
          (add1/f (apply max/f (map nt-get nts))))))
  
  ;; rhs->path : pattern -> (listof symbol)
  ;; determines all of the non-terminals in a pattern
  (define (rhs->nts pat)
    (let ([nts '()])
      (let loop ([pat pat])
        (match pat
          [(? symbol? pat)
           (when ((is-nt? lang) (symbol->nt pat))
             (set! nts (cons (symbol->nt pat) nts)))]
          [`(cross ,(? symbol? x-nt))
           (set! nts (cons x-nt nts))]
          [`() (void)]
          [`(,a ,'... . ,b) 
           (loop a)
           (loop b)]
          [`(,a . ,b)
           (loop a)
           (loop b)]
          [_ (void)]))
      nts))
  
  (let ([nts (append (compiled-lang-lang lang) (compiled-lang-cclang lang))])
    (let loop ()
      (set! changed? #f)
      (for-each process-nt nts)
      (when changed?
        (loop)))
    
    (let ([ht (make-hash)])
      (for-each
       (λ (nt) (hash-set! ht (nt-name nt) (map process-rhs (nt-rhs nt))))
       nts)
      ht)))

(define min/f
  (case-lambda
    [(a) a]
    [(a b) 
     (cond
       [(eq? a 'inf) b]
       [(eq? b 'inf) a]
       [else (min a b)])]
    [(a b . c) (min/f a (apply min/f b c))]))
(define max/f
  (case-lambda
    [(a) a]
    [(a b) 
     (cond
       [(eq? a 'inf) a]
       [(eq? b 'inf) b]
       [else (max a b)])]
    [(a b . c) (max/f a (apply max/f b c))]))
(define (add1/f a) (if (eq? a 'inf) 'inf (+ a 1)))

;; is-nt? : compiled-lang any -> boolean
(define ((is-nt? lang) x)
  (and (hash-ref (compiled-lang-ht lang) x #f) #t))

;; built-in? : any -> boolean
(define (built-in? x)
  (and (memq x underscore-allowed) #t))

(define named-nt-rx #rx"^([^_]+)_[^_]*$")
(define mismatch-nt-rx #rx"([^_]+)_!_[^_]*$")
(define named-ellipsis-rx #rx"^\\.\\.\\._[^_]*$")
(define mismatch-ellipsis-rx #rx"^\\.\\.\\._!_[^_]*$")

;; symbol-match : regexp -> any -> (or/c false symbol)
;; Returns the sub-symbol matching the sub-pattern inside
;; the first capturing parens.
(define ((symbol-match rx) x)
  (and (symbol? x) 
       (let ([match (regexp-match rx (symbol->string x))])
         (and match (cadr match) (string->symbol (cadr match))))))

(define-struct class (id) #:inspector (make-inspector))
(define-struct mismatch (id group) #:inspector (make-inspector))
(define-struct binder (name) #:inspector (make-inspector))

;; name: (or/c symbol? mismatch?)
;;   The generator records `name' in the environment when generating an ellipsis,
;;   to enforce sequence length constraints.
;; class: class?
;;   When one binding appears under two (non-nested) ellipses, the sequences generated
;;   must have the same length; `class' groups ellipses to reflect this constraint.
;; var: (list/c (or/c symbol? class? mismatch? binder?))
;;   the bindings within an ellipses, used to split and merge the environment before
;;   and after generating an ellipsis
(define-struct ellipsis (name pattern class vars) #:inspector (make-inspector))

;; parse-pattern : pattern compiled-lang (or/c 'cross 'top-level 'grammar) -> parsed-pattern
;; Turns "pat ...", "pat ..._id", and "pat ..._!_id" into ellipsis structs,
;; "nt_!_id" into mismatch structs, "nt_id" into binder structs, and
;; "nt/underscore-allowed" in top-level patterns into binder structs.
(define (parse-pattern pattern lang mode)
  (define (recur pat vars)
    (match pat
      [(or (app (symbol-match named-nt-rx) (or (? (is-nt? lang)) (? built-in?)))
           (and (? (λ (_) (eq? mode 'top-level))) (or (? (is-nt? lang)) (? built-in?))))
       (let ([b (make-binder pat)])
         (values b (cons b vars)))]
      [(app (symbol-match mismatch-nt-rx) (or (? (is-nt? lang)) (? built-in?)))
       (let ([mismatch (make-mismatch (gensym) pat)])
         (values mismatch (cons mismatch vars)))]
      [`(name ,name ,sub-pat)
       (let-values ([(parsed vars) (recur sub-pat vars)])
         (values `(name ,name ,parsed) (cons (make-binder name) vars)))]
      [(list-rest sub-pat (and (? symbol?) (app symbol->string (regexp named-ellipsis-rx)) name) rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(seq) (make-ellipsis name sub-pat-parsed (make-class name) sub-pat-vars)]
                     [(vars) (append (list* name (make-class name) sub-pat-vars) vars)]
                     [(rest-parsed vars) (recur rest vars)])
         (values (cons seq rest-parsed) vars))]
      [(list-rest sub-pat '... rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(class) (make-class (gensym))]
                     [(seq) (make-ellipsis '... sub-pat-parsed class sub-pat-vars)]
                     [(rest-parsed vars) (recur rest (cons class (append sub-pat-vars vars)))])
         (values (cons seq rest-parsed) vars))]
      [(list-rest sub-pat (and (? symbol?) (app symbol->string (regexp mismatch-ellipsis-rx)) name) rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(mismatch) (make-mismatch (gensym) name)]
                     [(class) (make-class (gensym))]
                     [(seq) (make-ellipsis mismatch sub-pat-parsed class sub-pat-vars)]
                     [(vars) (append (list* class mismatch sub-pat-vars) vars)]
                     [(rest-parsed vars) (recur rest vars)])
         (values (cons seq rest-parsed) vars))]
      [(and (? (λ (_) (not (eq? mode 'cross)))) `(cross ,(and (? (is-nt? lang)) nt)))
       (let ([nt-str (symbol->string nt)])
         (values `(cross ,(string->symbol (string-append nt-str "-" nt-str))) vars))]
      [(cons first rest)
       (let-values ([(first-parsed vars) (recur first vars)])
         (let-values ([(rest-parsed vars) (recur rest vars)])
           (values (cons first-parsed rest-parsed) vars)))]
      [_ (values pat vars)]))
  (let-values ([(parsed _) (recur pattern null)])
    parsed))

;; parse-language: compiled-lang -> compiled-lang
(define (parse-language lang)
  (define ((parse-nt mode) nt)
    (make-nt (nt-name nt) (map (parse-rhs mode) (nt-rhs nt))))
  (define ((parse-rhs mode) rhs)
    (make-rhs (reassign-classes (parse-pattern (rhs-pattern rhs) lang mode))
              (rhs-var-info rhs)))
  (struct-copy 
   compiled-lang lang
   [lang (map (parse-nt 'grammar) (compiled-lang-lang lang))]
   [cclang (map (parse-nt 'top-level) (compiled-lang-cclang lang))]))

;; unparse-pattern: parsed-pattern -> pattern
(define unparse-pattern
  (match-lambda
    [(struct binder (name)) name]
    [(struct mismatch (_ group)) group]
    [(list-rest (struct ellipsis (name sub-pat _ _)) rest)
     (let ([ellipsis (if (mismatch? name) (mismatch-group name) name)])
       (list* (unparse-pattern sub-pat) ellipsis (unparse-pattern rest)))]
    [(cons first rest) 
     (cons (unparse-pattern first) (unparse-pattern rest))]
    [else else]))

;; class-reassignments : parsed-pattern -> hash[sym -o> sym]
(define (class-reassignments pattern)
  ; union-find w/o balancing or path compression (at least for now)
  (define (union e f sets)
    (hash-set sets (find f sets) (find e sets)))
  (define (find e sets)
    (let recur ([chd e] [par (hash-ref sets e #f)])
      (if (and par (not (eq? chd par))) (recur par (hash-ref sets par #f)) chd)))
  
  (let* ([last-contexts (make-hasheq)]
         [record-binder
          (λ (pat under assignments)
            (if (null? under) 
                assignments
                (let ([last (hash-ref last-contexts pat #f)])
                  (if last
                      (foldl (λ (cur last asgns) (union cur last asgns)) assignments under last)
                      (begin
                        (hash-set! last-contexts pat under)
                        assignments)))))]
         [assignments
          (let recur ([pat pattern] [under null] [assignments #hasheq()])
            (match pat
              ;; `(name ,id ,sub-pat) not considered, since bindings introduced
              ;; by name must be unique.
              [(struct binder (name))
               (record-binder name under assignments)]
              [(struct ellipsis (name sub-pat (struct class (cls)) _))
               (recur sub-pat (cons cls under)
                 (if (and (symbol? name) (regexp-match named-ellipsis-rx (symbol->string name)))
                     (record-binder name under assignments)
                     assignments))]
              [(? list?)
               (foldl (λ (pat asgns) (recur pat under asgns)) assignments pat)]
              [_ assignments]))])
    (make-immutable-hasheq (hash-map assignments (λ (cls _) (cons cls (find cls assignments)))))))

(define (reassign-classes pattern)
  (let* ([reassignments (class-reassignments pattern)]
         [rewrite (λ (c) (make-class (hash-ref reassignments (class-id c) (class-id c))))])
    (let recur ([pat pattern])
      (match pat
        [(struct ellipsis (name sub-pat class vars))
         (make-ellipsis name (recur sub-pat) (rewrite class)
                        (map (λ (v) (if (class? v) (rewrite v) v)) vars))]
        [(? list?) (map (λ (p) (recur p)) pat)]
        [_ pat]))))

;; used in generating the `any' pattern
(define sexp
  (let ()
    (define-language sexp (sexp variable string number hole (sexp ...)))
    (parse-language sexp)))

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ lang pat attempts size property)
     (let-values ([(names names/ellipses) 
                   (extract-names (language-id-nts #'lang 'generate) 'check #t #'pat)])
       (with-syntax ([(name ...) names]
                     [(name/ellipses ...) names/ellipses])
         (syntax/loc stx
           (let ([generator (term-generator lang pat size random-decisions@)])
             (let loop ([remaining attempts])
               (if (zero? remaining)
                   #t
                   (let ([attempt (add1 (- attempts remaining))])
                     (let-values ([(term bindings) (generator attempt)])
                       (term-let ([name/ellipses (lookup-binding bindings 'name)] ...)
                                 (if (with-handlers 
                                         ([exn:fail? (λ (exn) (error 'check "term ~s raises ~s" term exn))])
                                       property)
                                     (loop (sub1 remaining))
                                     (fprintf (current-error-port) 
                                              "failed after ~s attempts: ~s" 
                                              attempt term)))))))))))]))

(define-syntax generate
  (syntax-rules ()
    [(_ lang pat size attempt)
     (let-values ([(term _) ((term-generator lang pat size random-decisions@) attempt)])
       term)]
    [(_ lang pat size) (generate lang pat size 0)]))

(define-syntax generate/decisions
  (syntax-rules ()
    [(_ lang pat size attempt decisions@)
     (let-values ([(term _) ((term-generator lang pat size decisions@) attempt)])
       term)]))

(define-syntax (term-generator stx)
  (syntax-case stx ()
    [(_ lang pat size decisions@)
     (with-syntax ([pattern 
                    (rewrite-side-conditions/check-errs 
                     (language-id-nts #'lang 'generate)
                     'generate #t #'pat)])
       (syntax/loc stx 
        (generate* 
         (parse-language lang)
         (reassign-classes (parse-pattern `pattern lang 'top-level))
         size decisions@)))]))

(define-signature decisions^
  (next-variable-decision
   next-number-decision
   next-non-terminal-decision
   next-sequence-decision
   next-any-decision
   next-string-decision))

(define random-decisions@
  (unit (import) (export decisions^)
        (define (next-variable-decision) pick-var)
        (define (next-number-decision) pick-from-list)
        (define (next-non-terminal-decision) pick-nt)
        (define (next-sequence-decision) pick-length)
        (define (next-any-decision) pick-any)
        (define (next-string-decision) pick-string)))

(provide pick-from-list pick-var pick-length min-prods decisions^ 
         is-nt? pick-char random-string pick-string check
         pick-nt unique-chars pick-any sexp generate parse-pattern
         class-reassignments reassign-classes unparse-pattern
         (struct-out ellipsis) (struct-out mismatch) (struct-out class)
         (struct-out binder) generate/decisions)

(provide/contract
 [find-base-cases (-> compiled-lang? hash?)])