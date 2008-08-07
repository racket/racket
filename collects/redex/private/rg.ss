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

(define (hash->keys hash) (hash-map hash (λ (k v) k)))

(define (lang-literals lang)
  (define (process-pattern pat lits)
    (cond [(symbol? pat) (process-pattern (symbol->string pat) lits)]
          [(string? pat) (hash-set lits pat (void))]
          [(number? pat) (process-pattern (number->string pat) lits)]
          [(or (procedure? pat) (boolean? pat) (null? pat)) lits]
          [(pair? pat) (foldl process-pattern lits pat)]
          [else (error 'lang-literals "unexpected pattern ~s" pat)]))
  (define (process-non-terminal nt chars)
    (foldl (λ (rhs chars) (process-pattern (rhs-pattern rhs) chars)) 
           chars (nt-rhs nt)))
  (hash->keys 
   (foldl process-non-terminal 
          (make-immutable-hash null) (compiled-lang-lang lang))))

(define (unique-chars strings)
  (define (record-chars char chars)
    (if (char=? char #\_) chars (hash-set chars char (void))))
  (hash->keys
   (foldl (λ (s c) (foldl record-chars c (string->list s)))
          (make-immutable-hash null) strings)))

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
         pat generation-retries))

;; used in generating the `any' pattern
(define-language sexp (sexp variable string number hole (sexp ...)))

(define (generate* lang nt size attempt [decisions@ random-decisions@])
  (define-values/invoke-unit decisions@
    (import) (export decisions^))
  
  (define lang-lits (lang-literals lang))
  (define lang-chars (unique-chars lang-lits))
  (define base-table (find-base-cases lang))
  
  (define (generate-nt nt bound-vars size in-hole)
    (let loop ([nts (compiled-lang-lang lang)])
      (cond
        [(null? nts) (error 'generate-nt "didn't find non-terminal ~s" nt)]
        [(eq? (nt-name (car nts)) nt) 
         (let* ([prods (if (zero? size) (min-prods (car nts) base-table) (nt-rhs (car nts)))]
                [rhs ((next-non-terminal-decision) prods bound-vars size)]
                [size (max 0 (sub1 size))])
           (generate-pat (rhs-pattern rhs) bound-vars (rhs-var-info rhs) size in-hole))]
        [else (loop (cdr nts))])))
    
  (define-struct found-vars (nt source bound-vars found-nt?))
  (define (generate-pat pat bound-vars var-info size in-hole)
    (let* ([found-vars-table (map (λ (binds) (make-found-vars (binds-binds binds) (binds-source binds) '() #f))
                                  var-info)]
           [matches (make-immutable-hasheq null)]
           [mismatches (make-immutable-hasheq null)])
      (let loop ([pat pat] [in-hole in-hole])
        (define (generate/retry #:gen [gen (λ (p) (loop p in-hole))] success? . subpatterns)
          (let ([old-fvt found-vars-table] 
                [old-matches matches]
                [old-mismatches mismatches])
            (let retry ([remaining generation-retries])
              (if (zero? remaining)
                  (generation-failure pat)
                  (let ([generated (map gen subpatterns)])
                    (if (apply success? generated)
                        (if (= 1 (length generated))
                            (car generated)
                            generated)
                        (begin
                          (set! found-vars-table old-fvt)
                          (set! matches old-matches)
                          (set! mismatches old-mismatches)
                          (retry (sub1 remaining)))))))))
        (match pat
          [`number ((next-number-decision) random-numbers)]
          [`(variable-except ,vars ...)
           (generate/retry (λ (var) (not (memq var vars))) 'variable)]
          [`variable ((next-variable-decision) lang-chars lang-lits bound-vars attempt)]
          [`variable-not-otherwise-mentioned
           (generate/retry (λ (var) (not (memq var (compiled-lang-literals lang)))) 'variable)]
          [`(variable-prefix ,prefix) 
           (string->symbol (string-append (symbol->string prefix)
                                          (symbol->string (loop 'variable in-hole))))]
          [`string ((next-string-decision) lang-chars lang-lits attempt)]
          [`(side-condition ,pattern ,(? procedure? condition))
           (define (condition-bindings bindings)
             (make-bindings (hash-map bindings (λ (name exp) (make-bind name exp)))))
           (generate/retry (λ _ (condition (condition-bindings matches))) pattern)]
          [`(name ,(? symbol? id) ,p)
           (define (generate/record)
             (let ([term (loop p in-hole)])
               (set! matches (hash-set matches id term))
               term))
           (hash-ref matches id generate/record)]
          [`hole (if in-hole (in-hole) the-hole)]
          [`(in-hole ,context ,contractum)
           (loop context (λ () (loop contractum in-hole)))]
          [`(hide-hole ,pattern) (loop pattern #f)]
          [`any
           (let-values ([(lang nt) ((next-any-decision) lang)])
             (generate* lang nt size attempt decisions@))]
          [(and (? symbol?) (? (λ (x) (or (is-nt? lang x) (underscored-built-in? x)))))
           (define ((generate-nt/underscored decorated) undecorated)
             (let* ([vars (append (extract-bound-vars decorated found-vars-table) bound-vars)]
                    [term (if (is-nt? lang undecorated)
                              (generate-nt undecorated vars size in-hole)
                              (generate-pat undecorated vars null size in-hole))])
               (begin
                 (set! found-vars-table (extend-found-vars decorated term found-vars-table))
                 term)))
           (match (symbol->string pat)
             [(regexp #rx"^([^_]*)_[^_]*$" (list _ undecorated))
              (hash-ref
               matches pat
               (λ ()
                 (let ([term ((generate-nt/underscored pat) (string->symbol undecorated))])
                   (set! matches (hash-set matches pat term))
                   term)))]
             [(regexp #rx"([^_]*)_!_[^_]*$" (list _ undecorated))
              (let* ([prior (hash-ref mismatches pat null)]
                     [term (generate/retry 
                            (λ (t) (not (member t prior)))
                            (string->symbol undecorated)
                            #:gen (generate-nt/underscored pat))])
                (set! mismatches (hash-set mismatches pat (cons term prior)))
                term)]
             [else ((generate-nt/underscored pat) pat)])]
          [(or (? symbol?) (? number?) (? string?) (? boolean?)) pat]
          [(? null? pat) '()]
          [(list-rest seq '... rest)
           (loop (expand-sequence seq ((next-sequence-decision)) rest) in-hole)]
          [(list-rest seq (? named-ellipsis? name) rest)
           (let* ([match-len (hash-ref matches name #f)]
                  [seq-len 
                   (if match-len 
                       match-len
                       (let ([len ((next-sequence-decision))])
                         (begin
                           (set! matches (hash-set matches name len))
                           len)))])
             (loop (expand-sequence seq seq-len rest) in-hole))]
          [(list-rest pat rest)
           (cons (loop pat in-hole) (loop rest in-hole))]
          [else
           (error 'generate "unknown pattern ~s\n" pat)]))))
  
  (define (extract-bound-vars pat found-vars-table)
    (let loop ([found-vars-table found-vars-table])
      (cond
        [(null? found-vars-table) '()]
        [else (let ([found-vars (car found-vars-table)])
                (if (eq? pat (found-vars-nt found-vars))
                    (found-vars-bound-vars found-vars)
                    (loop (cdr found-vars-table))))])))
    
  (define (extend-found-vars pat res found-vars-table)
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
     found-vars-table))
  
  (define (expand-sequence seq-pat seq-len rest-pat)
    (let loop ([remaining seq-len] [acc-pat rest-pat])
      (if (zero? remaining) 
          acc-pat
          (loop (sub1 remaining) (cons seq-pat acc-pat)))))
  
  (generate-pat nt '() '() size #f))

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
           (when (is-nt? lang pat)
             (set! nts (cons pat nts)))]
          [(or (? number?) (? string?) (? procedure?) (? boolean?)) (void)]
          [`() (void)]
          [`(,a ,'... . ,b) 
           (loop a)
           (loop b)]
          [`(,a . ,b)
           (loop a)
           (loop b)]))
      nts))
  
  (let loop ()
    (set! changed? #f)
    (for-each process-nt (compiled-lang-lang lang))
    (when changed?
      (loop)))
  
  (let ([ht (make-hash)])
    (for-each
     (λ (nt) (hash-set! ht (nt-name nt) (map process-rhs (nt-rhs nt))))
     (compiled-lang-lang lang))
    ht))

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

;; is-nt? : compiled-lang symbol -> boolean
(define (is-nt? lang sym)
  (ormap (λ (nt) (eq? (nt-name nt) (symbol->nt sym)))
         (compiled-lang-lang lang)))

;; underscored-built-in? : symbol -> boolean
(define (underscored-built-in? sym) 
  (and (memq #\_ (string->list (symbol->string sym)))
       (memq (symbol->nt sym) underscore-allowed)
       #t))

;; named-ellipsis? : any -> boolean
(define (named-ellipsis? x)
  (and (symbol? x)
       (memq #\_ (string->list (symbol->string x)))
       (eq? (symbol->nt x) '...)))

(define-syntax check
  (syntax-rules ()
    [(_ lang ([id pat] ...) attempts size property)
     (let loop ([remaining attempts])
       (if (zero? remaining)
           #t
           (let ([attempt (add1 (- attempts remaining))])
             (term-let 
              ([id (generate lang pat size attempt)] ...)
              (let ([generated (term ((,'id id) ...))])
                (if (with-handlers 
                        ([exn:fail? (λ (exn) (error 'check "term ~s raises ~s" generated exn))])
                      property)
                    (loop (sub1 remaining))
                    (format "failed after ~s attempts: ~s" 
                            attempt generated)))))))]))

(define-syntax (generate stx)
  (syntax-case stx ()
    [(_ lang pat size attempt)
     (syntax (generate lang pat size attempt random-decisions@))]
    [(_ lang pat size attempt decisions@)
     (with-syntax ([rewritten 
                    (rewrite-side-conditions/check-errs 
                     (language-id-nts #'lang 'generate)
                     'generate
                     #f
                     #'pat)])
       (syntax (generate* lang `rewritten size attempt decisions@)))]))

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

(define (sexp? x)
  (or (not (pair? x)) (and (list? x) (andmap sexp? x))))

(provide pick-from-list pick-var pick-length min-prods decisions^ 
         is-nt? lang-literals pick-char random-string pick-string
         check pick-nt unique-chars pick-any sexp generate)

(provide/contract
 [find-base-cases (-> compiled-lang? hash?)])

