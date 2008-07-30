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

(define (generate lang nt size attempt [decisions@ random-decisions@])
  (define-values/invoke-unit decisions@
    (import) (export decisions^))
  
  (define lang-lits (lang-literals lang))
  (define lang-chars (unique-chars lang-lits))
  (define base-table (find-base-cases lang))
  
  (define (generate-nt nt bound-vars size holes)
    (let loop ([nts (compiled-lang-lang lang)])
      (cond
        [(null? nts) (error 'generate-nt "didn't find non-terminal ~s" nt)]
        [(eq? (nt-name (car nts)) nt) 
         (let* ([prods (if (zero? size) (min-prods (car nts) base-table) (nt-rhs (car nts)))]
                [rhs ((next-non-terminal-decision) prods bound-vars size)]
                [size (max 0 (sub1 size))])
           (generate-pat (rhs-pattern rhs) bound-vars (rhs-var-info rhs) size holes))]
        [else (loop (cdr nts))])))
    
  (define-struct found-vars (nt source bound-vars found-nt?))
  (define (generate-pat pat bound-vars var-info size holes)
    (let* ([found-vars-table (map (λ (binds) (make-found-vars (binds-binds binds) (binds-source binds) '() #f))
                                  var-info)]
           [bindings (make-immutable-hasheq null)]
           [mismatches (make-immutable-hasheq null)])
      (let loop ([pat pat] [holes holes])
        (define (generate/retry #:gen [gen (λ (p) (loop p holes))] success? . subpatterns)
          (let ([old-fvt found-vars-table] 
                [old-bindings bindings]
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
                          (set! bindings old-bindings)
                          (set! mismatches old-mismatches)
                          (retry (sub1 remaining)))))))))
        
        (define (generate-hole name)
          (let* ([not-in-hole (gensym)]
                 [generate-contractum (hash-ref holes name not-in-hole)])
            (if (eq? generate-contractum not-in-hole)
                (if name (make-hole/intern name) (term hole))
                (generate-contractum))))
        (match pat
          [`number ((next-number-decision) random-numbers)]
          [`(variable-except ,vars ...)
           (generate/retry (λ (var) (not (memq var vars))) 'variable)]
          [`variable ((next-variable-decision) lang-chars lang-lits bound-vars attempt)]
          [`(variable-prefix ,prefix) 
           (string->symbol (string-append (symbol->string prefix)
                                          (symbol->string (loop 'variable holes))))]
          [`string ((next-string-decision) lang-chars lang-lits attempt)]
          [`(side-condition ,pattern ,(? procedure? condition))
           (define (condition-bindings bindings)
             (make-bindings (hash-map bindings (λ (name exp) (make-bind name exp)))))
           (generate/retry (λ _ (condition (condition-bindings bindings))) pattern)]
          [`(side-condition ,pattern ,uncompiled-condition)
           (error 'generate "side-condition not compiled: ~s" pat)]
          [`(name ,(? symbol? id) ,p)
           (define (generate/record)
             (let ([term (loop p holes)])
               (set! bindings (hash-set bindings id term))
               term))
           (hash-ref bindings id generate/record)]
          [`hole (generate-hole #f)]
          [`(in-hole ,context ,contractum)
           (loop context (hash-set holes #f (λ () (loop contractum holes))))]
          [`(hole ,(? symbol? name)) (generate-hole name)]
          [`(in-named-hole ,name ,context ,contractum)
           (loop context (hash-set holes name (λ () (loop contractum holes))))]
          [(and (? symbol?) (? (λ (x) (or (is-nt? lang x) (underscored-built-in? x)))))
           (define ((generate-nt/underscored decorated) undecorated)
             (let* ([vars (append (extract-bound-vars decorated found-vars-table) bound-vars)]
                    [term (if (is-nt? lang undecorated)
                              (generate-nt undecorated vars size holes)
                              (generate-pat undecorated vars null size holes))])
               (begin
                 (set! found-vars-table (extend-found-vars decorated term found-vars-table))
                 term)))
           (match (symbol->string pat)
             [(regexp #rx"^([^_]*)_[^_]*$" (list _ undecorated))
              (hash-ref
               bindings pat
               (λ ()
                 (let ([term ((generate-nt/underscored pat) (string->symbol undecorated))])
                   (set! bindings (hash-set bindings pat term))
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
          [(? pair? pat)
           (if (or (null? (cdr pat))
                   (not (eq? '... (cadr pat))))
               (cons (loop (car pat) holes)
                     (loop (cdr pat) holes))
               (append (build-list ((next-sequence-decision)) (λ (i) (loop (car pat) holes)))
                       (loop (cddr pat) holes)))]
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
  
  (generate-pat nt '() '() size (make-immutable-hash null)))

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
  (not (false? (and (memq #\_ (string->list (symbol->string sym)))
                    (memq (symbol->nt sym) underscore-allowed)))))

(define (try lang nt pred? #:attempts [attempts 1000] #:size [size 6])
  (let loop ([i attempts])
    (if (zero? i)
        (fprintf (current-error-port) "No failures in ~a attempts\n" attempts)
        (let ([t (generate lang nt size (- attempts i))])
          (if (pred? t) 
              (loop (- i 1))
              (begin
                (fprintf (current-error-port) "FAILED!\n")
                (pretty-print t (current-error-port))))))))

(define-syntax check
  (syntax-rules ()
    [(_ lang (id ...) expr attempts size)
     (try lang (quote (id ...)) 
          (λ (pat) 
            (let-values ([(id ...) (apply values pat)])
              (term-let ([id id] ...) expr)))
          #:attempts attempts #:size size)]))

(define-signature decisions^
  (next-variable-decision
   next-number-decision
   next-non-terminal-decision
   next-sequence-decision
   next-string-decision))

(define random-decisions@
  (unit (import) (export decisions^)
        (define (next-variable-decision) pick-var)
        (define (next-number-decision) pick-from-list)
        (define (next-non-terminal-decision) pick-nt)
        (define (next-sequence-decision) pick-length)
        (define (next-string-decision) pick-string)))

(define (sexp? x)
  (or (not (pair? x)) (and (list? x) (andmap sexp? x))))

(provide pick-from-list pick-var pick-length min-prods decisions^ 
         is-nt? lang-literals pick-char random-string pick-string
         check pick-nt unique-chars)

(provide/contract
 [generate any/c]
 [try (->* (compiled-lang? sexp?  (-> any/c any))
           (#:attempts number? #:size number?)
           void?)]
 [find-base-cases (-> compiled-lang? hash?)])

