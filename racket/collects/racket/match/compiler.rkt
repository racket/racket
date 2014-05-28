#lang racket/base

(require (for-template racket/base "runtime.rkt" racket/stxparam racket/unsafe/ops)
         syntax/boundmap
         syntax/stx
         "patterns.rkt"
         "split-rows.rkt"
         "reorder.rkt"
         racket/stxparam
         racket/syntax)

(provide compile*)

;; for non-linear patterns
(define vars-seen (make-parameter null))

(define (hash-on f elems #:equal? [eql #t])
  (define ht (if eql (make-hash) (make-hasheq)))
  ;; put all the elements e in the ht, indexed by (f e)
  (for ([r
         ;; they need to be in the original order when they come out
         (reverse elems)])
    (define k (f r))
    (hash-set! ht k (cons r (hash-ref ht k (lambda () null)))))
  ht)

;; generate a clause of kind k
;; for rows rows, with matched variable x and rest variable xs
;; escaping to esc
(define (gen-clause k rows x xs esc)
  (define-syntax-rule (constant-pat predicate-stx)
    (with-syntax ([rhs (compile* (cons x xs)
                                 (map (lambda (row)
                                        (define-values (p ps)
                                          (Row-split-pats row))
                                        (define p* (Atom-p p))
                                        (make-Row (cons p* ps)
                                                  (Row-rhs row)
                                                  (Row-unmatch row)
                                                  (Row-vars-seen row)))
                                      rows)
                                 esc)])
      #`[(#,predicate-stx #,x) rhs]))
  (define (compile-con-pat accs pred pat-acc)
    (with-syntax* ([(tmps ...) (generate-temporaries accs)]
                   [(accs ...) accs]
                   [pred pred]
                   [body (compile*
                          (append (syntax->list #'(tmps ...)) xs)
                          (map (lambda (row)
                                  (define-values (p1 ps) (Row-split-pats row))
                                  (make-Row (append (pat-acc p1) ps)
                                            (Row-rhs row)
                                            (Row-unmatch row)
                                            (Row-vars-seen row)))
                                rows)
                           esc)])
      #`[(pred #,x) (let ([tmps (accs #,x)] ...) body)]))
  (cond
    [(eq? 'box k)
     (compile-con-pat (list #'unsafe-unbox*) #'box? (compose list Box-p))]
    [(eq? 'pair k)
     (compile-con-pat (list #'unsafe-car #'unsafe-cdr) #'pair?
                      (lambda (p) (list (Pair-a p) (Pair-d p))))]
    [(eq? 'mpair k)
     ; XXX These should be unsafe-mcar* when mpairs have chaperones
     (compile-con-pat (list #'unsafe-mcar #'unsafe-mcdr) #'mpair?
                      (lambda (p) (list (MPair-a p) (MPair-d p))))]
    [(eq? 'string k)  (constant-pat #'string?)]
    [(eq? 'number k)  (constant-pat #'number?)]
    [(eq? 'symbol k)  (constant-pat #'symbol?)]
    [(eq? 'keyword k) (constant-pat #'keyword?)]
    [(eq? 'char k)    (constant-pat #'char?)]
    [(eq? 'bytes k)   (constant-pat #'bytes?)]
    [(eq? 'regexp k)  (constant-pat #'regexp?)]
    [(eq? 'boolean k) (constant-pat #'boolean?)]
    [(eq? 'null k)    (constant-pat #'null?)]
    ;; vectors are handled specially
    ;; because each arity is like a different constructor
    [(eq? 'vector k)
     (let ([ht (hash-on (lambda (r) 
                          (length (Vector-ps (Row-first-pat r)))) rows)])
       (with-syntax ([(clauses ...)
                      (hash-map
                       ht
                       (lambda (arity rows)
                         (define ns (build-list arity values))
                         (with-syntax ([(tmps ...) (generate-temporaries ns)])
                           (with-syntax ([body
                                          (compile*
                                           (append (syntax->list #'(tmps ...)) xs)
                                           (map (lambda (row)
                                                  (define-values (p1 ps)
                                                    (Row-split-pats row))
                                                  (make-Row (append (Vector-ps p1) ps)
                                                            (Row-rhs row)
                                                            (Row-unmatch row)
                                                            (Row-vars-seen row)))
                                                rows)
                                           esc)]
                                         [(n ...) ns])
                             #`[(#,arity)
                                (let ([tmps (unsafe-vector-ref #,x n)] ...)
                                  body)]))))])
         #`[(vector? #,x)
            (case (unsafe-vector-length #,x)
              clauses ...
              [else (#,esc)])]))]
    ;; it's a structure
    [(box? k)
     ;; all the rows are structures with the same predicate
     (let* ([s (Row-first-pat (car rows))]
            [accs (Struct-accessors s)]
            [accs (if (Struct-complete? s)
                      (build-list (length accs) (λ (i) #`(λ (x) (unsafe-struct-ref x #,i))))
                      accs)]
            [pred (Struct-pred s)])
       (compile-con-pat accs pred Struct-ps))]
    [else (error 'match-compile "bad key: ~a" k)]))


;; produces the syntax for a let clause
(define (compile-one vars block esc)
  (define-values (first rest-pats) (Row-split-pats (car block)))
  (define x (car vars))
  (define xs (cdr vars))
  (cond
    ;; the Exact rule
    [(Exact? first)
     (let ([ht (hash-on (compose Exact-v Row-first-pat) block #:equal? #t)])
       (with-syntax ([(clauses ...)
                      (hash-map
                       ht
                       (lambda (k v)
                         #`[(equal? #,x '#,k)
                            #,(compile* xs
                                        (map (lambda (row)
                                               (make-Row (cdr (Row-pats row))
                                                         (Row-rhs row)
                                                         (Row-unmatch row)
                                                         (Row-vars-seen row)))
                                             v)
                                        esc)]))])
         #`(cond clauses ... [else (#,esc)])))]
    ;; the Var rule
    [(Var? first)
     (let ([transform
            (lambda (row)
              (define-values (p ps) (Row-split-pats row))
              (define v (Var-v p))
              (define seen (Row-vars-seen row))              
              ;; a new row with the rest of the patterns
              (cond
                ;; if this was a wild-card variable, don't bind
                [(Dummy? p) (make-Row ps
                                      (Row-rhs row)
                                      (Row-unmatch row)
                                      (Row-vars-seen row))]
                ;; if we've seen this variable before, check that it's equal to
                ;; the one we saw
                [(for/or ([e seen])
                         (let ([v* (car e)] [id (cdr e)])
                           (and (bound-identifier=? v v*) id)))                 
                 =>
                 (lambda (id)
                   (make-Row ps
                             #`(if ((match-equality-test) #,x #,id)
                                 #,(Row-rhs row)
                                 (fail))
                             (Row-unmatch row)
                             seen))]
                ;; otherwise, bind the matched variable to x, and add it to the
                ;; list of vars we've seen
                [else (let ([v* (free-identifier-mapping-get
                                 (current-renaming) v (lambda () v))])
                        (make-Row ps
                                  #`(let ([#,v* #,x]) #,(Row-rhs row))
                                  (Row-unmatch row)
                                  (cons (cons v x) (Row-vars-seen row))))]))])
       ;; compile the transformed block
       (compile* xs (map transform block) esc))]
    ;; the Constructor rule
    [(CPat? first)
     (let ;; put all the rows in the hash, indexed by their constructor
         ([ht (hash-on (lambda (r) (pat-key (Row-first-pat r))) block)])
       (with-syntax ([(clauses ...)
                      (hash-map
                       ht (lambda (k v) (gen-clause k v x xs esc)))])
         #`(cond clauses ... [else (#,esc)])))]
    ;; the Or rule
    [(Or? first)
     ;; we only handle 1-row Ors atm - this is all the mixture rule should give
     ;; us
     (unless (null? (cdr block))
       (error 'compile-one "Or block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            [seen (Row-vars-seen row)]
            ;; all the pattern alternatives
            [qs (Or-ps (car pats))]
            ;; the variables bound by this pattern - they're the same for the
            ;; whole list
            [vars 
             (for/list ([bv (bound-vars (car qs))]
                        #:when (for/and ([seen-var seen])
                                        (not (free-identifier=? bv (car seen-var)))))
               bv)])
       (with-syntax ([(esc* success? var ...) (append (generate-temporaries '(esc* success?)) vars)])
         ;; do the or matching, and bind the results to the appropriate
         ;; variables
         #`(let ([esc* (lambda () (values #f #,@(for/list ([v vars]) #'#f)))])
             (let-values ([(success? var ...)
                           #,(compile* (list x)
                                       (map (lambda (q)
                                              (make-Row (list q)
                                                        #'(values #t var ...)
                                                        #f
                                                        seen))
                                            qs)
                                       #'esc*)])
               ;; then compile the rest of the row
               (if success?
                   #,(compile* xs
                               (list (make-Row (cdr pats)
                                               (Row-rhs row)
                                               (Row-unmatch row)
                                               (append (map cons vars vars) seen)))
                               esc)
                   (#,esc))))))]
    ;; the App rule
    [(App? first)
     ;; we only handle 1-row Apps atm - this is all the mixture rule should
     ;; give us
     (unless (null? (cdr block))
       (error 'compile-one "App block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            [app-pats (App-ps first)])
       (with-syntax ([(t ...) (generate-temporaries app-pats)])
         #`(let-values ([(t ...) (#,(App-expr first) #,x)])
             #,(compile* (append (syntax->list #'(t ...)) xs)
                         (list (make-Row (append app-pats (cdr pats))
                                         (Row-rhs row)
                                         (Row-unmatch row)
                                         (Row-vars-seen row)))
                         esc))))]
    ;; the And rule
    [(And? first)
     ;; we only handle 1-row Ands 
     ;; this is all the mixture rule should give us
     (unless (null? (cdr block))
       (error 'compile-one "And block with multiple rows: ~a" block))
     (define row (car block))
     (define pats (Row-pats row))
     ;; all the patterns
     (define qs (And-ps (car pats)))
     (compile* (append (map (lambda _ x) qs) xs)
               (list (make-Row (append qs (cdr pats))
                               (Row-rhs row)
                               (Row-unmatch row)
                               (Row-vars-seen row)))
               esc
               ;; don't re-order OrderedAnd patterns
               (not (OrderedAnd? first)))]
    ;; the Not rule
    [(Not? first)
     ;; we only handle 1-row Nots atm - this is all the mixture rule should
     ;; give us
     (unless (null? (cdr block))
       (error 'compile-one "Not block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            ;; the single pattern
            [q (Not-p (car pats))])
       (with-syntax ([(f) (generate-temporaries #'(f))])
         #`(let ;; if q fails, we jump to here
                ([f (lambda ()
                      #,(compile* xs
                                  (list (make-Row (cdr pats)
                                                  (Row-rhs row)
                                                  (Row-unmatch row)
                                                  (Row-vars-seen row)))
                                  esc))])
             #,(compile* (list x)
                         ;; if q doesn't fail, we jump to esc and fail the not
                         ;; pattern
                         (list (make-Row (list q)
                                         #`(#,esc)
                                         (Row-unmatch row)
                                         (Row-vars-seen row)))
                         #'f))))]
    [(Pred? first)
     ;; multiple preds iff they have the identical predicate
     (with-syntax ([pred? (Pred-pred first)]
                   [body (compile* xs
                                   (map (lambda (row)
                                          (define-values (_1 ps)
                                            (Row-split-pats row))
                                          (make-Row ps
                                                    (Row-rhs row)
                                                    (Row-unmatch row)
                                                    (Row-vars-seen row)))
                                        block)
                                   esc)])
       #`(cond [(pred? #,x) body] [else (#,esc)]))]
     ;; Generalized sequences... slightly tested
    [(GSeq? first)
     (let* ([headss (GSeq-headss first)]
            [mins (GSeq-mins first)]
            [maxs (GSeq-maxs first)]
            [onces? (GSeq-onces? first)]
            [tail (GSeq-tail first)]
            [mutable? (GSeq-mutable? first)]
            [make-Pair (if mutable? make-MPair make-Pair)]
            [k (Row-rhs (car block))]
            [xvar (car (generate-temporaries (list #'x)))]
            [complete-heads-pattern
             (lambda (ps)
               (define (loop ps pat)
                 (if (pair? ps)
                     (make-Pair (car ps) (loop (cdr ps) pat))
                     pat))
               (loop ps (make-Var xvar)))]
            [heads
             (for/list ([ps headss])
               (complete-heads-pattern ps))]
            [head-idss
             (for/list ([heads headss])
               (apply append (map bound-vars heads)))]
            [hid-argss (map generate-temporaries head-idss)]
            [head-idss* (map generate-temporaries head-idss)]
            [hid-args (apply append hid-argss)]
            [reps (generate-temporaries (for/list ([head heads]) 'rep))])
       (with-syntax ([x xvar]
                     [var0 (car vars)]
                     [((hid ...) ...) head-idss]
                     [((hid* ...) ...) head-idss*]
                     [((hid-arg ...) ...) hid-argss]
                     [(rep ...) reps]
                     [(maxrepconstraint ...)
                      ;; FIXME: move to side condition to appropriate pattern
                      (for/list ([repvar reps] [maxrep maxs])
                        (if maxrep #`(< #,repvar #,maxrep) #`#t))]
                     [(minrepclause ...)
                      (for/list ([repvar reps] [minrep mins] #:when minrep)
                        #`[(< #,repvar #,minrep) (fail)])]
                     [((hid-rhs ...) ...)
                      (for/list ([hid-args hid-argss] [once? onces?])
                        (for/list ([hid-arg hid-args])
                          (if once?
                              #`(car (reverse #,hid-arg))
                              #`(reverse #,hid-arg))))]
                     [(parse-loop failkv fail-tail)
                      (generate-temporaries #'(parse-loop failkv fail-tail))])
         (with-syntax ([(rhs ...)
                        #`[(let ([hid-arg (cons hid* hid-arg)] ...)
                             (if maxrepconstraint
                                 (let ([rep (add1 rep)])
                                   (parse-loop x #,@hid-args #,@reps fail))
                                 (begin (fail))))
                           ...]]
                       [tail-rhs
                        #`(cond minrepclause ...
                                [else
                                 (let ([hid hid-rhs] ... ...
                                       [fail-tail fail])
                                   #,(compile*
                                      (cdr vars)
                                      (list (make-Row rest-pats k
                                                      (Row-unmatch (car block))
                                                      (Row-vars-seen
                                                       (car block))))
                                      #'fail-tail))])])
           (parameterize ([current-renaming
                           (for/fold ([ht (copy-mapping (current-renaming))])
                               ([id (apply append head-idss)]
                                [id* (apply append head-idss*)])
                             (free-identifier-mapping-put! ht id id*)
                             (free-identifier-mapping-for-each
                              ht
                              (lambda (k v)
                                (when (free-identifier=? v id)
                                  (free-identifier-mapping-put! ht k id*))))
                             ht)])
             #`(let parse-loop ([x var0]
                                [hid-arg null] ... ...
                                [rep 0] ...
                                [failkv #,esc])
                 #,(compile* (list #'x)
                             (append
                              (map (lambda (pats rhs)
                                     (make-Row pats
                                               rhs
                                               (Row-unmatch (car block))
                                               (Row-vars-seen
                                                (car block))))
                                   (map list heads)
                                   (syntax->list #'(rhs ...)))
                              (list (make-Row (list tail)
                                              #`tail-rhs
                                              (Row-unmatch (car block))
                                              (Row-vars-seen
                                               (car block)))))
                             #'failkv))))))]
    [else (error 'compile "unsupported pattern: ~a\n" first)]))

(define (compile* vars rows esc [reorder? #t])
  (define (let/wrap clauses body)
    (if (stx-null? clauses)
      body
      (quasisyntax (let* #,clauses #,body))))
  (cond 
   ;; if there are no rows, then just call the esc continuation
   [(null? rows) #`(#,esc)]
    ;; if we have no variables, there are no more patterns to match
    ;; so we just pick the first RHS
   [(null? vars)
    (let ([fns
           (let loop ([blocks (reverse rows)] [esc esc] [acc null])
             (if (null? blocks)
                 ;; if we're done, return the blocks
                 (reverse acc)
                 (with-syntax
                  (;; f is the name this block will have
                   [(f) (generate-temporaries #'(f))]
                   ;; compile the block, with jumps to the previous esc
                   [c (with-syntax ([rhs #`(syntax-parameterize
                                            ([fail (make-rename-transformer
                                                    (quote-syntax #,esc))])
                                            #,(Row-rhs (car blocks)))])
                                   (define unmatch (Row-unmatch (car blocks)))
                                   (if unmatch
                                       (quasisyntax/loc unmatch
                                         (call-with-continuation-prompt
                                          (lambda () (let ([#,unmatch
                                                            (lambda ()
                                                              (abort-current-continuation match-prompt-tag))])
                                                       rhs))
                                          match-prompt-tag
                                          (lambda () (#,esc))))
                                       #'rhs))])
                  ;; then compile the rest, with our name as the esc
                  (loop (cdr blocks) #'f (cons #'[f (lambda () c)] acc)))))])
      (with-syntax ([(fns ... [_ (lambda () body)]) fns])
                   (let/wrap #'(fns ...) #'body)))]
    ;; otherwise, we split the matrix into blocks
    ;; and compile each block with a reference to its continuation
   [else
    (let*-values
        ([(rows vars) (if reorder?
                          (reorder-columns rows vars)
                          (values rows vars))]
         [(fns)
          (let loop ([blocks (reverse (split-rows rows))] [esc esc] [acc null])
            (if (null? blocks)
                ;; if we're done, return the blocks
                (reverse acc)
                (with-syntax (;; f is the name this block will have
                              [(f) (generate-temporaries #'(f))]
                              ;; compile the block, with jumps to the previous
                              ;; esc
                              [c (compile-one vars (car blocks) esc)])
                  ;; then compile the rest, with our name as the esc
                  (loop (cdr blocks) 
                        #'f 
                        (cons #`[f #,(syntax-property
                                      #'(lambda () c)
                                      'typechecker:called-in-tail-position #t)]
                              acc)))))])
      (with-syntax ([(fns ... [_ (lambda () body)]) fns])
        (let/wrap #'(fns ...) #'body)))]))

;; (require mzlib/trace)
;; (trace compile* compile-one)
