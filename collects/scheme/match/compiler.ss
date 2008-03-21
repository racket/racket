#lang scheme/base

(require (for-template scheme/base "patterns.ss" scheme/stxparam)
         mzlib/trace
         mzlib/etc
         syntax/boundmap
         syntax/stx
         "patterns.ss"
         "split-rows.ss"
         scheme/struct-info
         scheme/stxparam
         (only-in srfi/1 delete-duplicates))

(provide compile*)

;; for non-linear patterns
(define vars-seen (make-parameter null))

(define (hash-on f elems #:equal? [eql #t])
  (define ht (apply make-hash-table (if eql (list 'equal) null)))
  ;; put all the elements e in the ht, indexed by (f e)
  (for-each (lambda (r)
              (define k (f r))
              (hash-table-put! ht k (cons r (hash-table-get ht k (lambda () null)))))
            ;; they need to be in the original order when they come out
            (reverse elems))
  ht)

;; generate a clause of kind k
;; for rows rows, with matched variable x and rest variable xs
;; escaping to esc
(define (gen-clause k rows x xs esc)
  (define-syntax-rule (constant-pat predicate-stx)
    (with-syntax 
        ([rhs
          (compile* (cons x xs)
                    (map (lambda (row)
                           (define-values (p ps) (Row-split-pats row))
                           (define p* (Atom-p p))
                           (make-Row (cons p* ps) (Row-rhs row) (Row-unmatch row) (Row-vars-seen row)))
                         rows)
                    esc)])
      #`[(#,predicate-stx #,x) rhs]))
  (cond
    [(eq? 'box k) 
     (with-syntax ([(v) (generate-temporaries #'(v))])
       (with-syntax
           ([body (compile* 
                   (cons #'v xs)
                   (map (lambda (r)
                          (define-values (p1 ps) (Row-split-pats r))
                          (make-Row (cons (Box-p p1) ps) (Row-rhs r) (Row-unmatch r) (Row-vars-seen r)))
                        rows)
                   esc)])
         #`[(box? #,x)
            (let ([v (unbox #,x)])
              body)]))]
    [(eq? 'pair k)
     (with-syntax ([(v1 v2) (generate-temporaries #'(v1 v2))])
       (with-syntax
           ([body (compile*
                   (list* #'v1 #'v2 xs)
                   (map (lambda (r)
                          (define-values (p1 ps) (Row-split-pats r))
                          (make-Row (list* (Pair-a p1) (Pair-d p1) ps) (Row-rhs r) (Row-unmatch r) (Row-vars-seen r)))
                        rows)
                   esc)])
         #`[(pair? #,x)
            (let ([v1 (car #,x)]
                  [v2 (cdr #,x)])
              body)]))]
    [(eq? 'string k)  (constant-pat #'string?)]
    [(eq? 'number k)  (constant-pat #'number?)]
    [(eq? 'symbol k)  (constant-pat #'symbol?)]
    [(eq? 'keyword k) (constant-pat #'keyword?)]
    [(eq? 'char k)    (constant-pat #'char?)]
    [(eq? 'bytes k)   (constant-pat #'bytes?)]
    [(eq? 'regexp k)  (constant-pat #'regexp?)]
    [(eq? 'boolean k) (constant-pat #'boolean?)]
    [(eq? 'null k)    (constant-pat #'null?)]
    [(eq? 'vector k)
     (let ()
       (define ht (hash-on (lambda (r) (length (Vector-ps (Row-first-pat r)))) rows))
       (with-syntax ([(clauses ...)
                      (hash-table-map 
                       ht
                       (lambda (arity rows)
                         (define ns (build-list arity values))
                         (with-syntax ([(tmps ...) (generate-temporaries ns)])
                           (with-syntax
                               ([body (compile* (append (syntax->list #'(tmps ...)) xs)
                                                (map (lambda (row)
                                                       (define-values (p1 ps) (Row-split-pats row))
                                                       (make-Row (append (Vector-ps p1) ps) 
                                                                 (Row-rhs row)
                                                                 (Row-unmatch row)
                                                                 (Row-vars-seen row)))
                                                     rows)
                                                esc)]
                                [(n ...) ns])
                             #`[(#,arity) 
                                (let ([tmps (vector-ref #,x n)] ...)
                                  body)]))))])
         #`[(vector? #,x)
            (case (vector-length #,x)
              clauses ...)]))]    
    ;; it's a structure
    [(box? k)     
     ;; all the rows are structures with the same predicate
     (let* ([s (Row-first-pat (car rows))]
            [accs (Struct-accessors s)]
            [pred (Struct-pred s)])
       (with-syntax ([(tmps ...) (generate-temporaries accs)])
         (with-syntax ([(accs ...) accs]
                       [pred pred]
                       [body (compile*
                              (append (syntax->list #'(tmps ...)) xs)
                              (map (lambda (row)
                                     (define-values (p1 ps) (Row-split-pats row))
                                     (make-Row (append (Struct-ps p1) ps) (Row-rhs row) (Row-unmatch row) (Row-vars-seen row)))
                                   rows)
                              esc)])
           #`[(pred #,x)
              (let ([tmps (accs #,x)] ...)
                body)])))]
    [else (error 'compile "bad key: ~a" k)]))

;; produces the syntax for a let clause
(define (compile-one vars block esc)
  (define-values (first rest-pats) (Row-split-pats (car block)))
  (define x (car vars))
  (define xs (cdr vars))
  (cond 
    ;; the Exact rule
    [(Exact? first)
     (let ([ht (hash-on (compose Exact-v Row-first-pat) block #:equal? #t)])
       (with-syntax ([(clauses ...) (hash-table-map 
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
     (let ([transform (lambda (row)
                        (define-values (p ps) (Row-split-pats row))
                        (define v (Var-v p))
                        (define seen (Row-vars-seen row))
                        ;; a new row with the rest of the patterns
                        (cond 
                          ;; if this was a wild-card variable, don't bind                                      
                          [(Dummy? p) (make-Row ps (Row-rhs row) (Row-unmatch row) (Row-vars-seen row))]
                          ;; if we've seen this variable before, check that it's equal to the one we saw
                          [(ormap (lambda (e) 
                                    (let ([v* (car e)]
                                          [id (cdr e)])
                                      (and (bound-identifier=? v v*) id)))
                                  seen)
                           =>
                           (lambda (id)
                             (make-Row ps
                                       #`(if ((match-equality-test) #,x #,id)
                                             #,(Row-rhs row)
                                             (fail))
                                       (Row-unmatch row)
                                       seen))]
                          ;;otherwise, bind the matched variable to x, and add it to the list of vars we've seen
                          [else (make-Row ps
                                          #`(let ([#,v #,x]) #,(Row-rhs row))
                                          (Row-unmatch row)
                                          (cons (cons v x) (Row-vars-seen row)))]))])
       ;; compile the transformed block
       (compile* xs (map transform block) esc))]
    ;; the Constructor rule
    [(CPat? first)
     (let ;; put all the rows in the hash-table, indexed by their constructor
         ([ht (hash-on (lambda (r) (pat-key (Row-first-pat r))) block)])
       (with-syntax ([(clauses ...) (hash-table-map ht (lambda (k v) (gen-clause k v x xs esc)))])
         #`(cond clauses ... [else (#,esc)])))]
    ;; the Or rule
    [(Or? first)
     ;; we only handle 1-row Ors atm - this is all the mixture rule should give us
     (unless (null? (cdr block))
       (error 'compile-one "Or block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            ;; all the pattern alternatives
            [qs (Or-ps (car pats))]
            ;; the variables bound by this pattern - they're the same for the whole list
            [vars (bound-vars (car qs))])
       (with-syntax ([vars vars])
         ;; do the or matching, and bind the results to the appropriate variables
         #`(let/ec exit
             (let ([esc* (lambda () (exit (#,esc)))])
               (let-values ([vars #,(compile* (list x) (map (lambda (q) (make-Row (list q) #'(values . vars) #f (Row-vars-seen row)))
                                                            qs)
                                              #'esc*)])
                 ;; then compile the rest of the row
                 #,(compile* xs 
                             (list (make-Row (cdr pats) (Row-rhs row) (Row-unmatch row) 
                                             (let ([vs (syntax->list #'vars)])
                                               (append (map cons vs vs) (Row-vars-seen row)))))
                             esc))))))]
    ;; the App rule
    [(App? first)
     ;; we only handle 1-row Apps atm - this is all the mixture rule should give us
     (unless (null? (cdr block))
       (error 'compile-one "App block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)])
       (with-syntax ([(t) (generate-temporaries #'(t))])
         #`(let ([t (#,(App-expr first) #,x)])
             #,(compile* (cons #'t xs)
                         (list (make-Row (cons (App-p first) (cdr pats)) (Row-rhs row) (Row-unmatch row) (Row-vars-seen row)))
                         esc))))]
    ;; the And rule
    [(And? first)
     ;; we only handle 1-row Ands atm - this is all the mixture rule should give us
     (unless (null? (cdr block))
       (error 'compile-one "And block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            ;; all the patterns
            [qs (And-ps (car pats))])
       (compile* (append (map (lambda _ x) qs) xs)
                 (list (make-Row (append qs (cdr pats)) (Row-rhs row) (Row-unmatch row) (Row-vars-seen row)))
                 esc))]
    ;; the Not rule
    [(Not? first)
     ;; we only handle 1-row Nots atm - this is all the mixture rule should give us
     (unless (null? (cdr block))
       (error 'compile-one "Not block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            ;; the single pattern
            [q (Not-p (car pats))])
       (with-syntax ([(f) (generate-temporaries #'(f))])
         #`(let
               ;; if q fails, we jump to here
               ([f (lambda ()
                     #,(compile* xs
                                 (list (make-Row (cdr pats) (Row-rhs row) (Row-unmatch row) (Row-vars-seen row)))
                               esc))])
             #,(compile* (list x)
                         ;; if q doesn't fail, we jump to esc and fail the not pattern
                         (list (make-Row (list q) #`(#,esc) (Row-unmatch row) (Row-vars-seen row)))
                         #'f))))]
     [(Pred? first)
      ;; multiple preds iff they have the identical predicate     
     (with-syntax ([pred? (Pred-pred first)]
                   [body (compile* xs
                                   (map (lambda (row)
                                          (define-values (_1 ps) (Row-split-pats row))
                                          (make-Row ps (Row-rhs row) (Row-unmatch row) (Row-vars-seen row)))
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
            [k (Row-rhs (car block))]
            [xvar (car (generate-temporaries (list #'x)))]
            [complete-heads-pattern
             (lambda (ps)
               (define (loop ps pat)
                 (if (pair? ps)
                     (make-Pair (car ps)
                                (loop (cdr ps) pat))
                     pat))
               (loop ps (make-Var xvar)))]
            [heads
             (for/list ([ps headss])
               (complete-heads-pattern ps))]
            [head-idss
             (for/list ([heads headss])
               (apply append (map bound-vars heads)))]
            [hid-argss (map generate-temporaries head-idss)]
            [hid-args (apply append hid-argss)]
            [reps (generate-temporaries (for/list ([head heads]) 'rep))])
       (with-syntax ([x xvar]
                     [var0 (car vars)]
                     [((hid ...) ...) head-idss]
                     [((hid-arg ...) ...) hid-argss]
                     [(rep ...) reps]
                     [(maxrepconstraint ...)
                      ;; FIXME: move to side condition to appropriate pattern
                      (for/list ([repvar reps] [maxrep maxs])
                        (if maxrep
                            #`(< #,repvar #,maxrep)
                            #`#t))]
                     [(minrepclause ...)
                      (for/list ([repvar reps] [minrep mins] #:when minrep)
                        #`[(< #,repvar #,minrep)
                           (fail)])]
                     [((hid-rhs ...) ...)
                      (for/list ([hid-args hid-argss] [once? onces?])
                                (for/list ([hid-arg hid-args])
                                          (if once?
                                              #`(car (reverse #,hid-arg))
                                              #`(reverse #,hid-arg))))]
                     [(parse-loop failkv fail-tail) (generate-temporaries #'(parse-loop failkv fail-tail))])
         (with-syntax ([(rhs ...)
                        #`[(let ([hid-arg (cons hid hid-arg)] ...)
                             (if maxrepconstraint
                                 (let ([rep (add1 rep)])
                                   (parse-loop x #,@hid-args #,@reps fail))
                                 (begin
                                   (fail))))
                           ...]]
                       [tail-rhs
                        #`(cond minrepclause ...
                                [else
                                 (let ([hid hid-rhs] ... ...
                                       [fail-tail fail])
                                   #,(compile* (cdr vars) 
                                               (list (make-Row rest-pats k (Row-unmatch (car block)) (Row-vars-seen (car block))))
                                               #'fail-tail))])])
           #`(let parse-loop ([x var0] [hid-arg null] ... ... [rep 0] ... [failkv #,esc])
               #,(compile* (list #'x)
                             (append
                              (map (lambda (pats rhs) (make-Row pats rhs (Row-unmatch (car block)) null))
                                   (map list heads)
                                   (syntax->list #'(rhs ...)))
                              (list (make-Row (list tail) #`tail-rhs (Row-unmatch (car block)) null)))
                             #'failkv)))))]
    ;; doesn't work, never called
    #;
     [(VectorSeq? first)
     (let*-values ([(row) (car block)]
                   [(p ps) (Row-split-pats row)]
                   [(head) (VectorSeq-p p)]
                   [(start) (VectorSeq-start p)]
                   [(expr) (Row-rhs row)]
                   [(count) (VectorSeq-count p)]
                   [(head-vars) (bound-vars head)])
       (with-syntax ([var0 (car vars)]
                     [(x) (generate-temporaries #'(x))]
                     [(hid ...) head-vars]
                     [(hid-arg ...) (generate-temporaries head-vars)]
                     [(parse-k parse-loop head-var tail-var fail reps len)
                      (generate-temporaries
                       #'(parse-k parse-loop head-var tail-var fail reps len))])
         #`(if (vector? var0)
               (let ([len (vector-length var0)])
                 (define (parse-k hid ...)
                   #,(compile* xs
                               (list (make-Row ps expr))
                               esc))                 
                 (define (parse-loop reps hid-arg ...)
                   (define (fail)
                     (parse-k (reverse hid-arg) ...))
                   (if (and 
                        (< reps len)
                        #,@(if (number? count)
                               #`((reps . < . '#,(+ start count)))
                               #'()))
                       (let ([head-var (vector-ref var0 reps)])
                         #,(compile*
                            (list #'head-var)
                            (list
                             (make-Row (list head)
                                       #`(parse-loop (add1 reps)
                                                     (cons hid hid-arg) ...)))
                            #'fail))
                       (fail)))
                 (let ([hid null] ...)
                   (parse-loop #,start hid ...)))
                 (#,esc))))]
     [else (error 'compile "unsupported pattern: ~a~n" first)]))

(define (compile* vars rows esc)
  (define (let/wrap clauses body)
    (if (stx-null? clauses) 
        body
        (quasisyntax (let* #,clauses #,body))))
  (if (null? vars)
      ;; if we have no variables, there are no more patterns to match
      ;; so we just pick the first RHS
      (let ([fns
             (let loop ([blocks (reverse rows)] [esc esc] [acc null])
               (cond
                 ;; if we're done, return the blocks
                 [(null? blocks) (reverse acc)]
                 [else (with-syntax (;; f is the name this block will have
                                     [(f) (generate-temporaries #'(f))]
                                     ;; compile the block, with jumps to the previous esc
                                     [c (with-syntax ([rhs #`(syntax-parameterize ([fail (make-rename-transformer (quote-syntax #,esc))])
                                                                                  #,(Row-rhs (car blocks)))])
                                          (if 
                                           (Row-unmatch (car blocks))
                                           #`(let/ec k
                                               (let ([#,(Row-unmatch (car blocks)) (lambda () (k (#,esc)))])
                                                 rhs))
                                           #'rhs))])
                         ;; then compile the rest, with our name as the esc
                         (loop (cdr blocks) #'f (cons #'[f (lambda () c)] acc)))]))])
        (with-syntax ([(fns ... [_ (lambda () body)]) fns])
          (let/wrap #'(fns ...) #'body)))
      
      ;; otherwise, we split the matrix into blocks
      ;; and compile each block with a reference to its continuation
      (let ([fns
             (let loop ([blocks (reverse (split-rows rows))] [esc esc] [acc null])                      
               (cond 
                 ;; if we're done, return the blocks
                 [(null? blocks) (reverse acc)]
                 [else (with-syntax (;; f is the name this block will have
                                     [(f) (generate-temporaries #'(f))]
                                     ;; compile the block, with jumps to the previous esc
                                     [c (compile-one vars (car blocks) esc)])
                         ;; then compile the rest, with our name as the esc
                         (loop (cdr blocks) #'f (cons #'[f (lambda () c)] acc)))]))])
        (with-syntax ([(fns ... [_ (lambda () body)]) fns])
          (let/wrap #'(fns ...) #'body)))))







;(trace compile* compile-one)
