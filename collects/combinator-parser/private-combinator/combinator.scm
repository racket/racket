(module combinator scheme/base
  
  (require scheme/unit
           scheme/list
           (only-in (lib "etc.ss") opt-lambda))
  
  (require "structs.scm"
           "parser-sigs.ss"
           parser-tools/lex)
  
  (provide (all-defined-out))
  
  (define-unit combinators@
    (import error-format-parameters^ ranking-parameters^ language-dictionary^)
    (export combinator-parser-forms^)
    
    (define return-name "dummy")
    (define terminal-occurs "unique-eq")
    
    (define (make-weak-map) (make-weak-hasheq))
 
    (define (weak-map-put! m k v)
      (hash-set! m k (make-ephemeron k (box v))))
 
    (define weak-map-get 
      (opt-lambda (m k [def-v (lambda () (error 'weak-map-get "value unset"))])
        (let ([v (hash-ref m k #f)])
          (if v 
              (let ([v (ephemeron-value v)])
                (if v 
                    (unbox v)
                    def-v))
              def-v))))
    
    ;terminal: ('a -> bool 'a -> 'b string) -> ( (list 'a) -> res )
    (define terminal
      (opt-lambda (pred build name [spell? #f] [case? #f] [class? #f])
        (let* ([memo-table (make-weak-map)]
               [fail-str (string-append "failed " name)]
               [t-name (if src? (lambda (t) (token-name (position-token-token t))) token-name)]
               [t-val (if src? (lambda (t) (token-value (position-token-token t))) token-value)]
               [spell? (or spell? 
                           (lambda (token)
                             (if (t-val token) (misspelled name (t-val token)) 0)))]
               [case? (or case?
                          (lambda (token)
                            (and (t-val token) (misscap name (t-val token)))))]
               [class? (or class? (lambda (token) (missclass name (t-name token))))]
               [make-fail
                (lambda (c n k i u)
                  (make-terminal-fail c (if (and src? i)
                                            (make-src-lst (position-token-start-pos i)
                                                          (position-token-end-pos i))
                                            null)
                                      n 0 u k (if src? (position-token-token i) i)))]
               [value (lambda (t) (or (t-val t) name))]
               [builder
                (if src?
                    (lambda (token) (build (position-token-token token)
                                           (position-token-start-pos token)
                                           (position-token-end-pos token)))
                    build)])
          
          (opt-lambda (input [last-src (list 1 0 1 0)] [alts 1])
            #;(printf "terminal ~a\n" name)
            #;(cond
                [(eq? input return-name) (printf "name requested\n")]
                [(null? input) (printf "null input\n")]
                [else
                 (let ([token (position-token-token (car input))])
                   (printf "Token given ~a, match? ~a\n" token (pred token)))])
            (cond
              [(eq? input return-name) name]
              [(eq? input terminal-occurs) (list (make-occurs name 1))]
              [(weak-map-get memo-table input #f) (weak-map-get memo-table input)]
              [else
               (let ([result
                      (cond
                        [(null? input) 
                         (fail-res null (make-terminal-fail rank-end last-src name 0 0 'end #f))]
                        [else 
                         (let* ([curr-input (car input)]
                                [token (if src? (position-token-token curr-input) curr-input)])
                           (cond
                             [(pred token)
                              (make-res (list (builder curr-input)) 
                                        (cdr input) name 
                                        (value curr-input) 1 #f curr-input)]
                             [else 
                              #;(printf "Incorrect input for ~a : ~a miscase? ~a misspell? ~a \n" name 
                                        (cond 
                                          [(token-value token) (token-value token)]
                                          [else (token-name token)])
                                        (case? curr-input)
                                        (spell? curr-input))
                              (fail-res (cdr input) 
                                        (let-values ([(chance kind may-use)
                                                      (cond
                                                        [(case? curr-input) (values rank-caps 'misscase 1)]
                                                        [(> (spell? curr-input) 3/5)
                                                         (values (* rank-misspell 
                                                                  (spell? curr-input)) 'misspell 1)]
                                                      [(class? curr-input) (values rank-class 'missclass 1)]
                                                      [else (values rank-wrong 'wrong 0)])])
                                          (make-fail chance name kind curr-input may-use)))]))])])
                 (weak-map-put! memo-table input result)
                 result)])))))

    ;seq: ( (list ((list 'a) -> res)) ((list 'b) -> 'c) string -> ((list 'a) -> result)
    (define seq 
      (opt-lambda (sub-list build name [id-position 0])
        (let* ([sequence-length (length sub-list)]
               [memo-table (make-weak-map)]
               [terminal-counts #f]
               [prev (lambda (x) 
                       (cond [(eq? x return-name) "default previous"]
                             [else (fail-res null null)]))]
               [builder 
                (lambda (r) 
                  (cond
                    [(res? r)
                     (make-res (list (build (res-a r))) 
                               (res-rest r) 
                               name (res-id r) (res-used r)
                               (res-possible-error r)
                               (res-first-tok r))]
                    [(and (repeat-res? r) (res? repeat-res-a r))
                     (make-res (list (build (res-a (repeat-res-a r))))
                               (res-rest (repeat-res-a r))
                               name (res-id (repeat-res-a r))
                               (res-used (repeat-res-a r))
                               (repeat-res-stop r)
                               (res-first-tok (repeat-res-a r)))]
                    [else (error 'parser-internal-error1 (format "~a" r))]))]
               [my-error (sequence-error-gen name sequence-length)]
               [my-walker (seq-walker id-position name my-error)])
          (opt-lambda (input [last-src (list 1 0 1 0)] [alts 1])
            #;(unless (eq? input return-name) (printf "seq ~a\n" name))
            (cond
              [(eq? input return-name) name]
              [(eq? input terminal-occurs)
               (or terminal-counts
                   (begin
                     (set! terminal-counts 'counting)
                     (set! terminal-counts
                           (consolidate-count (map (lambda (symbol) (symbol terminal-occurs)) sub-list)))
                     terminal-counts))]
              [(weak-map-get memo-table input #f) 
               (weak-map-get memo-table input)]
              [(null? sub-list)
               (builder (make-res null input name #f 0 #f #f))]
              [else
               (let* ([pre-build-ans (my-walker sub-list input prev #f #f #f null 0 alts last-src)]
                      [ans 
                       (cond
                         [(and (res? pre-build-ans) (res-a pre-build-ans)) (builder pre-build-ans)]
                         [(and (pair? pre-build-ans) (null? (cdr pre-build-ans))) (builder (car pre-build-ans))]
                         [(pair? pre-build-ans) (map builder pre-build-ans)]
                         [else pre-build-ans])])
                 (weak-map-put! memo-table input ans)
                 #;(printf "sequence ~a returning \n" name)
                 #;(printf "answer is ~a \n" ans)
                 ans)])))))
    
    ;seq-walker: int string error-gen -> [(list parser) (list alpha) parser result (U bool string) (list string) int int -> result
    (define (seq-walker id-position seq-name build-error)
      (letrec ([next-res 
                (lambda (a id used tok rst)
                  (cond
                    [(res? rst)
                     (make-res (append a (res-a rst)) (res-rest rst)
                               seq-name (or id (res-id rst))
                               (+ used (res-used rst)) (res-possible-error rst) tok)]
                    [(and (repeat-res? rst) (res? (repeat-res-a rst)))
                     (make-res (append a (res-a (repeat-res-a rst)))
                               (res-rest (repeat-res-a rst)) seq-name
                               (or id (res-id (repeat-res-a rst)))
                               (+ used (res-used (repeat-res-a rst)))
                               (repeat-res-stop rst) tok)]
                    [else (error 'parser-internal-error2 (format "~a" rst))]
                    ))]
               [walker
                (lambda (subs input previous? look-back look-back-ref curr-id seen used alts last-src)
                  (let* ([next-preds (cdr subs)]
                         [curr-pred (car subs)]
                         [id-spot? (= id-position (add1 (length seen)))]
                         [next-call
                          (lambda (old-result curr curr-ref curr-name new-id tok alts)
                            (cond 
                              [(res? old-result)
                               (let* ([old-answer (res-a old-result)]
                                      [rest (res-rest old-result)]
                                      [old-used (res-used old-result)]
                                      [rsts (walker next-preds rest curr-pred curr curr-ref
                                                    (or new-id curr-id) (cons curr-name seen) 
                                                    (+ old-used used) alts 
                                                    (if (and src? (res-first-tok old-result)) 
                                                        (make-src-lst (position-token-start-pos (res-first-tok old-result))
                                                                      (position-token-end-pos (res-first-tok old-result)))
                                                        last-src))])
                                 #;(printf "next-call ~a ~a: ~a ~a ~a ~a\n" 
                                           seq-name (length seen) old-result (res? rsts)
                                           (and (res? rsts) (res-a rsts))
                                           (and (res? rsts) (choice-fail? (res-possible-error rsts))))
                                 (cond
                                   [(and (res? rsts) (res-a rsts))
                                    (next-res old-answer new-id old-used tok rsts)]
                                   [(res? rsts) (fail-res rest (res-msg rsts))]
                                   [(and (lazy-opts? rsts) (null? (lazy-opts-thunks rsts)))
                                    (make-lazy-opts
                                     (map (lambda (rst) (next-res old-answer new-id old-used tok rst))
                                          (lazy-opts-matches rsts))
                                     (make-options-fail 0 #f #f 0 0 null) null)]
                                   [(and (lazy-opts? rsts) (not (lazy-choice? rsts)))
                                    (make-lazy-opts
                                     (map (lambda (rst) (next-res old-answer new-id old-used tok rst))
                                          (lazy-opts-matches rsts))
                                     (lazy-opts-errors rsts)
                                     (map (lambda (thunk)
                                            (lambda ()
                                              (let ([ans (next-opt rsts)])
                                                (and ans (next-res old-answer new-id old-used tok ans)))))
                                          (lazy-opts-thunks rsts)))]
                                   [(lazy-choice? rsts)
                                    (make-lazy-choice
                                     (map (lambda (rst) (next-res old-answer new-id old-used tok rst))
                                          (lazy-opts-matches rsts))
                                     (lazy-opts-errors rsts)
                                     (map (lambda (thunk)
                                            (lambda ()
                                              (let ([ans (next-opt rsts)])
                                                (and ans (next-res old-answer new-id old-used tok ans)))))
                                          (lazy-opts-thunks rsts))
                                     (lazy-choice-name rsts))]
                                   [(pair? rsts)
                                    (map (lambda (rst) (next-res old-answer new-id old-used tok rst))
                                         (flatten (correct-list rsts)))]
                                   [(choice-res? rsts)
                                    #;(printf "next call, tail-end is choice ~a\n" rsts)
                                    (map (lambda (rst) (next-res old-answer new-id old-used tok 
                                                                 (update-possible-fail rst rsts)))
                                         (flatten (correct-list (choice-res-matches rsts))))]
                                   [(repeat-res? rsts)
                                    (next-res old-answer new-id old-used tok rsts)]
                                   [else (error 'parser-internal-error3 (format "~a" rsts))]))]
                              [else (error 'parser-internal-error11 (format "~a" old-result))]))])
                    (cond
                      [(null? subs) (error 'end-of-subs)]
                      [(null? next-preds)
                       #;(printf "seq-walker called: last case, ~a case of ~a \n" 
                               seq-name (curr-pred return-name))
                       (build-error (curr-pred input last-src) 
                                    (lambda () (previous? input)) 
                                    (previous? return-name) #f
                                    look-back look-back-ref used curr-id seen alts last-src)]
                      [else
                       #;(printf "seq-walker called: else case, ~a case of ~a ~ath case \n" 
                               seq-name (curr-pred return-name) (length seen))
                       (let ([fst (curr-pred input last-src)])
                         (cond
                           [(res? fst)
                            #;(printf "res case ~a ~a\n" seq-name (length seen))
                            (cond
                              [(res-a fst) (next-call fst fst fst (res-msg fst) 
                                                      (and id-spot? (res-id fst))
                                                      (res-first-tok fst) alts)]
                              [else
                               #;(printf "error situation ~a ~a\n" seq-name (length seen))
                               (build-error fst (lambda () (previous? input))
                                            (previous? return-name) 
                                            (car next-preds) look-back look-back-ref used curr-id 
                                            seen alts last-src)])]
                           [(repeat-res? fst)
                            #;(printf "repeat-res: ~a ~a\n" seq-name (length seen))
                            #;(printf "res? ~a\n" (res? (repeat-res-a fst)))
                            (next-call (repeat-res-a fst) fst fst
                                       (res-msg (repeat-res-a fst)) #f 
                                       (res-first-tok (repeat-res-a fst)) alts)]
                           [(lazy-opts? fst)
                            #;(printf "lazy res: ~a ~a ~a\n" fst seq-name (length seen))
                            (let* ([opt-r (make-lazy-opts null 
                                                          (make-options-fail 0 last-src seq-name 0 0 null)
                                                          null)]
                                   [name (if (lazy-choice? fst) (lazy-choice-name fst) seq-name)]
                                   [next-c (lambda (res)
                                             (cond 
                                               [(res? res)
                                                #;(printf "lazy-choice-res, res ~a ~a\n" seq-name (length seen))
                                                (next-call res fst res name (and id-spot? (res-id res))
                                                           (res-first-tok res) alts)]
                                               [(repeat-res? res)
                                                #;(printf "lazy- choice-res, repeat-res ~a ~a ~a\n"
                                                          (res? (repeat-res-a res)) seq-name (length seen))
                                                (next-call (repeat-res-a res) res (repeat-res-a res)
                                                           (res-msg (repeat-res-a res)) #f 
                                                           (res-first-tok (repeat-res-a res))
                                                           alts)]
                                               [else (error 'parser-internal-errora (format "~a" res))]))]
                                   [parsed-options (map (lambda (res) (lambda () (next-c res)))
                                                        (lazy-opts-matches fst))]
                                   [unparsed-options
                                    (map
                                     (lambda (thunked)
                                       (lambda ()
                                         (let ([res (next-opt fst)])
                                           (if res 
                                               (next-c res)
                                               (begin (set-lazy-opts-thunks! opt-r null) #f)))))
                                     (lazy-opts-thunks fst))])
                              (set-lazy-opts-thunks! opt-r (append parsed-options unparsed-options))
                              (if (next-opt opt-r)
                                  opt-r
                                  (fail-res input (lazy-opts-errors opt-r))))
                              ]
                           [(or (choice-res? fst) (pair? fst))
                            #;(printf "choice-res: ~a ~a ~a\n" fst seq-name (length seen))
                            (let*-values
                                ([(lst name curr)
                                  (cond
                                    [(choice-res? fst) 
                                     (values (choice-res-matches fst)
                                             (lambda (_) (choice-res-name fst))
                                             (lambda (_) fst))]
                                    [else (values fst res-msg (lambda (x) x))])]
                                 [(new-alts) (+ alts (length lst))]
                                 [(rsts)
                                  (map (lambda (res)
                                         (cond 
                                           [(res? res)
                                            #;(printf "choice-res, res ~a ~a\n" seq-name (length seen))
                                            (next-call res (curr res) res (name res) 
                                                       (and id-spot? (res-id res))
                                                       (res-first-tok res) new-alts)]
                                           [(repeat-res? res)
                                            #;(printf "choice-res, repeat-res ~a ~a ~a\n"
                                                         (res? (repeat-res-a res)) seq-name (length seen))
                                            (next-call (repeat-res-a res) res (repeat-res-a res)
                                                       (res-msg (repeat-res-a res)) #f 
                                                       (res-first-tok (repeat-res-a res))
                                                       new-alts)]
                                           [else (error 'parser-internal-error4 (format "~a" res))]))
                                       (flatten lst))]
                                 [(correct-rsts) (flatten (correct-list rsts))])
                              #;(printf "case ~a ~a, choice case: intermediate results are ~a\n"
                                      seq-name (length seen) lst)
                              (cond
                                [(and (null? correct-rsts) (or (not (lazy-choice? fst))
                                                               (null? (lazy-opts-thunks fst))))
                                 #;(printf "correct-rsts null for ~a ~a \n" seq-name (length seen))
                                 (let ([fails 
                                        (map 
                                         (lambda (rst)
                                           (res-msg 
                                            (build-error rst (lambda () (previous? input)) (previous? return-name)
                                                         (car next-preds) look-back look-back-ref used curr-id seen alts last-src)))
                                         rsts)])
                                   (fail-res input 
                                             (make-options-fail 
                                              (rank-choice (map fail-type-chance fails))
                                              (if (equal? last-src (list 1 0 1 0))
                                                  (map fail-type-src fails)
                                                  last-src)
                                              seq-name
                                              (rank-choice (map fail-type-used fails))
                                              (rank-choice (map fail-type-may-use fails)) fails)))]
                                [(and (null? correct-rsts) (lazy-choice? fst) (not (null? (lazy-opts-thunks fst))))
                                 (let loop ([next-res (next-opt fst)])
                                   (when next-res (loop (next-opt fst))))]
                                [else correct-rsts]))]
                           [else (error 'here3 (format "~a" fst))]))])))])
        walker))
    
    ;get-fail-info: fail-type -> (values symbol 'a 'b)
    (define (get-fail-info fail)
      (cond
        [(terminal-fail? fail)
         (values (terminal-fail-kind fail)
                 (fail-type-name fail)
                 (terminal-fail-found fail))]
        [(sequence-fail? fail)
         (values 'sub-seq (sequence-fail-expected fail) fail)]
        [(choice-fail? fail) (values 'choice null fail)]
        [(options-fail? fail) (values 'options null fail)]
        [else (error 'parser-internal-error5 (format "~a" fail))]))
    
    ;update-src: symbol src-list src-list token -> src-list
    (define (update-src error-kind src prev-src tok)
      (and src?
           (case error-kind
             [(choice options) prev-src]
             [(sub-seq misscase misspell end) src]
             [(missclass wrong) 
              (if tok
                  (update-src-start src (position-token-start-pos tok))
                  src)])))
    
    ;build-options-fail: name (list-of fail-type) -> fail-type
    (define (build-options-fail name fails)
      (make-options-fail (rank-choice (map fail-type-chance fails))
                         #f
                         name
                         (rank-choice (map fail-type-used fails))
                         (rank-choice (map fail-type-may-use fails))
                         fails))
    
    (define (add-to-choice-fails choice fail)
      (let ([fails (choice-fail-messages choice)])
        (make-choice-fail
         (rank-choice (cons (fail-type-chance fail) (map fail-type-chance fails)))
         (fail-type-src choice)
         (fail-type-name choice)
         (rank-choice (cons (fail-type-used fail) (map fail-type-used fails)))
         (rank-choice (cons (fail-type-may-use fail) (map fail-type-may-use fails)))
         (choice-fail-options choice)
         (choice-fail-names choice)
         (choice-fail-ended? choice)
         (cons fail fails))))
    
    ;update-possible-rail result result -> result
    (define (update-possible-fail res back)
      #;(printf "update-possible-fail ~a, ~a\n" res back)
      (cond
        [(and (res? res) (not (res-possible-error res)))
         (cond 
           [(res? back)
            (make-res (res-a res) (res-rest res) (res-msg res) (res-id res) (res-used res)
                      (res-possible-error back) (res-first-tok res))]
           [(choice-res? back)
            (make-res (res-a res) (res-rest res) (res-msg res) (res-id res) (res-used res)
                      (choice-res-errors back) (res-first-tok res))]
           [else res])]
        [(choice-res? res)
         (cond 
           [(and (choice-res? back) (choice-res-errors back) (choice-res-errors res))
            (make-choice-res (choice-res-name res)
                             (choice-res-matches res)
                             (add-to-choice-fails (choice-res-errors res)
                                                  (choice-res-errors back)))]
           
           [else res])]
        [else res]))
    
    ;build-sequence-error: result boolean result string int [U #f string] [listof string] int int -> result
    (define (sequence-error-gen name len)
      (letrec ([repeat->res
                (lambda (rpt back)
                  (cond 
                    [(pair? rpt) (map (lambda (r) (repeat->res r back)) (flatten rpt))]
                    [(and (repeat-res? rpt) (res? (repeat-res-a rpt)))
                     (let ([inn (repeat-res-a rpt)]
                           [stop (repeat-res-stop rpt)])
                       #;(printf "in repeat->res for ~a\n" name)
                       #;(when (fail-type? stop)
                         (printf "stoped on ~a\n" (fail-type-name stop)))
                       #;(printf "stop ~a\n" stop)
                       #;(when (choice-res? back)
                         (printf "back on ~a\n" (choice-res-name back)))
                       #;(when (choice-res? back) (printf "choice-res-errors back ~a\n" 
                                                        (choice-res-errors back)))
                       #;(when (and (fail-type? stop)
                                  (choice-res? back)
                                  (choice-res-errors back))
                         (printf "chances ~a > ~a -> ~a \n" 
                                 (fail-type-chance (choice-res-errors back))
                                 (fail-type-chance stop)
                                 (>= (fail-type-chance (choice-res-errors back))
                                    (fail-type-chance stop))))
                       (cond
                         [(fail-type? stop) 
                          (make-res (res-a inn) (res-rest inn) (res-msg inn) (res-id inn) (res-used inn)
                                    stop
                                    #;(if (and (zero? (res-used inn))
                                             (choice-res? back) (choice-res-errors back)
                                             (>= (fail-type-chance (choice-res-errors back))
                                                 (fail-type-chance stop)))
                                        (build-options-fail name
                                                            (list (choice-res-errors back)
                                                                  stop))
                                        stop)
                                    (res-first-tok inn))]
                         [else inn]))]
                    [else rpt]))]
               )
        (lambda (old-res prev prev-name next-pred look-back look-back-ref used id seen alts last-src)
          (cond
            [(and (pair? old-res) (null? (cdr old-res)) (res? (car old-res))) 
             (update-possible-fail (car old-res) look-back)]
            [(and (pair? old-res) (null? (cdr old-res)) (repeat-res? (car old-res)))
             (repeat->res (car old-res) look-back)]
            [(or (and (res? old-res) (res-a old-res)) (choice-res? old-res) (lazy-opts? old-res))
             (update-possible-fail old-res look-back)]
            [(repeat-res? old-res) 
             #;(printf "finished on repeat-res for ~a res \n" name #;old-res)
             (repeat->res old-res look-back)]
            [(pair? old-res) 
             #;(printf "finished on pairs of res for ~a\n" name #;old-res)
             (map (lambda (r) (repeat->res r look-back)) (flatten old-res))]
            [else
             #;(printf "There was an error for ~a\n" name)
             #;(printf "length seen ~a length rest ~a\n" (length seen) (length (res-rest old-res)))
             (fail-res (res-rest old-res)
                       (let*-values ([(fail) (res-msg old-res)]
                                     [(possible-fail)
                                      (cond 
                                        [(and (repeat-res? look-back)
                                              (fail-type? (repeat-res-stop look-back))
                                              (>= (fail-type-chance (repeat-res-stop look-back))
                                                  (fail-type-chance fail)))
                                         (repeat-res-stop look-back)]
                                        [(and (choice-res? look-back)
                                              (choice-res-errors look-back)
                                              (>= (fail-type-chance (choice-res-errors look-back))
                                                  (fail-type-chance fail)))
                                         (choice-res-errors look-back)]
                                        [(and (res? look-back)
                                              (fail-type? (res-possible-error look-back))
                                              (>= (fail-type-chance (res-possible-error look-back))
                                                  (fail-type-chance fail)))
                                         (res-possible-error look-back)]
                                        [else #f])]
                                     [(next-ok?)
                                      (and (= (fail-type-may-use fail) 1)
                                           (not (null? (res-rest old-res)))
                                           next-pred
                                           (next-pred (cdr (res-rest old-res))))]
                                     [(next-used)
                                      (if (and next-ok? (res? next-ok?) (res-a next-ok?))
                                          (res-used next-ok?)
                                          0)]
                                     [(kind expected found) (get-fail-info fail)]
                                     [(new-src) (update-src kind
                                                            (fail-type-src fail)
                                                            last-src
                                                            (res-first-tok old-res))]
                                     [(seen-len) (length seen)]
                                     [(updated-len) (+ (- used seen-len) len)])
                         #;(printf "sequence ~a failed.\n seen ~a\n" name (reverse seen))
                         #;(when (repeat-res? look-back)
                             (printf "look-back repeat-res ~a : ~a vs ~a : ~a > ~a\n"
                                     (fail-type? (repeat-res-stop look-back))
                                     (and (fail-type? (repeat-res-stop look-back)) (fail-type-name (repeat-res-stop look-back)))
                                     (fail-type-name (res-msg old-res))
                                     (and (fail-type? (repeat-res-stop look-back)) (fail-type-chance (repeat-res-stop look-back)))
                                     (fail-type-chance (res-msg old-res))))
                         #;(when (choice-res? look-back)
                           (printf "look-back choice: ~a vs ~a : ~a > ~a\n"
                                   (choice-res-name look-back)
                                   (fail-type-name (res-msg old-res))
                                   (and (choice-res-errors look-back)
                                        (fail-type-chance (choice-res-errors look-back)))
                                   (fail-type-chance (res-msg old-res)))
                           (printf "look-back choice and useds: ~a vs ~a -- ~a \n"
                                   used (and (res? look-back-ref) (res-used look-back-ref))
                                        (and (choice-res-errors look-back)
                                             (fail-type-used (choice-res-errors look-back)))))
                         #;(when (pair? look-back)
                             (printf "look-back is a pair\n"))
                         #;(when (res? look-back)
                           (printf "look-back res ~a : ~a vs ~a : ~a > ~a\n"
                                   (fail-type? (res-possible-error look-back))
                                   (and (fail-type? (res-possible-error look-back)) (fail-type-name (res-possible-error look-back)))
                                   (fail-type-name (res-msg old-res))
                                   (and (fail-type? (res-possible-error look-back)) (fail-type-chance (res-possible-error look-back)))
                                   (fail-type-chance (res-msg old-res)))
                           (printf "lookback ~a\n" (res-possible-error look-back)))
                         (let* ([seq-fail-maker
                                 (lambda (fail used)
                                   (let-values ([(kind expected found) (get-fail-info fail)])
                                     (make-sequence-fail 
                                      (compute-chance len seen-len used alts 
                                                      (fail-type-may-use fail)
                                                      (fail-type-chance fail))
                                      (fail-type-src fail)
                                      name used 
                                      (+ used (fail-type-may-use fail) next-used)
                                      id kind (reverse seen) expected found 
                                      prev
                                      prev-name)))]
                                [seq-fail (seq-fail-maker fail used)]
                                [pos-fail 
                                 (and possible-fail 
                                      (seq-fail-maker possible-fail 
                                                      (if (and (choice-res? look-back)
                                                               (res? look-back-ref))
                                                          (- used (res-used look-back-ref)) used)))]
                                [opt-fails (list seq-fail pos-fail)])
                           #;(printf "pos-fail? ~a\n" (and pos-fail #t))
                           #;(printf "seq-fail ~a\n" seq-fail)
                           #;(when pos-fail 
                             (printf "used ~a look-back-ref used ~a \n"
                                     used (when (res? look-back-ref) (res-used look-back-ref)))
                             (printf "opt-fails ~a\n" opt-fails))
                           (if pos-fail
                               (make-options-fail (rank-choice (map fail-type-chance opt-fails))
                                                  (map fail-type-src opt-fails)
                                                  name
                                                  (rank-choice (map fail-type-used opt-fails))
                                                  (rank-choice (map fail-type-may-use opt-fails))
                                                  opt-fails)
                               seq-fail))))]))))
    
    (define (compute-chance expected-length seen-length used-toks num-alts may-use sub-chance)
      (let* ([revised-expectation (+ (- used-toks seen-length) expected-length)]
             [possible-expectation (+ revised-expectation (max 0 (sub1 may-use)))]
             #;[probability-with-sub (* (/ (+ may-use used-toks) possible-expectation) (/ 1 num-alts))]
             [probability-with-sub (* (/ (add1 used-toks) revised-expectation) (/ 1 num-alts))]
             [probability-without-sub (* (/ used-toks revised-expectation) (/ 1 num-alts))]
             [expected-sub probability-with-sub]
             [expected-no-sub probability-without-sub]
             [probability (/ (* expected-sub sub-chance) (+ (* expected-sub sub-chance)
                                                            (* expected-no-sub (- 1 sub-chance))))])
        
        #;(when (zero? used-toks)
          (printf "compute-chance 0 case: ~a, ~a, ~a, ~a -> ~a\n" 
                  sub-chance expected-length num-alts may-use
                  (* (/ 1 num-alts) sub-chance)))
        (cond
          #;[(zero? used-toks) (* (/ 1 num-alts) sub-chance)]
          [(zero? used-toks) sub-chance #;probability-with-sub]
          [else
           #;(printf "compute-chance: args ~a ~a ~a ~a ~a ~a\n"
                   expected-length seen-length used-toks num-alts may-use sub-chance)
           #;(printf "compute-chance: intermediate values: ~a ~a ~a ~a ~a ~a\n"
                   revised-expectation possible-expectation probability-with-sub probability-without-sub expected-sub expected-no-sub)
           #;(printf "compute-chance answer ~a\n" probability)
           probability])))
    
    ;greedy-repeat: (list 'a) -> result -> (list 'a)  -> result
    (define (repeat-greedy sub)
      (letrec ([repeat-name (lambda () (string-append "any number of " (sub return-name)))]
               [memo-table (make-weak-map)]
               [inner-memo-table (make-weak-map)]
               [process-rest
                (lambda (curr-ans rest-ans)
                  (cond 
                    [(repeat-res? rest-ans)
                     #;(printf "building up the repeat answer for ~a\n" repeat-name)
                     (cond 
                       [(res? curr-ans)
                        (let* ([a (res-a curr-ans)]
                               [rest (repeat-res-a rest-ans)]
                               [repeat-build
                                (lambda (r)
                                  (cond
                                    [(res? r)
                                     #;(printf "rest is a res for ~a, res-a is ~a \n" a repeat-name)
                                     (make-repeat-res
                                      (make-res (append a (res-a r)) (res-rest r) (repeat-name) #f 
                                                (+ (res-used curr-ans) (res-used r)) 
                                                #f (res-first-tok r))
                                      (repeat-res-stop rest-ans))]
                                    [else
                                     (error 'parser-internal-error9 (format "~a" r))]))])
                          (cond
                            [(and (pair? rest) (null? (cdr rest)))
                             #;(printf "rest is a one-element list for ~a\n" repeat-name)
                             (repeat-build (car rest))]
                            [(pair? rest)
                             #;(printf "rest is a pair for ~a ~a\n" repeat-name (length rest))
                             (map repeat-build (flatten rest))]
                            [else (repeat-build rest)]))]
                       [else (error 'parser-internal-error12 (format "~a" curr-ans))])]
                    [(pair? rest-ans)
                     (map (lambda (r) (process-rest curr-ans r)) (flatten rest-ans))]
                    [else (error 'parser-internal-error10 (format "~a" rest-ans))]))]
               [update-src
                (lambda (input prev-src)
                  (cond
                    [(null? input) prev-src]
                    [src? (src-list (position-token-start-pos (car input))
                                    (position-token-end-pos (car input)))]
                    [else prev-src]))])
        (opt-lambda (input [start-src (list 1 0 1 0)] [alts 1])
          (cond
            [(eq? input return-name) (repeat-name)]
            [(eq? input terminal-occurs) (sub terminal-occurs)]
            [(weak-map-get memo-table input #f)(weak-map-get memo-table input)]
            [else
             (let ([ans
                    (let loop ([curr-input input] [curr-src start-src])
                      #;(printf "length of curr-input for ~a ~a\n" repeat-name (length curr-input))
                      #;(printf "curr-input ~a\n" (map position-token-token curr-input))
                      (cond 
                        [(weak-map-get inner-memo-table curr-input #f)(weak-map-get inner-memo-table curr-input)]
                        [(null? curr-input) 
                         #;(printf "out of input for ~a\n" (repeat-name))
                         (make-repeat-res (make-res null null (repeat-name) "" 0 #f #f) 'out-of-input)]
                        [else
                         (let ([this-res (sub curr-input curr-src)])
                           #;(printf "Repeat of ~a called it's repeated entity \n" (repeat-name))
                           (cond
                             [(and (res? this-res) (res-a this-res))
                              #;(printf "loop again case for ~a\n" (repeat-name))
                              (process-rest this-res 
                                            (loop (res-rest this-res)
                                                  (update-src (res-rest this-res) curr-src)))]
                             [(res? this-res)
                              #;(printf "fail for error case of ~a: ~a ~a\n" 
                                      repeat-name
                                      (cond 
                                        [(choice-fail? (res-msg this-res)) 'choice]
                                        [(sequence-fail? (res-msg this-res)) 'seq]
                                        [(options-fail? (res-msg this-res)) 'options]
                                        [else 'terminal])
                                      (fail-type-chance (res-msg this-res)))
                              (let ([fail (make-repeat-res (make-res null curr-input (repeat-name) "" 0 #f #f)
                                                           (res-msg this-res))])
                                (weak-map-put! inner-memo-table curr-input fail)
                                fail)]
                             [(repeat-res? this-res)
                              #;(printf "repeat-res case of ~a\n" repeat-name)
                              (process-rest (repeat-res-a this-res)
                                            (res-rest (repeat-res-a this-res)))]
                             [(lazy-opts? this-res)
                              (let ([process (lambda (res)
                                               (cond [(res? res)
                                                      (process-rest res (loop (res-rest res) (update-src (res-rest res) curr-src)))]
                                                     [(repeat-res? res)
                                                      (process-rest (repeat-res-a res) (res-rest (repeat-res-a res)))]
                                                     [else (error 'repeat-greedy-loop (format "Internal error, given ~a" res))]))])
                                (update-lazy-opts this-res
                                                  (map process (lazy-opts-matches this-res))
                                                  (map (lambda (t)
                                                         (lambda () 
                                                           (let ([next-res (next-opt this-res)])
                                                             (and next-res (process next-res)))))
                                                       (lazy-opts-thunks this-res))))]
                             [(or (choice-res? this-res) (pair? this-res))
                              (let ([list-of-answer
                                     (if (choice-res? this-res) (choice-res-matches this-res) (flatten this-res))])
                                #;(printf "repeat call of ~a, choice-res ~a\n" 
                                        repeat-name
                                        (and (choice-res? this-res) 
                                             (length list-of-answer)))
                                (cond 
                                  [(null? (cdr list-of-answer))
                                   (process-rest (car list-of-answer) 
                                                 (loop (res-rest (car list-of-answer))
                                                       (update-src (res-rest (car list-of-answer))
                                                                   curr-src)))]
                                  [else
                                   (map (lambda (match) 
                                          #;(printf "calling repeat loop again ~a, res-rest match ~a\n" 
                                                  (repeat-name) (length (res-rest match)))
                                          (process-rest match 
                                                        (loop (res-rest match)
                                                              (update-src (res-rest match) curr-src))))
                                        list-of-answer)]))]
                             [else (error 'internal-parser-error8 (format "~a" this-res))]))]))])
               (weak-map-put! memo-table input ans)
               #;(printf "repeat of ~a ended with ans  \n" repeat-name #;ans)
               ans)]))))
    
    ;choice: [list [[list 'a ] -> result]] name -> result
    (define (choice opt-list name)
      (let ([memo-table (make-weak-map)]
            [terminal-counts #f]
            [num-choices (length opt-list)]
            [choice-names (lambda () (map (lambda (o) (o return-name)) opt-list))])
        (opt-lambda (input [last-src (list 0 0 0 0)] [alts 1])
          #;(unless (eq? input return-name) (printf "choice ~a\n" name))
          #;(printf "possible options are ~a\n" (choice-names))
          (let ([sub-opts (sub1 (+ alts num-choices))])
            (cond
              [(eq? input return-name) name]
              [(eq? input terminal-occurs)
               (or terminal-counts
                   (begin
                     (set! terminal-counts 'counting)
                     (set! terminal-counts
                           (consolidate-count (map (lambda (symbol) (symbol terminal-occurs)) opt-list)))
                     terminal-counts))]
              [(weak-map-get memo-table input #f) (weak-map-get memo-table input)]              
              [else
               #;(printf "choice ~a\n" name)
               #;(printf "possible options are ~a\n" (choice-names))
               (let*-values
                   ([(options) (map (lambda (term) (term input last-src sub-opts)) opt-list)]
                    #;[a (printf "choice-options ~a \n ~a \n\n\n" choice-names options)]
                    [(fails) (map (lambda (x) 
                                    (cond 
                                      [(res? x) (res-msg x)]
                                      [(repeat-res? x) (res-msg (repeat-res-a x))]
                                      [(choice-res? x) (choice-res-errors x)]
                                      [else (error 'here-non-res x)]))
                                  (flatten options))]
                    [(corrects errors) (split-list options)]
                    [(fail-builder)
                     (lambda (fails)
                       (if (null? fails)
                           #f
                           (make-choice-fail (rank-choice (map fail-type-chance fails)) 
                                             (if (or (null? input)
                                                     (not (position-token? (car input))))
                                                 last-src
                                                 (update-src-end 
                                                  last-src 
                                                  (position-token-end-pos (car input))))
                                             name
                                             (rank-choice (map fail-type-used fails))
                                             (rank-choice (map fail-type-may-use fails))
                                             num-choices (choice-names) 
                                             (null? input)
                                             fails)))]
                    [(ans)
                     (cond
                       [(null? corrects) (fail-res input (fail-builder fails))]
                       [else (make-choice-res name corrects (fail-builder errors))])])
                 #;(printf "choice ~a is returning options were ~a \n" name (choice-names))
                 #;(printf "corrects were ~a\n" corrects)
                 #;(printf "errors were ~a\n" errors)
                 (weak-map-put! memo-table input ans) ans)])))))
    
    ;choice: [list [[list 'a ] -> result]] name -> result
    (define (choice2 opt-list name)
      (let ([memo-table (make-weak-map)]
            [num-choices (length opt-list)]
            [choice-names (lambda () (map (lambda (o) (o return-name)) opt-list))])
        (opt-lambda (input [last-src (list 0 0 0 0)] [alts 1])
          #;(unless (eq? input return-name) (printf "choice ~a\n" name))
          #;(printf "possible options are ~a\n" choice-names)
          (let ([sub-opts (sub1 (+ alts num-choices))])
            (cond
              [(weak-map-get memo-table input #f) (weak-map-get memo-table input)]
              [(eq? input return-name) name]
              [else
               (let* ([options (map (lambda (term) (lambda () (term input last-src sub-opts))) opt-list)]
                      [initial-fail (make-choice-fail 0
                                                      (if (or (null? input) (not (position-token? (car input))))
                                                          last-src
                                                          (update-src-end last-src 
                                                                          (position-token-end-pos (car input))))
                                                      name
                                                      0
                                                      0
                                                      num-choices
                                                      (choice-names)
                                                      (null? input)
                                                      null)]
                      [initial-ans (make-lazy-choice null initial-fail options name)]
                      [ans
                       (if (next-opt initial-ans)
                           initial-ans
                           (fail-res input (lazy-opts-errors initial-ans)))])
                 #;(printf "choice ~a is returning options were ~a, answer is ~a \n" name (choice-names) ans)
                 (weak-map-put! memo-table input ans) ans)])))))
    
    (define (flatten lst)
      (cond
        [(pair? lst)
         (cond
           [(pair? (car lst))
            (append (flatten (car lst))
                    (flatten (cdr lst)))]
           [else (cons (car lst) (flatten (cdr lst)))])]
        [else null]))
    
    ;correct-list: (list result) -> (list result)
    (define (correct-list subs)
      (cond
        [(pair? subs)
         (cond 
           [(and (res? (car subs)) (res-a (car subs)))
            (cons (car subs) (correct-list (cdr subs)))]
           [(choice-res? (car subs))
            (append (choice-res-matches (car subs)) (correct-list (cdr subs)))]
           [(repeat-res? (car subs))
            (cons (repeat-res-a (car subs)) (correct-list (cdr subs)))]
           [(pair? (car subs))
            (append (car subs) (correct-list (cdr subs)))]
           [else (correct-list (cdr subs))])]
        [(null? subs) null]
        [else (error 'parser-internal-error6 (format "~a" subs))]))
    
    (define (split-list subs)
      (let loop ([in subs] [correct null] [incorrect null])
        (cond
          [(pair? in)
           (cond 
             [(and (res? (car in)) (res-a (car in)))
              (loop (cdr in) (cons (car in) correct) incorrect)]
             [(choice-res? (car in))
              (loop (cdr in) 
                    (append (choice-res-matches (car in)) correct)
                    (if (choice-res-errors (car in))
                        (cons (choice-res-errors (car in)) incorrect)
                        incorrect))]
             [(repeat-res? (car in))
              (loop (cdr in)
                    (cons (repeat-res-a (car in)) correct)
                    incorrect)]
             [(pair? (car in))
              (loop (cdr in) (append (car in) correct) incorrect)]
             [(res? (car in))
              (loop (cdr in) correct (cons (res-msg (car in)) incorrect))]
             [else (error 'split-list (car in))])]
          [(null? in) 
           (values (flatten correct) (flatten incorrect))])))
    
    (define (src-list src-s src-e)
      (list (position-line src-s)
            (position-col src-s)
            (position-offset src-s)
            (- (position-offset src-s)
               (position-offset src-e))))
    
    (define (update-src-start src new-start)
      (list (position-line new-start)
            (position-col new-start)
            (position-offset new-start)
            (+ (- (third src)
                  (position-offset new-start))
               (fourth src))))
    
    (define (update-src-end src new-end)
      (when (null? src) (error 'update-src-end))
      (list (max (first src) 1)
            (second src) 
            (max (third src) 1)
            (- (position-offset new-end) (third src))))
    
    (define (repeat op)
      (letrec ([name (lambda () (string-append "any number of " (op return-name)))]
               [r* (opt-lambda (x [s (list 0 1 0 1)] [o 1])
                     ((choice (list #;op
                                    (seq (list op r*) (lambda (list-args) list-args) (name))
                                    (seq null (lambda (x) null) "epsilon"))
                              (name)) x s o))])
        r*))
    
    )
  )
