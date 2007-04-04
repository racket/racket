(module combinator (lib "lazy.ss" "lazy")
  
  (require (lib "unit.ss")
           (only (lib "etc.ss") opt-lambda))
  
  (require "structs.scm"
           "parser-sigs.ss"
           (lib "lex.ss" "parser-tools"))
  
  (provide (all-defined))
  
  (define-unit combinators@
    (import error-format-parameters^ ranking-parameters^ language-dictionary^)
    (export combinator-parser-forms^)
    
    (define return-name "dummy")
    
    ;terminal: ('a -> bool 'a -> 'b string) -> ( (list 'a) -> res )
    (define terminal
      (opt-lambda (pred build name [spell? #f] [case? #f] [class? #f])
        (let* ([fail-str (string-append "failed " name)]
               [t-name 
                (lambda (t) (if src? (token-name (position-token-token t)) (token-name t)))]
               [t-val
                (lambda (t) (if src? (token-value (position-token-token t)) (token-value t)))]
               [spell? (if spell? spell? 
                           (lambda (token)
                             (when (position-token? token) (set! token (position-token-token token)))
                             (and (token-value token)
                                  (misspelled name (token-value token)))))]
               [case? (if case? case?
                          (lambda (token)
                            (when (position-token? token) (set! token (position-token-token token)))
                            (and (token-value token)
                                 (misscap name (token-value token)))))]
               [class? (if class? class?
                           (lambda (token) 
                             (when (position-token? token) (set! token (position-token-token token)))
                             (missclass name (token-name token))))]
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
          
          (opt-lambda (input [last-src (list 0 0 0 0)] [alts 1])
            #;(!!! (printf "terminal ~a~n" name))
            (cond
              [(eq? input return-name) name]
              [(null? input) 
               (fail-res null (make-terminal-fail rank-end last-src name 0 0 'end #f))]
              [(pred (if src? (position-token-token (car input)) (car input)))
               (make-res (list (builder (car input))) (cdr input) 
                         name (value (car input)) 1 #f (car input))]
              [else 
               #;(printf "Incorrect input for ~a : ~a miscase? ~a misspell? ~a ~n" name 
                         (cond 
                           [(and (position-token? (car input))
                                 (token-value (position-token-token (car input))))
                            (token-value (position-token-token (car input)))]
                           [(position-token? (car input))
                            (token-name (position-token-token (car input)))]
                           [else (car input)])
                         (case? (car input))
                         (spell? (car input)))
                 (fail-res (cdr input) 
                           (let-values ([(chance kind may-use)
                                         (cond
                                           [(case? (car input)) (values rank-caps 'misscase 1)]
                                           [(> (spell? (car input)) 3/5)
                                            (values (* rank-misspell (spell? (car input))) 'misspell 1)]
                                           [(class? (car input)) (values rank-class 'missclass 1)]
                                           [else (values rank-wrong 'wrong 0)])])
                             (make-fail chance name kind (car input) may-use)))])))))
    
    ;seq: ( (list ((list 'a) -> res)) ((list 'b) -> 'c) string -> ((list 'a) -> result)
    (define seq 
      (opt-lambda (sub-list build name [id-position 0])
        (let* ([sequence-length (length sub-list)]
               [memo-table (make-hash-table 'weak)]
               [prev (lambda (x) 
                       (cond [(eq? x return-name) "default previous"]
                             [else (fail-res null null)]))]
               [builder 
                (lambda (r) 
                  (cond
                    [(res? r)
                     (make-res (list (build (res-a r))) (res-rest r) 
                               name (res-id r) (res-used r)
                               (res-possible-error r)
                               (res-first-tok r))]
                    [(repeat-res? r)
                     (make-res (list (build (res-a (repeat-res-a r))))
                               (res-rest (repeat-res-a r))
                               name (res-id (repeat-res-a r))
                               (res-used (repeat-res-a r))
                               (repeat-res-stop r)
                               (res-first-tok (repeat-res-a r)))]))]
               [my-error (sequence-error-gen name sequence-length)]
               [my-walker (seq-walker id-position name my-error)])
          (opt-lambda (input [last-src (list 0 0 0 0)] [alts 1])
            #;(printf "seq ~a~n" name)
            (cond
              [(eq? input return-name) name]
              [(hash-table-get memo-table input #f) (hash-table-get memo-table input)]
              [(null? sub-list)
               (builder (make-res null input name #f 0 #f #f))]
              [else
               (let* ([pre-build-ans (my-walker sub-list input prev #f #f null 0 alts last-src)]
                      [ans 
                       (cond
                         [(and (res? pre-build-ans) (res-a pre-build-ans)) (builder pre-build-ans)]
                         [(pair? pre-build-ans) (map builder pre-build-ans)]
                         [else pre-build-ans])])
                 (hash-table-put! memo-table input ans)
                 #;(printf "sequence ~a returning ~n" name)
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
                    [(repeat-res? rst)
                     (make-res (append a (res-a (repeat-res-a rst)))
                               (res-rest (repeat-res-a rst)) seq-name
                               (or id (res-id (repeat-res-a rst)))
                               (+ used (res-used (repeat-res-a rst)))
                               (repeat-res-stop rst) tok)]))]
               [walker
                (lambda (subs input previous? look-back curr-id seen used alts last-src)
                  (let* ([next-preds (cdr subs)]
                         [curr-pred (car subs)]
                         [id-spot? (= id-position (add1 (length seen)))]
                         [next-call
                          (lambda (old-result curr curr-name new-id tok alts)
                            (let* ([old-answer (res-a old-result)]
                                   [rest (res-rest old-result)]
                                   [old-used (res-used old-result)]
                                   [rsts (walker next-preds rest curr-pred curr 
                                                 (or new-id curr-id) (cons curr-name seen) 
                                                 (+ old-used used) alts 
                                                 (if (and src? (res-first-tok old-result)) 
                                                     (make-src-lst (position-token-start-pos (res-first-tok old-result))
                                                                   (position-token-end-pos (res-first-tok old-result)))
                                                     last-src))])
                              (cond
                                [(and (res? rsts) (res-a rsts))
                                 (next-res old-answer new-id old-used tok rsts)]
                                [(res? rsts) (fail-res rest (res-msg rsts))]
                                [(pair? rsts)
                                 (map (lambda (rst) (next-res old-answer new-id old-used tok rst))
                                      rsts)])))])
                    (cond
                      [(null? next-preds)
                       (build-error (curr-pred input last-src) 
                                    (previous? input) (previous? return-name) #f
                                    look-back used curr-id seen alts last-src)]
                      [else
                       #;(printf "seq-walker called: else case, ~a case of ~a~n" 
                               seq-name (curr-pred return-name))
                         (let ([fst (curr-pred input last-src)])
                           #;(printf "seq-walker predicate returned~n")
                           (cond
                             [(res? fst)
                              (cond
                                [(res-a fst) (next-call fst fst (res-msg fst) (and id-spot? (res-id fst))
                                                        (res-first-tok fst) alts)]
                                [else
                                 (build-error fst (previous? input) (previous? return-name) 
                                              (car next-preds) look-back used curr-id seen alts last-src)])]
                             [(repeat-res? fst) (next-call (repeat-res-a fst) fst 
                                                           (res-msg (repeat-res-a fst)) #f 
                                                           (res-first-tok (repeat-res-a fst)) alts)]
                             [(or (choice-res? fst) (pair? fst))
                              #;(printf "choice-res or pair: ~a ~a ~n"
                                        (if (choice-res? fst) (map res-rest (choice-res-matches fst)) fst)
                                        (if (choice-res? fst) (map res-a (choice-res-matches fst)) fst))
                                (let*-values
                                    ([(lst name curr)
                                      (if (choice-res? fst) 
                                          (values (choice-res-matches fst)
                                                  (lambda (_) (choice-res-name fst))
                                                  (lambda (_) fst))
                                          (values fst res-msg (lambda (x) x)))]
                                     [(new-alts) (+ alts (length lst))]
                                     [(rsts)
                                      (map (lambda (res)
                                             (cond 
                                               [(res? res)
                                                (next-call res (curr res) (name res) (and id-spot? (res-id res))
                                                           (res-first-tok res) new-alts)]
                                               [(repeat-res? res)
                                                (next-call (repeat-res-a res) res
                                                           (res-msg (repeat-res-a res)) #f 
                                                           (res-first-tok (repeat-res-a res)) new-alts)])) lst)]
                                     [(correct-rsts) (correct-list rsts)])
                                  #;(printf "correct-rsts ~a~n" (map res-a correct-rsts))
                                  #;(printf "rsts: ~a~n" (map res-a rsts))
                                  (cond
                                    [(null? correct-rsts)
                                     (let ([fails (map (lambda (rst) 
                                                         (res-msg 
                                                          (build-error rst (previous? input) (previous? return-name)
                                                                       (car next-preds) look-back used curr-id seen alts last-src)))
                                                       rsts)])
                                       (fail-res input 
                                                 (make-options-fail 
                                                  (rank-choice (map fail-type-chance fails)) #f seq-name
                                                  (rank-choice (map fail-type-used fails))
                                                  (rank-choice (map fail-type-may-use fails)) fails)))]
                                    [else correct-rsts]))]))])))])
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
        [(options-fail? fail) (values 'options null fail)]))
    
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
    
    ;build-sequence-error: result boolean result string int [U #f string] [listof string] int int -> result
    (define (sequence-error-gen name len)
      (lambda (old-res prev prev-name next-pred look-back used id seen alts last-src)
        (cond
          [(and (pair? old-res) (null? (cdr old-res))) (car old-res)]
          [(repeat-res? old-res) 
           (cond
             [(fail-type? (repeat-res-stop old-res))
              (let ([res (repeat-res-a old-res)])
                (make-res (res-a res) (res-rest res) (res-msg res) (res-id res) (res-used res)
                          (repeat-res-stop old-res) (res-first-tok res)))]
             [else (repeat-res-a old-res)])]
          [(or (and (res? old-res) (res-a old-res))
               (choice-res? old-res)
               (pair? old-res)) old-res]
          [else
           ;There actually was an error
           (fail-res (res-rest old-res)
                     (let*-values ([(fail) 
                                    (cond 
                                      [(and (repeat-res? look-back)
                                            (fail-type? (repeat-res-stop look-back))
                                            (> (fail-type-chance (repeat-res-stop look-back))
                                               (fail-type-chance (res-msg old-res))))
                                       (repeat-res-stop look-back)]
                                      [else (res-msg old-res)])]
                                   [(next-ok?)
                                    (and (= (fail-type-may-use fail) 1)
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
                       #;(printf "sequence ~a failed.~n seen ~a~n" name (reverse seen))
                       #;(when (repeat-res? look-back)
                           (printf "look-back ~a : ~a vs ~a : ~a > ~a~n"
                                   (repeat-res-stop look-back)
                                   (and (fail-type? (repeat-res-stop look-back)) (fail-type-name (repeat-res-stop look-back)))
                                   (fail-type-name (res-msg old-res))
                                   (and (fail-type? (repeat-res-stop look-back)) (fail-type-chance (repeat-res-stop look-back)))
                                   (fail-type-chance (res-msg old-res))))
                       #;(printf "old-probability ~a new probability ~a~n"
                                 (cond
                                   [(= 0 used) (fail-type-chance fail)]
                                   [else (+ (* (fail-type-chance fail) (/ 1 updated-len))
                                            (/ used updated-len))])
                                 (compute-chance len seen-len used alts (fail-type-chance fail)))
                       (make-sequence-fail 
                        (cond
                          [(= 0 used) (fail-type-chance fail)]
                          [else (compute-chance len seen-len used alts (fail-type-chance fail))])
                        (fail-type-src fail)
                        name used 
                        (+ used (fail-type-may-use fail) next-used)
                        id kind (reverse seen) expected found (and (res? prev) (res-a prev) (res-msg prev))
                        prev-name)))])))
    
    (define (compute-chance expected-length seen-length used-toks num-alts sub-chance)
      (let* ([revised-expectation (+ (- used-toks seen-length) expected-length)]
             [probability-with-sub (* (/ (add1 used-toks) revised-expectation) (/ 1 num-alts))]
             [probability-without-sub (* (/ used-toks revised-expectation) (/ 1 num-alts))]
             [expected-sub probability-with-sub]
             [expected-no-sub probability-without-sub]
             [probability (/ (* expected-sub sub-chance) (+ (* expected-sub sub-chance)
                                                            (* expected-no-sub (- 1 sub-chance))))])
        #;(printf "compute-chance: args ~a ~a ~a ~a ~a~n"
                  expected-length seen-length used-toks num-alts sub-chance)
        #;(printf "compute-chance: intermediate values: ~a ~a ~a ~a ~a~n"
                  revised-expectation probability-with-sub probability-without-sub expected-sub expected-no-sub)
        probability))
    
    ;repeat: (list 'a) -> result -> (list 'a)  -> result
    (define (repeat sub)
      (letrec ([repeat-name (string-append "any number of " (sub return-name))]
               [memo-table (make-hash-table 'weak)]
               [process-rest
                (lambda (curr-ans rest-ans)
                  (cond 
                    [(repeat-res? rest-ans)
                     (let ([a (res-a curr-ans)]
                           [rest (repeat-res-a rest-ans)])
                       (make-repeat-res
                        (cond
                          [(res? rest)
                           (make-res (append a (res-a rest)) (res-rest rest) repeat-name "" 
                                     (+ (res-used curr-ans) (res-used rest)) (res-possible-error rest)
                                     (res-first-tok curr-ans))]
                          [(and (pair? rest) (null? (cdr rest)))
                           (make-res (append a (res-a (car rest))) (res-rest (car rest)) repeat-name ""
                                     (+ (res-used curr-ans) (res-used (car rest)))
                                     (res-possible-error (car rest))
                                     (res-first-tok curr-ans))]
                          [(pair? rest)
                           (correct-list 
                            (map (lambda (rs)
                                   (make-res (append a (res-a rs)) (res-rest rs) repeat-name "" 
                                             (+ (res-used curr-ans) (res-used rs))
                                             (res-possible-error rs)
                                             (res-first-tok curr-ans)))
                                 rest))])
                        (repeat-res-stop rest-ans)))]
                    [(pair? rest-ans)
                     (map (lambda (r) (process-rest curr-ans r)) rest-ans)]))])
        (opt-lambda (input [last-src (list 0 0 0 0)] [alts 1])
          (cond
            [(eq? input return-name) repeat-name]
            [(hash-table-get memo-table input #f) (hash-table-get memo-table input)]
            [else
             (let ([ans
                    (let loop ([curr-input input])
                      (cond 
                        [(null? curr-input) 
                         (make-repeat-res (make-res null null repeat-name "" 0 #f #f) 'out-of-input)]
                        [else
                         (let ([this-res (sub curr-input last-src)])
                           #;(printf "Repeat of ~a called it's repeated entity: ~a~n" repeat-name this-res)
                           (cond
                             [(and (res? this-res) (res-a this-res))
                              (process-rest this-res (loop (res-rest this-res)))]
                             [(res? this-res)
                              (make-repeat-res (make-res null curr-input repeat-name "" 0 #f #f) (res-msg this-res))]
                             [(or (choice-res? this-res) (pair? this-res))
                              (map (lambda (match) (process-rest match (loop (res-rest match))))
                                   (if (choice-res? this-res) (choice-res-matches this-res) this-res))]))]))])
               (hash-table-put! memo-table input ans)
               #;(printf "repeat of ~a ended with ans ~a~n" repeat-name ans)
               ans)]))))
    
    ;choice: [list [[list 'a ] -> result]] name -> result
    (define (choice opt-list name)
      (let ([memo-table (make-hash-table 'weak)]
            [num-choices (length opt-list)]
            [choice-names (map (lambda (o) (o return-name)) opt-list)])
        (opt-lambda (input [last-src (list 0 0 0 0)] [alts 1])
          #;(printf "choice ~a~n" name)
          (let ([sub-opts (sub1 (+ alts num-choices))])
            (cond
              [(hash-table-get memo-table input #f) (hash-table-get memo-table input)]
              [(eq? input return-name) name]
              [else
               (let* ([options (map (lambda (term) (term input last-src sub-opts)) opt-list)]
                      [fails (map res-msg options)]
                      [corrects (correct-list options)]
                      [ans
                       (cond
                         [(null? corrects)
                          (fail-res input
                                    (make-choice-fail (rank-choice (map fail-type-chance fails)) #f name
                                                      (rank-choice (map fail-type-used fails))
                                                      (rank-choice (map fail-type-may-use fails))
                                                      num-choices choice-names fails))]
                         [(null? (cdr corrects)) (car corrects)]
                         [else (make-choice-res name corrects)])])
                 #;(printf "choice ~a is returning ~a options were ~a ~n" name ans choice-names)
                 (hash-table-put! memo-table input ans) ans)])))))
    
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
            (correct-list (cons (repeat-res-a (car subs)) (cdr subs)))]
           [(pair? (car subs))
            (append (car subs) (correct-list (cdr subs)))]
           [else (correct-list (cdr subs))])]
        [(null? subs) null]))
    
    (define (update-src-start src new-start)
      (list (position-line new-start)
            (position-col new-start)
            (position-offset new-start)
            (+ (- (!!! (third src))
                  (!!! (position-offset new-start)))
               (fourth src))))
    
    (define (update-src-end src new-end)
      (list (first src) (second src) (third src)
            (- (position-offset new-end) (third src))))
    
    )
  )