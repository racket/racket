#lang racket/base
;; This module implements a parser form like the parser-tools's
;; `parser', except that it works on an arbitrary CFG (returning
;; the first sucecssful parse).

;; I'm pretty sure that this is an implementation of Earley's
;; algorithm.

;; To a first approximation, it's a backtracking parser. Alternative
;; for a non-terminal are computed in parallel, and multiple attempts
;; to compute the same result block until the first one completes.  If
;; you get into deadlock, such as when trying to match
;;    <foo> := <foo>
;; then it means that there's no successful parse, so everything
;; that's blocked fails.

;; A cache holds the series of results for a particular non-terminal
;; at a particular starting location. (A series is used, instead of a
;; sinlge result, for backtracking.) Otherwise, the parser uses
;; backtracking search. Backtracking is implemented through explicit
;; success and failure continuations. Multiple results for a
;; particular nonterminal and location are kept only when they have
;; different lengths. (Otherwise, in the spirit of finding one
;; successful parse, only the first result is kept.)

;; The parser-tools's `parse' is used to transform tokens in the
;; grammar to tokens specific to this parser. In other words, this
;; parser uses `parser' so that it doesn't have to know anything about
;; tokens.


(require parser-tools/yacc
         parser-tools/lex)

(require (for-syntax racket/base
                     syntax/boundmap
                     parser-tools/private-lex/token-syntax))

(provide cfg-parser)

;; A raw token, wrapped so that we can recognize it:
(define-struct tok (name orig-name val start end))

;; Represents the thread scheduler:
(define-struct tasks (active active-back waits multi-waits cache progress?))

(define-for-syntax make-token-identifier-mapping make-hasheq)
(define-for-syntax token-identifier-mapping-get
  (case-lambda 
    [(t tok)
     (hash-ref t (syntax-e tok))]
    [(t tok fail)
     (hash-ref t (syntax-e tok) fail)]))
(define-for-syntax token-identifier-mapping-put!
  (lambda (t tok v)
    (hash-set! t (syntax-e tok) v)))
(define-for-syntax token-identifier-mapping-map
  (lambda (t f)
    (hash-map t f)))

;; Used to calculate information on the grammar, such as whether
;; a particular non-terminal is "simple" instead of recursively defined.
(define-for-syntax (nt-fixpoint nts proc nt-ids patss)
  (define (ormap-all val f as bs)
    (cond
      [(null? as) val]
      [else (ormap-all (or (f (car as) (car bs)) val)
                       f
                       (cdr as) (cdr bs))]))
  (let loop ()
    (when (ormap-all #f
                     (lambda (nt pats)
                       (let ([old (bound-identifier-mapping-get nts nt)])
                         (let ([new (proc nt pats old)])
                           (if (equal? old new)
                               #f
                               (begin
                                 (bound-identifier-mapping-put! nts nt new)
                                 #t)))))
                     nt-ids patss)
      (loop))))

;; Tries parse-a followed by parse-b. If parse-a is not simple,
;; then after parse-a succeeds once, we parallelize parse-b
;; and trying a second result for parse-a.
(define (parse-and simple-a? parse-a parse-b
                   stream depth end success-k fail-k 
                   max-depth tasks)
  (letrec ([mk-got-k
            (lambda (success-k fail-k)
              (lambda (val stream depth max-depth tasks next1-k)
                (if simple-a?
                    (parse-b val stream depth end
                             (mk-got2-k success-k fail-k next1-k)
                             (mk-fail2-k success-k fail-k next1-k)
                             max-depth tasks)
                    (parallel-or
                     (lambda (success-k fail-k max-depth tasks)
                       (parse-b val stream depth end
                                success-k fail-k
                                max-depth tasks))
                     (lambda (success-k fail-k max-depth tasks)
                       (next1-k (mk-got-k success-k fail-k)
                                fail-k max-depth tasks))
                     success-k fail-k max-depth tasks))))]
           [mk-got2-k
            (lambda (success-k fail-k next1-k)
              (lambda (val stream depth max-depth tasks next-k)
                (success-k val stream depth max-depth tasks
                           (lambda (success-k fail-k max-depth tasks)
                             (next-k (mk-got2-k success-k fail-k next1-k)
                                     (mk-fail2-k success-k fail-k next1-k)
                                     max-depth tasks)))))]
           [mk-fail2-k
            (lambda (success-k fail-k next1-k)
              (lambda (max-depth tasks)
                (next1-k (mk-got-k success-k fail-k)
                         fail-k
                         max-depth
                         tasks)))])
    (parse-a stream depth end
             (mk-got-k success-k fail-k)
             fail-k
             max-depth tasks)))

;; Parallel or for non-terminal alternatives
(define (parse-parallel-or parse-a parse-b stream depth end success-k fail-k max-depth tasks)
  (parallel-or (lambda (success-k fail-k max-depth tasks)
                 (parse-a stream depth end success-k fail-k max-depth tasks))
               (lambda (success-k fail-k max-depth tasks)
                 (parse-b stream depth end success-k fail-k max-depth tasks))
               success-k fail-k max-depth tasks))

;; Generic parallel-or
(define (parallel-or parse-a parse-b success-k fail-k max-depth tasks)
  (define answer-key (gensym))
  (letrec ([gota-k
            (lambda (val stream depth max-depth tasks next-k)
              (report-answer answer-key
                             max-depth
                             tasks
                             (list val stream depth next-k)))]
           [faila-k
            (lambda (max-depth tasks)
              (report-answer answer-key
                             max-depth
                             tasks
                             null))])
    (let* ([tasks (queue-task
                   tasks
                   (lambda (max-depth tasks)
                     (parse-a gota-k
                              faila-k
                              max-depth tasks)))]
           [tasks (queue-task
                   tasks
                   (lambda (max-depth tasks)
                     (parse-b gota-k
                              faila-k
                              max-depth tasks)))]
           [queue-next (lambda (next-k tasks)
                         (queue-task tasks
                                     (lambda (max-depth tasks)
                                       (next-k gota-k
                                               faila-k
                                               max-depth tasks))))])
      (letrec ([mk-got-one
                (lambda (immediate-next? get-nth success-k)
                  (lambda (val stream depth max-depth tasks next-k)
                    (let ([tasks (if immediate-next?
                                     (queue-next next-k tasks)
                                     tasks)])
                      (success-k val stream depth max-depth 
                                 tasks
                                 (lambda (success-k fail-k max-depth tasks)                                     
                                   (let ([tasks (if immediate-next?
                                                    tasks
                                                    (queue-next next-k tasks))])
                                     (get-nth max-depth tasks success-k fail-k)))))))]
               [get-first
                (lambda (max-depth tasks success-k fail-k)
                  (wait-for-answer #f max-depth tasks answer-key
                                   (mk-got-one #t get-first success-k)
                                   (lambda (max-depth tasks)
                                     (get-second max-depth tasks success-k fail-k))
                                   #f))]
               [get-second
                (lambda (max-depth tasks success-k fail-k)
                  (wait-for-answer #f max-depth tasks answer-key
                                   (mk-got-one #f get-second success-k)
                                   fail-k #f))])
        (get-first max-depth tasks success-k fail-k)))))

;; Non-terminal alternatives where the first is "simple" can be done
;; sequentially, which is simpler
(define (parse-or parse-a parse-b
                  stream depth end success-k fail-k max-depth tasks)
  (letrec ([mk-got-k
            (lambda (success-k fail-k)
              (lambda (val stream depth max-depth tasks next-k)
                (success-k val stream depth
                           max-depth tasks
                           (lambda (success-k fail-k max-depth tasks)
                             (next-k (mk-got-k success-k fail-k)
                                     (mk-fail-k success-k fail-k)
                                     max-depth tasks)))))]
           [mk-fail-k
            (lambda (success-k fail-k)
              (lambda (max-depth tasks)
                (parse-b stream depth end success-k fail-k max-depth tasks)))])
    (parse-a stream depth end
             (mk-got-k success-k fail-k)
             (mk-fail-k success-k fail-k)
             max-depth tasks)))

;; Starts a thread
(define queue-task
  (lambda (tasks t [progress? #t])
    (make-tasks (tasks-active tasks)
                (cons t (tasks-active-back tasks))
                (tasks-waits tasks)
                (tasks-multi-waits tasks)
                (tasks-cache tasks)
                (or progress? (tasks-progress? tasks)))))

;; Reports an answer to a waiting thread:
(define (report-answer answer-key max-depth tasks val)
  (let ([v (hash-ref (tasks-waits tasks) answer-key (lambda () #f))])
    (if v
        (let ([tasks (make-tasks (cons (v val)
                                       (tasks-active tasks))
                                 (tasks-active-back tasks)
                                 (tasks-waits tasks)
                                 (tasks-multi-waits tasks)
                                 (tasks-cache tasks)
                                 #t)])
          (hash-remove! (tasks-waits tasks) answer-key)
          (swap-task max-depth tasks))
        ;; We have an answer ready too fast; wait
        (swap-task max-depth
                   (queue-task tasks
                               (lambda (max-depth tasks)
                                 (report-answer answer-key max-depth tasks val))
                               #f)))))

;; Reports an answer to multiple waiting threads:
(define (report-answer-all answer-key max-depth tasks val k)
  (let ([v (hash-ref (tasks-multi-waits tasks) answer-key (lambda () null))])
    (hash-remove! (tasks-multi-waits tasks) answer-key)
    (let ([tasks (make-tasks (append (map (lambda (a) (a val)) v)
                                     (tasks-active tasks))
                             (tasks-active-back tasks)
                             (tasks-waits tasks)
                             (tasks-multi-waits tasks)
                             (tasks-cache tasks)
                             #t)])
      (k max-depth tasks))))

;; Waits for an answer; if `multi?' is #f, this is sole waiter, otherwise
;;  there might be many. Use wither #t or #f (and `report-answer' or 
;;  `report-answer-all', resptively) consistently for a particular answer key.
(define (wait-for-answer multi? max-depth tasks answer-key success-k fail-k deadlock-k)
  (let ([wait (lambda (val)
                (lambda (max-depth tasks)
                  (if val
                      (if (null? val)
                          (fail-k max-depth tasks)
                          (let-values ([(val stream depth next-k) (apply values val)])
                            (success-k val stream depth max-depth tasks next-k)))
                      (deadlock-k max-depth tasks))))])
    (if multi?
        (hash-set! (tasks-multi-waits tasks) answer-key
                         (cons wait (hash-ref (tasks-multi-waits tasks) answer-key
                                                    (lambda () null))))
        (hash-set! (tasks-waits tasks) answer-key wait))
    (let ([tasks (make-tasks (tasks-active tasks)
                             (tasks-active-back tasks)
                             (tasks-waits tasks)
                             (tasks-multi-waits tasks)
                             (tasks-cache tasks)
                             #t)])
      (swap-task max-depth tasks))))

;; Swap thread
(define (swap-task max-depth tasks)
  ;; Swap in first active:
  (if (null? (tasks-active tasks))
      (if (tasks-progress? tasks)
          (swap-task max-depth
                     (make-tasks (reverse (tasks-active-back tasks))
                                 null
                                 (tasks-waits tasks)
                                 (tasks-multi-waits tasks)
                                 (tasks-cache tasks)
                                 #f))
          ;; No progress, so issue failure for all multi-waits
          (if (zero? (hash-count (tasks-multi-waits tasks)))
              (error 'swap-task "Deadlock")
              (swap-task max-depth
                         (make-tasks (apply
                                      append
                                      (hash-map (tasks-multi-waits tasks)
                                                      (lambda (k l)
                                                        (map (lambda (v) (v #f)) l))))
                                     (tasks-active-back tasks)
                                     (tasks-waits tasks)
                                     (make-hasheq)
                                     (tasks-cache tasks)
                                     #t))))
      (let ([t (car (tasks-active tasks))]
            [tasks (make-tasks (cdr (tasks-active tasks))
                               (tasks-active-back tasks)
                               (tasks-waits tasks)
                               (tasks-multi-waits tasks)
                               (tasks-cache tasks)
                               (tasks-progress? tasks))])
        (t max-depth tasks))))

;; Finds the symbolic representative of a token class
(define-for-syntax (map-token toks tok)
  (car (token-identifier-mapping-get toks tok)))

(define no-pos-val (make-position #f #f #f))
(define-for-syntax no-pos 
  (let ([npv ((syntax-local-certifier) #'no-pos-val)])
    (lambda (stx) npv)))
(define-for-syntax at-tok-pos
  (lambda (sel expr)
    (lambda (stx)
      #`(let ([v #,expr]) (if v (#,sel v) no-pos-val)))))

;; Builds a matcher for a particular alternative
(define-for-syntax (build-match nts toks pat handle $ctx)
  (let loop ([pat pat]
             [pos 1])
    (if (null? pat)
        #`(success-k #,handle stream depth max-depth tasks 
                     (lambda (success-k fail-k max-depth tasks)
                       (fail-k max-depth tasks)))
        (let ([id (datum->syntax (car pat)
                                        (string->symbol (format "$~a" pos)))]
              [id-start-pos (datum->syntax (car pat)
                                                  (string->symbol (format "$~a-start-pos" pos)))]
              [id-end-pos (datum->syntax (car pat)
                                                (string->symbol (format "$~a-end-pos" pos)))]
              [n-end-pos (and (null? (cdr pat))
                              (datum->syntax (car pat) '$n-end-pos))])
          (cond
            [(bound-identifier-mapping-get nts (car pat) (lambda () #f))
             ;; Match non-termimal
             #`(parse-and
                ;; First part is simple? (If so, we don't have to parallelize the `and'.)
                #,(let ([l (bound-identifier-mapping-get nts (car pat) (lambda () #f))])
                    (or (not l)
                        (andmap values (caddr l))))
                #,(car pat)
                (lambda (#,id stream depth end success-k fail-k max-depth tasks)
                  (let-syntax ([#,id-start-pos (at-tok-pos #'tok-start #'(and (pair? stream) (car stream)))]
                               [#,id-end-pos (at-tok-pos #'tok-end #'(and (pair? stream) (car stream)))]
                               #,@(if n-end-pos
                                      #`([#,n-end-pos (at-tok-pos #'tok-end #'(and (pair? stream) (car stream)))])
                                      null))
                    #,(loop (cdr pat) (add1 pos))))
                stream depth 
                #,(let ([cnt (apply +
                                    (map (lambda (item)
                                           (cond
                                             [(bound-identifier-mapping-get nts item (lambda () #f))
                                              => (lambda (l) (car l))]
                                             [else 1]))
                                         (cdr pat)))])
                    #`(- end #,cnt))
                success-k fail-k max-depth tasks)]
            [else
             ;; Match token
             (let ([tok-id (map-token toks (car pat))])
               #`(if (and (pair? stream)
                          (eq? '#,tok-id (tok-name (car stream))))
                     (let* ([stream-a (car stream)]
                            [#,id (tok-val stream-a)]
                            [stream (cdr stream)]
                            [depth (add1 depth)])
                       (let ([max-depth (max max-depth depth)])
                         (let-syntax ([#,id-start-pos (at-tok-pos #'tok-start #'stream-a)]
                                      [#,id-end-pos (at-tok-pos #'tok-end #'stream-a)]
                                      #,@(if n-end-pos
                                             #`([#,n-end-pos (at-tok-pos #'tok-end #'stream-a)])
                                             null))
                           #,(loop (cdr pat) (add1 pos)))))
                     (fail-k max-depth tasks)))])))))

;; Starts parsing to match a non-terminal. There's a minor
;; optimization that checks for known starting tokens. Otherwise,
;; use the cache, block if someone else is already trying the match,
;; and cache the result if it's computed.
;; The cache maps nontermial+startingpos+iteration to a result, where
;; the iteration is 0 for the first match attempt, 1 for the second,
;; etc.
(define (parse-nt/share key min-cnt init-tokens stream depth end max-depth tasks success-k fail-k k)
  (if (and (positive? min-cnt)
           (pair? stream)
           (not (memq (tok-name (car stream)) init-tokens)))
      ;; No such leading token; give up
      (fail-k max-depth tasks)
      ;; Run pattern
      (let loop ([n 0]
                 [success-k success-k]
                 [fail-k fail-k]
                 [max-depth max-depth]
                 [tasks tasks]
                 [k k])
        (let ([answer-key (gensym)]
              [table-key (vector key depth n)]
              [old-depth depth]
              [old-stream stream])
          #;(printf "Loop ~a\n" table-key)
          (cond
            [(hash-ref (tasks-cache tasks) table-key (lambda () #f))
             => (lambda (result)
                  #;(printf "Reuse ~a\n" table-key)
                  (result success-k fail-k max-depth tasks))]
            [else
             #;(printf "Try ~a ~a\n" table-key (map tok-name stream))
             (hash-set! (tasks-cache tasks) table-key
                              (lambda (success-k fail-k max-depth tasks)
                                #;(printf "Wait ~a ~a\n" table-key answer-key)
                                (wait-for-answer #t max-depth tasks answer-key success-k fail-k
                                                 (lambda (max-depth tasks)
                                                   #;(printf "Deadlock ~a ~a\n" table-key answer-key)
                                                   (fail-k max-depth tasks)))))
             (let result-loop ([max-depth max-depth][tasks tasks][k k])
               (letrec ([orig-stream stream]
                        [new-got-k
                         (lambda (val stream depth max-depth tasks next-k)
                           ;; Check whether we already have a result that consumed the same amount:
                           (let ([result-key (vector #f key old-depth depth)])
                             (cond
                               [(hash-ref (tasks-cache tasks) result-key (lambda () #f))
                                ;; Go for the next-result
                                (result-loop max-depth
                                             tasks
                                             (lambda (end max-depth tasks success-k fail-k)
                                               (next-k success-k fail-k max-depth tasks)))]
                               [else
                                #;(printf "Success ~a ~a\n" table-key 
                                          (map tok-name (let loop ([d old-depth][s old-stream])
                                                          (if (= d depth)
                                                              null
                                                              (cons (car s) (loop (add1 d) (cdr s)))))))
                                (let ([next-k (lambda (success-k fail-k max-depth tasks)
                                                (loop (add1 n)
                                                      success-k
                                                      fail-k
                                                      max-depth
                                                      tasks
                                                      (lambda (end max-depth tasks success-k fail-k)
                                                        (next-k success-k fail-k max-depth tasks))))])
                                  (hash-set! (tasks-cache tasks) result-key #t)
                                  (hash-set! (tasks-cache tasks) table-key
                                                   (lambda (success-k fail-k max-depth tasks)
                                                     (success-k val stream depth max-depth tasks next-k)))
                                  (report-answer-all answer-key
                                                     max-depth
                                                     tasks
                                                     (list val stream depth next-k)
                                                     (lambda (max-depth tasks)
                                                       (success-k val stream depth max-depth tasks next-k))))])))]
                        [new-fail-k
                         (lambda (max-depth tasks)
                           #;(printf "Failure ~a\n" table-key)
                           (hash-set! (tasks-cache tasks) table-key
                                            (lambda (success-k fail-k max-depth tasks)
                                              (fail-k max-depth tasks)))
                           (report-answer-all answer-key
                                              max-depth
                                              tasks
                                              null
                                              (lambda (max-depth tasks)
                                                (fail-k max-depth tasks))))])
                 (k end max-depth tasks new-got-k new-fail-k)))])))))

(define-syntax (cfg-parser stx)
  (syntax-case stx ()
    [(_ clause ...)
     (let ([clauses (syntax->list #'(clause ...))])
       (let-values ([(start grammar cfg-error parser-clauses)
                     (let ([all-toks (apply
                                      append
                                      (map (lambda (clause)
                                             (syntax-case clause (tokens)
                                               [(tokens t ...)
                                                (apply
                                                 append
                                                 (map (lambda (t)
                                                        (let ([v (syntax-local-value t (lambda () #f))])
                                                          (cond
                                                            [(terminals-def? v)
                                                             (map (lambda (v)
                                                                    (cons v #f))
                                                                  (syntax->list (terminals-def-t v)))]
                                                            [(e-terminals-def? v)
                                                             (map (lambda (v)
                                                                    (cons v #t))
                                                                  (syntax->list (e-terminals-def-t v)))]
                                                            [else null])))
                                                      (syntax->list #'(t ...))))]
                                               [_else null]))
                                           clauses))]
                           [all-end-toks (apply
                                          append
                                          (map (lambda (clause)
                                                 (syntax-case clause (end)
                                                   [(end t ...)
                                                    (syntax->list #'(t ...))]
                                                   [_else null]))
                                               clauses))])
                       (let loop ([clauses clauses]
                                  [cfg-start #f]
                                  [cfg-grammar #f]
                                  [cfg-error #f]
                                  [src-pos? #f]
                                  [parser-clauses null])
                         (if (null? clauses)
                             (values cfg-start
                                     cfg-grammar
                                     cfg-error
                                     (reverse parser-clauses))
                             (syntax-case (car clauses) (start error grammar src-pos)
                               [(start tok)
                                (loop (cdr clauses) #'tok cfg-grammar cfg-error src-pos? parser-clauses)]
                               [(error expr)
                                (loop (cdr clauses) cfg-start cfg-grammar #'expr src-pos? parser-clauses)]
                               [(grammar [nt [pat handle0 handle ...] ...] ...)
                                (let ([nts (make-bound-identifier-mapping)]
                                      [toks (make-token-identifier-mapping)]
                                      [end-toks (make-token-identifier-mapping)]
                                      [nt-ids (syntax->list #'(nt ...))]
                                      [patss (map (lambda (stx)
                                                    (map syntax->list (syntax->list stx)))
                                                  (syntax->list #'((pat ...) ...)))])
                                  (for-each (lambda (nt)
                                              (bound-identifier-mapping-put! nts nt (list 0)))
                                            nt-ids)
                                  (for-each (lambda (t)
                                              (token-identifier-mapping-put! end-toks t #t))
                                            all-end-toks)
                                  (for-each (lambda (t)
                                              (unless (token-identifier-mapping-get end-toks (car t) (lambda () #f))
                                                (let ([id (gensym (syntax-e (car t)))])
                                                  (token-identifier-mapping-put! toks (car t)
                                                                                 (cons id (cdr t))))))
                                            all-toks)
                                  ;; Compute min max size for each non-term:
                                  (nt-fixpoint
                                   nts
                                   (lambda (nt pats old-list)
                                     (let ([new-cnt
                                            (apply
                                             min
                                             (map (lambda (pat)
                                                    (apply
                                                     +
                                                     (map (lambda (elem)
                                                            (car
                                                             (bound-identifier-mapping-get nts
                                                                                           elem
                                                                                           (lambda () (list 1)))))
                                                          pat)))
                                                  pats))])
                                       (if (new-cnt . > . (car old-list))
                                           (cons new-cnt (cdr old-list))
                                           old-list)))
                                   nt-ids patss)
                                  ;; Compute set of toks that must appear at the beginning
                                  ;;  for a non-terminal
                                  (nt-fixpoint
                                   nts
                                   (lambda (nt pats old-list)
                                     (let ([new-list
                                            (apply
                                             append
                                             (map (lambda (pat)
                                                    (let loop ([pat pat])
                                                      (if (pair? pat)
                                                          (let ([l (bound-identifier-mapping-get 
                                                                    nts
                                                                    (car pat)
                                                                    (lambda ()
                                                                      (list 1 (map-token toks (car pat)))))])
                                                            ;; If the non-terminal can match 0 things,
                                                            ;;  then it might match something from the
                                                            ;;  next pattern element. Otherwise, it must
                                                            ;;  match the first element:
                                                            (if (zero? (car l))
                                                                (append (cdr l) (loop (cdr pat)))
                                                                (cdr l)))
                                                          null)))
                                                  pats))])
                                       (let ([new (filter (lambda (id)
                                                            (andmap (lambda (id2)
                                                                      (not (eq? id id2)))
                                                                    (cdr old-list)))
                                                          new-list)])
                                         (if (pair? new)
                                             ;; Drop dups in new list:
                                             (let ([new (let loop ([new new])
                                                          (if (null? (cdr new))
                                                              new
                                                              (if (ormap (lambda (id)
                                                                           (eq? (car new) id))
                                                                         (cdr new))
                                                                  (loop (cdr new))
                                                                  (cons (car new) (loop (cdr new))))))])
                                               (cons (car old-list) (append new (cdr old-list))))
                                             old-list))))
                                   nt-ids patss)
                                  ;; Determine left-recursive clauses:
                                  (for-each (lambda (nt pats)
                                              (let ([l (bound-identifier-mapping-get nts nt)])
                                                (bound-identifier-mapping-put! nts nt (list (car l)
                                                                                            (cdr l)
                                                                                            (map (lambda (x) #f) pats)))))
                                            nt-ids patss)
                                  (nt-fixpoint
                                   nts
                                   (lambda (nt pats old-list)
                                     (list (car old-list)
                                           (cadr old-list)
                                           (map (lambda (pat simple?)
                                                  (or simple?
                                                      (let ([l (map (lambda (elem)
                                                                      (bound-identifier-mapping-get 
                                                                       nts
                                                                       elem
                                                                       (lambda () #f)))
                                                                    pat)])
                                                        (andmap (lambda (i)
                                                                  (or (not i)
                                                                      (andmap values (caddr i))))
                                                                l))))
                                                pats (caddr old-list))))
                                   nt-ids patss)
                                  ;; Build a definition for each non-term:
                                  (loop (cdr clauses)
                                        cfg-start
                                        (map (lambda (nt pats handles $ctxs)
                                               (define info (bound-identifier-mapping-get nts nt))
                                               (list nt
                                                     #`(let ([key (gensym '#,nt)])
                                                         (lambda (stream depth end success-k fail-k max-depth tasks)
                                                           (parse-nt/share
                                                            key #,(car info) '#,(cadr info) stream depth end
                                                            max-depth tasks
                                                            success-k fail-k
                                                            (lambda (end max-depth tasks success-k fail-k)
                                                              #,(let loop ([pats pats]
                                                                           [handles (syntax->list handles)]
                                                                           [$ctxs (syntax->list $ctxs)]
                                                                           [simple?s (caddr info)])
                                                                  (if (null? pats)
                                                                      #'(fail-k max-depth tasks)
                                                                      #`(#,(if (or (null? (cdr pats))
                                                                                   (car simple?s))
                                                                               #'parse-or
                                                                               #'parse-parallel-or)
                                                                         (lambda (stream depth end success-k fail-k max-depth tasks)
                                                                           #,(build-match nts
                                                                                          toks 
                                                                                          (car pats)
                                                                                          (car handles)
                                                                                          (car $ctxs)))
                                                                         (lambda (stream depth end success-k fail-k max-depth tasks)
                                                                           #,(loop (cdr pats)
                                                                                   (cdr handles)
                                                                                   (cdr $ctxs)
                                                                                   (cdr simple?s)))
                                                                         stream depth end success-k fail-k max-depth tasks)))))))))
                                             nt-ids
                                             patss
                                             (syntax->list #'(((begin handle0 handle ...) ...) ...))
                                             (syntax->list #'((handle0 ...) ...)))
                                        cfg-error
                                        src-pos?
                                        (list*
                                         (with-syntax ([((tok tok-id . $e) ...)
                                                        (token-identifier-mapping-map toks
                                                                                      (lambda (k v)
                                                                                        (list* k
                                                                                               (car v)
                                                                                               (if (cdr v)
                                                                                                   #f
                                                                                                   '$1))))]
                                                       [(pos ...) 
                                                        (if src-pos?
                                                            #'($1-start-pos $1-end-pos)
                                                            #'(#f #f))])
                                           #`(grammar (start [() null]
                                                             [(atok start) (cons $1 $2)])
                                                      (atok [(tok) (make-tok 'tok-id 'tok $e pos ...)] ...)))
                                         #`(start start)
                                         parser-clauses)))]
                               [(grammar . _)
                                (raise-syntax-error
                                 #f
                                 "bad grammar clause"
                                 stx
                                 (car clauses))]
                               [(src-pos)
                                (loop (cdr clauses)
                                      cfg-start
                                      cfg-grammar
                                      cfg-error
                                      #t
                                      (cons (car clauses) parser-clauses))]
                               [_else
                                (loop (cdr clauses)
                                      cfg-start
                                      cfg-grammar
                                      cfg-error
                                      src-pos?
                                      (cons (car clauses) parser-clauses))]))))])
         #`(let ([orig-parse (parser 
                              [error (lambda (a b c)
                                       (error 'cfg-parser "unexpected ~a token: ~a" b c))]
                              . #,parser-clauses)]
                 [error-proc #,cfg-error])
             (letrec #,grammar 
               (lambda (get-tok)
                 (let ([tok-list (orig-parse get-tok)])
                   (letrec ([success-k
                             (lambda (val stream depth max-depth tasks next) 
                               (if (null? stream)
                                   val
                                   (next success-k fail-k max-depth tasks)))]
                            [fail-k (lambda (max-depth tasks)
                                      (let ([bad-tok (list-ref tok-list 
                                                               (min (sub1 (length tok-list))
                                                                    max-depth))])
                                        (if error-proc
                                            (error-proc #t
                                                        (tok-orig-name bad-tok)
                                                        (tok-val bad-tok)
                                                        (tok-start bad-tok)
                                                        (tok-end bad-tok))
                                            (error
                                             'cfg-parse
                                             "failed at ~a" 
                                             (tok-val bad-tok)))))])
                     (#,start tok-list 0 
                              (length tok-list)
                              success-k
                              fail-k
                              0 (make-tasks null null 
                                            (make-hasheq) (make-hasheq)
                                            (make-hash) #t)))))))))]))


(module* test racket/base
  (require (submod "..")
           parser-tools/lex)
  
  ;; Tests used during development
  (define-tokens non-terminals (PLUS MINUS STAR BAR COLON EOF))
  
  (define lex
    (lexer
     ["+" (token-PLUS '+)]
     ["-" (token-MINUS '-)]
     ["*" (token-STAR '*)]
     ["|" (token-BAR '||)]
     [":" (token-COLON '|:|)]
     [whitespace (lex input-port)]
     [(eof) (token-EOF 'eof)]))
  
  
  (define parse
    (cfg-parser
     (tokens non-terminals)
     (start <program>)
     (end EOF)
     (error (lambda (a b stx) 
              (error 'parse "failed at ~s" stx)))
     (grammar [<program> [(PLUS) "plus"]
                         [(<minus-program> BAR <minus-program>) (list $1 $2 $3)]
                         [(<program> COLON) (list $1)]]
              [<minus-program> [(MINUS) "minus"]
                               [(<program> STAR) (cons $1 $2)]]
              [<simple> [(<alts> <alts> <alts> MINUS) "yes"]]
              [<alts> [(PLUS) 'plus]
                      [(MINUS) 'minus]]
              [<random> [() '0]
                        [(<random> PLUS) (add1 $1)]
                        [(<random> PLUS) (add1 $1)]])))
  
  (define (result)
    (let ([p (open-input-string #;"+*|-|-*|+**" #;"-|+*|+**" 
                                #;"+*|+**|-" #;"-|-*|-|-*"
                                #;"-|-*|-|-**|-|-*|-|-**"
                                "-|-*|-|-**|-|-*|-|-***|-|-*|-|-**|-|-*|-|-****|-|-*|-|-**|-|-*|-|-***
               |-|-*|-|-**|-|-*|-|-*****|-|-*|-|-**|-|-*|-|-***|-|-*|-|-**|-|-*|-|-****|
               -|-*|-|-**|-|-*|-|-***|-|-*|-|-**|-|-*|-|-*****"
                                ;; This one fails:
                                #;"+*")])
      (time (parse (lambda () (lex p))))))
  (result))
