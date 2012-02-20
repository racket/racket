#lang scheme/base
(require scheme/pretty
         scheme/match
         scheme/contract
         "gc-core.rkt")

(provide save-random-mutator 
         find-heap-values
         (struct-out terminal)
         (struct-out proc)
         (struct-out pair)
         (struct-out obj-graph)
         obj-graph->code)

;; graph : hash-table[number -o> obj]
;; path : sexp
;; result : flat-value
;; root : nat (the id of the reference to keep live)
(define-struct obj-graph (graph path result root) #:transparent)

;; an obj is either:
;;  - (make-terminal <constant>)
;;  - (make-proc (listof ids))
;;  - (make-pair id id)

(define-struct terminal (const) #:transparent)
(define-struct proc (ids) #:transparent)
(define-struct pair (hd tl) #:transparent)

;; select : (or/c 'first 'rest nat)
;;    numbers indicate the selector is to call a proc with that number
;;    'first and 'rest indicate the selector is either first or rest.
;; id : num
(define-struct connection (select id) #:transparent)

(define (random-obj-graph size heap-values)
  (let ([num-cells (+ 1 (random size))]
        [hash (make-hash)])
    (for ([i (in-range 0 num-cells)])
      (hash-set! hash i (random-obj i num-cells size heap-values)))
    (let-values ([(code last first-id) (random-path hash size)])
      (make-obj-graph hash code last first-id))))

(define (random-path hash size)
  (let-values ([(first-terminal first-id) (pick-first hash)])
    (let loop ([i (random size)]
               [last-id first-id]
               [codes '()])
      (let ([done
             (λ ()
               (values codes
                       (terminal-const first-terminal)
                       last-id))])
        (cond
          [(zero? i) (done)]
          [else
           (let ([next (find-connections-to hash last-id)])
             (cond
               [next (loop (- i 1)
                           (connection-id next)
                           (cons (connection-select next) codes))]
               [else (done)]))])))))

;; find-connection-to : hash id -> connection
;; returns a random choice of one of the nodes with a connection to the node 'id'
(define (find-connections-to hash id) 
  (let ([candidate-code '()]
        [candidate-ids '()])
    (hash-for-each
     hash
     (λ (k v)
       (cond
         [(pair? v) 
          (when (equal? id (pair-hd v))
            (set! candidate-code (cons 'first candidate-code))
            (set! candidate-ids (cons k candidate-ids)))
          (when (equal? id (pair-tl v))
            (set! candidate-code (cons 'rest candidate-code))
            (set! candidate-ids (cons k candidate-ids)))]
         [(terminal? v)
          (void)]
         [(proc? v)
          (for ([proc-id (in-list (proc-ids v))]
                [case-num (in-naturals)])
            (when (equal? proc-id id)
              (set! candidate-code (cons case-num candidate-code))
              (set! candidate-ids (cons k candidate-ids))))])))
    (cond
      [(null? candidate-code)
       #f]
      [else
       (let ([choice (random (length candidate-code))])
         (make-connection (list-ref candidate-code choice)
                          (list-ref candidate-ids choice)))])))

#;
(define (find-connections-to hash id) 
  (let ([candidate-code '()]
        [candidate-ids '()])
    (hash-for-each
     hash
     (λ (k v)
       (cond
         [(pair? v) 
          (when (equal? id (pair-hd v))
            (set! candidate-code (cons (λ (x) `(first ,x)) candidate-code))
            (set! candidate-ids (cons k candidate-ids)))
          (when (equal? id (pair-tl v))
            (set! candidate-code (cons (λ (x) `(rest ,x)) candidate-code))
            (set! candidate-ids (cons k candidate-ids)))]
         [(terminal? v)
          (void)]
         [(proc? v)
          (for ([proc-id (in-list (proc-ids v))]
                [case-num (in-naturals)])
            (when (equal? proc-id id)
              (set! candidate-code (cons (λ (x) `(,x ,case-num)) candidate-code))
              (set! candidate-ids (cons k candidate-ids))))])))
    (cond
      [(null? candidate-code)
       #f]
      [else
       (let ([choice (random (length candidate-code))])
         (make-connection (list-ref candidate-code choice)
                          (list-ref candidate-ids choice)))])))
  
(define (pick-first hash)
  (let ([candidate-terminals '()]
        [candidate-ids '()])
    (hash-for-each
     hash
     (λ (k v) 
       (when (terminal? v)
         (set! candidate-terminals (cons v candidate-terminals))
         (set! candidate-ids (cons k candidate-ids)))))
    (let ([choice (random (length candidate-terminals))])
      (values (list-ref candidate-terminals choice)
              (list-ref candidate-ids choice)))))

;; obj-graph->code : obj-graph? nat nat -> (listof sexp)
(define (obj-graph->code obj-graph iterations heap-size)
  (let ([graph (obj-graph-graph obj-graph)]
        [init-code '()])
    (list 
     `(define (build-one)
        (let* (,@(build-list (hash-count graph)
                             (λ (i) 
                               (let-values ([(binding-code cell-init-code)
                                             (obj->code i (hash-ref graph i))])
                                 (set! init-code (append cell-init-code init-code)) 
                                 `[,(obj-num->id i) ,binding-code]))))
          ,@(reverse init-code)
          ,(obj-num->id (obj-graph-root obj-graph))))
     `(define (traverse-one ,(obj-num->id (obj-graph-root obj-graph)))
        ,(result->comparison (obj-graph-result obj-graph)
                             (let loop ([path (reverse (obj-graph-path obj-graph))])
                               (cond
                                 [(null? path) (obj-num->id (obj-graph-root obj-graph))]
                                 [else 
                                  (case (car path)
                                    [(first) `(first ,(loop (cdr path)))]
                                    [(rest) `(rest ,(loop (cdr path)))]
                                    [else `(,(loop (cdr path)) ,(car path))])]))))
     
     `(define (trigger-gc n)
        (if (zero? n)
            0
            (begin 
              (cons n n)
              (trigger-gc (- n 1)))))
     `(define (loop i)
        (if (zero? i)
            'passed
            (let ([obj (build-one)])
              (trigger-gc ,heap-size)
              (if (traverse-one obj)
                  (loop (- i 1))
                  'failed))))
     `(loop ,iterations))))

(define (result->comparison expected-result exp)
  (cond
    [(number? expected-result) `(= ,expected-result ,exp)]
    [(symbol? expected-result)
     `(symbol=? ',expected-result ,exp)]
    [(eq? expected-result #t)
     `(let ([res ,exp])
        (if (boolean? res)
            res
            #f))]
    [(eq? expected-result #f) `(if ,exp #f #t)]
    [(null? expected-result) `(empty? ,exp)]
    [else (error 'result->comparison "unknown value ~s\n" expected-result)]))

(define (obj-num->id x) (string->symbol (format "x~a" x)))

;; random-obj : number obj -> (values sexp[constructor] (list sexp[init-code]))
(define (obj->code cell-number obj)
  (cond
    [(terminal? obj) (values (terminal->code (terminal-const obj)) '())]
    [(pair? obj)
     (let* ([hd-direct? (< (pair-hd obj) cell-number)]
            [tl-direct? (< (pair-tl obj) cell-number)]
            [code0 (if hd-direct? 
                       '()
                       (list `(set-first! ,(obj-num->id cell-number)
                                          ,(obj-num->id (pair-hd obj)))))]
            [code1 (if tl-direct?
                       code0
                       (cons `(set-rest! ,(obj-num->id cell-number)
                                         ,(obj-num->id (pair-tl obj)))
                             code0))])
       (values `(cons ,(if hd-direct?
                           (obj-num->id (pair-hd obj))
                           #f)
                      ,(if tl-direct?
                           (obj-num->id (pair-tl obj))
                           #f))
               code1))]
    [(proc? obj)
     (values `(lambda (x)
                ,(let loop ([eles (proc-ids obj)]
                            [i 0])
                   (cond
                     [(null? (cdr eles))
                      (obj-num->id (car eles))]
                     [else
                      `(if (= x ,i)
                           ,(obj-num->id (car eles))
                           ,(loop (cdr eles) (+ i 1)))])))
             '())]))

(define (terminal->code const)
  (cond
    [(symbol? const) `',const]
    [(null? const) 'empty]
    [else const]))

(define (random-obj cell-number num-cells size heap-values)
  (case (random (if (zero? cell-number) 1 3))
    [(0) (make-terminal (pick-from-list heap-values))]
    [(1) (make-pair (random num-cells)
                    (random num-cells))]
    [(2) (make-proc (build-list (+ (random size) 1) (λ (i) (random cell-number))))]))

(define (pick-from-list l) (list-ref l (random (length l))))

(define (save-random-mutator filename collector 
                             #:heap-values [heap-values (list 0 1 -1 'x 'y #f #t '())]
                             #:iterations [iterations 200]
                             #:program-size [program-size 10]
                             #:heap-size [heap-size 200]
                             #:gc2? [gc2? #f])
  (call-with-output-file filename
    (λ (port)
      (cond
        [collector
         (fprintf port "#lang plai/~amutator\n" (if gc2? "gc2/" ""))
         (fprintf port "~s\n" `(allocator-setup ,collector ,heap-size))]
        [else
         (fprintf port "#lang scheme\n")
         (for-each
          (λ (pair) (fprintf port "~s\n" `(define ,@pair)))
          '((cons mcons)
            (first mcar)
            (rest mcdr)
            (set-first! set-mcar!)
            (set-rest! set-mcdr!)))])
      (for-each (λ (x) (pretty-print x port))
                (obj-graph->code (random-obj-graph program-size heap-values) 
                                 iterations
                                 heap-size)))
    #:exists 'truncate))

(define (find-heap-values in)
  (cond
    [(input-port? in)
     (find-heap-values/main in)]
    [else
     (call-with-input-file in find-heap-values/main)]))

(define (find-heap-values/main port)
  (let* ([ht (make-hash)]
         [exp (parameterize ([read-accept-reader #t])
                (read port))]
         [plai-collector-lang?
          (match exp
            [`(module ,name ,langname ,the-rest ...)
             (and (regexp-match #rx"collector" (format "~s" langname))
                  (regexp-match #rx"plai" (format "~s" langname)))]
            [_ #f])])
    (let loop ([exp exp]
               [quoted? #f])
      (match exp
        [`',arg (loop arg #t)]
        [``,arg (loop arg #t)]
        [(? symbol?) 
         (cond
           [quoted? (hash-set! ht exp #t)]
           [else
            (case exp
              [(true) (hash-set! ht #t #t)]
              [(false) (hash-set! ht #f #t)]
              [(null) (hash-set! ht '() #t)]
              [(empty) (hash-set! ht '() #t)]
              [else (void)])])]
        [`() (when quoted?
               (hash-set! ht '() #t))]
        [(? heap-value?) (hash-set! ht exp #t)]
        [(? list?)
         (if (or quoted? (not plai-collector-lang?))
             (for-each (λ (x) (loop x quoted?)) exp)
             (match exp
               [`(error ,skippable ,rest ...)
                (for-each (λ (x) (loop x quoted?)) rest)]
               [`(test ,a ,b)
                (void)]
               [`(test/exn ,a ,b)
                (void)]
               [else
                (for-each (λ (x) (loop x quoted?)) exp)]))]
        [else (void)]))
    (sort (hash-map ht (λ (x y) x))
          string<=?
          #:key (λ (x) (format "~s" x)))))

