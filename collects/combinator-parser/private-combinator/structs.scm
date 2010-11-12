(module structs scheme/base
  
  (provide (all-defined-out))
  
  (require parser-tools/lex)
    
  ;fail-src: (list line col pos span loc)

  ;make-src-lst: position position -> src-list
  (define (make-src-lst start end)
    (list (position-line start)
          (position-col start)
          (position-offset start)
          (- (position-offset end)
             (position-offset start))))
  
  ;(make-fail-type float fail-src string int int)
  (define-struct fail-type (chance src name used may-use) #:transparent #:mutable)
  ;(make-terminal-fail float fail-src string symbol 'a)
  (define-struct (terminal-fail fail-type) (kind found))
  ;(make-sequence-fail float fail-src string symbol (list string) string 'a (-> boolean) string)
  (define-struct (sequence-fail fail-type) (id kind correct expected found repeat? last-seen) #:transparent)
  ;(make-choice-fail float fail-src string int (list string) (list fail-type) boolean)
  (define-struct (choice-fail fail-type) (options names ended? (messages #:mutable)) #:transparent)
  ;(make-options-fail float #f #f (list fail-type))
  (define-struct (options-fail fail-type) ((opts #:mutable)) #:transparent)
  
  ;result = res | choice-res | repeat-res | (listof (U res choice-res))
  
  ;(make-res parse-build (listof 'a) (U string fail-type) (U string 'a) int) [U #f fail-type] token
  (define-struct res (a rest msg id used possible-error first-tok) #:transparent)
  ;make-choice-res string (listof res) fail-type)
  (define-struct choice-res (name matches errors) #:transparent)
  ;(make-repeat-res answer (U symbol fail-type))
  (define-struct repeat-res (a stop) #:transparent)
  ;(make-lazy-opts (listof res) fail-type (listof (_ => res)))
  (define-struct lazy-opts ((matches #:mutable) errors (thunks #:mutable)) #:transparent)
  ;(make-lazy-choice (listof res) fail-type (listof (_ -> res)) string)
  (define-struct (lazy-choice lazy-opts) (name) #:transparent)

  ;(make-count string int)
  (define-struct occurs (terminal count))
  
  (define (consolidate-count cts)
    (cond 
      [(null? cts) cts]
      [(eq? 'counting (car cts)) (consolidate-count cts)]
      [(pair? (car cts)) (consolidate-count (append (car cts) (cdr cts)))]
      [else
       (let-values ([(front back) (augment-count (car cts) (cdr cts))])
         (cons front (consolidate-count back)))]))
  (define (augment-count count rst)
    (cond
      [(null? rst) (values count rst)]
      [(eq? 'counting (car rst)) (augment-count count (cdr rst))]
      [(pair? (car rst)) (augment-count count (append (car rst) (cdr rst)))]
      [else
       (let-values ([(current back) (augment-count count (cdr rst))])
         (cond 
           [(equal? (occurs-terminal count) (occurs-terminal (car rst)))
            (values (make-occurs (occurs-terminal count) (+ (occurs-count count)
                                                            (occurs-count current)
                                                            (occurs-count (car rst))))
                    back)]
           [else (values current (cons (car rst) back))]))])) 
         
  
  ;parse-build = answer | none
  ;(make-answer 'b)
  (define-struct answer (ast))
  (define-struct none ())
  
  (define (update-lazy-errors failc mss)
    (set-fail-type-chance! failc (max (fail-type-chance failc) (fail-type-chance mss)))
    (set-fail-type-used! failc (max (fail-type-used failc) (fail-type-used mss)))
    (set-fail-type-may-use! failc (max (fail-type-may-use failc) (fail-type-may-use mss)))
    (if (choice-fail? failc)
        (set-choice-fail-messages! failc (cons mss (choice-fail-messages failc)))
        (set-options-fail-opts! failc (cons mss (options-fail-opts failc)))))
  
  
  (define (next-opt lc)
    (letrec ([next 
              (lambda (lc update-errors)
                #;(printf "next-opt ~a\n" lc)
                (cond
                  [(null? (lazy-opts-thunks lc)) #f]
                  [else
                   (let ([curr-res ((car (lazy-opts-thunks lc)))])
                     (unless (null? (lazy-opts-thunks lc)) 
                       (set-lazy-opts-thunks! lc (cdr (lazy-opts-thunks lc))))
                     (cond
                       [(and (not curr-res) (null? (lazy-opts-thunks lc))) curr-res]
                       [(and (not curr-res) (not (null? (lazy-opts-thunks lc)))) (next lc update-errors)]
                       [(or (and (res? curr-res) (res-a curr-res)) (repeat-res? curr-res))
                        (set-lazy-opts-matches! lc (cons curr-res (lazy-opts-matches lc)))
                        curr-res]
                       [(lazy-opts? curr-res)
                        (let* ([next-matches (map (lambda (m) (lambda () m)) (lazy-opts-matches curr-res))]
                               [remaining (map (lambda (t) 
                                                 (lambda () 
                                                   (next curr-res 
                                                         (lambda (_ msg) (update-lazy-errors (lazy-opts-errors curr-res) msg)))))
                                               (lazy-opts-thunks curr-res))])
                          (set-lazy-opts-thunks! lc (append next-matches remaining (lazy-opts-thunks lc)))
                          (update-errors (lazy-opts-errors lc) (lazy-opts-errors curr-res))
                          (next lc update-errors))]
                       [else
                        (update-errors (lazy-opts-errors lc)
                                       (cond
                                         [(res? curr-res) (res-msg curr-res)]
                                         [else (error 'next (format "Internal error: failure other than res ~a" curr-res))]))
                        (next lc update-errors)]))]))])
      (next lc update-lazy-errors)))
  
  (define (update-lazy-opts old-opts matches thunks)
    (cond
      [(lazy-choice? old-opts)
       (make-lazy-choice matches (lazy-opts-errors old-opts) thunks (lazy-choice-name old-opts))]
      [(lazy-opts? old-opts)
       (make-lazy-opts matches (lazy-opts-errors old-opts) thunks)]))
  
  (define (fail-res rst msg) (make-res #f rst msg "" 0 #f #f))
  
)
