(module structs scheme/base
  
  (provide (except-out (all-defined-out) 
                       set-choice-fail-messages! 
                       set-lazy-opts-matches!)
           )
  
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
  ;(make-sequence-fail float fail-src string symbol (list string) string 'a boolean string)
  (define-struct (sequence-fail fail-type) (id kind correct expected found repeat? last-seen))
  ;(make-choice-fail float fail-src string int (list string) (list fail-type) boolean)
  (define-struct (choice-fail fail-type) (options names ended? (messages #:mutable)) #:transparent)
  ;(make-options-fail float #f #f (list fail-type))
  (define-struct (options-fail fail-type) ((opts #:mutable)))
  
  ;result = res | choice-res | repeat-res | (listof (U res choice-res))
  
  ;(make-res (U #f (listof 'b)) (listof 'a) (U string fail-type) (U string 'a) int) [U #f fail-type] token
  (define-struct res (a rest msg id used possible-error first-tok) #:transparent)
  ;make-choice-res string (listof res) fail-type)
  (define-struct choice-res (name matches errors) #:transparent)
  ;(make-repeat-res answer (U symbol fail-type))
  (define-struct repeat-res (a stop) #:transparent)
  ;(make-lazy-opts (listof res) fail-type (listof (_ => res)))
  (define-struct lazy-opts ((matches #:mutable) errors (thunks #:mutable)) #:transparent)
  ;(make-lazy-choice (listof res) fail-type (listof (_ -> res)) string)
  (define-struct (lazy-choice lazy-opts) (name) #:transparent)
  
  (define (update-choice-errors failc mss)
    (set-choice-fail-messages! failc (cons mss (choice-fail-messages failc)))
    (set-fail-type-chance! failc (max (fail-type-chance failc) (fail-type-chance mss)))
    (set-fail-type-used! failc (max (fail-type-used failc) (fail-type-used mss)))
    (set-fail-type-may-use! failc (max (fail-type-may-use failc) (fail-type-may-use mss))))
  (define (update-opt-errors failc mss)
    (set-options-fail-opts! failc (cons mss (options-fail-opts failc)))
    (set-fail-type-chance! failc (max (fail-type-chance failc) (fail-type-chance mss)))
    (set-fail-type-used! failc (max (fail-type-used failc) (fail-type-used mss)))
    (set-fail-type-may-use! failc (max (fail-type-may-use failc) (fail-type-may-use mss))))
   
  (define (make-force thunks set-thunks matches set-matches update-errors errors)
    (letrec ([next 
              (lambda (lc)
                (printf "next-opt ~a~n" lc)
                (cond
                  [(null? (thunks lc)) #f]
                  [else
                   (let ([curr-res ((car (thunks lc)))])
                     (set-thunks lc (cdr (thunks lc)))
                     (and curr-res
                          (cond
                            [(or (and (res? curr-res) (res-a curr-res))
                                 (repeat-res? curr-res)
                                 (choice-res? curr-res)
                                 (lazy-opts? curr-res)
                                 (and (lazy-choice? curr-res) (not (null? (lazy-opts-matches curr-res)))))
                             (set-matches lc (cons curr-res (matches lc)))
                             curr-res]
                            [else
                             (update-errors (errors lc)
                                            (cond
                                              [(res? curr-res) (res-msg curr-res)]
                                              [(lazy-choice? curr-res) (lazy-opts-errors curr-res)]))
                             (next lc)])))]))])
      next))
  
  (define next-choice 
    (make-force lazy-opts-thunks set-lazy-opts-thunks! 
                lazy-opts-matches set-lazy-opts-matches!
                update-choice-errors lazy-opts-errors))
  (define next-opt
    (make-force lazy-opts-thunks set-lazy-opts-thunks!
                lazy-opts-matches set-lazy-opts-matches!
                update-opt-errors lazy-opts-errors))
  
  (define (fail-res rst msg) (make-res #f rst msg "" 0 #f #f))
  
)
