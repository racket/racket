;;;
;;; COMPREHENSIONS
;;;

(module comprehensions mzscheme
  (provide (all-defined))
  
  (require "generators.scm")
  (require-for-syntax "generators.scm")
  (require-for-syntax "expansion.scm")
  (require-for-template mzscheme)
  
  ; This is the model comprehension
  
  #;(define-syntax (list-ec stx)
      (syntax-case stx ()
        [(_ body)
         #'(list body)]
        [(_ clause ... body)
         (begin
           (check-all-clauses-are-generators-or-filters #'(clause ...) 'list-ec)
           (when (and (null? (syntax->list #'(clause (... ...))))
                      (generator-clause? #'body))
             (raise-syntax-error
              'list-ec 
              (string-append "Generator used in body position. "
                             "Expected (list <generator-or-filter> ... <expr>), got: ")
              #'body))
           #`(let ([result '()])
               #,((expand-clauses #'(clause ...))
                  #'(set! result (cons body result)))
               (reverse result)))]
        [_
         (raise-syntax-error
          'name-ec
          "expected (list-ec <generator-or-filter> ... <expr>), got: "
          st)]))
  
  (define-syntax (define-comprehension stx)
    (syntax-case stx ()
      [(_ name-ec inserter body expansion)
       #'(define-syntax (name-ec st)
           (syntax-case st ()
             [(_ clause (... ...) body)
              (begin
                (check-all-clauses-are-generators-or-filters #'(clause (... ...)) 'name-ec)
                (when (and (null? (syntax->list #'(clause (... ...))))
                           (generator-clause? #'body))
                  (raise-syntax-error
                   'name-ec (format 
                             (string-append 
                              "Generator used in body position. "
                              "Expected (~a <generator-or-filter> ... <expr>), got: ")
                             'name-ec)
                   #'body))
                (let ([inserter (expand-clauses #'(clause (... ...)))])
                  expansion))]
             [_
              (raise-syntax-error
               'name-ec
               (format "expected (~a <generator-or-filter> ... <expr>), got: " 'name-ec)
               st)]))]
      [else
       (raise-syntax-error
        'define-comprehension
        "expected (define-comprehension <name> <inserter> <body> <expansion>) "
        stx)]))
  
  
  (define-comprehension list-ec 
    insert-payload-in-loop body
    #`(let ([result '()])
        #,(insert-payload-in-loop 
           #'(set! result (cons body result)))
        (reverse result)))
  
  (define-comprehension do-ec
    insert-payload-in-loop body
    (insert-payload-in-loop 
     #'body))
  
  (define-syntax (define-derived-comprehension stx)
    (syntax-case stx ()
      [(_ name-ec (literal ...) (pattern clauses-to-check template) ...)
       #'(define-syntax (name-ec st)
           (define (raise-error)
             (raise-syntax-error
              'name-ec
              (format "expected (~a <generator-or-filter> ... <expr>), got: " 'name-ec)
              st))
           (syntax-case st ()
             [(name clause (... ...) body)
              (begin
                (syntax-case #'(name clause (... ...) body) (literal ...)
                  [pattern 
                   (begin
                     (check-all-clauses-are-generators-or-filters #'clauses-to-check 'name-ec)
                     #'template)]
                  ...
                  [_else (raise-error)]))]
             [_ (raise-error)]))]
      [else
       (raise-syntax-error
        'define-derived-comprehension
        "expected (define-derived-comprehension name-ec (literal ...) (pattern template) ...), got: "
        stx)]))
  
  (define-derived-comprehension append-ec ()
    ((append-ec  etc ... body)
     (etc ...)
     (apply append (list-ec etc ... body))))
  
  (define-derived-comprehension string-ec ()
    ((string-ec etc ... body)
     (etc ...)
     (list->string (list-ec etc ... body)) ))
  
  (define-derived-comprehension string-append-ec ()
    ((string-append-ec etc ... body)
     (etc ...)
     (apply string-append (list-ec etc ... body)) ))
  
  (define-derived-comprehension vector-ec ()
    ((vector-ec etc ... body)
     (etc ...)
     (list->vector (list-ec etc ... body)) ))
  
  
  (define-derived-comprehension vector-of-length-ec (nested)
    ((vector-of-length-ec k (nested q1 ...) q etc1 etc ... body)
     (q1 ... q etc ... body)
     (vector-of-length-ec k (nested q1 ... q) etc1 etc ... body) )
    ((vector-of-length-ec k q1 q2             etc1 etc ... body)
     (q1 q2 etc1 etc ... body)
     (vector-of-length-ec k (nested q1 q2)    etc1 etc ... body) )
    ((vector-of-length-ec k body)
     ()
     (vector-of-length-ec k (nested) body) )
    ((vector-of-length-ec k qualifier body)
     (qualifier)
     (let ((len k))
       (let ((vec (make-vector len))
             (i 0) )
         (do-ec qualifier
                (if (< i len)
                    (begin (vector-set! vec i body)
                           (set! i (+ i 1)) )
                    (error "vector is too short for the comprehension") ))
         (if (= i len)
             vec
             (error "vector is too long for the comprehension") )))))
  
  
  (define-derived-comprehension fold3-ec (nested)
    ((fold3-ec x0 (nested q1 ...) q etc ... expr f1 f2)
     (q1 ... q)
     (fold3-ec x0 (nested q1 ... q) etc ... expr f1 f2) )
    ((fold3-ec x0 q1 q2 etc ... expr f1 f2)
     (q1 q2 etc ...)
     (fold3-ec x0 (nested q1 q2) etc ... expr f1 f2) )
    ((fold3-ec x0 expression f1 f2)
     ()
     (fold3-ec x0 (nested) expression f1 f2) )
    
    ((fold3-ec x0 qualifier expression f1 f2)
     (qualifier)
     (let ((result #f) (empty #t))
       (do-ec qualifier
              (let ((value expression)) ; don't duplicate
                (if empty
                    (begin (set! result (f1 value))
                           (set! empty #f) )
                    (set! result (f2 value result)) )))
       (if empty x0 result) )))
  
  (define-derived-comprehension fold-ec (nested)
    ((fold-ec x0 (nested q1 ...) q  etc1 etc ... body f2)
     (q q1 ... etc1 etc2 etc ...)
     (fold-ec x0 (nested q1 ... q)  etc1 etc ... body f2) )
    ((fold-ec x0 q1 q2 etc ... body f2)
     (q1 q2 etc ...)
     (fold-ec x0 (nested q1 q2) etc ... body f2) )
    ((fold-ec x0 body f2)
     ()
     (fold-ec x0 (nested) body f2) )
    ((fold-ec x0 qualifier body f2)
     (qualifier)
     (let ((result x0))
       (do-ec qualifier (set! result (f2 body result)))
       result )))
  
  (define-derived-comprehension sum-ec ()
    ((sum-ec etc ... body)
     (etc ...)
     (fold-ec (+) etc ... body +) ))
  
  (define-derived-comprehension product-ec ()
    ((product-ec etc ... body)
     (etc ...)
     (fold-ec (*) etc ... body *) ))
  
  
  (define-syntax (min-ec2 stx)
    (define (check clauses body)
      (check-all-clauses-are-generators-or-filters clauses 'min-ec)
      (when (and (null? (syntax->list clauses))
                 (generator-clause? body))
        (raise-syntax-error
         'min-ec (string-append 
                  "Generator used in body position. "
                  "Expected (min-ec <generator-or-filter> ... <expr>), got: ")
         body)))
      
    (syntax-case stx (on-new-min)
      [(_ clause ... (on-new-min on-min-expr ...) body)
       (begin
         (check #'(clause ...) #'body)
         (let ([insert-body (expand-clauses #'(clause ...))])
           #`(let ([minimum +inf.0])
               #,(insert-body 
                  #'(let ([x body])
                      (when (< x minimum)
                        (set! minimum x)
                        on-min-expr ...
                        x)))
               minimum)))]
      [(_ clause ... body)
       (syntax/loc stx (min-ec clause ... (on-new-min (void)) body))]
      [_
       (raise-syntax-error
        'min-ec
        (format "expected (~a <generator-or-filter> ... <expr>), got: " 'name-ec)
        stx)]))
  
  #;(define-derived-comprehension min-ec ()
      ((min-ec etc ... body)
       (etc ...)
       (fold3-ec (min) etc ... body min min)))
  
  (define-derived-comprehension min-ec (on-new-min)
    ((min-ec etc ... (on-new-min on-expr ...) body)
     (etc ...)
     (fold3-ec (min) etc ... body min (lambda (new old)
                                        (if (< new old)
                                            (begin
                                              on-expr ...
                                              new)
                                            old))))
    ((min-ec etc ... body)
     (etc ...)
     (fold3-ec (min) etc ... body min min)))
  
  (define-syntax (max-ec stx)
    (define (check clauses body)
      (check-all-clauses-are-generators-or-filters clauses 'max-ec)
      (when (and (null? (syntax->list clauses))
                 (generator-clause? body))
        (raise-syntax-error
         'max-ec (string-append 
                  "Generator used in body position. "
                  "Expected (min-ec <generator-or-filter> ... <expr>), got: ")
         body)))
      
    (syntax-case stx (on-new-max)
      [(_ clause ... (on-new-max on-max-expr ...) body)
       (begin
         (check #'(clause ...) #'body)
         (let ([insert-body (expand-clauses #'(clause ...))])
           #`(let ([maximum -inf.0])
               #,(insert-body 
                  #'(let ([x body])
                      (when (> x maximum)
                        (set! maximum x)
                        on-max-expr ...
                        x)))
               maximum)))]
      [(_ clause ... body)
       (syntax/loc stx (max-ec clause ... (on-new-max (void)) body))]
      [_
       (raise-syntax-error
        'max-ec
        (format "expected (~a <generator-or-filter> ... <expr>), got: " 'name-ec)
        stx)]))
  
  
  #;(define-derived-comprehension max-ec ()
    ((max-ec etc ... body)
     (etc ...)
     (fold3-ec (max) etc ... body max max) ))
  
  (define-derived-comprehension last-ec (nested)
    ((last-ec default (nested q1 ...) q etc ... body)
     (q1 ... q etc ...)
     (last-ec default (nested q1 ... q) etc ... body) )
    ((last-ec default q1 q2             etc ... body)
     (q1 q2 etc ...)
     (last-ec default (nested q1 q2)    etc ... body) )
    ((last-ec default body)
     ()
     (last-ec default (nested) body) )
    
    ((last-ec default qualifier body)
     (qualifier)
     (let ((result default))
       (do-ec qualifier (set! result body))
       result )))
  
  
  (define-derived-comprehension first-ec (nested)
    ((first-ec default (nested q1 ...) q etc ... body)
     (q1 ... q etc ...)
     (first-ec default (nested q1 ... q) etc ... body) )
    ((first-ec default q1 q2             etc ... body)
     (q1 q2 etc ...)
     (first-ec default (nested q1 q2)    etc ... body) )
    ((first-ec default body)
     ()
     (first-ec default (nested) body) )
    
    ((first-ec default qualifier body)
     (qualifier)
     (let ((result default) (stop #f))
       (ec-guarded-do-ec 
        stop 
        (nested qualifier)
        (begin (set! result body)
               (set! stop #t) ))
       result )))
  
  ; (ec-guarded-do-ec stop (nested q ...) cmd)
  ;   constructs (do-ec q ... cmd) where the generators gen in q ... are
  ;   replaced by (:until gen stop).
  
  (define-derived-comprehension ec-guarded-do-ec (nested if not and or begin)
    ((ec-guarded-do-ec stop (nested (nested q1 ...) q2 ...) cmd)
     (q1 ... q2 ...)
     (ec-guarded-do-ec stop (nested q1 ... q2 ...) cmd) )
    
    ((ec-guarded-do-ec stop (nested (if test) q ...) cmd)
     (q ...)
     (if test (ec-guarded-do-ec stop (nested q ...) cmd)) )
    
    ((ec-guarded-do-ec stop (nested (not test) q ...) cmd)
     (q ...)
     (if (not test) (ec-guarded-do-ec stop (nested q ...) cmd)) )
    
    ((ec-guarded-do-ec stop (nested (and test ...) q ...) cmd)
     (q ...)
     (if (and test ...) (ec-guarded-do-ec stop (nested q ...) cmd)) )
    
    ((ec-guarded-do-ec stop (nested (or test ...) q ...) cmd)
     (q ...)
     (if (or test ...) (ec-guarded-do-ec stop (nested q ...) cmd)) )
    
    ((ec-guarded-do-ec stop (nested (begin etc ...) q ...) cmd)
     (q ...)
     (begin etc ... (ec-guarded-do-ec stop (nested q ...) cmd)) )
    
    ((ec-guarded-do-ec stop (nested gen q ...) cmd)
     (q ...)
     (do-ec 
      (:until gen stop) 
      (ec-guarded-do-ec stop (nested q ...) cmd) ))
    
    ((ec-guarded-do-ec stop (nested) cmd)
     ()
     (do-ec cmd) ))
  
  ; ==========================================================================
  ; The early-stopping comprehensions any?-ec every?-ec
  ; ==========================================================================
  
  (define-derived-comprehension any?-ec (nested)
    ((any?-ec (nested q1 ...) q etc ... body)
     (q etc ...)
     (any?-ec (nested q1 ... q) etc ... body) )
    ((any?-ec q1 q2             etc ... body)
     (q1 q2 etc ...)
     (any?-ec (nested q1 q2)    etc ... body) )
    ((any?-ec expression)
     ()
     (any?-ec (nested) expression) )
    
    ((any?-ec qualifier expression)
     (qualifier)
     (first-ec #f qualifier (if expression) #t) ))
  
  
  (define-derived-comprehension every?-ec (nested)
    ((every?-ec (nested q1 ...) q etc ... body)
     (q1 ... q etc ...)
     (every?-ec (nested q1 ... q) etc ... body) )
    ((every?-ec q1 q2             etc ... body)
     (q1 q2 etc ...)
     (every?-ec (nested q1 q2)    etc ... body) )
    ((every?-ec expression)
     ()
     (every?-ec (nested) expression) )
    
    ((every?-ec qualifier expression)
     (qualifier)
     (first-ec #t qualifier (if (not expression)) #f) ))
  
  )
