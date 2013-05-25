;;;
;;; EXPANSION
;;;

(module expansion mzscheme
  (provide (all-from "generator-struct.scm")
           (all-from "generator-definitions.scm")
           (all-from "loops.scm"))
  (provide generator->loop
           check-all-clauses-are-generators-or-filters
           expand-clauses
           generator-clause?
           filter-clause?
           add-index)
  
  (require "generator-struct.scm"
           "loops.scm"
           "generator-definitions.scm")
  (require-for-template mzscheme)
  
  (define current-introducer (make-parameter #f))
  
  (define (generator-clause? clause-stx)
    (syntax-case clause-stx ()
      [(name . more)
       (and (identifier? #'name)
            (generator? (syntax-local-value #'name (lambda () #f))))]
      [_ #f]))
  
  (require (lib "stx.ss" "syntax"))
  (require (prefix base: scheme)
           (for-meta 1 (prefix base: scheme)))
  
  (define (if-filter? stx)
      (syntax-case stx ()
        [(head expr)
         (and (identifier? #'head)
              (eq? 'if (syntax-e #'head)))]
        [else #f]))
  
  (require (prefix new- scheme))
  
  #;(define (if-filter? stx)
    (syntax-case* stx (if new-if) module-or-top-identifier=?
      [(if expr) #t]
      [(new-if expr) #t]
      [_ #f]))
  
  (define (filter-clause? clause-stx)
    (or (if-filter? clause-stx)
        (syntax-case* clause-stx (if base:if not and or) module-or-top-identifier=?
          [(not . more) #t]
          [(and . more) #t]
          [(or  . more) #t]
          [_            #f])))
      
  (define (begin-clause? clause-stx)
    (syntax-case clause-stx (begin)
      [(begin  . more) #t]
      [_               #f]))
  
  (define (nested-clause? clause-stx)
    (syntax-case clause-stx (nested)
      [(nested  . more) #t]
      [_                #f]))
  
  ; generator->loop : clause-stx -> loop
  (define (generator->loop clause-stx)
    (define introduce (make-syntax-introducer))
    (define (mark s)
      (if (current-introducer) ; this is a subexpansion
          (introduce ((current-introducer) s))
          (introduce (syntax-local-introduce s))))
    (define (unmark s)
      (if (current-introducer) ; this is a subexpansion
          ((current-introducer) (introduce s))
          (syntax-local-introduce (introduce s))))
    (syntax-case clause-stx ()
      [(gen-name . more)
       (begin
         (unless (generator-clause? clause-stx)
           (raise-syntax-error
            'generator->loop 
            "expected a generator clause, got: "
            clause-stx ))
         (let* ([generator (syntax-local-value #'gen-name)]
                [marked-clause-stx (mark clause-stx)]
                [loop (parameterize ([current-introducer introduce])
                        ((generator-clause->loop generator) 
                         marked-clause-stx))])
           (cond
             [(loop? loop)                      (make-loop (unmark (loop-stx loop)))]
             [(generator-clause? (unmark loop)) (generator->loop (unmark loop))]
             [else
              (raise-syntax-error 
               'generator-loop 
               (apply string-append
                      (cons "generator expander returned neither a loop structure nor "
                            (cons "a syntax-object representing a generator clause. "
                                  (if (syntax? loop)
                                      (syntax-case loop ()
                                        [(name . more) 
                                         (identifier? #'name)
                                         (list (string-append "\nMaybe you forgot to define "
                                                              (symbol->string (syntax-object->datum #'name))
                                                              " to as a generator?"))]
                                        [_ '()])
                                      '()))))
               loop)])))]))
  
  
  
  ; expand-clauses : stx -> (stx -> stx)
  ;   Input:  A syntax-object representing a list of clauses
  ;   Output: A procedure of one argument. The input of which is
  ;           a syntax-object representing the body (aka payload)
  ;           of the loop. The output is a fully expanded loop.
  ;   Note:   This is used by comprehensions such as list-ec
  ;           to insert their payloads into the "middle" of the loop.
  (require (lib "stx.ss" "syntax"))
  
  (define (expand-clauses clauses-stx)
    (syntax-case clauses-stx () 
      [() 
       (lambda (body-stx) body-stx)]
      [(clause1 clause2 ...)
       (lambda (body-stx)
         (cond
           [(generator-clause? #'clause1)
            (let ([loop1    (generator->loop #'clause1)]
                  [loop2... (expand-clauses #'(clause2 ...))])
              (loop->syntax #'clause1 loop1
                            (loop2... body-stx)))]
           [(filter-clause? #'clause1)
            (let ([loop2... (expand-clauses #'(clause2 ...))])
              (cond
                [(if-filter? #'clause1)
                 (syntax-case #'clause1 ()
                   [(the-if expr)
                    #`(if expr #,(loop2... body-stx))]
                   [else (raise-syntax-error 'expand-clauses 
                                             "internal error: <if-filter> expected" #'clause1)])] 
                [else
                 (syntax-case* #'clause1 (if not and or) module-or-top-identifier=?  ; due to not
                   #;[(if expr)  
                      #`(if expr #,(loop2... body-stx))]
                   [(not expr)
                    #`(if (not expr) #,(loop2... body-stx))]
                   [(or expr ...)
                    #`(if (or expr ...) #,(loop2... body-stx))]
                   [(and expr ...)
                    #`(if (and expr ...) #,(loop2... body-stx))]
                   [_
                    (raise-syntax-error 'expand-clauses 
                                        "unimplemented <filter>" #'clause1)])]))]
           [(begin-clause? #'clause1)
            (let ([loop2... (expand-clauses #'(clause2 ...))])
              (syntax-case #'clause1 ()
                [(_ expr1 ...)
                 #`(begin expr1 ... #,(loop2... body-stx))]))]
           [(nested-clause? #'clause1)
            (syntax-case #'clause1 (nested)
              [(nested qualifier ...)
               ((expand-clauses 
                 #`(qualifier ... clause2 ...))
                body-stx)]
              [_
               (error)])]
           [else
            (begin
              (display clauses-stx) (newline)
              (error 'expand-clauses "this should have been caught earlier"))]))]
      [else
       (error "huh")]))
  
  (define (check-all-clauses-are-generators-or-filters clauses-stx caller)
    (syntax-case clauses-stx ()
      [(clause ...)
       (let loop ([cs (syntax->list #'(clause ...))])
         (cond
           [(null? cs)                   'all-ok]
           [(generator-clause? (car cs)) (loop (cdr cs))]
           [(filter-clause? (car cs))    (loop (cdr cs))]
           [(begin-clause? (car cs))     (loop (cdr cs))]
           [(nested-clause? (car cs))    (loop (cdr cs))]
           [else (raise-syntax-error 
                  caller "<generator> or <filter> expected, got:" (car cs))]))]))
  
  ; add-index : loc-stx loop-or-stx var-stx -> loop
  ;   add a loop binding to the loop, s.t.
  ;   var-stx now counts the number of
  ;   elements produced
  
  (require-for-template mzscheme)
  
  (define (add-index-proc l var-stx)
    (cond
      [(loop? l) 
       (with-syntax ([var var-stx])
         (syntax-case (loop-stx l) ()
           [(ob* oc* lb* ne1 ib* ic* ne2 ls*)
            (make-loop #'(ob* oc* ((var 0) . lb*)
                              ne1 ib* ic* ne2 ((add1 var) . ls*)))]))]
      [(syntax? l)
       (add-index-proc (generator->loop l) var-stx)]
      [else
       (raise-syntax-error 
        'add-index-proc "expected either a loop structure of a generator clause as first argument, got" l)]))
  
  (define-syntax (add-index stx)
    (syntax-case stx (syntax)
      [(_ loc #'loop var-stx)
       #'(add-index-proc (syntax/loc loc loop) var-stx)]
      [(_ #'loop var-stx)
       (raise-syntax-error
        'add-index
        "you forgot to add location info"
        stx)]
      [_ 
       (raise-syntax-error 'add-index "think" stx)]))
  )
