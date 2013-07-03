#lang scheme/load

(module auxs scheme 
  (define (world->world> proc0)
    (printf "a world to world function\n")
    proc0)
  
  (define (positive-number> rate0)
    (printf "a positive number")
    rate0)
  
  ;; Syntax String String Syntax[id] -> Syntax 
  (define (pre-post-name stx pre post name)
    (datum->syntax 
     name (string->symbol (string-append pre (symbol->string (syntax-e name)) post))))
  
  (provide (all-defined-out))
  
  (define-syntax-rule 
    (define-kwd name) 
    (begin
      (provide name)
      (define-syntax (name . x) 
        (raise-syntax-error 'name "used out of context" x))))
  
  (define-kwd on-tick)  
  
  (define-kwd on-mouse))

(module clauses scheme
  
  (require syntax/parse 'auxs
           (for-syntax scheme 'auxs unstable/syntax)
           (for-template scheme/base 'auxs))
  
  (provide define-clause)
  
  (define-syntax (define-clause stx)
    (syntax-case stx ()
      [(_ name (proc p-ctc) (rate r-ctc) ...)
       (with-syntax ([name-clause (pre-post-name stx "" "-clause" #'name)]
                     [(rate0 ...) (generate-temporaries #'(rate ...))])
         (with-syntax ([((thing ...) ...) #'((#:with rate #'(r-ctc rate0)) ...)])
           #`
           (begin
             (provide name name-clause)
             
             (define-syntax-class name-clause
               #:description (format "~a" 'name)
               #:literals (name)
               #:attributes (proc rate ...)
               (pattern (name proc0:expr)
                        #:with (rate0 ...) (map (lambda (x) #'0) '(rate0 ...))
                        #:with proc #'(world->world proc0)
                        thing ... ...)
               (pattern (name proc0:expr (~var rate0 expr) ...)
                        #:with proc #'(world->world> proc0)
                        thing ... ...))
             )))]))
  
  (define-clause on-mouse (proc world-nat-nat-mouse->world))
  (define-clause on-tick (proc world->world))
  )
(module utest scheme
  (require 'clauses (for-syntax 'clauses syntax/parse) (for-template 'clauses))

  (define-syntax (big-bang stx)
    (syntax-parse stx
      [(big-bang world0:expr 
                 (~or (~optional otk:on-tick-clause)
                      (~optional omc:on-mouse-clause))
                 ...)
       #`(printf "~s\n"
                 '(bb world0
                      #,(if (attribute omc) "mouse" "no mouse")
                      #,(if (attribute otk) "tick" "no tick")))]))

  (big-bang 0)
  (big-bang 1 (on-tick +) (on-mouse -))
  (big-bang 2 (on-tick +))
  (big-bang 3 (on-mouse +))
  )

(require 'utest)
