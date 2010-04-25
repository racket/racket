#lang scheme/load

(module auxs scheme 
  (define (world->world> proc0)
    (printf "a world to world function\n")
    proc0)
  
  (define (positive-number> rate0)
    (printf "a positive number")
    rate0)
  
  ;; String String Syntax[id] -> Syntax 
  (define (pre-post-name pre post name)
    (datum->syntax 
     name (string->symbol (string-append pre (symbol->string (syntax-e name)) post))))
  
  (provide (all-defined-out)))

(module clauses scheme 
  
  (require syntax/parse (for-syntax scheme 'auxs unstable/syntax)
           (for-template scheme/base 'auxs))
  
  
  (define-syntax (define-clause stx)
    (syntax-case stx ()
      [(_ name (proc p-ctc) (rate r-ctc) ...)
       (with-syntax ([name-clause (pre-post-name "" "-clause" #'name)]
                     [(rate0 ...) (generate-temporaries #'(rate ...))])
         (with-syntax ([((thing ...) ...) #'((#:with rate #'(r-ctc rate0)) ...)])
           #`
           (begin
             (provide name name-clause)
             
             (define-syntax (name . x)
               (raise-syntax-error 'name "used out of context" x))
             
             (define-syntax-class name-clause
               #:description (format "~a" 'name)
               #:literals (name)
               #:attributes (proc rate ...)
               (pattern (name proc0:expr)
                        #:with (rate0 ...) (map (lambda (x) #'0) '(rate0 ...))
                        #:with proc #'(world->world proc0)
                        thing ... ...)
               (pattern (on-tick proc0:expr (~var rate0 expr) ...)
                        #:with proc #'(world->world> proc0)
                        thing ... ...))
             )))]))
  
  (define-clause on-mouse (proc world-nat-nat-mouse->world))
  (define-clause on-tick (proc world->world) (rate (lambda (x) 1/28)))
  
  ;; --- on-tick --- 
  #|
  (define-syntax (on-tick . x)
    (raise-syntax-error 'on-tick "used out of context" x))
  
  (define-syntax-class on-tick-clause 
    #:description "on tick"
    #:literals (on-tick)
    #:attributes (proc rate)
    (pattern (on-tick proc0:expr)
             #:with proc #'(world->world proc0)
             #:with rate #'1/28)
    (pattern (on-tick proc0:expr rate0:expr)
             #:with proc #'(world->world> proc0)
             #:with rate #'(positive-number> rate0)))
  
  (provide on-tick on-tick-clause)
  |#
  ;; --- on-draw --- 
  (define-syntax (on-draw . x)
    (raise-syntax-error 'on-draw "used out of context" x))
  
  (define-syntax-class on-draw-clause 
    #:description "on draw"
    #:literals (on-draw)
    #:attributes (proc width height)
    (pattern (on-draw proc0:expr)
             #:with proc #'(wrap worldxkey->world proc0)
             #:with width #'#f
             #:with height #'#f)
    (pattern (on-draw proc0:expr width0:expr height0:expr)
             #:with proc #'(worldxkey->world> proc0)
             #:with width #'(natural-number> width0)
             #:with height #'(natural-number> height0)))
  
  (provide on-draw on-draw-clause))

(module utest scheme 
  (require (for-syntax syntax/parse 'clauses))
  
  (define-syntax (big-bang stx)
    (syntax-parse stx
      [(big-bang world0:expr 
                 (~or (~optional otc:on-tick-clause)
                      ; (~optional omc:on-mouse-clause)
                      (~optional odc:on-draw-clause))
                 ...)
       #`(printf "~s\n"
                 '(bb world0
                      #,(if (attribute otc)
                            #'otc.rate 
                            #'1/28)
                      #,(if (attribute odc)
                            #'odc.proc
                            #''not-draw)))]))
  
  (big-bang 0)   
  (big-bang 1 (on-tick add1))
  (big-bang 2 (on-tick add1 1/2))
  (big-bang 3 (on-draw add1 1/2 1/3))
  ; (big-bang 4 (on-mouse add1 1 2))
  )

(require 'utest)