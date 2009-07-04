#lang scheme

(require htdp/error)

(provide checked-cell%)

(define checked-cell<%>
  (interface ()
    set ;; Symbol Any -> Boolean 
    ;; does the new state differ from the old? 
    ;; effect: if so only, set state

    get ;; -> Any (ok?)
    ))

(define checked-cell% 
  (class* object% (checked-cell<%>) 
    (init-field msg value0 ok?)

    ;; Any -> ok?
    (define/private (coerce tag nw)
      (let ([b (ok? nw)])
        (check-result "check-with predicate" boolean? "Boolean" b)
        (check-result tag (lambda _ b) (format "~a (see check-with)" msg) nw)
        nw))
    
    (field [value (coerce "initial value" value0)])

    (define/public (set tag v) 
      (define nw  (coerce tag v))
      (if (equal? value nw)
          #t
          (begin
            (set! value nw)
            #f)))
    
    ;; -> ok?
    (define/public (get) value)
    
    (super-new)))

; (define c (new checked-cell% [msg "World"] [value0 1] [ok? positive?]))
; (send c set "tick" 10)
