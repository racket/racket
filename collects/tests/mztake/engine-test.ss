(module engine-test mzscheme
  
  (require "../engine.ss")
  
  (define stx (read-all-syntax "../demos/dijkstra/dijkstra.ss"))
  
  (define fn (pattern->pos stx))
  (define result (fn '(define (_ ...) _ ...)))
  
  (define expected  '((7 2 147) (18 2 463)))
  (printf "~a~n" (list (equal? result expected) result expected))
  )