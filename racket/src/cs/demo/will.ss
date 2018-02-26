(import (rumble))

(define-syntax check
  (syntax-rules ()
    [(_ a b)
     (let ([v b])
       (unless (equal? a v)
         (error 'check (format "failed ~s = ~s" 'b v))))]))
  
(define we (make-will-executor void))
(define we2 (make-will-executor void))
(define we3 (make-will-executor void))
(check #t (will-executor? we))

(define s1 (gensym))
(define s2 (gensym))
(will-register we s1 (let ([s2 s2]) (lambda (s) s2)))
(will-register we2 s1 (lambda (s) 'second))
(will-register we s1 (lambda (s) 'first))
(will-register we3 s2 (lambda (s) 'other))

(set! s1 #f)
(set! s2 #f)

(define (gc)
  (collect (collect-maximum-generation)))

(define (will-try-execute* we)
  (let ([p (will-try-execute we)])
    (and p
         ((car p) (cdr p)))))

(gc)
(check 'first (will-try-execute* we))
(gc)
(check 'second (will-try-execute* we2))
(gc)
(check #f (will-try-execute* we3))
(gc)
(check #t (gensym? (will-try-execute* we)))
(gc)
(check 'other (will-try-execute* we3))
(gc)
(check #f (will-try-execute* we))
(gc)
(check #f (will-try-execute* we2))
