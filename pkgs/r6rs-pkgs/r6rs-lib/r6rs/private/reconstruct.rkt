#lang scheme/base

(provide reconstruction-memory wrap)

(define reconstruction-memory (make-weak-hasheq))

(define (wrap r stx srcloc no-symbols?)
  (let wrap ([r r])
    (cond
     [(syntax? r) r]
     [(and (symbol? r)
           no-symbols?)
      (error 'macro
             "transformer result included a raw symbol: ~e"
             r)]
     [(mpair? r) 
      (let ([istx (or (hash-ref reconstruction-memory r #f)
                      stx)])
        (datum->syntax
         istx
         (cons (wrap (mcar r))
               (wrap (mcdr r)))
         (if (eq? istx stx)
             srcloc
             istx)))]
     [(vector? r) (datum->syntax
                   stx
                   (list->vector
                    (map (lambda (r) (wrap r))
                         (vector->list r)))
                   srcloc)]
     [else (datum->syntax stx r srcloc)])))
