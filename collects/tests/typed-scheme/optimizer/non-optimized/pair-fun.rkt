(module pair-fun typed/scheme 
  (require racket/unsafe/ops)
  (: f ((Listof Integer) -> Integer))
  (define (f x)
    (if (null? x)
        1
        (car x))))
