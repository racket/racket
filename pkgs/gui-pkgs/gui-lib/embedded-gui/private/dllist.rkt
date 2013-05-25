(module dllist mzscheme
  
  (require
   mzlib/class
   mzlib/list
   mzlib/etc
   framework
   "interface.rkt")
  
  (provide dllist-mixin dllist% head% tail%)
  
  (define dllist-mixin
    (mixin () (dllist<%>)
      (field [nextf #f] [prevf #f])
      (define/public next
        (case-lambda
          [(n) (set! nextf n)]
          [() nextf]))
      (define/public prev
        (case-lambda
          [(p) (set! prevf p)]
          [() prevf]))
      (define/public (for-each f . lol)
        (apply f this (map first lol))
        (send/apply nextf for-each f (map rest lol)))
      (define/public (map-to-list f)
        (cons (f this) (send nextf map-to-list f)))
      (super-new)))
  
  (define dllist% (dllist-mixin object%))
  
  (define head%
    (class* dllist% (dllist<%>)
      (inherit next)
      (define/override prev
        (opt-lambda ((p #f))
          (error 'prev "Head has no prev")))
      (define/override (for-each f . lol)
        (send/apply (next) for-each f lol))
      (define/override (map-to-list f)
        (send (next) map-to-list f))
      (super-new)))
  
  (define tail%
    (class* dllist% (dllist<%>)
      (define/override next
        (opt-lambda ((n #f))
          (error 'next "Tail has no next")))
      (define/override (for-each f . lol) (void))
      (define/override (map-to-list f) empty)
      (super-new)))
  )
