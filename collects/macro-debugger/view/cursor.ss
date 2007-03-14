
(module cursor mzscheme
  (provide cursor?
           cursor:new
           cursor:add-to-end!

           cursor:next
           cursor:prev

           cursor:at-start?
           cursor:at-end?

           cursor:has-next?
           cursor:has-prev?

           cursor:move-next
           cursor:move-prev
           cursor:move-to-start
           cursor:move-to-end

           cursor->list
           cursor:prefix->list
           cursor:suffix->list)

  (define-syntax stream-cons
    (syntax-rules ()
      [(stream-cons x y)
       (delay (cons x y))]))

  (define (stream-car x)
    (if (promise? x)
        (car (force x))
        (car x)))

  (define (stream-cdr x)
    (if (promise? x)
        (cdr (force x))
        (cdr x)))
  
  (define (stream-null? x)
    (or (null? x)
        (and (promise? x) (null? (force x)))))

  (define (stream-append x y)
    (if (stream-null? x)
        y
        (stream-cons (stream-car x) 
                     (stream-append (stream-cdr x) y))))

  (define (stream->list s)
    (if (stream-null? s)
        null
        (cons (stream-car s) (stream->list (stream-cdr s)))))

  ;; Cursors
  
  ;; A (Cursor-of 'a) is (make-cursor (list-of 'a) (Stream-of 'a))
  (define-struct cursor (prefix suffixp))
  
  (define (cursor:new items)
    (make-cursor null items))

  (define (cursor:add-to-end! c items)
    (let ([suffix (cursor-suffixp c)])
      (set-cursor-suffixp! c (stream-append suffix items))))

  (define (cursor:next c)
    (let ([suffix (cursor-suffixp c)])
      (if (stream-null? suffix)
          #f
          (stream-car suffix))))

  (define (cursor:prev c)
    (let ([prefix (cursor-prefix c)])
      (if (pair? prefix)
          (car prefix)
          #f)))

  (define (cursor:move-prev c)
    (when (pair? (cursor-prefix c))
      (let ([old-prefix-cell (cursor-prefix c)])
        (set-cursor-prefix! c (cdr old-prefix-cell))
        (set-cdr! old-prefix-cell (cursor-suffixp c))
        (set-cursor-suffixp! c old-prefix-cell))))
  
  (define (cursor:move-next c)
    (when (cursor:has-next? c)
      (let* ([old-suffixp (cursor-suffixp c)]
             [old-suffix-pair
              (if (pair? old-suffixp) old-suffixp (force old-suffixp))])
        (set-cursor-suffixp! c (cdr old-suffix-pair))
        (set-cdr! old-suffix-pair (cursor-prefix c))
        (set-cursor-prefix! c old-suffix-pair))))

  (define (cursor:at-start? c)
    (null? (cursor-prefix c)))
  (define (cursor:at-end? c)
    (stream-null? (cursor-suffixp c)))
  (define (cursor:has-next? c)
    (not (cursor:at-end? c)))
  (define (cursor:has-prev? c)
    (not (cursor:at-start? c)))
  
  (define (cursor:move-to-start c)
    (when (cursor:has-prev? c)
      (cursor:move-prev c)
      (cursor:move-to-start c)))
  
  (define (cursor:move-to-end c)
    (when (cursor:has-next? c)
      (cursor:move-next c)
      (cursor:move-to-end c)))

  (define (cursor->list c)
    (append (cursor:prefix->list c)
            (cursor:suffix->list c)))

  (define (cursor:prefix->list c)
    (reverse (cursor-prefix c)))

  (define (cursor:suffix->list c)
    (stream->list (cursor-suffixp c)))
  
  )
