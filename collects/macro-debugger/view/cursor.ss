
#lang scheme/base
(require scheme/promise)
(provide cursor?
         cursor:new
         cursor:add-to-end!
         cursor:remove-current!

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
         cursor:skip-to

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
(define-struct cursor (prefix suffixp) #:mutable)

(define (cursor:new items)
  (make-cursor null items))

(define (cursor:add-to-end! c items)
  (let ([suffix (cursor-suffixp c)])
    (set-cursor-suffixp! c (stream-append suffix items))))

(define (cursor:remove-current! c)
  (when (cursor:has-next? c)
    (set-cursor-suffixp! c (stream-cdr (cursor-suffixp c)))))

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
    (let ([old-prefix (cursor-prefix c)])
      (set-cursor-prefix! c (cdr old-prefix))
      (set-cursor-suffixp! c (cons (car old-prefix) (cursor-suffixp c))))))

(define (cursor:move-next c)
  (when (cursor:has-next? c)
    (let* ([old-suffixp (cursor-suffixp c)])
      (set-cursor-prefix! c (cons (stream-car old-suffixp)
                                  (cursor-prefix c)))
      (set-cursor-suffixp! c (stream-cdr old-suffixp)))))

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

(define (cursor:skip-to c i)
  (unless (or (eq? (cursor:next c) i) (cursor:at-end? c))
    (cursor:move-next c)
    (cursor:skip-to c i)))

(define (cursor->list c)
  (append (cursor:prefix->list c)
          (cursor:suffix->list c)))

(define (cursor:prefix->list c)
  (reverse (cursor-prefix c)))

(define (cursor:suffix->list c)
  (stream->list (cursor-suffixp c)))
