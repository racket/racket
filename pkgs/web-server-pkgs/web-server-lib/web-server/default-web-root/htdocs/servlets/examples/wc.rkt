#lang racket/base
(require web-server/servlet)
(define interface-version 'v1)
(define timeout +inf.0)
(provide start interface-version timeout)

(define (start initial-request)
  ; A top-level frame must exist
  (define counter1 (make-counter))
  (define counter2 (make-counter))
  ; counter1 and counter2 must have been added to the top-level frame    
  (define include1 (include-counter counter1))
  (define include2 (include-counter counter2))
  ; counter1 and counter2 may have been modified
  (send/suspend/dispatch
   (lambda (embed/url)
     (response/xexpr
      ; The frame (ref) must have been captured, any changes to web-cells after this will be lost
      `(html 
        (body (h2 "Web Cell Test")
              (div (h3 "First")
                   ,(include1 embed/url))
              (div (h3 "Second")
                   ,(include2 embed/url))))))))

(define (make-counter)
  (make-web-cell 0))

(define (include-counter a-counter)
  ; Note: This shouldn't be necessary (but is for testing, not in production)
  (call-with-current-continuation
   (lambda (k)
     (define (generate)
       (k
        (lambda (embed/url)
          `(div (h3 ,(number->string (web-cell-ref a-counter)))
                (a ([href ,(embed/url
                            (lambda _
                              ; A new frame has been created
                              (define last (web-cell-ref a-counter))
                              ; It is a child of the parent frame, so we can inspect the value
                              (web-cell-shadow a-counter (add1 last))
                              ; The new frame has been modified
                              (generate)))])
                   "+")))))
     (generate))
   servlet-prompt))
