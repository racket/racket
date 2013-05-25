(module unicode-chars mzscheme
  (require "util.rkt")
  
  (provide (all-defined))
  
  ;; mapped-chars : (listof (list nat nat bool))
  (define mapped-chars (make-known-char-range-list))
  
  
  ;; get-chars-for-x : (nat -> bool) (listof (list nat nat bool)) -> (listof (cons nat nat))
  (define (get-chars-for char-x? mapped-chars)
    (cond
      ((null? mapped-chars) null)
      (else
       (let* ((range (car mapped-chars))
              (low (car range))
              (high (cadr range))
              (x (char-x? low)))
         (cond
           ((caddr range)
            (if x
                (cons (cons low high)
                      (get-chars-for char-x? (cdr mapped-chars)))
                (get-chars-for char-x? (cdr mapped-chars))))
           (else
            (let loop ((range-start low)
                       (i (car range))
                       (parity x))
              (cond
                ((> i high)
                 (if parity
                     (cons (cons range-start high) (get-chars-for char-x? (cdr mapped-chars)))
                     (get-chars-for char-x? (cdr mapped-chars))))
                ((eq? parity (char-x? i))
                 (loop range-start (add1 i) parity))
                (parity
                 (cons (cons range-start (sub1 i)) (loop i (add1 i) #f)))
                (else
                 (loop i (add1 i) #t))))))))))

  (define (compute-ranges x?)
    (delay (get-chars-for (lambda (x) (x? (integer->char x))) mapped-chars)))
  
  (define alphabetic-ranges   (compute-ranges char-alphabetic?))  ;; 325
  (define lower-case-ranges   (compute-ranges char-lower-case?))  ;; 405
  (define upper-case-ranges   (compute-ranges char-upper-case?))  ;; 380
  (define title-case-ranges   (compute-ranges char-title-case?))  ;; 10
  (define numeric-ranges      (compute-ranges char-numeric?))     ;; 47
  (define symbolic-ranges     (compute-ranges char-symbolic?))    ;; 153
  (define punctuation-ranges  (compute-ranges char-punctuation?)) ;; 86
  (define graphic-ranges      (compute-ranges char-graphic?))     ;; 401
  (define whitespace-ranges   (compute-ranges char-whitespace?))  ;; 10
  (define blank-ranges        (compute-ranges char-blank?))       ;; 9
  #;(define hexadecimal-ranges  (compute-ranges char-hexadecimal?))
  (define iso-control-ranges  (compute-ranges char-iso-control?)) ;; 2



  (test-block ()
              ((get-chars-for odd? '()) '())
              ((get-chars-for odd? '((1 4 #f) (8 13 #f))) '((1 . 1) (3 . 3) (9 . 9) (11 . 11) (13 . 13)))
              ((get-chars-for (lambda (x)
                                (odd? (quotient x 10)))
                              '((1 5 #t) (17 19 #t) (21 51 #f)))
               '((17 . 19) (30 . 39) (50 . 51))))
  
  )
