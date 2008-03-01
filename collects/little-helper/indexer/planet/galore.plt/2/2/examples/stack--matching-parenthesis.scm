;;; stack--matching-parenthesis.scm  --  Jens Axel Søgaard

;;; This example uses stack to check whether the parenthesis
;;; in a given string are properly nested.

(require (planet "stack.scm" ("soegaard" "galore.plt" 2 1)))
(require (lib "42.ss" "srfi"))

; paren-pair : char char -> boolean
;   checks whether the left and right parenthesis
;   are of the same type
(define (paren-pair? l r)
  (or (and (char=? l #\() (char=? r #\)))
      (and (char=? l #\[) (char=? r #\]))
      (and (char=? l #\{) (char=? r #\}))))

(define (left-parenthesis? l)
  (or (char=? l #\()
      (char=? l #\[)
      (char=? l #\{)))

(define (right-parenthesis? l)
  (or (char=? l #\))
      (char=? l #\])
      (char=? l #\})))

; matches? : string -> boolean or string
;   checks whether the parenthesis (), [] and {}
;   are properly matched in the given string 
(define (matches? s)
  (let loop ([cs      (string->list s)]
             [pending empty])
    (cond
      [(and (null? cs) (not (empty? pending)))
       (format "the following parenthesis weren't closed: ~a" 
               (string-ec (: c pending) c))]
      [(null? cs)
       #t]
      [(left-parenthesis? (car cs))
       (loop (cdr cs) (insert (car cs) pending))]
      [(and (right-parenthesis? (car cs)) 
            (or (empty? pending)
                (not (paren-pair? (first pending) (car cs)))))
       (format "right parenthesis with no correspoding left parenthesis found: ~a" (car cs))]
      [(and (right-parenthesis? (car cs)) 
            (paren-pair? (first pending) (car cs)))
       (loop (cdr cs) (remove pending))]
      [else
       (loop (cdr cs) pending)])))

;;; TESTS

(matches? "()")
(matches? "([]{[[()]]})")
(matches? "([b]{[a[s()]]})")
(matches? "()]")
(matches? "(){")
