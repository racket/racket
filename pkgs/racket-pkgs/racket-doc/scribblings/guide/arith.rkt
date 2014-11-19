#lang racket
(require syntax/readerr)

(provide read read-syntax)

(define (read in)
  (syntax->datum (read-syntax #f in)))
      
(define (read-syntax src in)
  (skip-whitespace in)
  (read-arith src in))

(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))

(define (read-arith src in)
  (define-values (line col pos) (port-next-location in))
  (define expr-match 
    (regexp-match 
     ;; Match an operand followed by any number of 
     ;; operator--operand sequences, and prohibit an
     ;; additional operator from following immediately:
     #px"^([a-z]|[0-9]+)(?:[-+*/]([a-z]|[0-9]+))*(?![-+*/])"
     in))

  (define (to-syntax v delta span-str)
    (datum->syntax #f v (make-srcloc delta span-str)))
  (define (make-srcloc delta span-str)
    (and line
         (vector src line (+ col delta) (+ pos delta)
                 (string-length span-str))))

  (define (parse-expr s delta)
    (match (or (regexp-match #rx"^(.*?)([+-])(.*)$" s)
               (regexp-match #rx"^(.*?)([*/])(.*)$" s))
      [(list _ a-str op-str b-str)
       (define a-len (string-length a-str))
       (define a (parse-expr a-str delta))
       (define b (parse-expr b-str (+ delta 1 a-len)))
       (define op (to-syntax (string->symbol op-str)
                             (+ delta a-len) op-str))
       (to-syntax (list op a b) delta s)]
      [_ (to-syntax (or (string->number s) 
                        (string->symbol s))
                    delta s)]))

  (unless expr-match
    (raise-read-error "bad arithmetic syntax" 
                      src line col pos 
                      (and pos (- (file-position in) pos))))
  (parse-expr (bytes->string/utf-8 (car expr-match)) 0))
