#lang scheme

(provide current-request
         get-binding
         get-type-normal
         get-sensitivity
         get-contain-all)

(require web-server/servlet)

(define current-request (make-parameter #f))

(define (get-binding sym default)
  (if (current-request)
      (with-handlers ([(λ (x) #t) (lambda (x) default)])
        (extract-binding/single sym (request-bindings (current-request))))
      default))

(define (convert v vs)
      (let ([v (if (string? v) (string->symbol v) v)])
        (cond [(assoc v vs) => cadr]
              [else (error (list v vs))])))

(define (get-type-normal)
  (convert (get-binding 't  'normal) '((normal #t) (regular #f))))

(define (get-sensitivity)
  (convert (get-binding 's  'yes)    '((yes #t) (no #f))))

(define (get-contain-all)
  (convert (get-binding 'ca  'yes)   '((yes #t) (no #f))))

      