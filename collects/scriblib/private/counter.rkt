#lang scheme
(require scribble/struct
         scribble/decode)

(provide new-counter
         counter-target
         counter-ref
         counter-collect-value)

(define-struct counter ([n #:mutable] name))

(define (new-counter name)
  (make-counter 0 name))

(define (counter-target counter tag label . content)
  (let ([content (decode-content content)])
    (make-target-element
     #f
     (list
      (make-collect-element
       #f
       (list
        (make-delayed-element
         (lambda (renderer part ri)
           (let ([n (resolve-get part ri `(counter (,(counter-name counter) ,tag "value")))])
             (let ([l (cons (format "~a" n) content)])
               (if label
                   (list* label 'nbsp l)
                   l))))
         (lambda () (if label
                        (list* label 'nbsp "N" content)
                        (cons "N" content)))
         (lambda () (if label
                        (list* label 'nbsp "N" content)
                        (cons "N" content)))))
       (lambda (ci)
         (let ([n (add1 (counter-n counter))])
           (set-counter-n! counter n)
           (collect-put! ci `(counter (,(counter-name counter) ,tag "value")) n)))))
     `(counter (,(counter-name counter) ,tag)))))

(define (counter-ref counter tag label)
  (let ([n (make-delayed-element
            (lambda (renderer part ri)
              (let ([n (resolve-get part ri `(counter (,(counter-name counter) ,tag "value")))])
                (list (format "~a" n))))
            (lambda () (if label
                           (list label 'nbsp "N")
                           (list "N")))
            (lambda () (if label
                           (list label 'nbsp "N")
                           (list "N"))))])
    (make-link-element
     #f
     (if label
         (list
          label
          'nbsp
          n)
         n)
     `(counter (,(counter-name counter) ,tag)))))

(define (counter-collect-value counter)
  (counter-n counter))
