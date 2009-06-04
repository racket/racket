#lang scheme
(require scribble/struct
         scribble/decode)

(provide new-counter
         counter-target
         counter-ref)

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
       (if label
           (list
            (make-delayed-element
             (lambda (renderer part ri)
               (let ([n (resolve-get part ri `(counter (,(counter-name counter) ,tag "value")))])
                 (list* label 'nbsp (format "~a" n)
                        content)))
             (lambda () (if label
                            (list* label 'nbsp "N" content)
                            content))
             (lambda () (if label
                            (list* label 'nbsp "N" content)
                            content))))
           content)
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
    (if label
        (make-link-element
         #f
         (list
          label
          'nbsp
          n)
         `(counter (,(counter-name counter) ,tag)))
        n)))
