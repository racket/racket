#lang racket/base
(require scribble/core
         scribble/decode)

(provide new-counter
         counter-target
         counter-ref
         counter-collect-value)

(define-struct counter ([n #:mutable] name target-wrap ref-wrap))

(define (new-counter name
                     #:target-wrap [target-wrap (lambda (c s) c)]
                     #:ref-wrap [ref-wrap (lambda (c s) c)])
  (make-counter 0 name target-wrap ref-wrap))

(define (tag->counter-tag counter tag . kind)
  (if (generated-tag? tag)
      `(,(string->symbol (format "counter-~a" kind)) ,tag)
      `(counter (,(counter-name counter) ,tag ,@kind))))

(define (counter-target counter tag label 
                        #:continue? [continue? #f]
                        . content)
  (let ([content (decode-content content)])
    (define c
      (make-target-element
       #f
       (list
        (make-collect-element
         #f
         (list
          (make-delayed-element
           (lambda (renderer part ri)
             (let ([n (resolve-get part ri (tag->counter-tag counter tag "value"))])
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
           (let ([n (if continue?
                        (counter-n counter)
                        (add1 (counter-n counter)))])
             (set-counter-n! counter n)
             (collect-put! ci (generate-tag (tag->counter-tag counter tag "value") ci) n)))))
       (tag->counter-tag counter tag)))
    (if (counter-target-wrap counter)
        ((counter-target-wrap counter)
         c
         (format "t:~a" (t-encode (tag->counter-tag counter tag))))
        c)))

(define (t-encode s)
  (apply
   string-append
   (map (lambda (c)
          (cond
            [(and (or (char-alphabetic? c) (char-numeric? c))
                  ((char->integer c) . < . 128))
             (string c)]
            [(char=? c #\space) "_"]
            [else (format "x~x" (char->integer c))]))
        (string->list (format "~s" s)))))

(define (counter-ref counter tag label)
  (let ([n (make-delayed-element
            (lambda (renderer part ri)
              (let ([n (resolve-get part ri (tag->counter-tag counter tag "value"))])
                (if (counter-ref-wrap counter)
                    (let ([id (format "t:~a" (t-encode (list 'counter (list (counter-name counter) tag))))])
                      ((counter-ref-wrap counter)
                       (format "~a" n) 
                       id))
                    (list (format "~a" n)))))
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
     (tag->counter-tag counter tag))))

(define (counter-collect-value counter)
  (counter-n counter))
