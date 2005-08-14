(module useful-code (lib "frtime.ss" "frtime")
  
  (require (lib "string.ss")
           (lib "contract.ss")
           (lib "list.ss"))
  
  (provide (all-defined))
  
  ; Keeps a list of the last n values of a behavior
  (define/contract history-e (case-> (event? . -> . any)
                                     (event? number? . -> . any))
    (case-lambda [(stream)
                  (define ((add-to-complete-hist x) hist) (append hist (list x)))
                  (accum-e (stream . ==> . add-to-complete-hist) empty)]
                 
                 [(stream n)
                  (define ((add-to-short-hist x) hist) (append (if (< (length hist) n) hist (rest hist)) (list x)))
                  (accum-e (stream . ==> . add-to-short-hist) empty)]))
  
  (define/contract history-b (case-> (event? . -> . any)
                                     (event? number? . -> . any))    
    (case-lambda [(stream) (hold (history-e stream) empty)]
                 [(stream n) (hold (history-e stream n) empty)]))
  
  ; Counts number of events on an event stream
  (define/contract count-b (event? . -> . any)
    (lambda (stream)
      (hold (accum-e (stream . -=> . add1) 0) 0)))
  
  ; Keeps track of the largest value seen on a stream
  (define/contract largest-val-b (event? . -> . any)
    (lambda (stream)
      (hold (accum-e (stream
                      . ==> .
                      (lambda (last)
                        (lambda (x)
                          (if (> x last) x last))))
                     -inf.0))))
  
  ; Keeps track of the smallest value seen on a stream
  (define/contract smallest-val-b (event? . -> . any)
    (lambda (stream)
      (hold (accum-e (stream
                      . ==> .
                      (lambda (last)
                        (lambda (x)
                          (if (< x last) x last))))
                     +inf.0))))
  
  ; Matches a sequence of items in a list to event pings
  (define/contract sequence-match? ((listof any/c) . -> . any)
    (lambda (seq evs)
      (equal? seq (history-b evs (length seq)))))
  
  ; Cheap printf for behaviors
  (define printf-b format)
  
  ; Flattens a list
  (define (flatten x)
    (cond ((empty? x) '())
          ((and (list? x)
                (list? (first x)))
           (append (flatten (car x)) (flatten (cdr x))))
          (else (list x)))))