(module useful-code (lib "frtime.ss" "frtime")
  
  (require (lib "string.ss")
           (lib "contract.ss")
           (lib "list.ss"))
  
  (provide (all-defined))
  
  ; Keeps a list of the last n values of a behavior
  (define (history-b n stream)
    (define ((add-to-hist thing) hist) (append (if ((length hist) . < . n) hist (rest hist)) (list thing)))
    (accum-b  (stream . ==> . add-to-hist) empty))
  
  ; Counts number of event pings on an eventstream
  (define (count-e evs)
    (accum-b (evs . -=> . add1) 0))
  
  ; Counts number of times a behavior updates/changes
  (define (count-b b)
    (accum-b ((changes b) . -=> . add1) 0))
  
  ; Matches a sequence of items in a list to event pings
  (define (sequence-match? seq evs)
    (equal? seq (history-b (length seq) evs)))
  
  ; Cheap printf for behaviors
  (define printf-b format)
  
  ; Flattens a list
  (define (flatten x)
    (cond ((empty? x) '())
          ((and (list? x)
                (list? (first x)))
           (append (flatten (car x)) (flatten (cdr x))))
          (else (list x)))))
