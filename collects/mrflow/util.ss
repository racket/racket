
(module util  (lib "mrflow.ss" "mrflow")
  (require (prefix list: (lib "list.ss"))
           (lib "pretty.ss")
           (lib "class.ss")
           (prefix cst: "constants.ss"))
  
  (provide (all-defined))
  
  ;;
  ;; Number functions
  ;;
  (define natural? (lambda (n) (and (integer? n) (>= n 0))))
  
  
  ;;
  ;; List functions
  ;;
  (define length-one?
    (lambda (x) (and (pair? x) (null? (cdr x)))))
  
  (define nonempty-list-of?
    (lambda (p) (lambda (xs) (and (pair? xs) (andmap p xs)))))
  
  (define unfold-onto
    (lambda (p f g seed onto)
      (if (p seed) onto
          (cons (f seed) (unfold-onto p f g (g seed) onto)))))
  
  (define unfold
    (lambda (p f g seed)
      (unfold-onto p f g seed '())))
  
  ;; int -> list int
  (define iota
    (lambda (n)
      (unfold (lambda (x) (= x n)) (lambda (x) x) add1 0)))
  
  (define min-list-numbers
    (let ([remove-duplicates              ;; remove duplicate numbers from a sorted list
           (lambda (xs)                   ;; of numbers, returned list is reversed
             (if (null? xs) '()
                 (let loop ((xs (cdr xs)) (acc (list (car xs))))
                   (if (null? xs) acc
                       (if (< (car xs) (car acc))
                           (loop (cdr xs) (cons (car xs) acc))
                           (loop (cdr xs) acc))))))])
      (lambda (nums)
        (remove-duplicates (list:sort nums >)))))
  
  (define/contract lol->vov ((listof (listof any/c)) . -> . vector?)
    (lambda (xss) (list->vector (map list->vector xss))))
  
  (define map2deep
    (lambda (f xss)
      (map (lambda (xs) (map f xs)) xss)))
  
  (define no-duplicates?/c
    (flat-named-contract "List without duplicates"
                         (lambda (xs)
                           (let ([tbl (make-hash-table)])
                             (let/ec return-with
                               (for-each (lambda (x)
                                           (when (hash-table-get tbl x cst:thunk-false)
                                             (return-with #f))
                                           (hash-table-put! tbl x #t))
                                         xs)
                               #t)))))
  
  ;;
  ;; Vector functions
  ;;
  (define foldr-vector
    (lambda (f init v)
      (let loop ([i 0])
        (if (= i (vector-length v)) init
            (f (vector-ref v i) (loop (add1 i)))))))
  
  (define interval->list
    (lambda (v lo hi)
      (let loop ([i lo])
        (if (= i hi) '()
            (cons (vector-ref v i) (loop (add1 i)))))))
  
  (define list->immutable-vector
    (lambda xs
      (apply vector-immutable xs)))
  
  (define/contract map-vector ((any/c . -> . any) vector? . -> . vector?)
    (lambda (f v)
      (let* ([len (vector-length v)]
             [new-v (make-vector len #f)])
        (let loop ([i 0])
          (when (< i len)
            (vector-set! new-v i (f (vector-ref v i)))
            (loop (add1 i))))
        new-v)))
  
  (define/contract map-vector-of-vector ((any/c . -> . any) (vectorof vector?) . -> . (vectorof vector?))
    (lambda (f vov)
      (map-vector (lambda (v) (map-vector f v)) vov)))
  
  (define/contract for-each-vector ((any/c . -> . any) vector? . -> . void?)
    (lambda (f v)
      (let ([len (vector-length v)])
        (let loop ([i 0])
          (when (< i len)
            (f (vector-ref v i))
            (loop (add1 i)))))
      cst:void))
  
  ; Replace each element e in a vector with (f e)
  (define/contract for-each-vector! ((any/c . -> . any) vector? . -> . vector?)
    (lambda (f v)
      (let ([len (vector-length v)])
        (let loop ([i 0])
          (when (< i len)
            (vector-set! v i (f (vector-ref v i)))            
            (loop (add1 i)))))
      v))
  
  (define/contract for-each-vov ((any/c . -> . any) (vectorof (vectorof any/c)) . -> . void?)
    (lambda (f vov)
      (for-each-vector (lambda (v) (for-each-vector f v) v) vov)))
  
  ; Replace each element in a vector of vectors with (f e)
  (define/contract for-each-vov! ((any/c . -> . any) (vectorof (vectorof any/c)) . -> . any)
    (lambda (f vov)
      (for-each-vector! (lambda (v) (for-each-vector! f v) v) vov)
      vov))
  
  (define vector-of?
    (lambda (pred v)
      (let/ec escape
        (let loop ([i 0])
          (if (= i (vector-length v)) #t
              (if (pred (vector-ref v i))
                  (loop (add1 i))
                  (escape #f)))))))
  
  (define vector-of-vector-of?
    (lambda (pred vov)
      (vector-of? (lambda (v) (vector-of? pred v)) vov)))
  
  (define vector-has?
    (lambda (pred v)
      (let/ec escape
        (let loop ([i 0])
          (if (= i (vector-length v)) #f
              (if (pred (vector-ref v i))
                  (escape #t)
                  (loop (add1 i))))))))
  
  (define vector-of-vector-has?
    (lambda (pred vov)
      (vector-has? (lambda (v) (vector-has? pred v)) vov)))
  
  
  ;;
  ;; Hash functions
  ;;
  
  (define hash-table-size
    (lambda (h)
      (let ([size 0])
        (hash-table-for-each h (lambda (_ _2) (set! size (add1 size))))
        size)))
  
  (define hash-table-empty?
    (lambda (h)
      (let/ec escape
        (hash-table-for-each h (lambda (k v) (escape #f)))
        #t)))
  
  (define/contract hash-table-has-key? (hash-table? any/c . -> . boolean?)
    (lambda (hash-table key)
      (if (hash-table-get hash-table key cst:thunk-false) #t #f)))
  
  ;; (hash-table key (list value)) key value -> (hash-table key (list value))
  (define/contract hash-table-prepend! (hash-table? any/c any/c . -> . any)
    (lambda (hash-table key value)
      (hash-table-put! hash-table key
                       (if (hash-table-has-key? hash-table key)
                           (cons value (hash-table-get hash-table key
                                                       (lambda () (error 'hash-table-prepend! "Could not prepend"))))
                           (list value)))))
  
  ;;
  ;; Function functions
  ;; 
  (define (curry f)
    (lambda (x) (f x)))
  
  ;;
  ;; Boolean functions
  ;;
  (define true?
    (lambda (x) (eq? x #t)))
  
  ;;
  ;; Random functions
  ;; 
  
  (define/contract numberify-symbol (symbol? integer? . -> . symbol?)
    (lambda (sym x)
      (string->symbol (string-append (symbol->string sym) ":" (number->string x)))))
  
  (define/contract numberify-list ((cons/c symbol? (listof any/c)) integer? . -> . (cons/c symbol? (listof any/c)))
    (lambda (syms x)
      (cons (numberify-symbol (car syms)) (cdr syms))))
  
  (define/contract pretty-error (symbol? any/c . -> . any)
    (lambda (sym v)
      (let ([out (open-output-string)])
        (pretty-print v out)
        (error sym (get-output-string out)))))
  
  (define andmap4-vector
    (lambda (f v0 v1 v2 v3)
      (let loop ([i 0])
        (if (= i (vector-length v0)) #t
            (and (f (vector-ref v0 i) (vector-ref v1 i) (vector-ref v2 i) (vector-ref v3 i))
                 (loop (add1 i)))))))
  
  (define andmap2-vector-interval
    (lambda (f v0 v1 lo high)
      (let loop ([i lo])
        (if (= i high) #t
            (and (f (vector-ref v0 i) (vector-ref v1 i))
                 (loop (add1 i)))))))
  
  (define andmap2-vector
    (lambda (f v0 v1)
      (andmap2-vector-interval f v0 v1 0 (vector-length v0))))
  
  ; return #t if the p(i) = # for all i in the half-open interval lo <= i < hi 
  (define andmap-vector-interval
    (lambda (f v0 lo high)
      (let loop ([i lo])
        (if (= i high) #t
            (and (f (vector-ref v0 i))
                 (loop (add1 i)))))))
  
  (define andmap-vector
    (lambda (f v0)
      (andmap-vector-interval f v0 0 (vector-length v0))))
  
  (define ormap4-vector
    (lambda (f v0 v1 v2 v3)
      (let loop ([i 0])
        (if (= i (vector-length v0)) #f
            (or (f (vector-ref v0 i) (vector-ref v1 i) (vector-ref v2 i) (vector-ref v3 i))
                (loop (add1 i)))))))
  
  
  ;; Classes
  
  (define counter%
    (class object%
      (init-field [start 0])
      (define count start)
      
      (define/public get
        (lambda () count))
      
      (define/public next!
        (lambda () (set! count (add1 count)) count))
      (super-new)))
  )
