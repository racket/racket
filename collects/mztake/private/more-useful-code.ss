(module more-useful-code mzscheme
  (require (lib "list.ss")
           (lib "pretty.ss")
	   (lib "etc.ss"))
  
  (provide assert
           cons-to-end
	   assoc-get
	   debug
           make-to-string
	   make-debug
	   to-string
	   member-eq?	   
	   string->char
	   last
	   member-str?
	   quicksort-vector!
	   struct->list/deep
           make-for-each
           begin0/rtn
           with-handlers/finally
           pretty-print-syntax
           with-semaphore

	   make-hash
           hash?
	   hash-get
	   hash-put!
	   hash-remove!
	   hash-map
	   hash-for-each
           hash-size/slow
	   hash-mem?
           hash-fold
           hash-filter!
           hash-keys
           hash-values
           hash-pairs
           hash-add-all!
           hash-get-or-define!

	   (all-from (lib "list.ss"))
	   (all-from (lib "etc.ss")))
  
  (define-struct (exn:assert exn) ())

  (define-syntax (assert stx)
    (syntax-case stx ()
      [(src-assert bool) #'(src-assert bool "")]
      [(src-assert bool msg ...)
       (with-syntax ([src-text (datum->syntax-object
                                (syntax src-assert)
                                (format "~a:~a:~a: assertion failed: "
                                        (syntax-source (syntax bool))
                                        (syntax-line (syntax bool))
                                        (syntax-column (syntax bool))))])
         #'(unless bool
             (raise (make-exn:assert (apply string-append
                                            (cons src-text
                                                  (map (lambda (item) 
                                                         (string-append (to-string item) " "))
                                                       (list msg ...))))
                                     (current-continuation-marks)))))]))

  (define-syntax (begin0/rtn stx)
    (syntax-case stx ()
      [(begin0/rtn body bodies ...)
       (with-syntax ([rtn (datum->syntax-object (syntax begin0/rtn) 'rtn)])
         (syntax (let ([rtn body]) bodies ... rtn)))]))

  (define-syntax with-handlers/finally
    (syntax-rules ()
      [(_ (handler ...) body finally)
       (let ([finally-fn (lambda () finally)])
         (begin0
             (with-handlers
                 (handler ...
                          [(lambda (exn) #t)
                           (lambda (exn) (finally-fn) (raise exn))])
               body)
           (finally-fn)))]))

  (define (make-for-each . iterator-fns)
    (lambda (obj fn)
      (cond ((list? obj) (for-each fn obj))
            ((vector? obj) (let loop ((x 0))
                             (if (< x (vector-length obj))
                                 (begin (fn (vector-ref obj x)) (loop (+ x 1))))))
            ((hash-table? obj) (hash-for-each obj (lambda (key val) (fn key))))
            (true (let loop ((cur iterator-fns))
                    (if (empty? cur) 
                        (if (struct? obj) (error "for-each: no iterator for struct `" (struct-name obj) "' value:" obj)
                            (error "for-each: no iterator for value:" obj))
                        (or ((first cur) obj fn)
                            (loop (rest cur)))))))))

  
  (define (quicksort-vector! v less-than)
    (let ([count (vector-length v)])
      (let loop ([min 0][max count])
	(if (< min (sub1 max))
	    (let ([pval (vector-ref v min)])
	      (let pivot-loop ([pivot min]
			       [pos (add1 min)])
		(if (< pos max)
		    (let ([cval (vector-ref v pos)])
		      (if (less-than cval pval)
			  (begin
			    (vector-set! v pos (vector-ref v pivot))
			    (vector-set! v pivot cval)
			    (pivot-loop (add1 pivot) (add1 pos)))
			  (pivot-loop pivot (add1 pos))))
		    (if (= min pivot)
			(loop (add1 pivot) max)
			(begin
			  (loop min pivot)
			  (loop pivot max)))))))))
    v)
  
  
  
  (define (member-str? s ls)
    (cond
     ((empty? ls) false)
     ((string=? s (first ls)) true)
     (else (member-str? s (rest ls)))))
  
  (define (last ls)
    (cond
     ((empty? ls) (error "took a last but it was emptry"))
     ((empty? (rest ls)) (first ls))
     (else (last (rest ls)))))

  (define (string->char s)
    (first (string->list s)))
  
  (define (member-eq? x ls)
    (not (empty? (filter (lambda (y) (eq? x y)) ls))))
  
  (define (to-string arg . extra-printers)
    (let ([on-stack-ids (make-hash)]
          [used-ids (make-hash)]
          [free-id 0])
      (let loop ((arg arg))
        (if (hash-mem? on-stack-ids arg)
            (begin
              (hash-put! used-ids arg true)
              (format "#~a#" (hash-get on-stack-ids arg)))
            (let ([my-id free-id])
              (hash-put! on-stack-ids arg my-id)
              (set! free-id (add1 free-id))
              (let ([result
                     (or
                      (let printer-loop ([printers extra-printers])
                        (if (empty? printers) 
                            false
                            (or (if (procedure-arity-includes? (car printers) 2)
                                    ((car printers) arg (lambda (arg) (apply to-string (cons arg extra-printers))))
                                    ((car printers) arg))
                                (printer-loop (cdr printers)))))
                      (cond
                       [(not arg) "#f"]
                       [(void? arg) "#<void>"]
                       [(eq? arg #t) "#t"]
                       [(char? arg) (list->string (list arg))]
                       [(string? arg) (format "\"~a\"" arg)]
                       [(symbol? arg) (symbol->string arg)]
                       [(number? arg) (number->string arg)]
                       [(vector? arg) (string-append "#" (loop (vector->list arg)))]
                       [(box? arg) (string-append "#&" (loop (unbox arg)))]
                       [(empty? arg) "empty"]
                       [(list? arg) 
                        (apply
                         string-append
                         `("(" ,@(cons (loop (first arg))
                                       (map (lambda (item) (string-append " " (loop item))) (rest arg)))
                           ")"))]
                       [(cons? arg) (format "(~a . ~a)"
                                            (loop (first arg))
                                            (loop (rest arg)))]
       
                       [(hash-table? arg)
                        (apply
                         string-append
                         `("[hash:"
                           ,@(map (lambda (item) (string-append " " (loop item))) (hash-pairs arg))
                           "]"))]
       
                       [(syntax? arg)
                        (format "[syntax: ~a:~a]" (syntax-line arg) (syntax-column arg))]

                       [(struct? arg)
                        (let ([as-list (vector->list (struct->vector arg))])
                          (apply
                           string-append
                           `("[" ,@(cons (loop (first as-list))
                                         (map (lambda (item) (string-append " " (loop item)))
                                              (rest as-list))) "]")))]

                       [else
                        (format "~a" arg)]))])
                (hash-remove! on-stack-ids arg)
                (if (hash-mem? used-ids arg)
                    (format "#~a=~a" my-id result)
                    result)))))))
  
  ;; make-debug: usage example: (define debug-f (make-debug (make-to-string `([,is-type? ,type-to-string]))))
  ;; The printers have to take two arguments: the item to converts and the to-string function for subitems
  (define (make-debug to-string-fn)
    (lambda args
      (for-each (lambda (x) 
                  (display (if (string? x) x (to-string-fn x)))
                  (display " "))
                args)
      (newline)))

  (define debug (make-debug to-string))
  
  (define (make-to-string predicate-printer-pairs)
    (let ([printers (map (lambda (pair) (lambda (arg printer) 
                                     (cond [(not ((first pair) arg)) false]
                                           [(procedure-arity-includes? (second pair) 2)
                                            ((second pair) arg printer)]
                                           [else ((second pair) arg)])))
                         predicate-printer-pairs)])
      (case-lambda 
        [(arg) (apply to-string arg printers)]
        [(arg extra-printers) (apply to-string (append (list arg) printers extra-printers))])))
  
  (define (assoc-get label ls)
    (cond
     ((empty? ls) (error (string-append "failed to find " (to-string label))))
     ((eq? label (first (first ls)))
      (first ls))
     (else (assoc-get label (rest ls)))))
  
  (define (cons-to-end a ls)
    (cond
     ((empty? ls) (cons a ls))
     (else (cons (first ls)
		 (cons-to-end a (rest ls))))))
  
  (define (struct->list/deep item)
    (cond [(struct? item) (map struct->list/deep (vector->list (struct->vector item)))]
	  [(list? item)   (map struct->list/deep item)]
          [(vector? item) (list->vector (map struct->list/deep (vector->list item)))]
	  [else item]))

  (define (struct-name s) (vector-ref (struct->vector s) 0))

  (define (pretty-print-syntax width stx)
    (pretty-print-columns width)
    (pretty-print (syntax-object->datum stx)))

  (define (with-semaphore sem proc)
    (semaphore-wait sem)
    (let ([result (proc)])
      (semaphore-post sem)
      result))

  (define make-hash make-hash-table)
  (define hash? hash-table?)
  (define hash-get hash-table-get)
  (define hash-put! hash-table-put!)
  (define hash-remove! hash-table-remove!)
  (define hash-map hash-table-map)
  (define hash-for-each hash-table-for-each)
  (define (hash-empty? hash)(let/ec k (hash-for-each hash (lambda (k v) (k false))) true))
  (define (hash-size/slow hash) (hash-fold hash 0 (lambda (key val acc) (+ acc 1))))
  (define (hash-mem? hash item) (let/ec k (begin (hash-get hash item (lambda () (k false))) true)))
  (define (hash-fold hash init fn)
    (hash-for-each hash (lambda (key val) (set! init (fn key val init)))) init)
  (define (hash-filter! hash predicate)
    (hash-for-each 
     hash (lambda (key val) (if (not (predicate key val))
                           (hash-remove! hash key)))))
  (define (hash-keys hash) 
    (hash-fold hash empty (lambda (key val acc) (cons key acc))))
  (define (hash-values hash) 
    (hash-fold hash empty (lambda (key val acc) (cons val acc))))
  (define (hash-pairs hash)
    (hash-fold hash empty (lambda (key val acc) (cons (list key val) acc))))
  (define (hash-add-all! to-hash from-hash) ;; // memcpy-style argument order
    (hash-for-each from-hash
                   (lambda (key val) (hash-put! to-hash key val))))

  (define (hash-get-or-define! hash key val-fn)
    (if (not (hash-mem? hash key)) 
        (begin (let ((v (val-fn)))
                 (hash-put! hash key v)
                 v))
        (hash-get hash key))))
