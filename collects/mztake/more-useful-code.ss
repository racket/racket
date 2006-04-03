(module more-useful-code mzscheme
  (require (lib "list.ss")
	   (lib "etc.ss"))
  
  (provide make-hash
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
           hash-get-or-define!)
  
  
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
