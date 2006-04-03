(module base-gm mzscheme
  (require (lib "list.ss")
	   (lib  "etc.ss"))
  
  (provide make-hash
	   hash-get
	   hash-put!
	   hash-remove!
	   hash-map
	   hash-for-each
	   hash-mem?)
  
  (define make-hash make-hash-table)
  (define hash-get hash-table-get)
  (define hash-put! hash-table-put!)
  (define hash-remove! hash-table-remove!)
  (define hash-map hash-table-map)
  (define hash-for-each hash-table-for-each)
  (define (hash-mem? hash item) (hash-get hash item (lambda () false))))