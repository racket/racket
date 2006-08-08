(module pretty-helper mzscheme
  (require (lib "class.ss")
           "partition.ss")
  (provide (all-defined))
  
  ;; Fixme: null object still confusable.
  
  ;; Problem: If stx1 and stx2 are two distinguishable syntax objects, it
  ;; still may be the case that (syntax-e stx1) and (syntax-e stx2) are 
  ;; indistinguishable.
  
  ;; Solution: Rather than map stx to (syntax-e stx), in the cases where
  ;; (syntax-e stx) is confusable, map it to a different, unique, value.
  ;;   - stx is identifier : map it to an uninterned symbol w/ same rep
  ;;     (Symbols are useful: see pretty-print's style table)
  ;;   - else : map it to a syntax-dummy object
  
  (define-struct syntax-dummy (val))
  
  ;; syntax->datum/tables : stx [partition% num boolean]
  ;;                        -> (values s-expr hashtable hashtable)
  ;; When partition is not false, tracks the partititions that subterms belong to
  ;; When limit is a number, restarts processing with numbering? set to true
  ;; When numbering? is true, suffixes identifiers with partition numbers.
  ;; 
  ;; Returns three values:
  ;;   - an S-expression
  ;;   - a hashtable mapping S-expressions to syntax objects
  ;;   - a hashtable mapping syntax objects to S-expressions
  ;; Syntax objects which are eq? will map to same flat values
  (define syntax->datum/tables
    (case-lambda
      [(stx) (table stx #f #f #f)]
      [(stx partition limit numbering?) (table stx partition limit numbering?)]))
  
  ;; table : syntax partition%-or-#f num-or-#f -> (values s-expr hashtable hashtable)
  (define (table stx partition limit numbering?)
    (define (make-identifier-proxy id)
      (let ([n (send partition get-partition id)])
        (cond [(or (zero? n) (not numbering?))
               (string->uninterned-symbol (symbol->string (syntax-e id)))]
              [else
               (string->uninterned-symbol
                (format "~a:~a" (syntax-e id) n))])))
    (let/ec escape
      (let ([flat=>stx (make-hash-table)]
            [stx=>flat (make-hash-table)])
        (values (let loop ([obj stx])
                  (cond
                    [(hash-table-get stx=>flat obj (lambda _ #f))
                     => (lambda (datum) datum)]
                    [(and partition (identifier? obj))
                     (let ([lp-datum (make-identifier-proxy obj)])
                       (when (and limit (> (send partition count) limit))
                         (call-with-values (lambda () (table stx partition #f #t))
                                           escape))
                       (hash-table-put! flat=>stx lp-datum obj)
                       (hash-table-put! stx=>flat obj lp-datum)
                       lp-datum)]
                    [(syntax? obj)
                     (void (send partition get-partition obj))
                     (let ([lp-datum (loop (syntax-e obj))])
                       (hash-table-put! flat=>stx lp-datum obj)
                       (hash-table-put! stx=>flat obj lp-datum)
                       lp-datum)]
                    [(pair? obj) 
                     (cons (loop (car obj))
                           (loop (cdr obj)))]
                    [(vector? obj) 
                     (list->vector (map loop (vector->list obj)))]
                    [(symbol? obj)
                     #;(make-syntax-dummy obj)
                       (string->uninterned-symbol (symbol->string obj))]
                    [(number? obj)
                     (make-syntax-dummy obj)]
                    #;[(null? obj)
                       (make-syntax-dummy obj)]
                      [else obj]))
                flat=>stx
                stx=>flat))))
  )
