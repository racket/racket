
(define (linklet-virtual-machine-bytes)
  (string->utf8 (symbol->string (machine-type))))

(define (linklet-cross-machine-type l)
  (let ([prep (linklet-preparation l)])
    (and (pair? prep) (cdr prep))))

(define (write-linklet-bundle-hash ht dest-o)
  (let-values ([(ls cross-machine) (encode-linklet-literals ht)])
    (let ([bstr (if cross-machine
                    (let-values ([(bstr literals) (cross-fasl-to-string cross-machine ls #f 'data)])
                      (unless (equal? literals '#())
                        (#%error 'write-linklet "cross fasl produced additional literals"))
                      bstr)
                    (let-values ([(o get) (open-bytevector-output-port)])
                      (fasl-write* ls o)
                      (get)))])
      (write-bytes (integer->integer-bytes (bytes-length bstr) 4 #f #f) dest-o)
      (write-bytes bstr dest-o))))

(define (encode-linklet-literals orig-ht)
  (let loop ([i (hash-iterate-first orig-ht)] [accum '()] [cross-machine #f])
    (cond
      [(not i) (values accum cross-machine)]
      [else
       (let-values ([(key v) (hash-iterate-key+value orig-ht i)])
         (when (linklet? v) (check-fasl-preparation v))
         (let ([new-v (cond
                        [(linklet? v)
                         (adjust-cross-preparation
                          (let ([literals (linklet-literals v)])
                            (cond
                              [(and (#%vector? literals)
                                    (fx= 0 (#%vector-length literals)))
                               v]
                              [else
                               (set-linklet-literals
                                v
                                (fasl-literals (extract-literals literals) uninterned-symbol?))])))]
                        [else v])])
           (when (linklet? new-v)
             (linklet-pack-exports-info! new-v))
           (let ([accum (cons* key (to-portable new-v) accum)])
             (loop (hash-iterate-next orig-ht i)
                   accum
                   (or cross-machine
                       (and (linklet? v)
                            (let ([prep (linklet-preparation v)])
                              (and (pair? prep) (cdr prep)))))))))])))

;; Before fasl conversion, change 'cross or 'faslable-unsafe to 'faslable
(define (adjust-cross-preparation l)
  (let ([p (linklet-preparation l)])
    (if (or (pair? p) (eq? p 'faslable-unsafe))
        (set-linklet-preparation l 'faslable)
        l)))

(define (check-fasl-preparation l)
  (case (linklet-preparation l)
    [(callable lazy)
     (raise-arguments-error 'write "linklet is not serializable")]))

(define (to-portable v)
  (cond
    [(linklet? v)
     ;; Linklet records have a different shape across platforms (at least
     ;; for 32-bit versus 64-bit]. Convert to a shape that can be unfasled
     ;; on any platform. It might also work to omit RTDs in the fasled
     ;; form and rely on matching field counts, but this approach is a
     ;; little more direct.
     (linklet->vector v)]
    [else v]))
