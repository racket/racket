
(define-record placeholder (val))
(define-record-type (hash-placeholder create-hash-placeholder hash-placeholder?)
  (fields (mutable alist)))
(define-record-type (hasheq-placeholder create-hasheq-placeholder hasheq-placeholder?)
  (parent hash-placeholder)
  (fields))
(define-record-type (hasheqv-placeholder create-hasheqv-placeholder hasheqv-placeholder?)
  (parent hash-placeholder)
  (fields))
(define-record-type (hashalw-placeholder create-hashalw-placeholder hashalw-placeholder?)
  (parent hash-placeholder)
  (fields))

(define/who (placeholder-set! ph datum)
  (check who placeholder? ph)
  (set-placeholder-val! ph datum))

(define/who (placeholder-get ph)
  (check who placeholder? ph)
  (placeholder-val ph))

(define/who (make-hash-placeholder alst)
  (check who
         :test (and (list? alst)
                    (andmap pair? alst))
         :contract "(listof pair?)"
         alst)
  (create-hash-placeholder alst))

(define/who (make-hasheq-placeholder alst)
  (check who
         :test (and (list? alst)
                    (andmap pair? alst))
         :contract "(listof pair?)"
         alst)
  (create-hasheq-placeholder alst))

(define/who (make-hasheqv-placeholder alst)
  (check who
         :test (and (list? alst)
                    (andmap pair? alst))
         :contract "(listof pair?)"
         alst)
  (create-hasheqv-placeholder alst))

(define/who (make-hashalw-placeholder alst)
  (check who
         :test (and (list? alst)
                    (andmap pair? alst))
         :contract "(listof pair?)"
         alst)
  (create-hashalw-placeholder alst))

(define/who (make-reader-graph orig-v)
  (let ([ht (make-eq-hashtable)])
    (let loop ([v orig-v])
      (cond
       [(hashtable-ref ht v #f)
        => (lambda (p) p)]
       [(placeholder? v)
        (let ([next (placeholder-val v)])
          (when (eq? v next)
            (raise-arguments-error who
                                   "illegal placeholder cycle in value"
                                   "value" orig-v))
          (loop next))]
       [(pair? v)
        (let ([p (cons #f #f)])
          (hashtable-set! ht v p)
          (set-car! p (loop (car v)))
          (set-cdr! p (loop (cdr v)))
          (cond
           [(and (eq? (car p) (car v))
                 (eq? (cdr p) (cdr v)))
            ;; No change, so we don't have to make a copy:
            (hashtable-set! ht v v)
            v]
           [else p]))]
       [(vector? v)
        (let* ([len (vector-length v)]
               [p (#%make-vector len)])
          (hashtable-set! ht v p)
          (let vloop ([i 0] [diff? #f])
            (cond
             [(fx= i len)
              (cond
               [diff?
                (if (mutable-vector? v)
                    p
                    (begin
                      (#%$vector-set-immutable! p)
                      p))]
               [else
                (hashtable-set! ht v v)
                v])]
             [else
              (vector-set! p i (loop (vector-ref v i)))
              (vloop (fx1+ i) (or diff? (not (eq? (vector-ref v i) (vector-ref p i)))))])))]
       [(box? v)
        (let ([p (box #f)])
          (hashtable-set! ht v p)
          (set-box! p (loop (unbox v)))
          (cond
           [(eq? (unbox p) (unbox v))
            (hashtable-set! ht v v)
            v]
           [(mutable-box? v)
            p]
           [else
            ;; FIXME: need a way to change a box to immutable
            p]))]
       [(hash? v)
        (let* ([mutable? (mutable-hash? v)]
               [orig-p (if mutable?
                           (if (hash-weak? v)
                               (cond
                                [(hash-eq? v) (make-weak-hasheq)]
                                [(hash-eqv? v) (make-weak-hasheqv)]
                                [(hash-equal-always? v) (make-weak-hashalw)]
                                [else (make-weak-hash)])
                               (cond
                                [(hash-eq? v) (make-hasheq)]
                                [(hash-eqv? v) (make-hasheqv)]
                                [(hash-equal-always? v) (make-hashalw)]
                                [else (make-hash)]))
                           (cond
                            [(zero? (hash-count v)) v]
                            [else
                             (cond
                              [(hash-eq? v) (make-intmap-shell 'eq)]
                              [(hash-eqv? v) (make-intmap-shell 'eqv)]
                              [(hash-equal-always? v) (make-intmap-shell 'equal-always)]
                              [else (make-intmap-shell 'equal)])]))]
               [p (if mutable?
                      orig-p
                      (cond
                       [(hash-eq? v) empty-hasheq]
                       [(hash-eqv? v) empty-hasheqv]
                       [(hash-equal-always? v) empty-hashalw]
                       [else empty-hash]))])
          (hashtable-set! ht v orig-p)
          (let hloop ([p p] [i (hash-iterate-first v)] [diff? #f])
            (cond
             [(not i)
              (cond
               [diff?
                (cond
                 [mutable? orig-p]
                 [else
                  (unless (zero? (hash-count p))
                    (intmap-shell-sync! orig-p p))
                  orig-p])]
               [else
                (hashtable-set! ht v v)
                v])]
             [else
              (let-values ([(key val) (hash-iterate-key+value v i)])
                (let ([new-key (loop key)]
                      [new-val (loop val)])
                  (hloop (if mutable?
                             (hash-set! orig-p new-key new-val)
                             (hash-set p new-key new-val))
                         (hash-iterate-next v i)
                         (or diff? (not (and (eq? key new-key) (eq? val new-val)))))))])))]
       [(hash-placeholder? v)
        (let ([alst (hash-placeholder-alist v)])
          (cond
           [(null? alst)
            (cond
             [(hasheq-placeholder? v) empty-hasheq]
             [(hasheqv-placeholder? v) empty-hasheqv]
             [(hashalw-placeholder? v) empty-hashalw]
             [else empty-hash])]
           [else
            (let-values ([(orig-p p)
                          (cond
                           [(hasheq-placeholder? v) (values (make-intmap-shell 'eq) empty-hasheq)]
                           [(hasheqv-placeholder? v) (values (make-intmap-shell 'eqv) empty-hasheqv)]
                           [(hashalw-placeholder? v) (values (make-intmap-shell 'equal-always) empty-hashalw)]
                           [else (values (make-intmap-shell 'equal) empty-hash)])])
              (hashtable-set! ht v orig-p)
              (let hloop ([p p] [alst alst])
                (cond
                 [(null? alst)
                  (intmap-shell-sync! orig-p p)
                  orig-p]
                 [else
                  (hloop (hash-set p (loop (caar alst)) (loop (cdar alst)))
                         (cdr alst))])))]))]
       [(prefab-struct-key v)
        => (lambda (key)
             (let ([args (cdr (vector->list (struct->vector v)))])
               (let ([p (apply make-prefab-struct key args)])
                 (hashtable-set! ht v p)
                 (let aloop ([args args] [i 0] [diff? #f])
                   (cond
                    [(null? args)
                     (cond
                      [diff? p]
                      [else
                       (hashtable-set! ht v v)
                       v])]
                    [else
                     (let* ([a (car args)]
                            [new-a (loop a)])
                       (unless (eq? a new-a)
                         (unsafe-struct-set! p i new-a))
                       (aloop (cdr args) (fx1+ i) (or diff? (not (eq? a new-a)))))])))))]
       [else v]))))
