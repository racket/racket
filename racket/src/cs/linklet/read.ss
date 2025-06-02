
(define (read-linklet-bundle-hash in cross-machine-type)
  (performance-region
   'read-bundle
   (let* ([len (integer-bytes->integer (read-bytes 4 in) #f #f)]
          [bstr (read-bytes len in)])
     (adjust-linklet-bundle-laziness-and-literals
      (fasl-read (open-bytevector-input-port bstr))
      cross-machine-type))))

(define read-on-demand-source
  (make-parameter #f
                  (lambda (v)
                    (unless (or (eq? v #t) (eq? v #f) (and (path? v)
                                                           (complete-path? v)))
                      (raise-argument-error 'read-on-demand-source
                                            "(or/c #f #t (and/c path? complete-path?))"
                                            v))
                    v)
		  'read-on-demand-source))

(define (adjust-linklet-bundle-laziness-and-literals ls cross-machine-type)
  (let loop ([ls ls] [ht (hasheq)])
    (cond
     [(null? ls) ht]
     [else
      (let ([key (car ls)]
            [val (cadr ls)])
        (loop (cddr ls)
              (hash-set ht
                        key
                        (cond
                          [(linklet-vector? val)
                           (adjust-linklet-cross-preparation
                            (adjust-linklet-laziness
                            (decode-linklet-literals
                             (vector->linklet val)))
                            cross-machine-type)]
                          [else val]))))])))

(define (adjust-linklet-laziness linklet)
  (set-linklet-code linklet
                    (linklet-code linklet)
                    (cond
                     [(not (eq? root-inspector (|#%app| current-code-inspector)))
                      ;; Originally, the idea was that bytecode can be loaded in
                      ;; a non-original code inspector as long as it doesn't refer
                      ;; to unsafe operation. But increasing use of compilation to
                      ;; unsafe operations, not to mention compilation to machine
                      ;; code, means that all "bytecode" is unsafe:
                      'faslable-unsafe]
                     [(|#%app| read-on-demand-source)
                      ;; Remember that the linklet can be lazier:
                      'faslable]
                     [else
                      'faslable-strict])))

(define (decode-linklet-literals linklet)
  (let ([literals (linklet-literals linklet)])
    (cond
      [(vector? literals) linklet]
      [else
       (set-linklet-literals linklet
                             (unfasl-literals/lazy literals))])))

(define (adjust-linklet-cross-preparation l cross-machine-type)
  (if cross-machine-type
      (set-linklet-preparation l (cons 'cross l))
      l))
