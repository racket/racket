(module registry mzscheme

  (provide register
           make-entry
           lookup-entry
           lookup-scheme?
           lookup-getter
           lookup-setter
           lookup-labels
           lookup-supers
           lookup-copier
           lookup-predicate)

  (require (prefix s1: srfi/1))

  (define reg '())

  (define (make-entry name
                      is-scheme?
                      predicate
                      supers
                      labels
                      pos-labels
                      fields
                      copier)
    (vector name
            is-scheme?
            predicate
            supers
            labels
            pos-labels
            fields
            copier))

  (define (entry.name entry)       (vector-ref entry 0))
  (define (entry.is-scheme? entry) (vector-ref entry 1))
  (define (entry.predicate entry)  (vector-ref entry 2))
  (define (entry.supers entry)     (vector-ref entry 3))
  (define (entry.labels entry)     (vector-ref entry 4))
  (define (entry.pos-labels entry) (vector-ref entry 5))
  (define (entry.fields entry)     (vector-ref entry 6))
  (define (entry.copier entry)     (vector-ref entry 7))

  (define (register name entry)
    (set! reg
          (let loop ([reg reg])
            (cond
             [(null? reg) 
              (list (cons name entry))]
             [(free-identifier=? name (caar reg))
              (cons (cons name entry)
                    (cdr reg))]
             [else (cons (car reg)
                         (loop (cdr reg)))]))))

  (define (lookup-entry name)
    (s1:assoc name reg free-identifier=?))

  (define (lookup-getter name label)
    (cond ((s1:assoc label
                     (entry.fields (cdr (lookup-entry name)))
                     free-identifier=?)
           => cadr)
          (else #f)))

  (define (lookup-setter name label)
    (cond ((s1:assoc label
                     (entry.fields (cdr (lookup-entry name)))
                     free-identifier=?)
           => caddr)
          (else #f)))

  (define (lookup-scheme? name)   (entry.is-scheme? (cdr (lookup-entry name))))
  (define (lookup-labels name)    (entry.labels     (cdr (lookup-entry name))))
  (define (lookup-supers name)    (entry.supers     (cdr (lookup-entry name))))
  (define (lookup-copier name)    (entry.copier     (cdr (lookup-entry name))))
  (define (lookup-predicate name) (entry.predicate  (cdr (lookup-entry name))))

  ) ; registry
