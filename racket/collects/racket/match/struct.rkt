#lang racket/base
(require racket/match/match-expander
         (for-syntax racket/base
                     racket/struct-info
                     syntax/id-table
                     racket/list))

(define-match-expander
  struct*
  (lambda (stx)
    (syntax-case stx ()
      [(_ struct-name (field+pat ...))
       (let* ([fail (lambda ()
                      (raise-syntax-error
                       'struct* "not a structure definition"
                       stx #'struct-name))]
              [v (if (identifier? #'struct-name)
                     (syntax-local-value #'struct-name fail)
                     (fail))]
              [field-acc->pattern (make-free-id-table)])
         (unless (struct-info? v) (fail))
         ; Check each pattern and capture the field-accessor name
         (for-each (lambda (an)
                     (syntax-case an ()
                       [(field pat)
                        (unless (identifier? #'field)
                          (raise-syntax-error 
                           'struct* "not an identifier for field name" 
                           stx #'field))
                        (let ([field-acc
                               (datum->syntax #'field
                                              (string->symbol
                                               (format "~a-~a"
                                                       (syntax-e #'struct-name)
                                                       (syntax-e #'field)))
                                              #'field)])
                          (when (free-id-table-ref field-acc->pattern field-acc #f)
                            (raise-syntax-error 'struct* "Field name appears twice" stx #'field)) 
                          (free-id-table-set! field-acc->pattern field-acc #'pat))]
                       [_
                        (raise-syntax-error
                         'struct* "expected a field pattern of the form (<field-id> <pat>)"
                         stx an)]))
                   (syntax->list #'(field+pat ...)))
         (let* (; Get the structure info
                [acc (fourth (extract-struct-info v))]
                ;; the accessors come in reverse order
                [acc (reverse acc)]
                ;; remove the first element, if it's #f
                [acc (cond [(empty? acc) acc]
                           [(not (first acc)) (rest acc)]
                           [else acc])]
                ; Order the patterns in the order of the accessors
                [pats-in-order
                 (for/list ([field-acc (in-list acc)])
                   (begin0
                     (free-id-table-ref
                      field-acc->pattern field-acc
                      (syntax/loc stx _))
                     ; Use up pattern
                     (free-id-table-remove! field-acc->pattern field-acc)))])
           ; Check that all patterns were used
           (free-id-table-for-each
            field-acc->pattern
            (lambda (field-acc pat)
              (when pat
                (raise-syntax-error 'struct* "field name not associated with given structure type"
                                    stx field-acc))))
           (quasisyntax/loc stx
             (struct struct-name #,pats-in-order))))])))

(provide struct* ==)

(define-match-expander
  ==
  (lambda (stx)
    (syntax-case stx ()
      [(_ val comp)
       #'(? (lambda (x) (comp val x)))]
      [(_ val) #'(? (lambda (x) (equal? val x)))])))
