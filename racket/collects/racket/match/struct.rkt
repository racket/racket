#lang racket/base
(require racket/match/match-expander
         (for-syntax racket/base
                     racket/struct-info
                     racket/list
                     "../private/struct-util.rkt"))

(define-for-syntax (extract-field-names orig-stx the-struct-info)
  (define accessors (list-ref the-struct-info 3))
  (define parent (list-ref the-struct-info 5))
  (define num-fields (length accessors))
  (define num-super-fields
    (if (identifier? parent)
        (length (cadddr (id->struct-info parent orig-stx)))
        0))
  (define num-own-fields (- num-fields num-super-fields))
  (define own-accessors (take accessors num-own-fields))
  (define struct-name (predicate->struct-name 'struct* orig-stx (list-ref the-struct-info 2)))
  (for/list ([accessor (in-list own-accessors)])
    ;; add1 for hyphen
    (string->symbol (substring (symbol->string (syntax-e accessor))
                               (add1 (string-length struct-name))))))

(define-for-syntax (id->struct-info id stx)
  (define compile-time-info (syntax-local-value id (lambda () #f)))
  (unless (struct-info? compile-time-info)
    (raise-syntax-error #f "identifier is not bound to a structure type" stx id))
  (extract-struct-info compile-time-info))

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
              [field->pattern (make-hash)])
         (unless (struct-info? v) (fail))
         (define the-struct-info (extract-struct-info v))

         ;; own-fields and all-accessors are in the reverse order
         (define all-accessors (list-ref the-struct-info 3))
         (define own-fields
           (if (struct-field-info? v)
               (struct-field-info-list v)
               (extract-field-names stx the-struct-info)))
         ;; Use hash instead of set so that we don't need to require racket/set
         (define field-set (for/hash ([field own-fields]) (values field #t)))

         ;; Check that all field names are valid
         (for ([an (in-list (syntax->list #'(field+pat ...)))])
           (syntax-case an ()
             [(field pat)
              (let ([fail-field (λ (msg) (raise-syntax-error 'struct* msg stx #'field))])
                (unless (identifier? #'field)
                  (fail-field "not an identifier for field name"))
                (define name (syntax-e #'field))
                (unless (hash-has-key? field-set name)
                  (fail-field "field name not associated with given structure type"))
                (when (hash-has-key? field->pattern name)
                  (fail-field "field name appears twice"))
                (hash-set! field->pattern name #'pat))]
             [_ (raise-syntax-error
                 'struct* "expected a field pattern of the form (<field-id> <pat>)"
                 stx an)]))

         ;; pats is in the reverse order
         (define pats
           (for/list ([field (in-sequences (in-list own-fields)
                                           (in-cycle '(#f)))]
                      [accessor (in-list all-accessors)]
                      #:when accessor)
             (hash-ref field->pattern field (syntax/loc stx _))))
         (quasisyntax/loc stx (struct struct-name #,(reverse pats))))])))

(provide struct* ==)

(define-match-expander
  ==
  (lambda (stx)
    (syntax-case stx ()
      [(_ val comp)
       #'(? (lambda (x) (comp val x)))]
      [(_ val) #'(? (lambda (x) (equal? val x)))])))
