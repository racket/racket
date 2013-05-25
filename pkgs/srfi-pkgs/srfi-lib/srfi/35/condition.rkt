(module condition mzscheme
  
  (require srfi/1/list
           srfi/9)
  
  (provide make-condition-type
           condition-type?
           make-condition
           condition?
           condition-has-type?
           condition-ref
           make-compound-condition
           extract-condition
           define-condition-type
           condition
           &condition
           &message
           &serious
           &error)
  
  (define-record-type :condition-type
                      (really-make-condition-type name supertype fields all-fields)
                      condition-type?
                      (name condition-type-name)
                      (supertype condition-type-supertype)
                      (fields condition-type-fields)
                      (all-fields condition-type-all-fields))
  
  (define (make-condition-type name supertype fields)
    (if (not (symbol? name))
        (error "make-condition-type: name is not a symbol"
               name))
    (if (not (condition-type? supertype))
        (error "make-condition-type: supertype is not a condition type"
               supertype))
    (if (not
         (null? (lset-intersection eq?
                                   (condition-type-all-fields supertype)
                                   fields)))
        (error "duplicate field name" ))
    (really-make-condition-type name
                                supertype
                                fields
                                (append (condition-type-all-fields supertype)
                                        fields)))
  
  (define-syntax define-condition-type
    (syntax-rules ()
      ((define-condition-type ?name ?supertype ?predicate
                              (?field1 ?accessor1) ...)
       (begin
         (define ?name
           (make-condition-type '?name
                                ?supertype
                                '(?field1 ...)))
         (define (?predicate thing)
           (and (condition? thing)
                (condition-has-type? thing ?name)))
         (define (?accessor1 condition)
           (condition-ref (extract-condition condition ?name)
                          '?field1))
         ...))))
  
  (define (condition-subtype? subtype supertype)
    (let recur ((subtype subtype))
      (cond ((not subtype) #f)
            ((eq? subtype supertype) #t)
            (else
             (recur (condition-type-supertype subtype))))))
  
  (define (condition-type-field-supertype condition-type field)
    (let loop ((condition-type condition-type))
      (cond ((not condition-type) #f)
            ((memq field (condition-type-fields condition-type))
             condition-type)
            (else
             (loop (condition-type-supertype condition-type))))))
  
  ; The type-field-alist is of the form
  ; ((<type> (<field-name> . <value>) ...) ...)
  (define-record-type :condition
                      (really-make-condition type-field-alist)
                      condition?
                      (type-field-alist condition-type-field-alist))
  
  (define (make-condition type . field-plist)
    (let ((alist (let label ((plist field-plist))
                   (if (null? plist)
                       '()
                       (cons (cons (car plist)
                                   (cadr plist))
                             (label (cddr plist)))))))
      (if (not (lset= eq?
                      (condition-type-all-fields type)
                      (map car alist)))
          (error "condition fields don't match condition type"))
      (really-make-condition (list (cons type alist)))))
  
  (define (condition-has-type? condition type)
    (any (lambda (has-type)
           (condition-subtype? has-type type))
         (condition-types condition)))
  
  (define (condition-ref condition field)
    (type-field-alist-ref (condition-type-field-alist condition)
                          field))
  
  (define (type-field-alist-ref type-field-alist field)
    (let loop ((type-field-alist type-field-alist))
      (cond ((null? type-field-alist)
             (error "type-field-alist-ref: field not found"
                    type-field-alist field))
            ((assq field (cdr (car type-field-alist)))
             => cdr)
            (else
             (loop (cdr type-field-alist))))))
  
  (define (make-compound-condition condition-1 . conditions)
    (really-make-condition
     (apply append (map condition-type-field-alist
                        (cons condition-1 conditions)))))
  
  (define (extract-condition condition type)
    (let ((entry (find (lambda (entry)
                         (condition-subtype? (car entry) type))
                       (condition-type-field-alist condition))))
      (if (not entry)
          (error "extract-condition: invalid condition type"
                 condition type))
      (really-make-condition
       (list (cons type
                   (map (lambda (field)
                          (assq field (cdr entry)))
                        (condition-type-all-fields type)))))))
  
  (define-syntax condition
    (syntax-rules ()
      ((condition (?type1 (?field1 ?value1) ...) ...)
       (type-field-alist->condition
        (list
         (cons ?type1
               (list (cons '?field1 ?value1) ...))
         ...)))))
  
  (define (type-field-alist->condition type-field-alist)
    (really-make-condition
     (map (lambda (entry)
            (cons (car entry)
                  (map (lambda (field)
                         (or (assq field (cdr entry))
                             (cons field
                                   (type-field-alist-ref type-field-alist field))))
                       (condition-type-all-fields (car entry)))))
          type-field-alist)))
  
  (define (condition-types condition)
    (map car (condition-type-field-alist condition)))
  
  (define (check-condition-type-field-alist the-type-field-alist)
    (let loop ((type-field-alist the-type-field-alist))
      (if (not (null? type-field-alist))
          (let* ((entry (car type-field-alist))
                 (type (car entry))
                 (field-alist (cdr entry))
                 (fields (map car field-alist))
                 (all-fields (condition-type-all-fields type)))
            (for-each (lambda (missing-field)
                        (let ((supertype
                               (condition-type-field-supertype type missing-field)))
                          (if (not
                               (any (lambda (entry)
                                      (let ((type (car entry)))
                                        (condition-subtype? type supertype)))
                                    the-type-field-alist))
                              (error "missing field in condition construction"
                                     type
                                     missing-field))))
                      (lset-difference eq? all-fields fields))
            (loop (cdr type-field-alist))))))
  
  (define &condition (really-make-condition-type '&condition
                                                 #f
                                                 '()
                                                 '()))
  
  (define-condition-type &message &condition
                         message-condition?
                         (message condition-message))
  
  (define-condition-type &serious &condition
                         serious-condition?)
  
  (define-condition-type &error &serious
                         error?))
