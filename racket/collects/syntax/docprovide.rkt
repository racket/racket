#lang racket/base
(require (for-syntax racket/base
                     "private/doctable.rkt"))

(define-syntaxes (provide-and-document provide-and-document/wrap)
  (let ()
    (define (add-prefix prefix rows)
      (map (lambda (row)
             (cons (car row)
                   (map
                    (lambda (x) 
                      (cons prefix x))
                    (cdr row))))
           rows))
    
    (define (remove-prefixes rows)
      (map (lambda (row)
             (cons (car row)
                   (map (lambda (proc)
                          (let ([rest (cdr proc)])
                            (if (pair? (car rest))
                                (cons (cadar rest)
                                      (cdr rest))
                                rest)))
                        (cdr row))))
           rows))
    
    (define (remove-docs rows exceptions)
      (map (lambda (row)
             (cons (car row)
                   (let loop ([l (cdr row)])
                     (cond
                       [(null? l) null]
                       [(memq (let ([i (cadar l)])
                                (if (symbol? i)
                                    i
                                    (cadr i)))
                              exceptions)
                        (loop (cdr l))]
                       [else (cons (car l) (loop (cdr l)))]))))
           rows))
    
    (define (go stx label wrap rows)
      (unless (identifier? label)
        (raise-syntax-error
         'provide-and-document
         "label is not an identifier"
         stx
         label))
      (when wrap
        (unless (identifier? wrap)
          (raise-syntax-error
           'provide-and-document
           "wrap is not an identifier"
           stx
           wrap)))
      (let ([rows (map (lambda (row)
                         ;; Helper:
                         (define (get-existing tag path label exceptions)
                           (unless (identifier? tag)
                             (raise-syntax-error
                              'provide-and-document
                              "prefix tag is not an identifier"
                              stx
                              tag))
                           (unless (identifier? label)
                             (raise-syntax-error
                              'provide-and-document
                              "label is not an identifier"
                              stx
                              label))
                           (for-each
                            (lambda (except)
                              (unless (identifier? except)
                                (raise-syntax-error
                                 'provide-and-document
                                 "exclusion is not an identifier"
                                 stx
                                 except)))
                            exceptions)
                           (let ([mod ((current-module-name-resolver) path #f #f #t)])
                             ;; Execute syntax part at top-level:
                             (dynamic-require mod (void))
                             ;; Extract documentation via top-level:
                             (let ([docs ((dynamic-require-for-syntax 
                                           'syntax/private/doctable
                                           'lookup-documentation)
                                          mod
                                          (syntax-e label))])
                               (unless docs
                                 (raise-syntax-error
                                  'provide-and-document
                                  "could not find provided documentation"
                                  stx
                                  row))
                               (remove-docs (add-prefix tag docs)
                                            (map syntax-e exceptions)))))
                         ;; Parse row:
                         (syntax-case row ()
                           [(header proc ...)
                            (string? (syntax-e (syntax header)))
                            (begin
                              ;; check form:
                              (map (lambda (proc)
                                     (syntax-case proc ()
                                       [(name type-sexpr doc-string ...)
                                        (and (or (identifier? (syntax name))
                                                 (let ([l (syntax->list (syntax name))])
                                                   (and l
                                                        (= (length l) 2)
                                                        (andmap identifier? l))))
                                             (andmap (lambda (s) (string? (syntax-e s)))
                                                     (syntax->list (syntax (doc-string ...)))))
                                        'ok]))
                                   (syntax->list (syntax (proc ...))))
                              (add-prefix #f (list (syntax->datum row))))]
                           [(all-from tag path label)
                            (eq? 'all-from (syntax-e (syntax all-from)))
                            (let ([tag (syntax tag)]
                                  [label (syntax label)]
                                  [path (syntax->datum (syntax path))])
                              (get-existing tag path label null))]
                           [(all-from-except tag path label exception ...)
                            (eq? 'all-from-except (syntax-e (syntax all-from-except)))
                            (let ([tag (syntax tag)]
                                  [label (syntax label)]
                                  [path (syntax->datum (syntax path))]
                                  [exceptions (syntax->list (syntax (exception ...)))])
                              (get-existing tag path label exceptions))]))
                       rows)]
            [imports (apply
                      append
                      (map (lambda (row)
                             (syntax-case row ()
                               [(header . _)
                                (string? (syntax-e (syntax header)))
                                null]
                               [(all-from/-except tag path label except ...)
                                (list (with-syntax ([pf (datum->syntax
                                                         stx
                                                         (syntax-e
                                                          (syntax (prefix-in tag path))))])
                                        (syntax (require pf))))]))
                           rows))])
        ;; Collapse rows for a section name:
        (let ([rows (let loop ([rows (apply append rows)])
                      (if (null? rows)
                          null
                          (let ([rest (loop (cdr rows))])
                            (let ([a (assoc (caar rows) rest)])
                              (if a
                                  (cons (cons (caar rows)
                                              (append (cdar rows)
                                                      (cdr a)))
                                        (let loop ([l rest])
                                          (cond
                                            [(null? l) null]
                                            [(equal? (caar l) (caar rows))
                                             (cdr l)]
                                            [else (cons (car l) (loop (cdr l)))])))
                                  (cons (car rows) rest))))))])
          ;; Extract procs and eliminate duplicates
          (let ([procs (let ([ht (make-hasheq)])
                         (for-each
                          (lambda (proc-line)
                            (let-values ([(loc-name ext-name)
                                          (let ([n (cadr proc-line)])
                                            (if (pair? n)
                                                (values (car n) (cadr n))
                                                (values n n)))])
                              (hash-set! ht ext-name (list* (car proc-line)
                                                            loc-name
                                                            ext-name))))
                          (apply append (map cdr rows)))
                         (hash-map ht (lambda (key val) val)))])
            (let ([names (map (lambda (proc)
                                (cond
                                  [(car proc)
                                   ;; Source prefixed:
                                   `(,#'rename-out [,(string->symbol (format "~a~a" 
                                                                             (syntax-e (car proc))
                                                                             (cadr proc)))
                                                    ,(cadr proc)])]
                                  [(eq? (cadr proc) (cddr proc))
                                   ;; Plain
                                   (cadr proc)]
                                  [else
                                   ;; Local renamed:
                                   `(,#'rename-out [,(cadr proc)
                                                    ,(cddr proc)])]))
                              procs)]
                  [wrapped-name
                   (lambda (name)
                     (string->symbol (format "~a>>~a" 
                                             (syntax-e wrap)
                                             (if (pair? name)
                                                 (cadadr name)
                                                 name))))])
              (with-syntax ([procs (datum->syntax
                                    stx
                                    (if wrap
                                        (map (lambda (name)
                                               `(,#'rename-out 
                                                 [,(wrapped-name name)
                                                  ,(if (pair? name)
                                                       (cadadr name)
                                                       name)]))
                                             names)
                                        names))]
                            [(wrap ...) (if wrap
                                            (map (lambda (name)
                                                   `(,wrap ,(datum->syntax
                                                             wrap
                                                             (wrapped-name name))
                                                           ,(datum->syntax
                                                             wrap
                                                             (if (pair? name)
                                                                 (caadr name)
                                                                 name))))
                                                 names)
                                            null)]
                            [(import ...) imports]
                            [src (datum->syntax stx 'source)]
                            [rows (remove-prefixes rows)]
                            [label label])
                (syntax/loc stx
                  (begin
                    import ...
                    wrap ...
                    (provide . procs)
                    (define-syntaxes ()
                      (begin
                        (register-documentation (quote-syntax src) 'label 'rows)
                        (values)))))))))))
    
    (values
     (lambda (stx)
       (syntax-case stx ()
         [(_ label row ...)
          (go stx
              (syntax label)
              #f
              (syntax->list (syntax (row ...))))]))
     (lambda (stx)
       (syntax-case stx ()
         [(_ label wrap row ...)
          (go stx
              (syntax label)
              (let ([s (syntax wrap)])
                (and (syntax-e s) s))
              (syntax->list (syntax (row ...))))])))))


(define (lookup-documentation path label)
  (let ([mod ((current-module-name-resolver) path #f #f #t)])
    (dynamic-require mod (void))
    ((dynamic-require-for-syntax 
      'syntax/private/doctable
      'lookup-documentation)
     mod
     label)))

(provide provide-and-document
         provide-and-document/wrap
         lookup-documentation)
