;; This library is used by match.ss

;;!(function parse-quasi
;;          (form (parse-quasi syn) -> syntax)
;;          (contract syntax -> syntax))
;; This function parses a quasi pattern in to a regular pattern
;; and returns it.  This function does not parse the quasi pattern
;; recursively in order to find nested quasi patterns.  It only
;; parses the top quasi pattern.
(define parse-quasi
  (lambda (stx)
    (letrec 
        ((q-error (opt-lambda (syn [msg ""])
                    (match:syntax-err
                     stx
                     (string-append 
                      "syntax error in quasi-pattern: "
                      msg))))
         (parse-q
          (lambda (phrase)
                                        ;(write phrase)(newline)
            (syntax-case phrase (quasiquote unquote unquote-splicing)
              (p
               (let ((pat (syntax-object->datum (syntax p))))
                 (or (string? pat)
                     (boolean? pat)
                     (char? pat)
                     (number? pat)
                     (dot-dot-k? pat)))
               (syntax p))
              (p
               (stx-null? (syntax p))
               (syntax/loc stx (list)))
              (p
               ;; although it is not in the grammer for quasi patterns
               ;; it seems important to not allow unquote splicing to be
               ;; a symbol in this case `,@(a b c). In this unquote-splicing
               ;; is treated as a symbol and quoted to be matched.
               ;; this is probably not what the programmer intends so
               ;; it may be better to throw a syntax error
               (identifier? (syntax p)) 
               (syntax/loc stx 'p))
              ;; ((var p)  ;; we shouldn't worry about this in quasi-quote
              ;;  (identifier? (syntax p))
              ;;  (syntax/loc phrase 'p))
            (,p (syntax p))
            (,@pat
             (q-error (syntax ,@pat) "unquote-splicing not nested in list"))
            ((x . y) 
             (let* ((list-type 'list)
                    (result
                     (let loop 
                         ((l (syntax-e (syntax (x . y)))))
                                        ;(write l)(newline)
                       (cond ((null? l) '())
                             ((and (stx-pair? (car l))
                                   (equal? (car (syntax-object->datum (car l))) 
                                           'unquote-splicing))
                              (let ((first-car  
                                     (syntax-case (car l) 
                                         (unquote-splicing quasiquote)
                                       (,@`p  ;; have to parse forward here
                                        (let ((pq (parse-q (syntax p))))
                                          (if (stx-list? pq)
                                              (cdr (syntax->list pq))
                                              (q-error (syntax ,@`p)
                                                       "unquote-splicing not followed by list"))))
                                       (,@p
                                        (if (stx-list? (syntax p))
                                            (cdr (syntax->list (syntax p)))
                                            (begin ; (write (syntax-e (syntax p))) 
                                                   (q-error (syntax ,@p)
                                                     "unquote-splicing not followed by list")))))))
                                (syntax-case (cdr l) (unquote unquote-splicing)
                                  (,@p (q-error (syntax ,@p) 
                                                "unquote-splicing can not follow dot notation"))
                                  (,p  
                                   (let ((res (parse-q (syntax ,p))))
                                     (set! list-type 'list-rest)
                                     `(,@first-car ,res)))
                                  (p (or (stx-pair? (syntax p))
                                         (stx-null? (syntax p)))
                                     (append first-car 
                                             (loop (syntax-e (syntax p)))))
                                  (p ;; must be an atom
                                   (let ((res (parse-q (syntax p))))
                                     (set! list-type 'list-rest)
                                     `(,@first-car ,res))))))
                             (else 
                              (syntax-case (cdr l) (unquote unquote-splicing)
                                (,@p (q-error (syntax p) 
                                              "unquote-splicing can not follow dot notation"))
                                (,p  (begin 
                                       (set! list-type 'list-rest)
                                       (list (parse-q (car l))  
                                             (parse-q (syntax ,p)))))
                                (p (or (stx-pair? (syntax p))
                                       (stx-null? (syntax p)))
                                   (cons (parse-q (car l))
                                         (loop (syntax-e (syntax p)))))
                                (p ;; must be an atom
                                 (begin 
                                   (set! list-type 'list-rest)
                                   (list (parse-q (car l)) 
                                         (parse-q (syntax p)))))))))))
               (quasisyntax/loc stx (#,list-type #,@result))))
            (p
             (vector? (syntax-object->datum (syntax p)))
             (quasisyntax/loc 
              stx
              (vector #,@(cdr 
                          (syntax-e 
                           (parse-q
                            (quasisyntax/loc 
                             stx 
                             #,(vector->list (syntax-e (syntax p))))))))))
            (p
             (box? (syntax-object->datum (syntax p)))
             (quasisyntax/loc 
              stx
              (box #,(parse-q (unbox (syntax-e (syntax p)))))))
            (p (q-error (syntax p)))))))
      (parse-q stx))))
    
