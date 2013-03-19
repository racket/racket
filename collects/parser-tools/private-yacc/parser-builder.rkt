(module parser-builder mzscheme
  
  (require "input-file-parser.rkt"
           "grammar.rkt"
           "table.rkt"
           mzlib/class
           mzlib/contract)
  (require-for-template mzscheme)
  
  (provide/contract
   (build-parser ((string? any/c any/c (listof identifier?) (listof identifier?)
                   (listof identifier?) (union syntax? false/c) syntax?)
                  . ->* .
                  (any/c any/c any/c any/c))))
  
  ;; fix-check-syntax : (listof identifier?) (listof identifier?) (listof identifier?)
  ;;                    (union syntax? false/c) syntax?) -> syntax?
  (define (fix-check-syntax input-terms start ends assocs prods)
    (let* ((term-binders (get-term-list input-terms))
           (get-term-binder
            (let ((t (make-hash-table)))
              (for-each 
               (lambda (term)
                 (hash-table-put! t (syntax-e term) term))
               term-binders)
              (lambda (x)
                (let ((r (hash-table-get t (syntax-e x) (lambda () #f))))
                  (if r
                      (syntax-local-introduce (datum->syntax-object r (syntax-e x) x x))
                      x)))))
           (rhs-list
            (syntax-case prods ()
              (((_ rhs ...) ...)
               (syntax->list (syntax (rhs ... ...)))))))
      (with-syntax (((tmp ...) (map syntax-local-introduce term-binders))
                    ((term-group ...)
                     (map (lambda (tg)
                            (syntax-property
                             (datum->syntax-object tg #f)
                             'disappeared-use
                             tg))
                          input-terms))
                    ((end ...)
                     (map get-term-binder ends))
                    ((start ...)
                     (map get-term-binder start))
                    ((bind ...)
                     (syntax-case prods ()
                       (((bind _ ...) ...)
                        (syntax->list (syntax (bind ...))))))
                    (((bound ...) ...)
                     (map
                      (lambda (rhs)
                        (syntax-case rhs ()
                          (((bound ...) (_ pbound) __)
                           (map get-term-binder
                                (cons (syntax pbound)
                                      (syntax->list (syntax (bound ...))))))
                          (((bound ...) _)
                           (map get-term-binder
                                (syntax->list (syntax (bound ...)))))))
                      rhs-list))
                    ((prec ...)
                     (if assocs
                         (map get-term-binder
                              (syntax-case assocs ()
                                (((__ term ...) ...)
                                 (syntax->list (syntax (term ... ...))))))
                         null)))
        #`(when #f
            (let ((bind void) ... (tmp void) ...)
              (void bound ... ... term-group ... start ... end ... prec ...))))))
  (require mzlib/list "parser-actions.rkt")
  (define (build-parser filename src-pos suppress input-terms start end assocs prods)
    (let* ((grammar (parse-input input-terms start end assocs prods src-pos))
           (table (build-table grammar filename suppress))
           (all-tokens (make-hash-table))
           (actions-code
            `(vector ,@(map prod-action (send grammar get-prods)))))
      (for-each (lambda (term)
                  (hash-table-put! all-tokens (gram-sym-symbol term) #t))
                (send grammar get-terms))
      #;(let ((num-states (vector-length table))
            (num-gram-syms (+ (send grammar get-num-terms)
                              (send grammar get-num-non-terms)))
            (num-ht-entries (apply + (map length (vector->list table))))
            (num-reduces
             (let ((ht (make-hash-table)))
               (for-each
                (lambda (x)
                  (when (reduce? x)
                    (hash-table-put! ht x #t)))
                (map cdr (apply append (vector->list table))))
               (length (hash-table-map ht void)))))
        (printf "~a states, ~a grammar symbols, ~a hash-table entries, ~a reduces\n"
                num-states num-gram-syms num-ht-entries num-reduces)
        (printf "~a -- ~aKB, previously ~aKB\n"
                (/ (+ 2 num-states
                      (* 4 num-states) (* 2 1.5 num-ht-entries)
                      (* 5 num-reduces)) 256.0)
                (/ (+ 2 num-states
                      (* 4 num-states) (* 2 2.3 num-ht-entries)
                      (* 5 num-reduces)) 256.0)
                (/ (+ 2 (* num-states num-gram-syms) (* 5 num-reduces)) 256.0)))
      (values table
              all-tokens
              actions-code
              (fix-check-syntax input-terms start end assocs prods))))
      
  )
