(meta define no-early-reference?
      (lambda (stx ids)
        (cond
         [(#%identifier? stx)
          (not (#%ormap (lambda (id) (free-identifier=? id stx)) ids))]
         [(let ([d (syntax->datum stx)])
            (or (number? d) (boolean? d) (string? d) (bytevector? d)))
          #t]
         [else
          (syntax-case stx (quote |#%name| lambda case-lambda)
            [(quote _) #t]
            [(|#%name| _ exp) (no-early-reference? #'exp ids)]
            [(lambda . _) #t]
            [(case-lambda . _) #t]
            [_ #f])])))

(meta define no-early-references?
      (lambda (rhss ids)
        (cond
         [(null? rhss) #t]
         [else (and (no-early-reference? (car rhss) ids)
                    (no-early-references? (cdr rhss) (cdr ids)))])))

;; Like `letrec*`, but makes use-before-definition checks explicit so
;; that a source name is included in the error messages. Also, the
;; expansion allows `call/cc`-based capture and multiple return on the
;; right-hand side.
(define-syntax (letrec*/names stx)
  (syntax-case stx ()
    [(_ (name ...) ([id rhs] ...) body ...)
     (cond
      [(no-early-references? #'(rhs ...) #'(id ...))
       #'(letrec* ([id rhs] ...) body ...)]
      [else
       (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
         #'(let ([tmp-id unsafe-undefined] ...)
             (let-syntax ([id (identifier-syntax
                               [id (check-not-unsafe-undefined tmp-id 'name)]
                               [(set! id exp)
                                (let ([id exp])
                                  (check-not-unsafe-undefined/assign tmp-id 'name)
                                  (set! tmp-id id))])]
                          ...)
               (set! tmp-id rhs)
               ...
               (let ()
                 body ...))))])]))
