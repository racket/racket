(module mpair "pre-base.rkt"

  (define-values (struct:mcons mcons mpair? accessor setter)
    (make-struct-type 'mpair #f 2 0 #f
                      (list (cons prop:equal+hash
                                (list (λ (a b rec)
                                          (and (rec (mcar a) (mcar b))
                                               (rec (mcdr a) (mcdr b))))
                                        (λ (a rec)
                                          (+ (rec (mcar a)) (rec (mcdr a))))
                                        (λ (a rec)
                                          (+ (rec (mcar a)) (rec (mcdr a))))))
                          (cons prop:custom-write
                                (λ (mp prt mode)
                                  (case mode
                                    [(#t #f)
                                     (write-string "{" prt)
                                     ((if mode write display) (mcar mp) prt)
                                     (write-string " . " prt)
                                     ((if mode write display) (mcdr mp) prt)
                                     (write-string "}" prt)]
                                    [(0 1)
                                     (write-string "(mcons " prt)
                                     (print (mcar mp) prt)
                                     (write-string " " prt)
                                     (print (mcdr mp) prt)
                                     (write-string ")" prt)]))))
                    (current-inspector)
                    #f
                    null
                    #f
                    'mcons))

(define mcar (procedure-rename (make-struct-field-accessor accessor 0) 'mcar))
(define mcdr (procedure-rename (make-struct-field-accessor accessor 1) 'mcdr))

(define set-mcar! (procedure-rename (make-struct-field-mutator setter 0) 'set-mcAr!))
(define set-mcdr! (procedure-rename (make-struct-field-mutator setter 1) 'set-mcdr!))

(provide mcons mcdr mcar set-mcar! set-mcdr! mpair?)
)
