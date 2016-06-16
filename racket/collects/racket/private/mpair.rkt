(module mpair '#%kernel
  (#%require "stx.rkt" "small-scheme.rkt" '#%unsafe)

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
                                  (if (boolean? mode)
                                      (begin
                                        (write-string "{" prt)
                                        ((if mode write display) (mcar mp) prt)
                                        (write-string " . " prt)
                                        ((if mode write display) (mcdr mp) prt)
                                        (write-string "}" prt))
                                      (begin
                                       (write-string "(mcons " prt)
                                       (print (mcar mp) prt)
                                       (write-string " " prt)
                                       (print (mcdr mp) prt)
                                       (write-string ")" prt))))))
                    (current-inspector)
                    #f
                    null
                    #f
                    'mcons))

  (-define mcar (procedure-rename (make-struct-field-accessor accessor 0) 'mcar))
  (-define mcdr (procedure-rename (make-struct-field-accessor accessor 1) 'mcdr))
  
  (-define set-mcar! (procedure-rename (make-struct-field-mutator setter 0) 'set-mcar!))
  (-define set-mcdr! (procedure-rename (make-struct-field-mutator setter 1) 'set-mcdr!))

  (module* unsafe #f
    (#%provide unsafe-mcar unsafe-mcdr unsafe-set-mcar! unsafe-set-mcdr!)
    (-define (unsafe-mcar v) (unsafe-struct-ref v 0))
    (-define (unsafe-mcdr v) (unsafe-struct-ref v 1))
    (-define (unsafe-set-mcar! v new) (unsafe-struct-set! v 0 new))
    (-define (unsafe-set-mcdr! v new) (unsafe-struct-set! v 1 new)))
  
  (#%provide mcons mcdr mcar set-mcar! set-mcdr! mpair?))
