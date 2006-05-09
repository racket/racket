(module assoc-list mzscheme
  
  (require (lib "class.ss"))
  
  (provide assoc%)
  
  (define assoc<%> 
    (interface () 
      ;; type X, Y
      add    ;; X Y -> Void
      ;; add (cons x y) to fields or update the existing association
      remove ;; X -> Void 
      list   ;; -> [Listof Y]
      lookup ;; X -> Y 
      update ;; X Y -> Void
      ))
  
  (define assoc%
    (class object%
      (super-new)
      
      ;; (Listof (Cons X Y))
      (define fields '())
      
      ;; X Y -> Void
      (define/public (add type name)
        (define a (assq type fields))
        (if a 
            (send this update type name)
            (set! fields (cons (cons type name) fields))))
      
      ;; X -> Void
      (define/public (remove type)
        (set! fields
              (let loop ([fields fields])
                (cond
                  [(null? fields) '()]
                  [(eq? (caar fields) type) (cdr fields)]
                  [else (cons (car fields) (loop (cdr fields)))]))))
      
      ;; -> (Listof Y)
      ;; extract all y in the order in which they were entered
      (define/public (list) (reverse! (map (lambda (f) (cdr f)) fields)))
      
      ;; X -> Y
      (define/public (lookup to-edit)        
        (let loop ([v fields])
          (cond
            [(null? v) (error 'internal "can't find field")]
            [(eq? (caar v) to-edit) (cdar v)]
            [else (loop (cdr v))])))
      
      ;; X Y -> Void
      (define/public (update to-edit new-v)
        (set! fields
              (let loop ([v fields])
                (cond
                  [(null? v) (error 'internal "can't find variant")]
                  [(eq? (caar v) to-edit) (cons (cons (caar v) new-v) (cdr v))]
                  [else (cons (car v) (loop (cdr v)))]))))))
      
      #| Tests:
      (require (lib "testing.scm" "testing"))
      
      (define al (new assoc%))
      
      (send al add 0 1)
      (test== (send al list) (list 1))
      
      (send al add 'a 'b)
      (test== (send al list) (list 1 'b))
      
      (send al update 0 33)
      (test== (send al lookup 0) 33)
      
      (send al remove 0)
      (test== (send al list) (list 'b))
      
      (send al remove 'a)
      (test== (send al list) '())
      |#
      
      )