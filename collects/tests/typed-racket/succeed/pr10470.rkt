(module pr10470 typed-scheme

  (define-type-alias (Memo alpha) (U (Option alpha) (-> (Option alpha))))

  (define-struct: table ([val : (Memo Number)]) #:mutable)

  (: f (table -> (Option Number)))
  (define (f tab)
    (let ([proc-or-num (table-val tab)])
      (cond
        [(procedure? proc-or-num)
         (let ([result (proc-or-num)])
           (set-table-val! tab result)
           result)]
        [else proc-or-num]))))
