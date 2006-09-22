(module observe-step mzscheme
  (provide observe-step)
  
    (define current-expand-observe
    (dynamic-require '#%expobs 'current-expand-observe))
  
  (define (observe-step pre mpre mpost post)
    (define (call-obs ev . args) 
      (let ([obs (current-expand-observe)])
        (if obs 
            (let ([evn (case ev
                         [(visit) 0]
                         [(enter-prim) 6]
                         [(prim-stop) 100]
                         [(exit-prim) 7]
                         [(return) 2]
                         [(macro-enter) 8]
                         [(macro-exit) 9]
                         [(macro-pre) 21]
                         [(macro-post) 22]
                         [(local-enter) 130]
                         [(local-exit) 131]
                         [(local-pre) 132]
                         [(local-post) 133])])
              (apply obs evn args)))))
    
    (call-obs 'local-enter pre)
    (call-obs 'local-pre pre)
    (call-obs 'visit pre)
    (call-obs 'macro-enter pre)
    (call-obs 'macro-pre mpre)
    (call-obs 'macro-post mpost)
    (call-obs 'macro-exit post)
    (call-obs 'visit post)
    (call-obs 'enter-prim post)
    (call-obs 'prim-stop #f)
    (call-obs 'exit-prim post)
    (call-obs 'return post)
    (call-obs 'local-post post)
    (call-obs 'local-exit post)
    )

  )