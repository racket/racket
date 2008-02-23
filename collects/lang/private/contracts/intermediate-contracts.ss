(module intermediate-contracts mzscheme
  
  (require "hilighters.ss"
           "contracts-helpers.ss"
           mzlib/etc)
  
  (require mzlib/list)
  
  (provide vector-contract 
           listof-contract 
           vectorof-contract
           boxof-contract)
  
  (define vector-contract (lambda (stx) (build-flat-contract vector? 'vector stx)))

  (define (somethingof-contract thing enforcer predicate cnt stx) 
    (if (not (flat-contract? cnt))
        (error 'contracts "~a contracts can only be created with flat values!" thing)
        (make-flat-contract
         enforcer
         (mk-listof-hilighter (contract-hilighter cnt))
         (syntax-source stx)
         (syntax-line stx)
         (syntax-column stx)
         (syntax-position stx)
         (syntax-span stx)
         predicate)))

  (define (listof-contract cnt stx) 
    (rec me
         (somethingof-contract 
          'listof
          (lambda (l) 
            (if (null? l) l
                (if (pair? l)
                    (let loop ([list l])  
                      (catch-contract-error 'car me ((contract-enforcer cnt) (car list)))
                      (if (null? (cdr list))
                          l
                          (loop (cdr list))))
                    (contract-error l me '()))))

          (lambda (x) (or (null? x) (and (pair? x) (andmap (flat-contract-predicate cnt) x))))
          cnt
          stx)))
  
  (define (vectorof-contract cnt stx)
    (rec me
         (somethingof-contract
          'vectorof
          (lambda (v)
            (if (vector? v)
                (let loop ([i 0])  
                  (catch-contract-error 'car me ((contract-enforcer cnt) (vector-ref v i)))
                  (if (< (+ 1 i) (vector-length v))
                      (loop (+ i 1))
                      v))
                (contract-error v me '())))

          (lambda (x) (and (vector? x) (andmap (flat-contract-predicate cnt) (vector->list x))))
          cnt 
          stx)))

  (define (boxof-contract cnt stx)
    (rec me
         (somethingof-contract
          'boxof
          (lambda (v)
            (if (box? v)
                (begin 
                  (catch-contract-error 'car me ((contract-enforcer cnt) (unbox v)))
                  v)
                (contract-error v me '())))
          (lambda (v) (and (box? v) ((flat-contract-predicate cnt) (unbox v))))
          cnt
          stx))))
