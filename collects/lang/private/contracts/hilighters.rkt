(module hilighters mzscheme
  
  (provide (all-defined))
  
  (define identifying-mark 'xxx)
  
  ;; a path is a 
  ;;         list of symbols 'car 'cdr (or empty list)
  ;;         or  #f 
  ;;that give instructions to a hilighter function to find a certain subexpression in an S-exp.
  
  ;; a highlighter function is a function that takes a path and maybe other hilighters and returns an 
  ;; s-expression with the sub-exp denoted by the path hilighted
  
  ;; error message. invalid path
  (define (path-error type p) 
    (error 'paths (format "something bad happened: invalid ~e path at ~e" type p)))
  
  ;;hilighter for (add1 ... ) contracts
  (define (mk-add1-hilighter type-hilighter)
    (lambda (p)
      (cond 
       [(not p) `(add1 ,(type-hilighter #f))]
       [(null? p) `(,identifying-mark (add1 ,(type-hilighter #f)))]
       [(eq? (car p) 'car) `(add1 ,(type-hilighter (cdr p)))]
       [else (path-error 'add1 p)])))
  
  
  ;;hilighter for (somethingof type) contract
  (define (mk-somethingof-hilighter thing type-hilighter)
    (lambda (p) 
      (cond 
       [(not p) `(,thing ,(type-hilighter #f))]
       [(null? p) `(,identifying-mark (,thing ,(type-hilighter #f)))]
       [(eq? (car p) 'car) `(,thing ,(type-hilighter (cdr p)))]
       [else (path-error thing p)])))

  ;;hilighter for (listof type) contracts
  (define (mk-listof-hilighter type-hilighter)
    (mk-somethingof-hilighter 'listof type-hilighter))

  ;;hilighter for (vectorof type) contracts
  (define (mk-vectorof-hilighter type-hilighter)
    (mk-somethingof-hilighter 'vectorof type-hilighter))

  ;;hilighter for (vectorof type) contracts
  (define (mk-boxof-hilighter type-hilighter)
    (mk-somethingof-hilighter 'boxof type-hilighter))
  
  ;;hilighter function for (define-data a cnt1 cnt2 ...)
  (define (mk-define-data-hilighter name cnt-hilighters)
    (lambda (p)
      (let ([list-hl (mk-list-hilighter cnt-hilighters)])
        (cond
         [(not p) name]
         [(null? p) `(,identifying-mark ,name)]
         [(or (eq? (car p) 'car) (eq? (car p) 'cdr)) `(define-data ,name ,@(list-hl p))]
         [else (path-error name p)]))))
  
  ;;returns a hilighter function for (make-<blah> a b c ...) style contracts
  (define (mk-struct-hilighter name field-hilighters)
    (lambda (p)
      (let ([constructor (string->symbol (string-append "make-" (symbol->string name)))])
        (cond 
         [(not p) `(,constructor ,@(map (lambda (f) (f #f)) field-hilighters))]
         [(null? p) `(,identifying-mark (,constructor ,@(map (lambda (f) (f #f)) field-hilighters)))]
         [(eq? (car p) 'car) `(,constructor ,((car field-hilighters) (cdr p)) ,@(map (lambda (f) (f #f)) (cdr field-hilighters)))]
         [(eq? (car p) 'cdr) `(,constructor ,((car field-hilighters) #f) ,@((mk-list-hilighter (cdr field-hilighters)) (cdr p)))]
         [else (path-error name p)]))))
  
  
  ;; returns a hilighter function for (cons a b) style contracts
  (define (mk-cons-hilighter car-hilighter cdr-hilighter) 
    (lambda (p)
      (cond
       [(not p) `(cons ,(car-hilighter #f) ,(cdr-hilighter #f))]
       [(null? p) `(,identifying-mark (cons ,(car-hilighter #f) ,(cdr-hilighter #f)))]
       [(eq? (car p) 'cdr) `(cons ,(car-hilighter #f) ,(cdr-hilighter (cdr p)))]
       [(eq? (car p) 'car) `(cons ,(car-hilighter (cdr p)) ,(cdr-hilighter #f))]
       [else (path-error 'cons p)])))
  
  ;; returns a hilighter function for a (a b c -> d) style contract
  (define (mk-arrow-hilighter domain-hilighter-list range-hilighter)
    (lambda (p)
      (let ([list-hilighter (mk-list-hilighter domain-hilighter-list)])
        (cond
         [(not p) `( ,@(list-hilighter #f) -> ,(range-hilighter #f))]
         [(null? p) `(,identifying-mark (,@(list-hilighter #f) -> ,(range-hilighter #f)))]
         [(eq? (car p) 'car) `( ,@(list-hilighter (cdr p)) -> , (range-hilighter #f))]
         [(eq? (car p) 'cdr) `( ,@(list-hilighter #f) -> ,(range-hilighter (cdr p)))]
         [else (path-error 'arrow p)]))))
  
  ;; returns a hilighter for a list (a b c)
  (define (mk-list-hilighter lhilighters) 
    (lambda (p)
      (let ([false-func (lambda (f) (f #f))])
        (cond
         [(not p) `( ,@(map false-func lhilighters))]
         [(null? p) `(,identifying-mark ,@(map false-func lhilighters))]
         [(eq? (car p) 'car) `( ,@(cons ((car lhilighters) (cdr p)) (map false-func (cdr lhilighters))))]
         [(eq? (car p) 'cdr) `( ,@(cons ((car lhilighters) #f) ((mk-list-hilighter (cdr lhilighters)) (cdr p))))]
         [else (path-error 'list p)]))))
  
  ;; returns a hilighter for a flat name
  (define (mk-flat-hilighter name)
    (lambda (p)
      (cond 
       [(not p) name]
       [(or (null? p)(eq? (car p) 'car))  `(,identifying-mark ,name)]
       [else (path-error 'flat p)]))))
