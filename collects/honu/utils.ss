(module utils mzscheme

  (require (lib "contract.ss")
           (prefix srfi1: (lib "list.ss" "srfi" "1"))
           (lib "list.ss"))

  (require-for-template (lib "contract.ss"))

  (provide define/p)
  (define-syntax (define/p stx)
    (syntax-case stx ()
      [(_ (NAME . ARGS) BODY ...)
       #`(begin
           (define (NAME . ARGS) BODY ...)
           (provide NAME))]
      [(_ NAME BODY ...)
       #`(begin
           (define NAME BODY ...)
           (provide NAME))]
      ))

  (provide define/c)
  (define-syntax (define/c stx)
    (syntax-case stx ()
      [(_ (NAME . ARGS) CONTRACT BODY ...)
       #`(begin
           (define (NAME . ARGS) BODY ...)
           (provide/contract [NAME CONTRACT]))]
      [(_ NAME CONTRACT BODY ...)
       #`(begin
           (define NAME BODY ...)
           (provide/contract [NAME CONTRACT]))]
      ))

  (provide define-struct/p)
  (define-syntax (define-struct/p stx)
    (syntax-case stx ()
      [(_ (NAME SUPER) (FIELD ...) REST ...)
       #`(begin
           (define-struct (NAME SUPER) (FIELD ...) REST ...)
           (provide (struct NAME (FIELD ...))))]
      [(_ NAME (FIELD ...) REST ...)
       #`(begin
           (define-struct NAME (FIELD ...) REST ...)
           (provide (struct NAME (FIELD ...))))]))

  (provide define-struct/c)
  (define-syntax (define-struct/c stx)
    (syntax-case stx ()
      [(_ (NAME SUPER) ([FIELD CONTRACT] ...) REST ...)
       #`(begin
           (define-struct (NAME SUPER) (FIELD ...) REST ...)
           (provide/contract (struct NAME ([FIELD CONTRACT] ...))))]
      [(_ NAME ([FIELD CONTRACT] ...) REST ...)
       #`(begin
           (define-struct NAME (FIELD ...) REST ...)
           (provide/contract (struct NAME ([FIELD CONTRACT] ...))))]))

  (define (map-values-rev-accs f lists accs)
    (cond [(andmap empty? lists) (apply values (map reverse accs))]
          [(ormap empty? lists) (error 'map-values "expects lists of equal length")]
          [else (call-with-values (lambda () (apply f (map first lists)))
                  (lambda vs (map-values-rev-accs f (map rest lists) (map cons vs accs))))]))
  
  (define/p (map-values f . lists)
    (cond [(empty? lists) (error 'map-values "expects 1 or more input lists")]
          [(ormap empty? lists) (error 'map-values "expects non-empty lists")]
          [else
           (call-with-values (lambda () (apply f (map first lists)))
             (lambda vs (map-values-rev-accs f (map rest lists) (map list vs))))]))

  (define (identifier<? a b)
    (string<? (symbol->string (syntax-e a))
              (symbol->string (syntax-e b))))
  
  (provide get-first-non-unique-name)
  (define (get-first-non-unique-name lst)
    (let loop ([lst (quicksort lst identifier<?)])
      (cond
        [(null? lst)       #f]
        [(null? (cdr lst)) #f]
        [(bound-identifier=? (car lst) (cadr lst))
         ;; since quicksort isn't stable, just return the first
         (car lst)]
        [else #f])))

  (provide fold-with-rest)
  (define (fold-with-rest f init l)
    (if (null? l)
        init
        (fold-with-rest f (f (car l) (cdr l) init) (cdr l))))
  
  (provide unique?)
  (define (unique? cs)
    (fold-with-rest (lambda (c cs acc)
                      (and acc
                           (not (member c cs))))
                    #t cs))
  
  (define (get-names ds p f)
    (srfi1:filter-map (lambda (defn)
                        (and (p defn)
                             (f defn)))
                      ds))
  
  (provide map-and-fold)
  (define (map-and-fold f i l)
    (let loop ((l l)
               (mapped '())
               (folded i))
      (if (null? l)
          (values (reverse mapped) folded)
          (let-values ([(res folded) (f (car l) folded)])
            (loop (cdr l)
                  (cons res mapped)
                  folded)))))
  
  (provide map-two-values)
  (define (map-two-values f . lists)
    (let loop ((lists lists)
               (map1  '())
               (map2  '()))
      (if (ormap empty? lists)
          (values (reverse map1) (reverse map2))
          (let-values ([(m1 m2) (apply f (map car lists))])
            (loop (map cdr lists)
                  (cons m1 map1)
                  (cons m2 map2))))))
  
  (provide partition-first)
  (define (partition-first f lis)
    (let loop ([lis    lis]
               [passed '()])
      (cond
        [(null? lis)
         (values #f (reverse passed))]
        [(f (car lis))
         (values (car lis) (append (reverse passed) (cdr lis)))]
        [else
         (loop (cdr lis) (cons (car lis) passed))])))
  )
