(module general mzscheme

  (require (prefix srfi1: (lib "list.ss" "srfi" "1"))
           (lib "list.ss"))

  (provide fold-with-rest
           get-first-non-unique-name
           map-and-fold
           map-two-values
           map-values
           partition-first
           unique?
           curry
           false?)

  (define (map-values-rev-accs f lists accs)
    (cond [(andmap empty? lists) (apply values (map reverse accs))]
          [(ormap empty? lists) (error 'map-values "expects lists of equal length")]
          [else (call-with-values (lambda () (apply f (map first lists)))
                  (lambda vs (map-values-rev-accs f (map rest lists) (map cons vs accs))))]))
  
  (define (map-values f . lists)
    (cond [(empty? lists) (error 'map-values "expects 1 or more input lists")]
          [(ormap empty? lists) (error 'map-values "expects non-empty lists")]
          [else
           (call-with-values (lambda () (apply f (map first lists)))
             (lambda vs (map-values-rev-accs f (map rest lists) (map list vs))))]))

  (define (identifier<? a b)
    (string<? (symbol->string (syntax-e a))
              (symbol->string (syntax-e b))))
  
  (define (get-first-non-unique-name lst)
    (let loop ([lst (quicksort lst identifier<?)])
      (cond
        [(null? lst)       #f]
        [(null? (cdr lst)) #f]
        [(bound-identifier=? (car lst) (cadr lst))
         ;; since quicksort isn't stable, just return the first
         (car lst)]
        [else #f])))

  (define (fold-with-rest f init l)
    (if (null? l)
        init
        (fold-with-rest f (f (car l) (cdr l) init) (cdr l))))
  
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

  (define (curry f . args)
    (lambda rest
      (apply f (append args rest))))

  (define (false? v)
    (eq? v #f))
  
  )
