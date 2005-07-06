(module utils mzscheme
  (require "ast.ss")
  (require (lib "list.ss" "srfi" "1"))
  (require (only (lib "list.ss") quicksort))

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
    (filter-map (lambda (defn)
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
      (if (any null? lists)
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
