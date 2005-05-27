(module xl-util mzscheme
  (require (lib "list.ss"))
  
  (provide 
   cell-alpha->number
   number->cell-alpha
   get-range-cells
   numbers->cellref
   cellref->numbers
   split-string
   in-list?
   make-equiv-class
   find-rep
   union
   to-lower
   show-constraints
   all-hashed-values
   in-hash?
   init-hash
   left-of
   top-of)   
      
  (define (show-constraints constraints)
    (for-each (lambda (c) (printf "constraint: ~a~n" c)) constraints))

  (define (in-hash? ht v)
    (not (not (hash-table-get ht v (lambda () #f)))))
  
  (define (init-hash ht l)
    (if (not (empty? l))
        (begin
          (let ([cell-name (car (first l))]
                [u (cadr (first l))])
            (if (not (in-hash? ht cell-name))
                (hash-table-put! ht cell-name u)))
          (init-hash ht (rest l)))))
  
  (define (all-hashed-values ht)
    (hash-table-for-each ht
                         (lambda (key val)
                           (printf "Key:~a -> Value:~a~n" key val))))

  (define (make-equiv-class a) (list a))
  
  (define (find-rep equiv-class)
    (let ([rep (last-pair equiv-class)])
      (when (not (eq? equiv-class rep))
        (set-cdr! equiv-class rep))
      rep))
  
  (define (union equiv-class1 equiv-class2)
    (let ([rep1 (find-rep equiv-class1)]
          [rep2 (find-rep equiv-class2)])
      (unless (eq? rep1 rep2)
        (set-cdr! rep2 rep1))
      rep1))
  
  (define (in-list? e l)
    (cond [(empty? l) #f]
          [(equal? e (first l)) #t]
          [else (in-list? e (rest l))]))
  
  (define (to-lower str)
    (letrec ([loop (lambda (l)
                     (cond
                       ((empty? l) empty)
                       ((char-upper-case? (first l))
                        (cons (integer->char 
                               (+ (- (char->integer (first l))
                                     (char->integer #\A))
                                  (char->integer #\a)))
                              (loop (rest l))))
                       (else (cons (first l) (loop (rest l))))))])
      (list->string (loop (string->list str)))))
  
  (define (split-string str c)
    (letrec ([loop (lambda (i first max c)
                     (cond
                       ((>= first max) empty)
                       ((>= i max) 
                        (list (substring str first max)))
                       ((eq? (string-ref str i) c)
                        (cons 
                         (substring str first i)
                         (loop (add1 i) (add1 i) max c)))
                       (else (loop (add1 i) first max c))))])
      (loop 0 0 (string-length str) c)))
  
  ; #\a = 0, #\b = 1, etc.
  (define (letter->number c)
    (- (char->integer c) 97)) ; #\a
  (define (number->letter n)
    (integer->char (+ n 97)))
  
  ; "a" = 0, "b" = 1, ... , "aa" = 26, ... , "ba" = 52, ...
  (define (cell-alpha->number s)
    (let ([len (string-length s)])
      (if (= len 1)
	  (letter->number (string-ref s 0))
	  (+  (* 26 (add1 (letter->number (string-ref s 0))))
	      (letter->number (string-ref s 1))))))
  (define (number->cell-alpha n)
    (if (< n 26)
	(string (number->letter n))
	(string-append 
	 (string (number->letter (sub1 (quotient n 26))))
	 (string (number->letter (modulo n 26))))))
  
  
  ; given a cell address as a symbol, such as 'D5
  ; returns the alpha and numeric pieces as a string and a number
  (define (get-cell-components csym)
    (let* ([c (symbol->string csym)]
	   [len (string-length c)])
      (if (char-numeric? (string-ref c 1))
	  (values (substring c 0 1) 
		  (string->number (substring c 1 len)))
	  (values (substring c 0 2) 
		  (string->number (substring c 2 len))))))
  
  ; from pair of numbers representing a cell address in its alpha
  ; and numeric components, return symbol
  ; (1 1) = A1, ... , (27 0) = AA0, ...
  (define (numbers->cellref ns)
    (let ([alpha-part (car ns)]
	  [num-part (cadr ns)])
      (string->symbol
       (string-append
	(number->cell-alpha alpha-part)
	(number->string num-part)))))
  (define (cellref->numbers cell)
    (letrec ([iter (lambda (l x y)
                     (cond
                       ((empty? l) (list x y))
                       (else
                        (let ([c (first l)])
                          (cond
                            ((char-numeric? c)
                             (iter (rest l) x (+ (* 10 y)
                                                 (- (char->integer c)
                                                    (char->integer #\0)))))
                            (else
                             (iter (rest l) (+ (* 26 x)
                                               (- (char->integer c)
                                                  (char->integer #\a)) 1) y)))))))])
      (iter (string->list (symbol->string cell)) 0 0)))
  
  (define (left-of loc)
    (let* ([xy (cellref->numbers loc)]
           [x (- (car xy) 2)]
           [y (cadr xy)])
      (numbers->cellref (list x y))))
  
  (define (top-of loc)
    (let* ([xy (cellref->numbers loc)]
           [x (sub1 (car xy))]
           [y (sub1 (cadr xy))])
      (numbers->cellref (list x y))))
  
  ; given two corner cells (as symbols), return list of symbols
  ; for all contained cells
  (define (get-range-cells c1 c2)
    (let*-values
        ([(a1 n1) (get-cell-components c1)]
         [(a2 n2) (get-cell-components c2)]
         [(alow ahigh) 
          (if (string<? a1 a2) 
              (values a1 a2)
              (values a2 a1))]
         [(nlow nhigh)
          (if (< n1 n2) 
              (values n1 n2)
              (values n2 n1))]
         [(numalow numahigh)
          (values (cell-alpha->number alow)
                  (cell-alpha->number ahigh))])
      (let oloop ([curr-a numalow]
                  [oresult null])
        (if (> curr-a numahigh)
            (map numbers->cellref oresult)
            (let iloop ([curr-n nlow]
                        [iresult null])
              (if (> curr-n nhigh)
                  (oloop (add1 curr-a)
                         (append iresult oresult))
                  (iloop (add1 curr-n)
                         (cons (list curr-a curr-n) iresult)))))))))

