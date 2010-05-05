;;; some tests for SrPersist

;; we can't depend on the available of a DBMS for these tests
;; and most DBMSs don't implement many parts of ODBC

;; but we can test SrPersist-specific procedures

;; we can't construct all possible ODBC values from Scheme (yet)
;;  so we don't do comparisons against expected results
;;  basically, looking for segfaults and garbage values

(require srpersist)

(define c-types
 '(sql-c-char
   sql-c-wchar
   sql-c-long
   sql-c-short
   sql-c-float
   sql-c-double
   sql-c-date
   sql-c-time
   sql-c-timestamp
   sql-c-binary
   sql-c-bit
   sql-c-tinyint
   sql-c-slong
   sql-c-sshort
   sql-c-stinyint
   sql-c-ulong
   sql-c-ushort
   sql-c-utinyint
   sql-c-bookmark
   ; [ODBC 3.0 or greater]
   sql-c-numeric
   sql-c-type-timestamp
   sql-c-type-date
   sql-c-type-time
   sql-c-interval-year
   sql-c-interval-month
   sql-c-interval-day
   sql-c-interval-hour
   sql-c-interval-minute
   sql-c-interval-second
   sql-c-interval-year-to-month
   sql-c-interval-day-to-hour
   sql-c-interval-day-to-minute
   sql-c-interval-day-to-second
   sql-c-interval-hour-to-minute
   sql-c-interval-hour-to-second
   sql-c-interval-minute-to-second
   sql-c-sbigint
   sql-c-ubigint
   sql-c-varbookmark
   ; [ODBC 3.5 or greater]
   sql-c-guid))
 
(define buff-len 10)

(define buffs-1 
  (map make-buffer c-types))

(define buffs-2
  (map (lambda (ty)
	 (make-buffer ty buff-len))
       c-types))

(define count 0)

(define results-1
  (map read-buffer buffs-1))

(define results-2
  (map (lambda (buff)
	 (let loop ([n 0])
	   (if (>= n buff-len)
	       '()
	       (cons (read-buffer buff n)
		     (loop (add1 n))))))
       buffs-2))

(define results-3
  (map read-buffer buffs-2))

(define ind-1
  (make-indicator))

(define ind-2
  (make-indicator 100))

(define ind-result-1 (read-indicator ind-1))

(define ind-result-2 (read-indicator ind-1 0))

(define ind-result-3 (read-indicator ind-2))

(define ind-result-4 (let loop ([n 0])
		       (if (>= n 100)
			   '()
			   (cons (read-indicator ind-2 n)
				 (loop (add1 n))))))

(define bi (make-boxed-uint 42))

(printf "~a~n" results-1)
(printf "~a~n" results-2)
(printf "~a~n" results-3)
(printf "~a~n" ind-result-1)
(printf "~a~n" ind-result-2)
(printf "~a~n" ind-result-3)
(printf "~a~n" ind-result-4)

(printf "~a~n" (read-boxed-uint bi))


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
