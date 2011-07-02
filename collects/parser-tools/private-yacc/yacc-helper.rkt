(module yacc-helper mzscheme

  (require mzlib/list
           "../private-lex/token-syntax.rkt")

  ;; General helper routines
  
  (provide duplicate-list? remove-duplicates overlap? vector-andmap display-yacc)
  
  (define (vector-andmap f v)
    (let loop ((i 0))
      (cond
       ((= i (vector-length v)) #t)
       (else (if (f (vector-ref v i))
		 (loop (add1 i))
		 #f)))))

  ;; duplicate-list?: symbol list -> #f | symbol
  ;; returns a symbol that exists twice in l, or false if no such symbol 
  ;; exists
  (define (duplicate-list? l)
    (letrec ((t (make-hash-table))
	     (dl? (lambda (l)
		    (cond
		     ((null? l) #f)
		     ((hash-table-get t (car l) (lambda () #f)) => 
		      (lambda (x) x))
		     (else
		      (hash-table-put! t (car l) (car l))
		      (dl? (cdr l)))))))
      (dl? l)))

  ;; remove-duplicates: syntax-object list -> syntax-object list
  ;; removes the duplicates from the lists
  (define (remove-duplicates sl)
    (let ((t (make-hash-table)))
      (letrec ((x
		(lambda (sl)
		  (cond
		   ((null? sl) sl)
		   ((hash-table-get t (syntax-object->datum (car sl)) (lambda () #f))
		    (x (cdr sl)))
		   (else 
		    (hash-table-put! t (syntax-object->datum (car sl)) #t)
		    (cons (car sl) (x (cdr sl))))))))
	(x sl))))

  ;; overlap?: symbol list * symbol list -> #f | symbol
  ;; Returns an symbol in l1 intersect l2, or #f is no such symbol exists
  (define (overlap? l1 l2)
    (let/ec ret
      (let ((t (make-hash-table)))
	(for-each (lambda (s1)
		    (hash-table-put! t s1 s1))
		  l1)
	(for-each (lambda (s2)
		    (cond 
		     ((hash-table-get t s2 (lambda () #f)) =>
		      (lambda (o) (ret o)))))
		  l2)
	#f)))

  
  (define (display-yacc grammar tokens start precs port)
    (let-syntax ((p (syntax-rules ()
                      ((_ args ...) (fprintf port args ...)))))
      (let* ((tokens (map syntax-local-value tokens))
             (eterms (filter e-terminals-def? tokens))
             (terms (filter terminals-def? tokens))
             (term-table (make-hash-table))
             (display-rhs
              (lambda (rhs)
                (for-each (lambda (sym) (p "~a " (hash-table-get term-table sym (lambda () sym))))
                          (car rhs))
                (if (= 3 (length rhs))
                    (p "%prec ~a" (cadadr rhs)))
                (p "\n"))))
        (for-each
         (lambda (t)
           (for-each
            (lambda (t)
              (hash-table-put! term-table t (format "'~a'" t)))
            (syntax-object->datum (e-terminals-def-t t))))
         eterms)
        (for-each
         (lambda (t)
           (for-each
            (lambda (t)
              (p "%token ~a\n" t)
              (hash-table-put! term-table t (format "~a" t)))
            (syntax-object->datum (terminals-def-t t))))
         terms)
        (if precs
            (for-each (lambda (prec)
                        (p "%~a " (car prec))
                        (for-each (lambda (tok)
                                    (p " ~a" (hash-table-get term-table tok)))
                                  (cdr prec))
                        (p "\n"))
                      precs))
        (p "%start ~a\n" start)
        (p "%%\n")

        (for-each (lambda (prod)
                    (let ((nt (car prod)))
                      (p "~a: " nt)
                      (display-rhs (cadr prod))
                      (for-each (lambda (rhs)
                                  (p "| ")
                                  (display-rhs rhs))
                                (cddr prod))
                      (p ";\n")))
          grammar)
        (p "%%\n"))))
  

)  
  
