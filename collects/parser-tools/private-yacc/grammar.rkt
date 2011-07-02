;; Constructs to create and access grammars, the internal
;; representation of the input to the parser generator.

(module grammar mzscheme
  
  (require mzlib/class
           mzlib/list
           "yacc-helper.rkt"
           mzlib/contract)
  
  ;; Each production has a unique index 0 <= index <= number of productions
  (define-struct prod (lhs rhs index prec action) (make-inspector))

  ;; The dot-pos field is the index of the element in the rhs
  ;; of prod that the dot immediately precedes.
  ;; Thus 0 <= dot-pos <= (vector-length rhs).
  (define-struct item (prod dot-pos) (make-inspector))
  
  ;; gram-sym = (union term? non-term?)
  ;; Each term has a unique index 0 <= index < number of terms
  ;; Each non-term has a unique index 0 <= index < number of non-terms  
  (define-struct term (sym index prec) (make-inspector))
  (define-struct non-term (sym index) (make-inspector))

  ;; a precedence declaration.
  (define-struct prec (num assoc) (make-inspector))
  
  (provide/contract
   (make-item (prod? (union false/c natural-number/c) . -> . item?))
   (make-term (symbol? (union false/c natural-number/c) (union prec? false/c) . -> . term?))
   (make-non-term (symbol? (union false/c natural-number/c) . -> . non-term?))
   (make-prec (natural-number/c (symbols 'left 'right 'nonassoc) . -> . prec?))
   (make-prod (non-term? (vectorof (union non-term? term?))
               (union false/c natural-number/c) (union false/c prec?) syntax? . -> . prod?)))
  
  (provide 
     
   
   ;; Things that work on items
   start-item? item-prod item->string 
   sym-at-dot move-dot-right item<? item-dot-pos
   
   ;; Things that operate on grammar symbols
   gram-sym-symbol gram-sym-index term-prec gram-sym->string
   non-term? term? non-term<? term<?
   term-list->bit-vector term-index non-term-index
   
   ;; Things that work on precs
   prec-num prec-assoc

   grammar%
   
   ;; Things that work on productions
   prod-index prod-prec prod-rhs prod-lhs prod-action)


  ;;---------------------- LR items --------------------------
  
  ;; item<?: LR-item * LR-item -> bool
  ;; Lexicographic comparison on two items.
  (define (item<? i1 i2)
    (let ((p1 (prod-index (item-prod i1)))
	  (p2 (prod-index (item-prod i2))))
      (or (< p1 p2)
	  (and (= p1 p2)
	       (let ((d1 (item-dot-pos i1))
		     (d2 (item-dot-pos i2)))
		 (< d1 d2))))))

  ;; start-item?: LR-item -> bool
  ;; The start production always has index 0
  (define (start-item? i)
    (= 0 (non-term-index (prod-lhs (item-prod i)))))

  
  ;; move-dot-right: LR-item -> LR-item | #f
  ;; moves the dot to the right in the item, unless it is at its 
  ;; rightmost, then it returns false
  (define (move-dot-right i)
    (cond
     ((= (item-dot-pos i) (vector-length (prod-rhs (item-prod i)))) #f)
     (else (make-item (item-prod i)
		      (add1 (item-dot-pos i))))))

  ;; sym-at-dot: LR-item -> gram-sym | #f
  ;; returns the symbol after the dot in the item or #f if there is none  
  (define (sym-at-dot i)
    (let ((dp (item-dot-pos i))
          (rhs (prod-rhs (item-prod i))))
      (cond
       ((= dp (vector-length rhs)) #f)
       (else (vector-ref rhs dp)))))
  
 
  ;; print-item: LR-item ->
  (define (item->string it)
    (let ((print-sym (lambda (i)
		       (let ((gs (vector-ref (prod-rhs (item-prod it)) i)))
			 (cond
			  ((term? gs) (format "~a " (term-sym gs)))
			  (else (format "~a " (non-term-sym gs))))))))
      (string-append
       (format "~a -> " (non-term-sym (prod-lhs (item-prod it))))
       (let loop ((i 0))
	 (cond
	  ((= i (vector-length (prod-rhs (item-prod it)))) 
	   (if (= i (item-dot-pos it))
	       ". "
	       ""))
	  ((= i (item-dot-pos it)) 
	   (string-append ". " (print-sym i) (loop (add1 i))))
	  (else (string-append (print-sym i) (loop (add1 i)))))))))

  ;; --------------------- Grammar Symbols --------------------------

  (define (non-term<? nt1 nt2)
    (< (non-term-index nt1) (non-term-index nt2)))
  
  (define (term<? nt1 nt2)
    (< (term-index nt1) (term-index nt2)))

  (define (gram-sym-index gs)
    (cond
     ((term? gs) (term-index gs))
     (else (non-term-index gs))))

  (define (gram-sym-symbol gs)
    (cond
     ((term? gs) (term-sym gs))
     (else (non-term-sym gs))))

  (define (gram-sym->string gs)
    (symbol->string (gram-sym-symbol gs)))

  ;; term-list->bit-vector: term list -> int
  ;; Creates a number where the nth bit is 1 if the term with index n is in 
  ;; the list, and whose nth bit is 0 otherwise
  (define (term-list->bit-vector terms)
    (cond
      ((null? terms) 0)
      (else 
       (bitwise-ior (arithmetic-shift 1 (term-index (car terms))) (term-list->bit-vector (cdr terms))))))
  

  ;; ------------------------- Grammar ------------------------------

  (define grammar%
    (class object%
      (super-instantiate ())
      ;; prods: production list list
      ;; where there is one production list per non-term
      (init prods)
      ;; init-prods: production list
      ;; The productions parsing can start from
      ;; nullable-non-terms is indexed by the non-term-index and is true iff non-term is nullable
      (init-field init-prods terms non-terms end-terms)
  
      ;; list of all productions
      (define all-prods (apply append prods))
      (define num-prods (length all-prods))
      (define num-terms (length terms))
      (define num-non-terms (length non-terms))
      
      (let ((count 0))
        (for-each
         (lambda (nt)
           (set-non-term-index! nt count)
           (set! count (add1 count)))
         non-terms))
      
      (let ((count 0))
        (for-each
         (lambda (t)
           (set-term-index! t count)
           (set! count (add1 count)))
         terms))

      (let ((count 0))
        (for-each
         (lambda (prod)
           (set-prod-index! prod count)
           (set! count (add1 count)))
         all-prods))
      
      ;; indexed by the index of the non-term - contains the list of productions for that non-term
      (define nt->prods
        (let ((v (make-vector (length prods) #f)))
          (for-each (lambda (prods)
                      (vector-set! v (non-term-index (prod-lhs (car prods))) prods))
                    prods)
          v))
      
      (define nullable-non-terms
        (nullable all-prods num-non-terms))
            
      (define/public (get-num-terms) num-terms)
      (define/public (get-num-non-terms) num-non-terms)
      
      (define/public (get-prods-for-non-term nt)
        (vector-ref nt->prods (non-term-index nt)))
      (define/public (get-prods) all-prods)
      (define/public (get-init-prods) init-prods)
      
      (define/public (get-terms) terms)
      (define/public (get-non-terms) non-terms)
      
      (define/public (get-num-prods) num-prods)
      (define/public (get-end-terms) end-terms)
      
      (define/public (nullable-non-term? nt)
        (vector-ref nullable-non-terms (non-term-index nt)))

      (define/public (nullable-after-dot? item)
        (let* ((rhs (prod-rhs (item-prod item)))
               (prod-length (vector-length rhs)))
          (let loop ((i (item-dot-pos item)))
            (cond
              ((< i prod-length)
               (if (and (non-term? (vector-ref rhs i)) (nullable-non-term? (vector-ref rhs i)))
                   (loop (add1 i))
                   #f))
              ((= i prod-length) #t)))))
      
      (define/public (nullable-non-term-thunk)
        (lambda (nt)
          (nullable-non-term? nt)))
      (define/public (nullable-after-dot?-thunk)
        (lambda (item)
          (nullable-after-dot? item)))))
  
  
  ;; nullable: production list * int -> non-term set
  ;; determines which non-terminals can derive epsilon
  (define (nullable prods num-nts)
    (letrec ((nullable (make-vector num-nts #f))
	     (added #f)
	     
	     ;; possible-nullable: producion list -> production list
	     ;; Removes all productions that have a terminal
	     (possible-nullable
	      (lambda (prods)
		(filter (lambda (prod)
			  (vector-andmap non-term? (prod-rhs prod)))
			prods)))
	     
	     ;; set-nullables: production list -> production list
	     ;; makes one pass through the productions, adding the ones
	     ;; known to be nullable now to nullable and returning a list
	     ;; of productions that we don't know about yet. 
	     (set-nullables
	      (lambda (prods)
		(cond
                  ((null? prods) null)
                  ((vector-ref  nullable 
                                (gram-sym-index (prod-lhs (car prods))))
                   (set-nullables (cdr prods)))
                  ((vector-andmap (lambda (nt) 
                                    (vector-ref nullable (gram-sym-index nt)))
                                  (prod-rhs (car prods)))
                   (vector-set! nullable 
                                (gram-sym-index (prod-lhs (car prods)))
                                #t)
                   (set! added #t)
                   (set-nullables (cdr prods)))
                  (else
                   (cons (car prods) 
                         (set-nullables (cdr prods))))))))
      
      (let loop ((P (possible-nullable prods)))
	(cond
          ((null? P) nullable)
          (else
           (set! added #f)
           (let ((new-P (set-nullables P)))
             (if added
                 (loop new-P)
                 nullable)))))))

  
)
