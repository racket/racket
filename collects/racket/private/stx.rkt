;;----------------------------------------------------------------------
;; basic syntax utilities

(module stx '#%kernel

  ;; These utilities facilitate operations on syntax objects.
  ;; A syntax object that represents a parenthesized sequence
  ;; can contain a mixture of cons cells and syntax objects,
  ;; hence the need for `stx-null?', `stx-car', etc.

  ;; a syntax identifier?
  (define-values (identifier?)
    (lambda (p)
      (if (syntax? p)
	  (symbol? (syntax-e p))
	  #f)))

  ;; a syntax null?
  (define-values (stx-null?)
    (lambda (p)
      (if (null? p)
	  #t
	  (if (syntax? p) 
	      (null? (syntax-e p))
	      #f))))

  ;; null if a syntax null?, else #f
  (define-values (stx-null/#f)
    (lambda (p)
      (if (null? p)
	  null
	  (if (syntax? p) 
	      (if (null? (syntax-e p))
		  null
		  #f)
	      #f))))

  ;; a syntax pair?
  (define-values (stx-pair?)
    (lambda (p)
      (if (pair? p)
	  #t
	  (if (syntax? p)
	      (pair? (syntax-e p))
	      #f))))

  ;; a syntax list?
  (define-values (stx-list?)
    (lambda (p)
      (if (list? p)
	  #t
	  (if (syntax? p) 
	      (if (list? (syntax-e p))
		  #t
		  (letrec-values ([(loop)
                                   (lambda (l)
                                     (if (pair? l)
                                         (loop (cdr l))
                                         (stx-list? l)))])
                    (loop (syntax-e p))))
	      (if (pair? p)
		  (stx-list? (cdr p))
		  #f)))))

  ;; car of a syntax pair
  (define-values (stx-car)
    (lambda (p)
      (if (pair? p)
	  (car p)
	  (car (syntax-e p)))))

  ;; cdr of a syntax pair
  (define-values (stx-cdr)
    (lambda (p)
      (if (pair? p)
	  (cdr p)
	  (cdr (syntax-e p)))))

  ;; Flattens a syntax list into a list
  (define-values (stx->list)
    (lambda (e)
      (if (syntax? e)
	  (syntax->list e)
	  (let-values ([(flat-end)
                        (letrec-values ([(loop)
                                         (lambda (l)
                                           (if (null? l) 
                                               #f
                                               (if (pair? l)
                                                   (loop (cdr l))
                                                   (if (syntax? l) 
                                                       (syntax->list l)
                                                       #f))))])
                          (loop e))])
	    (if flat-end
		;; flatten
		(letrec-values ([(loop)
                                 (lambda (l)
                                   (if (null? l) 
                                       null
                                       (if (pair? l) 
                                           (cons (car l) (loop (cdr l)))
                                           (if (syntax? l) 
                                               flat-end
                                               #f))))])
                  (loop e))
		e)))))

  ;; a syntax vector?
  (define-values (stx-vector?)
    (lambda (p len)
      (if (syntax? p) 
	  (if (vector? (syntax-e p))
	      (if len
		  (= len (vector-length (syntax-e p)))
		  #t)
	      #f)
	  #f)))

  ;; syntax vector reference
  (define-values (stx-vector-ref)
    (lambda (p pos)
      (vector-ref (syntax-e p) pos)))

  (define-values (stx-prefab?)
    (lambda (key v)
      (if (syntax? v)
          (equal? key (prefab-struct-key (syntax-e v)))
          #f)))

  ;; used in pattern-matching with an escape proc
  (define-values (stx-check/esc)
    (lambda (v esc)
      (if v
	  v
	  (esc #f))))

  ;; used in pattern-matching where #f on the cdr
  ;; is a failure
  (define-values (cons/#f)
    (lambda (i l)
      (if l
	  (cons i l)
	  #f)))

  ;; used in pattern-matching where either
  ;;  list can be a failure; if it's null, the first
  ;;  part might be an improper list
  (define-values (append/#f)
    (lambda (l1 l2)
      (if l1
	  (if l2
	      (if (null? l2)
		  l1
		  (append l1 l2))
	      #f)
	  #f)))

  ;; The rotate procedures are used to
  ;;  rotate a list of matches with multiple variables to
  ;;  get a list of multiple matches for single variables

  (define-values (stx-rotate)
    (lambda (l)
      (apply map list l)))

  (define-values (stx-rotate*)
    (lambda (l)
      (apply list* (apply map list l))))

  ;; The split procedure is used when matching ellipses
  ;;  followed by a certain number of patterns
  (define-values (split-stx-list)
    (lambda (s n prop?)
      (let-values ([(pre post m)
		    (letrec-values ([(loop)
                                     (lambda (s)
                                       (if (stx-pair? s)
                                           (let-values ([(pre post m) (loop (stx-cdr s))])
                                             (if (< m n)
                                                 (values '() s (add1 m))
                                                 (values (cons (stx-car s) pre) post m)))
                                           (values '() s (if prop?
                                                             (if (stx-null? s) 
                                                                 0 
                                                                 -inf.0)
                                                             1))))])
                      (loop s))])
	(values pre post (= m n)))))

  (define-values (intro) #f)
  (define-values (gen-temp-id)
    ;; Even though we gensym, using an introducer helps the
    ;;  syntax system simplify renamings that can't apply
    ;;  to other identifiers (when the generated identifier
    ;;  is used as a binding id)
    (lambda (pfx)
      (if intro
          (void)
          (set! intro (make-syntax-introducer)))
      (intro (datum->syntax #f (gensym pfx)))))

  (#%provide identifier? stx-null? stx-null/#f stx-pair? stx-list?
             stx-car stx-cdr stx->list
             stx-vector? stx-vector-ref
             stx-prefab?
             stx-check/esc cons/#f append/#f
             stx-rotate stx-rotate*
             split-stx-list
             gen-temp-id))
