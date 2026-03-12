;;----------------------------------------------------------------------
;; basic syntax utilities

(module stx '#%kernel
  (#%declare #:cross-phase-persistent)
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

  ;; a syntax box?
  (define-values (stx-box?)
    (lambda (p)
      (if (syntax? p) 
	  (if (box? (syntax-e p))
	      #t
	      #f)
	  #f)))

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

  ; Used to generate identifiers in low-level macros which need a variable number
  ; of temporaries
  (define-values (make-stx-id-counter)
    (lambda (prefix)
      (let-values ([(counter) 0])
        (lambda ()
          (set! counter (add1 counter))
          (string->uninterned-symbol (format "~a~a" prefix counter))))))

  ; (raise-syntax-error-if condition msg stx [part-stx])
  ;   condition : boolean?
  ;   msg       : string?   (suitable for 2nd argument of `raise-syntax-error`)
  ;   stx       : syntax?   (suitable for 3rd argument of `raise-syntax-error`)
  ;   part-stx  : (or/c syntax? #f) = #f  (suitable for 4th argument of `raise-syntax-error`)
  ;  -> (if/c condition none/c void?)
  (define-values (raise-syntax-error-if)
    (case-lambda
      [(condition msg stx part-stx)
       (if condition (raise-syntax-error #f msg stx part-stx) (void))]
      [(condition msg stx)
       (if condition (raise-syntax-error #f msg stx #f) (void))]))

  ; (raise-syntax-error-if condition msg stx [part-stx])
  ;   condition : boolean?
  ;   msg       : string?   (suitable for 2nd argument of `raise-syntax-error`)
  ;   stx       : syntax?   (suitable for 3rd argument of `raise-syntax-error`)
  ;   part-stx  : (or/c syntax? #f) = #f  (suitable for 4th argument of `raise-syntax-error`)
  ;  -> (if/c condition void? none/c)
  (define-values (raise-syntax-error-unless)
    (case-lambda
      [(condition msg stx part-stx)
       (if condition (void) (raise-syntax-error #f msg stx part-stx))]
      [(condition msg stx)
       (if condition (void) (raise-syntax-error #f msg stx #f))]))

  ; (stx-bound-id-member? id id-list)
  ;   id      : identifier?
  ;   id-list : (listof identifier?)
  ;  -> (or/c identifier? #f)
  ; Returns the first id in `id-list` which is `bound-identifier=?` to `id`
  (define-values (stx-bound-id-member?)
    (lambda (id l)
      (if (null? l)
          #f
          (if (bound-identifier=? id (car l))
              (car l)
              (stx-bound-id-member? id (cdr l))))))

  ; (stx-first-duplicate-id id-list)
  ;   id-list : (listof identifier?)
  ;  -> (or/c identifier? #f)
  ; Returns the first id in `id-list` which is bound-identifier=? a preceding
  ; identifier in `id-list`. This is identical to `check-duplicate-identifier`
  ; from racket/base, but does not do explicit argument type checks. This
  ; implementation is based on a previous implementation which lived in a file
  ; `core-syntax.rkt` and was used for let handling. The choice of length 5 to
  ; decide between the hash-based and quadratic implementations has not been
  ; benchmarked recently and perhaps should be.
  (define-values (stx-first-duplicate-id)
    (lambda (lst)
      (if ((length lst) . > . 5)
          (let-values ([(ht) (make-hasheq)])
            (letrec-values ([(check) (lambda (lst)
                                       (if (null? lst)
                                           #f
                                           (let-values ([(id) (car lst)])
                                             (let-values ([(alts) (hash-ref ht (syntax-e id) null)])
                                               (let-values ([(alt) (stx-bound-id-member? id alts)])
                                               (if (stx-bound-id-member? id alts)
                                                   alt
                                                   (begin
                                                     (hash-set! ht (syntax-e id) (cons id alts))
                                                     (check (cdr lst)))))))))])
              (check lst)))
          (letrec-values ([(check) (lambda (lst)
                                     (if (null? lst)
                                         #f
                                         (let-values ([(alt) (stx-bound-id-member? (car lst) (cdr lst))])
                                           (if alt
                                               alt
                                               (check (cdr lst))))))])
            (check lst)))))


  (#%provide identifier? stx-null? stx-null/#f stx-pair? stx-list?
             stx-car stx-cdr stx->list
             stx-vector? stx-vector-ref
             stx-box?
             stx-prefab?
             stx-check/esc cons/#f append/#f
             stx-rotate stx-rotate*
             split-stx-list
             make-stx-id-counter
             raise-syntax-error-if
             raise-syntax-error-unless
             stx-bound-id-member?
             stx-first-duplicate-id))
