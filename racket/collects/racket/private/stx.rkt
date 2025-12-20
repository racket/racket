;;----------------------------------------------------------------------
;; basic syntax utilities

(module stx '#%kernel
  (#%declare #:cross-phase-persistent)
  (#%declare #:require=define)
  
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

 
; (stx-make-id context chunk ...)
;   context : (or/c syntax? #f)
;   chunk   : (or/c symbol? string? identifier?)
;  -> identifier?
;
; Concatenates all the chunks together to form the textual component of an
; identifier; the lexical context is taken from `context`. For chunks which
; were identifiers, a `'sub-range-binders` property is added to the output
; identifier. When `syntax-transforming?`, it is assumed that all input
; identifiers and the output identifier need to be `syntax-local-introduce`'d
; before including in the sub-range-binders list.
;
; The call `(stx-make-id context chunk ...)` is similar in spirit to
; `(format-id context "~a~a~a...~a" chunk ... #:subs? #t)`.
(define-values (stx-make-id)
  (lambda (lex . all-chunks)
    (define-values (intro)
      (if (syntax-transforming?) syntax-local-introduce values))
    ; args
    (if (if lex (syntax? lex) #t)
        (void)
        (raise-argument-error 'format-identifier- "(or/c syntax? #f)" lex))
    (for-each (lambda (chunk)
                (if (if (identifier? chunk) #t
                        (if (symbol? chunk) #t
                            (string? chunk)))
                    (void)
                    (raise-argument-error 'format-identifier- "(or/c symbol? string? identifier?)" chunk)))
              all-chunks)
    ; Prepare for loop
    (define-values (loop)
      (lambda (strings-reversed  ; strings to concat (reverse order) into the output identifier
               srb-specs         ; (listof (list/c from-start from-end to-id to-start to-end))
               next-index        ; cumulative length of strings-reversed
               remaining-chunks) ; chunks not yet processed into strings-reversed+srb-specs
        (if (null? remaining-chunks)
            ; If no chunks left, construct tentative output...
            (let-values ([(syn) (datum->syntax lex
                                               (string->symbol (apply string-append-immutable
                                                                      (reverse strings-reversed))))])
              (if (null? srb-specs)
                  syn
                  ; ...and, if there were any sub-range-binders, add them; need to add the
                  ; output id (now that we know it) onto the front of each sub-range-binders.
                  (syntax-property syn
                                   'sub-range-binders
                                   (map (lambda (srb-spec)
                                          (list->vector (cons (intro syn) srb-spec)))
                                        srb-specs))))
            ; Else...
            (let-values ()
              ; Stringify first chunk...
              (define-values (fst rst-chunks) (values (car remaining-chunks) (cdr remaining-chunks)))
              (define-values (fst/not-syn) (if (syntax? fst) (syntax-e fst) fst))
              (define-values (fst/str) (if (symbol? fst/not-syn) (symbol->immutable-string fst/not-syn) fst/not-syn))
              ; ...derive new sub-range-binder list....
              (define-values (len) (string-length fst/str))
              (define-values (new-srbs) (if (syntax? fst)
                                            (cons (list next-index (+ next-index len) (intro fst) 0 len)
                                                  srb-specs)
                                            srb-specs))
              ; ...and loop.
              (loop (cons fst/str strings-reversed)
                    new-srbs
                    (+ next-index len)
                    rst-chunks)))))
    (loop null null 0 all-chunks)))

  (#%provide identifier? stx-null? stx-null/#f stx-pair? stx-list?
             stx-car stx-cdr stx->list
             stx-vector? stx-vector-ref
             stx-box?
             stx-prefab?
             stx-check/esc cons/#f append/#f
             stx-rotate stx-rotate*
             split-stx-list
             make-stx-id-counter
             stx-make-id))
