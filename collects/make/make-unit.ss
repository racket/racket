
(module make-unit (lib "a-unit.ss")
  (require "make-sig.ss")

      (import)
      (export make^)

      (define-struct (exn:fail:make exn:fail) (target orig-exn))
      
      (define make-print-checking (make-parameter #t))
      (define make-print-dep-no-line (make-parameter #t))
      (define make-print-reasons (make-parameter #t))
      (define make-notify-handler (make-parameter void))

      ;(define-type line (list (union path-string (list-of path-string)) (list-of path-string) thunk))
      ;(define-type spec (list-of line))

      (define (path-string=? a b)
	(equal? (if (string? a) (string->path a) a)
		(if (string? b) (string->path b) b)))
      (define (path-string->string s)
	(if (string? s)
            s 
            (path->string s)))

      ; find-matching-line : path-string spec -> (union line #f)
      (define (find-matching-line str spec)
	(let ([match? (lambda (s) (path-string=? s str))])
	  (let loop ([lines spec])
	    (cond
	     [(null? lines) #f]
	     [else (let* ([line (car lines)]
			  [names (if (path-string? (car line))
				     (list (car line))
				     (car line))])
		     (if (ormap match? names)
			 line
			 (loop (cdr lines))))]))))

      ; form-error : TST TST -> a 
      (define (form-error s p) (error 'make/proc "~a: ~s" s p))

      ; line-error : TST TST TST -> a 
      (define (line-error s p n) (error 'make/proc "~a: ~s for line: ~a" s p n))

      ; check-spec : TST -> #t
      ; effect : calls error if input is not a spec
      (define (check-spec spec)
	(and (or (list? spec) (form-error "specification is not a list" spec))
	     (or (pair? spec) (form-error "specification is an empty list" spec))
	     (andmap
	      (lambda (line)
		(and (or (and (list? line) (<= 2 (length line) 3))
			 (form-error "list is not a list with 2 or 3 parts" line))
		     (or (or (path-string? (car line))
			     (and (list? (car line))
				  (andmap path-string? (car line))))
			 (form-error "line does not start with a path/string or list of paths/strings" line))
		     (let ([name (car line)])
		       (or (list? (cadr line))
			   (line-error "second part of line is not a list" (cadr line) name)
			   (andmap (lambda (dep)
				     (or (path-string? dep)
					 (form-error "dependency item is not a path/string" dep name)))
				   (cadr line)))
		       (or (null? (cddr line))
			   (and (procedure? (caddr line))
				(procedure-arity-includes? (caddr line) 0))
			   (line-error "command part of line is not a thunk" (caddr line) name)))))
	      spec)))

      ; check-spec : TST -> #t
      ; effect : calls error if input is not a (union path-string (vector-of path-string))
      (define (check-argv argv)
	(or (path-string? argv)
	    (and (vector? argv)
		 (andmap path-string? (vector->list argv)))
	    (raise-type-error 'make/proc "path/string or path/string vector" argv)))

      ; make/proc/helper : spec (union path-string (vector-of path-string)) -> void
      ; effect : make, according to spec and argv. See doc.txt for details
      (define (make/proc/helper spec argv)
	(check-spec spec)
	(check-argv argv)

	(letrec ([made null]
		 [make-file
		  (lambda (s indent)
		    (let ([line (find-matching-line s spec)]
			  [date (and (or (directory-exists? s)
					 (file-exists? s))
				     (file-or-directory-modify-seconds s))])

		      (when (and (make-print-checking)
				 (or line (make-print-dep-no-line)))
			(printf "make: ~achecking ~a~n" indent s)
                        (flush-output))

		      (if line
                        (let ([deps (cadr line)])
                          (for-each (let ([new-indent (string-append " " indent)])
                                      (lambda (d) (make-file d new-indent)))
                                    deps)
                          (let ([reason
                                 (or (not date)
                                     (ormap (lambda (dep)
                                              (unless (or (file-exists? dep)
                                                          (directory-exists? dep))
                                                (error 'make "dependancy ~a was not made~n" dep))
                                              (and (> (file-or-directory-modify-seconds dep) date)
                                                   dep))
                                            deps))])
                            (when reason
                              (let ([l (cddr line)])
                                (unless (null? l)
                                  (set! made (cons s made))
                                  ((make-notify-handler) s)
                                  (printf "make: ~amaking ~a~a~n"
                                          (if (make-print-checking) indent "")
                                          (path-string->string s)
                                          (if (make-print-reasons)
                                            (cond
                                              [(not date)
                                               (string-append " because " (path-string->string s) " does not exist")]
                                              [(path-string? reason)
                                               (string-append " because " (path-string->string reason) " changed")]
                                              [else
                                               (string-append 
                                                (format " because (reason: ~a date: ~a)" 
                                                        reason date))])
                                            ""))
                                  (flush-output)
                                  (with-handlers ([exn:fail?
                                                   (lambda (exn)
                                                     (raise (make-exn:fail:make 
                                                             (format "make: Failed to make ~a; ~a"
                                                                     (let ([fst (car line)])
                                                                       (if (pair? fst)
                                                                         (map path-string->string fst)
                                                                         (path-string->string fst)))
                                                                     (if (exn? exn)
                                                                       (exn-message exn)
                                                                       exn))
                                                             (if (exn? exn)
                                                               (exn-continuation-marks exn)
                                                               (current-continuation-marks))
                                                             (car line)
                                                             exn)))])
                                    ((car l))))))))
                        (unless date
                          (error 'make "don't know how to make ~a"
                                 (path-string->string s))))))])
	  (cond
            [(path-string? argv) (make-file argv "")]
            [(equal? argv #()) (make-file (caar spec) "")]
            [else (for-each (lambda (f) (make-file f "")) (vector->list argv))])

	  (for-each (lambda (item)
		      (printf "make: made ~a~n" (path-string->string item)))
		    (reverse made))
          (flush-output)))

      (define make/proc
	(case-lambda
	 [(spec) (make/proc/helper spec #())]
	 [(spec argv) (make/proc/helper spec argv)])))
