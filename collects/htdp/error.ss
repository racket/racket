#cs(module error mzscheme
  (require (lib "etc.ss") (lib "list.ss"))
  ;; --------------------------------------------------------------------------
  (provide check-arg check-arity check-proc check-result check-list-list find-non
           tp-exn? number->ord)

  ;; (_ -> Boolean) (listof X) -> (union X false)
  (define (find-non pred? l)
    (let ([r (filter (compose not pred?) l)])
      (if (null? r) #f (car r))))

  #| Tests ------------------------------------------------------------------
  (not (find-non list? '((1 2 3) (a b c))))
  (symbol? (find-non number? '(1 2 3 a)))
  (symbol? (find-non list? '((1 2 3) a (b c))))
  |#

  (define-struct (tp-exn exn) ())
     
  (define (tp-error name fmt . args)
    (raise (make-tp-exn (string->immutable-string (string-append (format "~a: " name) (apply format fmt args)))
                        (current-continuation-marks))))

  (define (number->ord i)
    (if (= i 0)
        "zeroth"
        (case (modulo i 10)
          [(0 4 5 6 7 8 9) (format "~ath" i)]
          [(1) (format "~ast" i)]
          [(2) (format "~and" i)]
          [(3) (format "~ard" i)])))
     
  ;; Symbol (union true String) String X -> void
  (define (check-list-list pname condition pred given)
    (when (string? condition)
      (tp-error pname (string-append condition (format "~nin ~e" given)))))

  ;; Symbol (_ -> Boolean) String X -> X 
  (define (check-result pname pred? expected given)
    (if (pred? given)
	given
	(tp-error pname "expected ~a result, given: ~e" expected given)))

  ;; check-arg : sym bool str str TST -> void
  (define (check-arg pname condition expected arg-posn given)
    (unless condition
      (tp-error pname "expected <~a> as ~a argument, given: ~e"
	expected arg-posn given)))
  
  ;; check-arity : sym num (list-of TST) -> void
  (define (check-arity name arg# args)
    (if (= (length args) arg#)
        (void)
        (tp-error name "expects ~a arguments, given ~e" arg# (length args))))
  
  ;; check-proc :
  ;;   sym (... *->* ...) num (union sym str) (union sym str) -> void
  (define (check-proc proc f exp-arity arg# arg-err)
    (unless (procedure? f)
      (tp-error proc "procedure expected as ~s argument; given ~e" arg# f))
    (unless (procedure-arity-includes? f exp-arity)
      (let ([arity-of-f (procedure-arity f)])
        (tp-error proc "procedure of ~a expected as ~a argument; given procedure of ~a "
	  arg-err arg# 
	  (cond
	    [(number? arity-of-f)
	     (if (= arity-of-f 1)
		 (format "1 argument")
		 (format "~s arguments" arity-of-f))]
	    [(arity-at-least? arity-of-f) (format "at least ~s arguments" (arity-at-least-value arity-of-f))]
	    [else (format "multiple arities (~s)" arity-of-f)]))))))
