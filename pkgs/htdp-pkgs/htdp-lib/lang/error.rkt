(module error mzscheme
  (require mzlib/etc mzlib/list)
  ;; --------------------------------------------------------------------------
  (provide
   check-arg
   check-arity
   check-proc
   check-result
   check-list-list
   find-non
   ;; --- error constants 
   result-error ; String[format: String[expected], Any[given]]
   arg-error    ; String [format: String[expected], String[position] Any[given]]
   arity-error  ; String [format: String[expected] Any[given]]
   proc-error   ; String [format: String[expected] Any[given]]
   arity-error2 ; String [format: String[expected], String[position] Number[given]]
   )

  ;; (_ -> Boolean) (listof X) -> (union X false)
  (define (find-non pred? l)
    (let ([r (filter (compose not pred?) l)])
      (if (null? r) #f (car r))))

  #| Tests ------------------------------------------------------------------
  (not (find-non list? '((1 2 3) (a b c))))
  (symbol? (find-non number? '(1 2 3 a)))
  (symbol? (find-non list? '((1 2 3) a (b c))))
  |#

  ;; Symbol (union true String) String X -> void
  (define (check-list-list pname condition pred given)
    (when (string? condition)
      (error pname (string-append condition (format "\nin ~e" given)))))

  ;; Symbol (_ -> Boolean) String X -> X 
  (define (check-result pname pred? expected given)
    (if (pred? given) given (error pname result-error expected given)))

  ;; String[format: String[expected], Any[given]
  (define result-error "expected ~a result, given: ~e")

  ;; check-arg : sym bool str str TST -> void
  (define (check-arg pname condition expected arg-posn given)
    (unless condition (error pname arg-error expected arg-posn given)))
  
  ;; String [format: String[expected], String[position] Any[given]
  (define arg-error "expected <~a> as ~a argument, given: ~e")

  ;; check-arity : sym num (list-of TST) -> void
  (define (check-arity name arg# args)
    (let ([x (length args)])
      (if (>= x arg#) (void) (error name arity-error arg# x))))

  ;; String [format: String[expected] Any[given]
  (define arity-error "expects at least ~a arguments, given ~e")
  
  ;; String [format: String[expected] Any[given]
  (define proc-error "a function was expected as ~s argument, given ~e")
  
  ;; check-proc :
  ;;   sym (... *->* ...) num (union sym str) (union sym str) -> void
  (define (check-proc proc f exp-arity arg# arg-err)
    (unless (procedure? f) (error proc proc-error arg# f))
    (unless (procedure-arity-includes? f exp-arity)
      (let ([arity-of-f (procedure-arity f)])
        (error proc arity-error2
	  arg-err arg# 
	  (cond
	    [(number? arity-of-f)
	     (if (= arity-of-f 1)
		 (format "1 argument")
		 (format "~s arguments" arity-of-f))]
	    [(arity-at-least? arity-of-f) (format "at least ~s arguments" (arity-at-least-value arity-of-f))]
	    [else (format "multiple arities (~s)" arity-of-f)])))))

  ;; String [format: String[expected], String[position] Number[given]  
  (define arity-error2 "a function that expects ~a expected as ~s argument, given a function that expects ~a ")
  )
