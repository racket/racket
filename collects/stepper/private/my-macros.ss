(module my-macros mzscheme

  (require-for-syntax (lib "list.ss"))
                       
  ;;;;;;;;;;
  ;;
  ;;  paul graham's [ _ ] macro
  ;;
  ;;;;;;;;;;
  
  (provide lx)
  
  (define-syntax (lx stx)
    (syntax-case stx ()
      [(lx term)
       (with-syntax ([binder (datum->syntax-object (syntax term) `_)])
         (syntax (lambda (binder) term)))]))
  
  
  
  ;;;;;;;;;;
  ;;
  ;;  ccond implementation
  ;; 
  ;;;;;;;;;;
  
(provide ccond)
  
  (define-syntax (ccond stx)
    (syntax-case stx ()
      [(_ (question answer) ...)
       (syntax
	(cond
          (question answer) ...
          (else (error 'ccond "fell off end of cond expression"))))]))
  
              
  
  ;;;;;;;;;;
  ;;
  ;;  2vals implementation
  ;; 
  ;;;;;;;;;;
  
  (provide 2vals let*-2vals 2vals-first 2vals-second 2vals-map)
  
  (define 2vals vector)
  
  (define-syntax (let*-2vals stx)
    (syntax-case stx (let*-2vals)
      [(let*-2vals () . bodies)
       (syntax/loc stx (begin . bodies))]
      [(let*-2vals ([(id-a id-b) rhs] binding ...) . bodies)  ; 2 values in a vector
       (syntax/loc stx (let* ([_a rhs] [id-a (vector-ref _a 0)] [id-b (vector-ref _a 1)])
                         (let*-2vals (binding ...) . bodies)))]
      [(let*-2vals ([id-a rhs] binding ...) . bodies)         ; just 1 value
       (syntax/loc stx (let* ([id-a rhs]) 
                         (let*-2vals (binding ...) . bodies)))]))
  
  (define-syntax (2vals-first stx)
    (syntax-case stx (2vals-first)
      [(2vals-first a)
       (syntax (vector-ref a 0))]))
  
  (define-syntax (2vals-second stx)
    (syntax-case stx (2vals-second)
      [(2vals-second a)
       (syntax (vector-ref a 1))]))

 ; 2vals-map : (('a -> (2vals 'b 'c)) ('a list)) -> (2vals ('b list) ('c list))
  ;  dual-map is like map, only for a procedure that returns (values a b), and its
  ;  result is (values a-list b-list)... the contract specifies this more clearly.
  
  (define (2vals-map f . lsts)
    (if (null? (car lsts))
        (2vals null null)
        (let*-2vals ([(a b) (apply f (map car lsts))]
                     [(a-rest b-rest) (apply 2vals-map f (map cdr lsts))])
          (2vals (cons a a-rest) (cons b b-rest))))))

; test cases
; (require my-macros)
;
;(= (2vals-first (2vals 3 4)) 3)
;(= (2vals-second (2vals 3 4)) 4)
;
;(= 
; (let*-2vals
;     ([a (2vals 3 4)]
;      [(b c) a])
;   a
;   c)
; 4)
;
;(make-contract-checker my-type (lambda (x) (= x 3)))
;
;(contract-check-my-type? 3 'second-arg)
;;(contract-check-my-type? 14 'first-arg)
;
;((checked-lambda (x (y my-type) (z my-type))
;    (+ x y z))
; 3 3 5)
;
