#lang racket

(require (only-in "reduction.rkt" -> load)
         (only-in "verification.rkt" bytecode-ok?)
         (only-in "model-impl.rkt" compile-bytecode impl->model)
         redex
         rackunit)

;; eval: e ((x e) ...) -> (listof result)
;; Evaluates a bytecode program.
;; result ::= v | closure | stuck
(define (eval expr [cycles '()])
  (map (match-lambda
         [`((clos ,_) ,_ ,_ ,_ ()) (closure)]
         [`(,v ,_ ,_ ,_ ()) v]
         [`(,_ ,_ ,_ ,_ (,_ ,_ ...)) (stuck)]
         ['error (error)])
       (let ([seen (make-hash)]
             [finals '()])
         (let loop ([state (program expr cycles)])
           (unless (hash-ref seen state #f)
             (hash-set! seen state #t)
             (let ([succs (apply-reduction-relation -> state)])
               (if (null? succs)
                   (set! finals (cons state finals))
                   (map loop succs)))))
         finals)))

(define (program expr cycles)
  (term (load ,expr ,cycles)))

(struct closure () #:transparent)
(struct stuck () #:transparent)
(struct error () #:transparent)

(check-equal? (eval '(application (proc-const (val) (loc 0)) 'y))
              '('y))
(check-equal? (eval '(application (indirect x) 'y) '((x (proc-const (val) (loc 0)))))
              '('y))
(check-equal? (eval '(proc-const (val) (loc 0)))
              (list (closure)))
(check-equal? (eval '(application (proc-const (val) (loc 0))))
              (list (error)))
(define bug-#3
  '(let-one (proc-const (val) (loc 0)) 
            (application 
             (loc-noclr 1) 
             (install-value 1 (proc-const (val) 'x) 
                            'y))))
(let ([sorted (λ (xs) (sort xs string<=? #:key (curry format "~s")))])
  (check-pred (negate bytecode-ok?) bug-#3)
  (check-equal? (sorted (eval bug-#3)) '('x 'y)))

(define (trace expr [cycles '()])
  (traces -> (program expr cycles)))
(define (step expr [cycles '()])
  (stepper -> (program expr cycles)))

(when (let ([pop-ups? #t])
        (command-line
         #:once-any
         ["--no-pop-ups"
          "Avoid opening the `traces' and `stepper' windows"
          (set! pop-ups? #f)])
        pop-ups?)
  (trace bug-#3)
  (step bug-#3))

;; racket->bytecode: syntax -> (e ((x e) ...))
;; Compiles a Racket expression into bytecode.
(define racket->bytecode
  (compose impl->model compile-bytecode))

(define a-racket-program
  #'(let ([cons (λ (x y) (λ (s) (s x y)))]
          [car (λ (p) (p (λ (x y) x)))]
          [cdr (λ (p) (p (λ (x y) y)))]
          [null #f]
          [null? (λ (x) (if x #f #t))])
      (letrec ([find (lambda (it? xs)
                       (if (null? xs)
                           #f
                           (let ([x (car xs)])
                             (if (it? x)
                                 x
                                 (find it? (cdr xs))))))])
        (find (λ (x) x) (cons #f (cons #f (cons 1 (cons 2 null))))))))

;; If this test fails, it's probably because the compiler has changed
;; and no longer maps this program into the subset supported in the
;; model. Try the version bundled with Redex (see the README).
(match-let ([(cons expr cycles) (racket->bytecode a-racket-program)])
  (check-equal? (eval expr cycles) '(1)))

;; Test mode: no windows:
(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "--no-pop-ups")])
    (dynamic-require (quote-module-path "..") #f)))
