#lang racket

(require (only-in redex/examples/racket-machine/reduction -> load)
         (only-in redex/examples/racket-machine/model-impl compile-bytecode impl->model)
         "sexp-trie.rkt"
         redex/reduction-semantics
         rackunit)

(define capture-trace #f)
(define program-size 2)
(command-line
 #:once-each
 ["--capture-trace" trace-path
                    "Writes the terms inserted and looked up to the given path"
                    (set! capture-trace trace-path)]
 ["--program-size" n "The length of the list processed by the bytecode program"
                   (set! program-size (string->number n))]
 #:args () (void))

(define ((make-eval normal-forms) expr [cycles '()])
  (map (match-lambda
         [`((clos ,_) ,_ ,_ ,_ ()) (closure)]
         [`(,v ,_ ,_ ,_ ()) v]
         [`(,_ ,_ ,_ ,_ (,_ ,_ ...)) (stuck)]
         ['error (error)])
       (normal-forms (program expr cycles))))

(define (show-memory)
  (collect-garbage)
  (collect-garbage)
  (printf "~s MB\n" (exact->inexact (/ (current-memory-use) (expt 2 20)))))

(define eval-hash
  (make-eval 
   (λ (p)
     (let* ([seen (make-hash)]
            [finals '()]
            [go (λ (trace)
                  (time
                   (let loop ([state p])
                     (when trace (write (cons 'lookup state) trace))
                     (unless (hash-ref seen state #f)
                       (when trace (write (cons 'insert state) trace))
                       (hash-set! seen state #t)
                       (let ([succs (apply-reduction-relation -> state)])
                         (if (null? succs)
                             (set! finals (cons state finals))
                             (map loop succs)))))))])
       (if capture-trace
           (call-with-output-file #:exists 'truncate capture-trace go)
           (go #f))
       (show-memory)
       (fprintf (open-output-nowhere) "~s" seen)
       finals))))

(define trie-friendly
  (match-lambda
    [(list V S H T C)
     (list T (reverse H) (reverse C) (reverse S) V)]
    [x x]))

(define eval-trie
  (make-eval
   (λ (p)
     (let ([seen empty-sexp-trie]
           [finals '()])
       (time 
        (let loop ([state p])
          (define swapped (trie-friendly state))
          (unless (lookup swapped seen)
            (set! seen (insert swapped #t seen))
            (let ([succs (apply-reduction-relation -> state)])
              (if (null? succs)
                  (set! finals (cons state finals))
                  (map loop succs))))))
       (show-memory)
       (fprintf (open-output-nowhere) "~s" seen)
       finals))))

;; eval: e ((x e) ...) -> (listof result)
;; Evaluates a bytecode program.
;; result ::= v | closure | stuck
(define eval eval-hash)

(define (program expr cycles)
  (term (load ,expr ,cycles)))

(struct closure () #:transparent)
(struct stuck () #:transparent)
(struct error () #:transparent)

;; racket->bytecode: syntax -> (e ((x e) ...))
;; Compiles a Racket expression into bytecode.
(define racket->bytecode
  ;; make sure compilation doesn't insert unhandled debugging stuff
  (compose impl->model compile-bytecode))

(define a-racket-program
  #`(let ([cons (λ (x y) (λ (s) (s x y)))]
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
        (find (λ (x) x) #,(let loop ([n program-size] [tail #'(cons 1 (cons 2 null))])
                            (if (zero? n)
                                tail
                                (loop (sub1 n) #`(cons #f #,tail))))))))

(match-let ([(cons expr cycles) (racket->bytecode a-racket-program)])
  ;; Warm Redex's caches
  (parameterize ([current-output-port (open-output-nowhere)])
    (eval-hash expr cycles))
  
  (unless capture-trace
    (printf "Begin: ")
    (show-memory)
    (newline)
    
    (printf "Hash: ")
    (eval-hash expr cycles)
    (newline)
    
    (printf "Between: ")
    (show-memory)
    (newline)
    
    (printf "Trie: ")
    (eval-trie expr cycles)
    (newline)
    
    (printf "End: ")
    (show-memory)))
