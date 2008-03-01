(module extra-generators mzscheme
  (provide :combinations
           :do-until
           :iterate
           :let-values
           :list-by
           :match
           :pairs
           :pairs-by
           :plt-match
           :repeat
           :vector-combinations)
  
  (require "comprehensions.ss"
           (only (lib "43.ss" "srfi") vector-copy))
  
  ;;;
  ;;; :let-values
  ;;;
  
  (define-syntax :let-values
    (syntax-rules (index)
      ((:let-values cc var (index i) expression)
       (:do cc (let-values ((var expression) (i 0))) () #t (let ()) #f ()))
      ((:let-values cc var expression)
       (:do cc (let-values ((var expression))) () #t (let ()) #f ()) )))
  
  ;;;
  ;;; :match
  ;;;

  (define-syntax :match
    (syntax-rules (index)
      ((:match cc pat (index i) expression)
       (:do cc (let-match ((pat expression) (i 0))) () #t (let ()) #f ()))
      ((:match cc pat expression)
       (:do cc (let-match ((pat expression))) () #t (let ()) #f ()) )))

  ;;;
  ;;; :plt-match
  ;;;
  
  (define-syntax :plt-match
    (syntax-rules (index)
      ((:plt-match cc pat (index i) expression)
       (:do cc (let-plt-match ((pat expression) (i 0))) () #t (let ()) #f ()))
      ((:plt-match cc pat expression)
       (:do cc (let-plt-match ((pat expression))) () #t (let ()) #f ()) )))
  
  
  ;;; Combinations
  
  ; The problem of generating all k combinations of the n numbers 
  ; 0,1,...,n-1 provides a nice example of the advanced :do-generator. 
  ; The list of 3,5-combinations are
  
  ;    (#3(0 1 2) #3(0 1 3) #3(0 1 4) #3(0 2 3) #3(0 2 4) #3(0 3 4) 
  ;     #3(1 2 3) #3(1 2 4) #3(1 3 4) 
  ;     #3(2 3 4))
  
  ; The first combination is #(0 1 2) and the last combination is #3(2 3 4).
  ; Given helper funcions first-combination, last-combination?, and
  ; next-combination we can use the advanced :do-generator as follows.
  
  (define (vr v i)                  (vector-ref v i))
  (define (vs! v i x)               (vector-set! v i x))
  (define (incrementable? v i k n)  (< (vr v i) (+ n (- k) i)))
  
  (define (last-combination? k n v) (= (vr v 0) (- n k)))
  
  (define (first-combination k n)
    (if (<= 1 k n)
        (vector-ec (: i 0 k) i)
        #f))
  
  (define (next-combination k n v)
    (last-ec #f ; default, when there is no next combination
             (:let v (vector-copy v))
             ; find the last incrementable index
             (:let i (last-ec #f (:until (: i (- k 1) -1 -1) 
                                         (incrementable? v i k n)) 
                              i))
             (if i)
             ; increment index i and fix indices to the right of i
             (:parallel (: j i k)
                        (: vj (+ (vr v i) 1) n))
             (begin (vs! v j vj))
             ; if all indices is fixed we have a new combination
             (if (= j (- k 1)))
             ; return the new combination
             v))
  
  ;;;
  ;;; Fixed number of repetitions  :repeat
  ;;;
  
  ; Let us start with a simple generator. First we write examples on 
  ; how to use it:
  
  ; (list-ec (:repeat 5) 
  ;          1)            ; => (list 1 1 1 1 1)
  
  ; (list-ec (:repeat 3) 
  ;          (:repeat 2)
  ;          1)            ; => (1 1 1 1 1 1)
  
  (define-syntax :repeat
    (syntax-rules (index)
      ((:repeat cc expr)
       (:range cc i expr))
      ((:repeat cc expr (index i))
       (:range cc i (index j) expr))))
  
  ;;;
  ;;; Iteration  :iterate
  ;;;
  
  ; An iterative process can be seen as a triple
  ; of an initial state, a transition function next-state
  ; from state to state, and a predicate end-state? that 
  ; determines whether and terminal state has been reached.
  
  ; Using the simple version of :do we can define an
  ; :iterate generator like this:
  
  (define-syntax :iterate
    (syntax-rules (index)
      [(:iterate cc state initial-state next-state end-state?)
       (:do cc 
            ((state initial-state)) 
            (not (end-state? state))
            ((next-state state)))]
      [(:iterate cc state (index i) initial-state next-state end-state?)
       (:parallel cc  (:integers i)
                  (:iterate state initial-state next-state end-state?))]))
  
  ;;;
  ;;; Pairs of a list
  ;;;
  
  ; The normal :list generator allows one to work with the elements
  ; of a list. In order to work with the pairs of the list, we
  ; define :pairs that generate the pairs of the list.
  
  (define-syntax :pairs
    (syntax-rules (index)
      ((:pairs cc p l)
       (:iterate cc p l cdr null?))
      ((:pairs cc p (index i) l)
       (:iterate cc p (index i) l cdr null?))))
  
  (define-syntax :pairs-by
    (syntax-rules (index)
      ((:pairs-by cc p (index i) l)           (:pairs-by cc p (index i) l cdr))
      ((:pairs-by cc p (index i) l next)      (:pairs-by cc p (index i) l next null?))
      ((:pairs-by cc p (index i) l next end?) (:iterate  cc p (index i) l next end?))
      
      ((:pairs-by cc p l)                     (:pairs-by cc p l cdr))
      ((:pairs-by cc p l next)                (:pairs-by cc p l next null?))
      ((:pairs-by cc p l next end?)           (:iterate  cc p l next end?))))
  
  
  ;;; Combinations :combinations, :vector-combinations
  
  ; In the section on the advanced :do-generator we showed that
  ; how to use :do to generate all k,n-combinations of the
  ; indices 0,1,...,n-1. 
  
  ; We can use this to define a the :combinations generator
  ; that generates all k combinations of elements from a 
  ; given list l.
  
  (define (indices->list indices elements)
    ; (indices->list '#(0 1 4) '#(a b c d e))  => (a b e)
    (list-ec (:vector i indices)
             (vector-ref elements i)))
  
  (define-syntax :combinations
    (syntax-rules (index)
      ((:combinations cc lc (index i) k l)
       (:parallel cc (:integers i) (:combinations lc k l)))
      ((:combinations cc lc k l)
       (:do cc
            (let ((n (length l))
                  (v (list->vector l))))
            ((c (first-combination k n)))
            c
            (let ((lc (indices->list c v))))
            (not (last-combination? k n c))
            ((next-combination k n c))))))
  
  ; The vector version is similar.
  
  (define (indices->vector k indices elements)
    (vector-of-length-ec k
                         (:vector i indices)
                         (vector-ref elements i)))
  
  (define-syntax :vector-combinations
    (syntax-rules (index)
      ((:vector-combinations cc vc (index i) k v)
       (:parallel cc (:integers i) (:vector-combinations vc k v)))
      ((:vector-combinations cc vc k v)
       (:do cc
            (let ((n (vector-length v))))
            ((c (first-combination k n)))
            c
            (let ((vc (indices->vector k c v))))
            (not (last-combination? k n c))
            ((next-combination k n c))))))

  
  ;;; An alternative to :do, the :do-until         
  
  ; The simple :do is a "do-while" loop. As we saw previously
  ; this we had to use the advanced :do-generator in order
  ; to write :list in terms of :do, due to the last element
  ; missing: 
  
  ; If only the the termination test were done *after* and
  ; not before the loop payload ...  This leads to the
  ; idea of an :do-until.
  
  (define-syntax :do-until
    (syntax-rules ()
      ((:do-until cc lb* ne1? ls*)
       (:do cc (let ()) lb* #t (let ()) (not ne1?) ls*))))
  
  
  ;;;
  ;;; A more flexible :list, the :list-by
  ;;;
  
  (define-syntax :list-by
    (syntax-rules (index)
      ((:list-by cc x (index i) l)           (:list-by cc x (index i) l cdr))
      ((:list-by cc x (index i) l next)      (:list-by cc x (index i) l next null?))
      ((:list-by cc x (index i) l next end?) (:parallel cc 
                                                        (:integers i)
                                                        (:do  (let ()) ((t l)) (not (end? t)) 
                                                              (let ((x (car t)))) #t ((next t)))))
      ((:list-by cc x l)                     (:list-by cc x l cdr))
      ((:list-by cc x l next)                (:list-by cc x l next null?))
      ((:list-by cc x l next end?)           (:do cc (let ()) ((t l)) (not (end? t)) (let ((x (car t)))) #t ((next t))))))
  
)