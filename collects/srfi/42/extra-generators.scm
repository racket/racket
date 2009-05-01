;;;
;;; EXTRA GENERATORS (NOT IN SRFI-42)
;;;

(module extra-generators mzscheme
  (provide :let-values
	   :repeat
	   :iterate
	   :combinations
	   :vector-combinations
	   :do-until
	   :pairs
           :pairs-by
	   :list-by
	   :alist
	   :hash-table
	   :hash-table-keys
	   :hash-table-values
           indices->list
           indices->vector
           last-combination?
           next-combination
           first-combination)
  (require "ec-core.scm")
  (require-for-syntax "ec-core.scm")
  
  ;;; :let-values
  
  (define-generator (:let-values form-stx)
    (syntax-case form-stx (index)
      [(_ (var ...) (index i) expression)
       #'(:do (let-values ([(var ...) expression] [(i) 0])) () #t (let ()) #f ())]
      [(_ (var ...) expression)
       #'(:do (let-values ([(var ...) expression])) () #t (let ()) #f ())]
      [_
       (raise-syntax-error 
        ':let-values 
        "expected (:let-values (<var> ...) (index i) <expr> where the index is optional, got: "
        form-stx)]))
  
  ;;; :repeat   Fixed number of iterations
  
  ; (list-ec (:repeat 5) 1) => '(1 1 1 1 1)
  
  (define-generator (:repeat form-stx)
    (syntax-case form-stx (index)
      [(_ (index i) expr)
       #'(:range i expr)]
      [(_ expr)
       #'(:range i expr)]
      [_
       (raise-syntax-error
        ':repeat
        "expected (:repeat <expr>) ot (:repeat (index i) <expr>), got: "
        form-stx)]))
  
  ;;; Iteration  :iterate
  
  ; An iterative process can be seen as a triple
  ; of an initial state, a transition function next-state
  ; from state to state, and a predicate end-state? that
  ; determines whether and terminal state has been reached.
  
  ; (list-ec (:iterate e 0 (lambda (x) (+ x 2)) (lambda (x) (>= x 10))) 
  ;          e) ; => (0 2 4 6 8)
  
  (define-generator (:iterate stx)
    (syntax-case stx (index)
      [(:iterate state initial-state next-state end-state?)
       (begin
         (unless (identifier? #'state)
           (raise-syntax-error 
            ':iterate "expected variable (for the state), got: " #'state))
         #'(:do (let ((initial initial-state) (end? end-state?) (next next-state)))
                ((state initial))
                (not (end? state))
                (let ())
                #t
                ((next state))))]
      [(:iterate state (index i) initial-state next-state end-state?)
       (add-index stx #'(:iterate state initial-state next-state end-state?) #'i)]
      [_
       (raise-syntax-error
        ':iterate
        "expected (:iterate <state-var> <initial-state> <next-state> <end-state?>), got: "
        stx)]))
  
  
  ;;;; Combinations
  ;; The problem of generating all k combinations of the n numbers
  ;; 0,1,...,n-1 provides a nice example of the advanced :do-generator.
  ;; The list of 3,5-combinations are
  ;
  ;;    (#3(0 1 2) #3(0 1 3) #3(0 1 4) #3(0 2 3) #3(0 2 4) #3(0 3 4)
  ;;     #3(1 2 3) #3(1 2 4) #3(1 3 4)
  ;;     #3(2 3 4))
  ;
  ;; The first combination is #(0 1 2) and the last combination is #3(2 3 4).
  ;; Given helper funcions first-combination, last-combination?, and
  ;; next-combination we can use the advanced :do-generator as follows.
  ;
  (define-syntax vr  (syntax-rules () [(_ v i)   (vector-ref v i)]))
  (define-syntax vs! (syntax-rules () [(_ v i x) (vector-set! v i x)]))
  (define-syntax incrementable? (syntax-rules () [(_ v i k n)  (< (vr v i) (+ n (- k) i))]))
  (define-syntax last-combination? (syntax-rules () [(_ k n v) (= (vr v 0) (- n k))]))
  
  (define (first-combination k n)
    (if (<= 1 k n)
        (vector-ec (: i 0 k) i)
        #f))
  
  (define (vector-copy v)
    (vector-of-length-ec (vector-length v)
                         (:vector x v)
                         x))
  
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
  
  ;;;; Combinations :combinations, :vector-combinations
  ;
  ;; In the section on the advanced :do-generator we showed that
  ;; how to use :do to generate all k,n-combinations of the
  ;; indices 0,1,...,n-1.
  ;
  ;; We can use this to define a the :combinations generator
  ;; that generates all k combinations of elements from a
  ;; given list l.
  
  (define (indices->list indices elements)
    ; (indices->list '#(0 1 4) '#(a b c d e))  => (a b e)
    (list-ec (:vector i indices)
             (vector-ref elements i)))
  
  (define-generator (:combinations stx)
    (syntax-case stx (index)
      ((:combinations lc (index i) k l)
       #'(:parallel  (:integers i) (:combinations lc k l)))
      ((:combinations lc k l)
       #'(:do (let ((n (length l))
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
  
  (define-generator (:vector-combinations stx)
    (syntax-case stx (index)
      ((:vector-combinations vc (index i) k v)
       #'(:parallel (:integers i) (:vector-combinations vc k v)))
      ((:vector-combinations vc k v)
       #'(:do (let ((n (vector-length v))))
              ((c (first-combination k n)))
              c
              (let ((vc (indices->vector k c v))))
              (not (last-combination? k n c))
              ((next-combination k n c))))))
  
  ;;; An alternative to :do, the :do-until        
  
  ; The simple :do is a "do-while" loop. As we saw previously
  ; this we had to use the advanced :do-generator in order
  ; to write :list in terms of :do, due to the last element
  ; missing. Compare:
  
  ; (list-ec (:do-until ((x 0)) (> x 5) ((+ x 1))) x)
  ;    => '(0 1 2 3 4 5 6)
  
  ; (list-ec (:do ((x 0)) (<= x 5) ((+ x 1))) x)
  ;    => '(0 1 2 3 4 5)
  
  ; If only the termination test were done *after* and
  ; not before the loop payload ...  This leads to the
  ; idea of an :do-until.
  
  (define-generator (:do-until stx)
    (syntax-case stx ()
      [(:do-until lb* ne1? ls*)
       #'(:do (let ()) lb* #t (let ()) (not ne1?) ls*)]
      [_
       (raise-syntax-error
        ':do-until
        "expected (:do-until <loop-bindings> <not-end?> <loop-steppers>), got: "
        stx)]))
  
  ;;;
  ;;; Pairs of a list
  ;;;
  
  ; The normal :list generator allows one to work with the elements
  ; of a list. In order to work with the pairs of the list, we
  ; define :pairs that generate the pairs of the list.
  
  ;  (list-ec (:pairs p '(1 2 3)) p) ; => '(1 2 3) (2 3) (3))
  
  (define-generator (:pairs stx)
    (syntax-case stx (index)
      [(:pairs p (index i) l)
       (add-index stx #'(:pairs p l) #'i)]
      [(:pairs p l)
       (begin 
         (unless (identifier? #'p)
           (raise-syntax-error 
            ':pairs "expected identifier to bind, got: " #'p))
         #'(:iterate p l cdr null?))]
      [_ 
       (raise-syntax-error
        ':pairs
        "expected (:pairs <var> (index <var>) <expr>), got: "
        stx)]))
  
  (define-generator (:pairs-by stx)
    (syntax-case stx (index)
      ((:pairs-by p (index i) l)           #'(:pairs-by p (index i) l cdr))
      ((:pairs-by p (index i) l next)      #'(:pairs-by p (index i) l next null?))
      ((:pairs-by p (index i) l next end?) (add-index stx #'(:iterate  p l next end?) #'i))
      
      ((:pairs-by p l)                     #'(:pairs-by p l cdr))
      ((:pairs-by p l next)                #'(:pairs-by p l next null?))
      ((:pairs-by p l next end?)           #'(:iterate  p l next end?))
      (_
       (raise-syntax-error
        ':pairs-by 
        (string-append
         "expected (:pairs-by <var> (index var) <list-expr> <next-expr> <end-expr>), where "
         "the index is optional, and the defaults for <next-expr> and <end-expr> are cdr and null?. Got: ")
        stx))))
  
  ;;;
  ;;; A more flexible :list, the :list-by
  ;;;
  
  (define-generator (:list-by stx)
    (syntax-case stx (index)
      ((:list-by x (index i) l)
       #'(:list-by  x (index i) l cdr))
      ((:list-by x (index i) l next)
       #'(:list-by x (index i) l next null?))
      ((:list-by x (index i) l next end?)
       (add-index stx #'(:do  (let ()) ((t l)) (not (end? t)) 
                              (let ((x (car t)))) #t ((next t)))
                  #'i))
      ((:list-by x l)
       #'(:list-by x l cdr))
      ((:list-by x l next)
       #'(:list-by x l next null?))
      ((:list-by x l next end?)
       #'(:do (let ()) ((t l)) (not (end? t)) (let ((x (car t)))) #t ((next t))))
      (_
       (raise-syntax-error
        ':list-by
        (string-append
         "expected (:list-by x (index <id>) <list-expr> <next-expr> <end-expr>), where "
         "the (index <id>), <next-exp>, and <end-expr> are optional, got: ")
        stx))))
  
  (define-generator (:alist stx)
    (syntax-case stx (index)
      [(:alist vars (index i) al-expr)
       (add-index stx #'(:alist vars al-expr) #'i)]
      [(:alist (key val) al-expr)
       #'(:do (let ([al al-expr]))
              ((al al))
              (not (null? al))
              (let-values ([(key val) (values (caar al) (cdar al))]))
              #t
              ((cdr al)))]))
  
  
  (define-generator (:hash-table stx)
    (syntax-case stx (index)
      [(:hash-table vars (index i) ht-expr)
       (add-index stx #'(:hash-table vars ht-expr) #'i)]
      [(:hash-table (key-var val-var) ht-expr)
       #'(:alist (key-var val-var) (hash-table-map ht-expr cons))]
      [(:hash-table var ht-expr)
       #'(:list var (hash-table-map ht-expr cons))]
      [_
       (raise-syntax-error 
        ':hash-table
        "expected (:hash-table (<key-var> <val-var>) <ht-expr>) or (:hash-table <var> <ht-expr>) "
        stx)]))
  
  (define-generator (:hash-table-keys stx)
    (syntax-case stx (index)
      [(_ var (index i) ht-expr)
       (add-index stx #'(:hash-table-keys vars ht-expr) #'i)]
      [(_ var ht-expr)
       #'(:list var (hash-table-map ht-expr (lambda (k v) k)))]
      [_
       (raise-syntax-error 
        ':hash-table-keys
        "expected (:hash-table-keys <var> (index <var>) <ht-expr>) where the index is optional "
        stx)]))
  
  (define-generator (:hash-table-values stx)
    (syntax-case stx (index)
      [(_ var (index i) ht-expr)
       (add-index stx #'(:hash-table-keys vars ht-expr) #'i)]
      [(_ var ht-expr)
       #'(:list var (hash-table-map ht-expr (lambda (k v) v)))]
      [_
       (raise-syntax-error 
        ':hash-table-values
        "expected (:hash-table-values <var> (index <var>) <ht-expr>) where the index is optional "
        stx)]))
  
  #;(require-for-syntax (lib "private/match/gen-match.ss")
                      (lib "private/match/convert-pat.ss"))
  
  #;(define-generator (:plt-match stx)
    (syntax-case stx ()
      [(_  pat expr)
       (identifier? #'pat)
       #'(:let pat expr)]
      [(_ pat expr)
       (let* ((**match-bound-vars** '())
              (compiled-match
               (gen-match #'the-expr
                          #'((pat never-used))
                          stx
                          (lambda (sf bv)
                            (set! **match-bound-vars** bv)
                            #`(begin
                                #,@(map (lambda (x)
                                          #`(set! #,(car x) #,(cdr x)))
                                        (reverse bv)))))))
         #`(:do (let ((the-expr expr)
                      (match-found? #t)
                      #,@(map (lambda (x) #`(#,(car x) #f))
                              (reverse **match-bound-vars**)))
                  (with-handlers ([exn:fail? (lambda (exn) (set! match-found? #f))])
                    #,compiled-match))
                () match-found? (let ()) #f ()))]
      [_
       (raise-syntax-error 
        ':plt-match
        "expected (:plt-match <pattern> <expr>)"
        stx)]))
  
  #;(define-generator (:match stx)
    (syntax-case stx ()
      [(_  pat expr)
       (identifier? #'pat)
       #'(:let path expr)]
      [(_ pat expr)
       (with-syntax ([new-pat (convert-pat #'pat)])
         #'(:plt-match new-pat expr))]
      [_
       (raise-syntax-error 
        'match
        "expected (:match <pattern> <expr>)"
        stx)]))
  )
