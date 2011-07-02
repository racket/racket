(module deriv mzscheme
  
  (require mzlib/list
           (prefix is: mzlib/integer-set)
           "re.rkt"
           "util.rkt")

  (provide build-dfa print-dfa (struct dfa (num-states start-state final-states/actions transitions)))

  (define e (build-epsilon))
  (define z (build-zero))
  
  
  ;; Don't do anything with this one but extract the chars
  (define all-chars (->re `(char-complement (union)) (make-cache)))
  
  ;; get-char-groups : re bool -> (list-of char-setR?)
  ;; Collects the char-setRs in r that could be used in
  ;; taking the derivative of r.
  (define (get-char-groups r found-negation)
    (cond
      ((or (eq? r e) (eq? r z)) null)
      ((char-setR? r) (list r))
      ((concatR? r)
       (if (re-nullable? (concatR-re1 r))
           (append (get-char-groups (concatR-re1 r) found-negation)
                   (get-char-groups (concatR-re2 r) found-negation))
           (get-char-groups (concatR-re1 r) found-negation)))
      ((repeatR? r)
       (get-char-groups (repeatR-re r) found-negation))
      ((orR? r)
       (apply append (map (lambda (x) (get-char-groups x found-negation)) (orR-res r))))
      ((andR? r)
       (apply append (map (lambda (x) (get-char-groups x found-negation)) (andR-res r))))
      ((negR? r)
       (if found-negation
           (get-char-groups (negR-re r) #t)
           (cons all-chars (get-char-groups (negR-re r) #t))))))

  (test-block ((c (make-cache))
               (r1 (->re #\1 c))
               (r2 (->re #\2 c)))
              ((get-char-groups e #f) null)
              ((get-char-groups z #f) null)
              ((get-char-groups r1 #f) (list r1))
              ((get-char-groups (->re `(concatenation ,r1 ,r2) c) #f)
               (list r1))
              ((get-char-groups (->re `(concatenation ,e ,r2) c) #f) 
               (list r2))
              ((get-char-groups (->re `(concatenation (repetition 0 +inf.0 ,r1) ,r2) c) #f) 
               (list r1 r2))
              ((get-char-groups (->re `(repetition 0 +inf.0 ,r1) c) #f) 
               (list r1))
              ((get-char-groups
                (->re `(union (repetition 0 +inf.0 ,r1) 
                              (concatenation (repetition 0 +inf.0 ,r2) "3") "4") c) #f)
               (list r1 r2 (->re "3" c) (->re "4" c)))
              ((get-char-groups (->re `(complement ,r1) c) #f) 
               (list all-chars r1))
              ((get-char-groups 
                (->re `(intersection (repetition 0 +inf.0 ,r1)
                                     (concatenation (repetition 0 +inf.0 ,r2) "3") "4") c) #f)
               (list r1 r2 (->re "3" c) (->re "4" c)))
              )
  (define loc:member? is:member?)
  
  ;; deriveR : re char cache -> re
  (define (deriveR r c cache)
    (cond
      ((or (eq? r e) (eq? r z)) z)
      ((char-setR? r)
       (if (loc:member? c (char-setR-chars r)) e z))
      ((concatR? r)
       (let* ((r1 (concatR-re1 r))
              (r2 (concatR-re2 r))
              (d (build-concat (deriveR r1 c cache) r2 cache)))
         (if (re-nullable? r1)
             (build-or (list d (deriveR r2 c cache)) cache)
             d)))
      ((repeatR? r)
       (build-concat (deriveR (repeatR-re r) c cache)
                     (build-repeat (sub1 (repeatR-low r))
                                   (sub1 (repeatR-high r))
                                   (repeatR-re r) cache)
                     cache))
      ((orR? r)
       (build-or (map (lambda (x) (deriveR x c cache))
                      (orR-res r))
                 cache))
      ((andR? r)
       (build-and (map (lambda (x) (deriveR x c cache))
                       (andR-res r))
                  cache))
      ((negR? r)
       (build-neg (deriveR (negR-re r) c cache) cache))))

  (test-block ((c (make-cache))
               (a (char->integer #\a))
               (b (char->integer #\b))
               (r1 (->re #\a c))
               (r2 (->re `(repetition 0 +inf.0 #\a) c))
               (r3 (->re `(repetition 0 +inf.0 ,r2) c))
               (r4 (->re `(concatenation #\a ,r2) c))
               (r5 (->re `(repetition 0 +inf.0 ,r4) c))
               (r6 (->re `(union ,r5 #\a) c))
               (r7 (->re `(concatenation ,r2 ,r2) c))
               (r8 (->re `(complement ,r4) c))
               (r9 (->re `(intersection ,r2 ,r4) c)))
              ((deriveR e a c) z)
              ((deriveR z a c) z)
              ((deriveR r1 b c) z)
              ((deriveR r1 a c) e)
              ((deriveR r2 a c) r2)
              ((deriveR r2 b c) z)
              ((deriveR r3 a c) r2)
              ((deriveR r3 b c) z)
              ((deriveR r4 a c) r2)
              ((deriveR r4 b c) z)
              ((deriveR r5 a c) (->re `(concatenation ,r2 ,r5) c))
              ((deriveR r5 b c) z)
              ((deriveR r6 a c) (->re `(union (concatenation ,r2 ,r5) "") c))
              ((deriveR r6 b c) z)
              ((deriveR r7 a c) (->re `(union (concatenation ,r2 ,r2) ,r2) c))
              ((deriveR r7 b c) z)
              ((deriveR r8 a c) (->re `(complement, r2) c))
              ((deriveR r8 b c) (->re `(complement ,z) c))
              ((deriveR r9 a c) r2)
              ((deriveR r9 b c) z)
              ((deriveR (->re `(repetition 1 2 "ab") c) a c)
               (->re `(concatenation "b" (repetition 0 1 "ab")) c)))
  
  ;; An re-action is (cons re action)

  ;; derive : (list-of re-action) char cache -> (union (list-of re-action) #f)
  ;; applies deriveR to all the re-actions's re parts.
  ;; Returns #f if the derived state is equivalent to z.
  (define (derive r c cache)
    (let ((new-r (map (lambda (ra)
                        (cons (deriveR (car ra) c cache) (cdr ra)))
                      r)))
      (if (andmap (lambda (x) (eq? z (car x)))
                  new-r)
          #f
          new-r)))

  (test-block ((c (make-cache))
               (r1 (->re #\1 c))
               (r2 (->re #\2 c)))
              ((derive null (char->integer #\1) c) #f)
              ((derive (list (cons r1 1) (cons r2 2)) (char->integer #\1) c)
               (list (cons e 1) (cons z 2)))
              ((derive (list (cons r1 1) (cons r2 2)) (char->integer #\3) c) #f))
  

  ;; get-final : (list-of re-action) -> (union #f syntax-object)
  ;; An re that accepts e represents a final state.  Return the
  ;; action from the first final state or #f if there is none.
  (define (get-final res)
    (cond
      ((null? res) #f)
      ((re-nullable? (caar res)) (cdar res))
      (else (get-final (cdr res)))))

  (test-block ((c->i char->integer)
               (c (make-cache))
               (r1 (->re #\a c))
               (r2 (->re #\b c))
               (b (list (cons z 1) (cons z 2) (cons z 3) (cons e 4) (cons z 5)))
               (a (list (cons r1 1) (cons r2 2))))
              ((derive null (c->i #\a) c) #f)
              ((derive a (c->i #\a) c) (list (cons e 1) (cons z 2)))
              ((derive a (c->i #\b) c) (list (cons z 1) (cons e 2)))
              ((derive a (c->i #\c) c) #f)
              ((derive (list (cons (->re `(union " " "\n" ",") c) 1)
                             (cons (->re `(concatenation (repetition 0 1 "-")
                                                         (repetition 1 +inf.0 (char-range "0" "9"))) c) 2)
                             (cons (->re `(concatenation "-" (repetition 1 +inf.0 "-")) c) 3)
                             (cons (->re "[" c) 4)
                             (cons (->re "]" c) 5)) (c->i #\[) c)
               b)
              ((get-final a) #f)
              ((get-final (list (cons e 1) (cons e 2))) 1)
              ((get-final b) 4))
  
  
  ;; A state is (make-state (list-of re-action) nat)
  (define-struct state (spec index))
        
  ;; get->key : re-action -> (list-of nat)
  ;; states are indexed by the list of indexes of their res
  (define (get-key s)
    (map (lambda (x) (re-index (car x))) s))  
          
  (define loc:partition is:partition)
  
  ;; compute-chars : (list-of state) -> (list-of char-set)
  ;; Computed the sets of equivalent characters for taking the
  ;; derivative of the car of st.  Only one derivative per set need to be taken.
  (define (compute-chars st)
    (cond
      ((null? st) null)
      (else
       (loc:partition (map char-setR-chars
                          (apply append (map (lambda (x) (get-char-groups (car x) #f))
                                             (state-spec (car st)))))))))
  
  (test-block ((c (make-cache))
               (c->i char->integer)
               (r1 (->re `(char-range #\1 #\4) c))
               (r2 (->re `(char-range #\2 #\3) c)))
              ((compute-chars null) null)
              ((compute-chars (list (make-state null 1))) null)
              ((map is:integer-set-contents
                    (compute-chars (list (make-state (list (cons r1 1) (cons r2 2)) 2))))
               (list (is:integer-set-contents (is:make-range (c->i #\2) (c->i #\3)))
                     (is:integer-set-contents (is:union (is:make-range (c->i #\1))
                                                        (is:make-range (c->i #\4)))))))
  
  
  ;; A dfa is (make-dfa int int
  ;;                    (list-of (cons int syntax-object))
  ;;                    (list-of (cons int (list-of (cons char-set int)))))
  ;; Each transitions is a state and a list of chars with the state to transition to.
  ;; The finals and transitions are sorted by state number, and duplicate free.
  (define-struct dfa (num-states start-state final-states/actions transitions) (make-inspector))
  
  (define loc:get-integer is:get-integer)
  
  ;; build-dfa : (list-of re-action) cache -> dfa
  (define (build-dfa rs cache)
    (let* ((transitions (make-hash-table))
           (get-state-number (make-counter))
           (start (make-state rs (get-state-number))))
      (cache (cons 'state (get-key rs)) (lambda () start))
      (let loop ((old-states (list start))
                 (new-states null)
		 (all-states (list start))
                 (cs (compute-chars (list start))))
        (cond
          ((and (null? old-states) (null? new-states))
           (make-dfa (get-state-number) (state-index start)
                     (sort (filter (lambda (x) (cdr x))
                                   (map (lambda (state)
                                          (cons (state-index state) (get-final (state-spec state))))
                                        all-states))
                           (lambda (a b) (< (car a) (car b))))
                     (sort (hash-table-map transitions
                                           (lambda (state trans)
                                             (cons (state-index state)
                                                   (map (lambda (t)
                                                          (cons (car t)
                                                                (state-index (cdr t))))
                                                        trans))))
                           (lambda (a b) (< (car a) (car b))))))
          ((null? old-states)
           (loop new-states null all-states (compute-chars new-states)))
          ((null? cs)
           (loop (cdr old-states) new-states all-states (compute-chars (cdr old-states))))
          (else
           (let* ((state (car old-states))
                  (c (car cs))
                  (new-re (derive (state-spec state) (loc:get-integer c) cache)))
             (cond
               (new-re
                (let* ((new-state? #f)
                       (new-state (cache (cons 'state (get-key new-re))
                                         (lambda ()
                                           (set! new-state? #t)
                                           (make-state new-re (get-state-number)))))
		       (new-all-states (if new-state? (cons new-state all-states) all-states)))
                  (hash-table-put! transitions 
                                   state
                                   (cons (cons c new-state)
                                         (hash-table-get transitions state
                                                         (lambda () null))))
                  (cond
                    (new-state?
                     (loop old-states (cons new-state new-states) new-all-states (cdr cs)))
                    (else
                     (loop old-states new-states new-all-states (cdr cs))))))
               (else (loop old-states new-states all-states (cdr cs))))))))))
  
  (define (print-dfa x)
    (printf "number of states: ~a\n" (dfa-num-states x))
    (printf "start state: ~a\n" (dfa-start-state x))
    (printf "final states: ~a\n" (map car (dfa-final-states/actions x)))
    (for-each (lambda (trans)
                (printf "state: ~a\n" (car trans))
                (for-each (lambda (rule)
                            (printf "  -~a-> ~a\n"
                                    (is:integer-set-contents (car rule))
                                    (cdr rule)))
                          (cdr trans)))
              (dfa-transitions x)))
 
  (define (build-test-dfa rs)
    (let ((c (make-cache)))
      (build-dfa (map (lambda (x) (cons (->re x c) 'action))
                      rs)
                 c)))

  
#|
  (define t1 (build-test-dfa null))
  (define t2 (build-test-dfa `(#\a)))
  (define t3 (build-test-dfa `(#\a #\b)))
  (define t4 (build-test-dfa `((repetition 0 +inf.0 #\a)
                               (repetition 0 +inf.0 (concatenation #\a #\b)))))
  (define t5 (build-test-dfa `((concatenation (repetition 0 +inf.0 (union #\0 #\1)) #\1))))
  (define t6 (build-test-dfa `((repetition 0 +inf.0 (repetition 0 +inf.0 #\a))
                               (repetition 0 +inf.0 (concatenation #\b (repetition 1 +inf.0 #\b))))))
  (define t7 (build-test-dfa `((concatenation (repetition 0 +inf.0 #\a) (repetition 0 +inf.0 #\b)
                                              (repetition 0 +inf.0 #\c) (repetition 0 +inf.0 #\d)
                                              (repetition 0 +inf.0 #\e)))))
  (define t8
    (build-test-dfa `((concatenation (repetition 0 +inf.0 (union #\a #\b)) #\a (union #\a #\b)
                                     (union #\a #\b) (union #\a #\b) (union #\a #\b)))))
  (define t9 (build-test-dfa `((concatenation "/*"
                                  (complement (concatenation (intersection) "*/" (intersection)))
                                  "*/"))))
  (define t11 (build-test-dfa `((complement "1"))))
  (define t12 (build-test-dfa `((concatenation (intersection (concatenation (repetition 0 +inf.0 "a") "b")
                                                             (concatenation "a" (repetition 0 +inf.0 "b")))
                                               "ab"))))
  (define x (build-test-dfa `((union " " "\n" ",")
                              (concatenation (repetition 0 1 "-") (repetition 1 +inf.0 (char-range "0" "9")))
                              (concatenation "-" (repetition 1 +inf.0 "-"))
                              "["
                              "]")))
  (define y (build-test-dfa
             `((repetition 1 +inf.0
                       (union (concatenation "|" (repetition 0 +inf.0 (char-complement "|")) "|")
                              (concatenation "|" (repetition 0 +inf.0 (char-complement "|"))))))))
  (define t13 (build-test-dfa `((intersection (concatenation (intersection) "111" (intersection))
                                              (complement (union (concatenation (intersection) "01")
                                       (repetition 1 +inf.0 "1")))))))
  (define t14 (build-test-dfa `((complement "1"))))
|#
  )
