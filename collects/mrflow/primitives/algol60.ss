; Algol 60 primitives and runtime support
(
 ; primitives
 
 (!= (number number -> boolean))
 (! (boolean -> boolean))
 (& (boolean boolean -> boolean))
 (\| (boolean boolean -> boolean))
 (=> (boolean boolean -> boolean))
 (== (boolean boolean -> boolean))
 
 (sign (forall ([a top])
               ((real -> a) (-> real) -> a)))
 (entier (forall ([a top])
                 ((real -> a) (-> real) -> a)))
 
 (a60:sin (forall ([a top])
                  ((real -> a) (-> real) -> a)))
 (a60:cos (forall ([a top])
                  ((real -> a) (-> real) -> a)))
 (a60:arctan (forall ([a top])
                     ((real -> a) (-> real) -> a)))
 (a60:sqrt (forall ([a top])
                   ((real -> a) (-> real) -> a)))
 (a60:abs (forall ([a top])
                  ((real -> a) (-> real) -> a)))
 (a60:ln (forall ([a top])
                 ((real -> a) (-> real) -> a)))
 (a60:exp (forall ([a top])
                  ((real -> a) (-> real) -> a)))

 (prints (forall ([a top])
                 ((void -> a) (-> top) -> a)))
 (printn (forall ([a top])
                 ((void -> a) (-> top) -> a)))
 (printsln (forall ([a top])
                   ((void -> a) (-> top) -> a)))
 (printnln (forall ([a top])
                   ((void -> a) (-> top) -> a)))
 
 ; Algol60 runtime support
 
 ;(a60:array (struct a60:array (dependant type)))
 ;(a60:switch (struct a60:switch (choices))
 
 (undefined undefined)
 
 (check-boolean (forall ([a top]) (a -> a)))
 (goto (forall ([a top]) ((-> a) -> a)))
 (get-value (forall ([a top]) ((-> a) -> a)))
 (set-target! (forall ([a top][b top])
                      ((a -> b) a -> b)))
 ;make-array
 ;array-ref
 ;array-set!
 ;make-switch
 ;switch-ref
 
 (coerce (forall ([a top])
                 (symbol a -> a)))
 
 
 ; R5RS runtime support
 
 (void (-> void))
 
 (= (real real -> boolean))
 (< (real real -> boolean))
 (> (real real -> boolean))
 (<= (real real -> boolean))
 (>= (real real -> boolean))

 (+ (real real -> real)) 
 (* (real real -> real)) 
 (- (real real -> real))
 (/ (real real -> real))
 
 (quotient (integer integer -> integer))
 (remainder (integer integer -> integer))
 (modulo (integer integer -> integer))
 
 (values (forall ([a_values top])
                 (case-lambda
                   [(rest a_values) (values a_values)]
                   )))

 )