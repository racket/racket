(module re mzscheme
  (require mzlib/list
           scheme/match
           (prefix is: mzlib/integer-set)
           "util.rkt")
  
  (provide ->re build-epsilon build-zero build-char-set build-concat
           build-repeat build-or build-and build-neg
           epsilonR? zeroR? char-setR? concatR? repeatR? orR? andR? negR?
           char-setR-chars concatR-re1 concatR-re2 repeatR-re repeatR-low repeatR-high
           orR-res andR-res negR-re
           re-nullable? re-index)
  
  ;; get-index : -> nat
  (define get-index (make-counter))

  ;; An re is either
  ;; - (make-epsilonR bool nat)
  ;; - (make-zeroR bool nat)
  ;; - (make-char-setR bool nat char-set)
  ;; - (make-concatR bool nat re re)
  ;; - (make-repeatR bool nat nat nat-or-+inf.0 re)
  ;; - (make-orR bool nat (list-of re))    Must not directly contain any orRs
  ;; - (make-andR bool nat (list-of re))   Must not directly contain any andRs
  ;; - (make-negR bool nat re)
  ;;
  ;; Every re must have an index field globally different from all
  ;; other re index fields.
  (define-struct re (nullable? index) (make-inspector))
  (define-struct (epsilonR re) () (make-inspector))
  (define-struct (zeroR re) () (make-inspector))
  (define-struct (char-setR re) (chars) (make-inspector))
  (define-struct (concatR re) (re1 re2) (make-inspector))
  (define-struct (repeatR re) (low high re) (make-inspector))
  (define-struct (orR re) (res) (make-inspector))
  (define-struct (andR re) (res) (make-inspector))
  (define-struct (negR re) (re) (make-inspector))

  ;; e : re
  ;; The unique epsilon re
  (define e (make-epsilonR #t (get-index)))

  ;; z : re
  ;; The unique zero re
  (define z (make-zeroR #f (get-index)))


  ;; s-re = char                       constant
  ;;      | string                     constant (sequence of characters)
  ;;      | re                         a precompiled re
  ;;      | (repetition low high s-re) repetition between low and high times (inclusive)
  ;;      | (union s-re ...)           
  ;;      | (intersection s-re ...)    
  ;;      | (complement s-re)          
  ;;      | (concatenation s-re ...)   
  ;;      | (char-range rng rng)       match any character between two (inclusive)
  ;;      | (char-complement char-set) match any character not listed
  ;; low = natural-number
  ;; high = natural-number or +inf.0
  ;; rng = char or string with length 1
  ;; (concatenation) (repetition 0 0 x), and "" match the empty string.
  ;; (union) matches no strings.
  ;; (intersection) matches any string.
  
  (define loc:make-range is:make-range)
  (define loc:union is:union)
  (define loc:split is:split)
  (define loc:complement is:complement)
  
  ;; ->re : s-re cache -> re
  (define (->re exp cache)
    (match exp
      ((? char?) (build-char-set (loc:make-range (char->integer exp)) cache))
      ((? string?) (->re `(concatenation ,@(string->list exp)) cache))
      ((? re?) exp)
      (`(repetition ,low ,high ,r)
        (build-repeat low high (->re r cache) cache))
      (`(union ,rs ...)
        (build-or (flatten-res (map (lambda (r) (->re r cache)) rs)
                               orR? orR-res loc:union cache)
                  cache))
      (`(intersection ,rs ...)
        (build-and (flatten-res (map (lambda (r) (->re r cache)) rs)
                                andR? andR-res (lambda (a b)
                                                 (let-values (((i _ __) (loc:split a b))) i))
                                cache)
                   cache))
      (`(complement ,r)
        (build-neg (->re r cache) cache))
      (`(concatenation ,rs ...)
        (foldr (lambda (x y)
                 (build-concat (->re x cache) y cache))
               e
               rs))
      (`(char-range ,c1 ,c2)
        (let ((i1 (char->integer (if (string? c1) (string-ref c1 0) c1)))
              (i2 (char->integer (if (string? c2) (string-ref c2 0) c2))))
          (if (<= i1 i2)
              (build-char-set (loc:make-range i1 i2) cache)
              z)))
      (`(char-complement ,crs ...)
        (let ((cs (->re `(union ,@crs) cache)))
          (cond
            ((zeroR? cs) (build-char-set (loc:make-range 0 max-char-num) cache))
            ((char-setR? cs)
             (build-char-set (loc:complement (char-setR-chars cs) 0 max-char-num) cache))
            (else z))))))
              

        

  ;; flatten-res: (list-of re) (re -> bool) (re -> (list-of re))
  ;;              (char-set char-set -> char-set) cache -> (list-of re)
  ;; Takes all the char-sets in l and combines them into one char-set using the combine function.
  ;; Flattens out the values of type?.  get-res only needs to function on things type? returns
  ;; true for.
  (define (flatten-res l type? get-res combine cache)
    (let loop ((res l)
               ;; chars : (union #f char-set)
               (chars #f)
               (no-chars null))
      (cond
        ((null? res) 
         (if chars
             (cons (build-char-set chars cache) no-chars)
             no-chars))
        ((char-setR? (car res))
         (if chars
             (loop (cdr res) (combine (char-setR-chars (car res)) chars) no-chars)
             (loop (cdr res) (char-setR-chars (car res)) no-chars)))
        ((type? (car res))
         (loop (append (get-res (car res)) (cdr res)) chars no-chars))
        (else (loop (cdr res) chars (cons (car res) no-chars))))))
    
  ;; build-epsilon : -> re
  (define (build-epsilon) e)
  
  (define (build-zero) z)
    
  (define loc:integer-set-contents is:integer-set-contents)
  
  ;; build-char-set : char-set cache -> re
  (define (build-char-set cs cache)
    (let ((l (loc:integer-set-contents cs)))
      (cond
        ((null? l) z)
        (else
         (cache l
                (lambda ()
                  (make-char-setR #f (get-index) cs)))))))
  
  
  
  ;; build-concat : re re cache -> re
  (define (build-concat r1 r2 cache)
      (cond
        ((eq? e r1) r2)
        ((eq? e r2) r1)
        ((or (eq? z r1) (eq? z r2)) z)
        (else
           (cache (cons 'concat (cons (re-index r1) (re-index r2)))
                  (lambda ()
                    (make-concatR (and (re-nullable? r1) (re-nullable? r2))
                                  (get-index)
                                  r1 r2))))))
  
  ;; build-repeat : nat nat-or-+inf.0 re cache -> re
  (define (build-repeat low high r cache)
    (let ((low (if (< low 0) 0 low)))
      (cond
        ((eq? r e) e)
        ((and (= 0 low) (or (= 0 high) (eq? z r))) e)
        ((and (= 1 low) (= 1 high)) r)
        ((and (repeatR? r)
              (eq? (repeatR-high r) +inf.0)
              (or (= 0 (repeatR-low r))
                  (= 1 (repeatR-low r))))
         (build-repeat (* low (repeatR-low r))
                       +inf.0
                       (repeatR-re r)
                       cache))
        (else
         (cache (cons 'repeat (cons low (cons high (re-index r))))
                (lambda ()
                  (make-repeatR (or (re-nullable? r) (= 0 low)) (get-index) low high r)))))))
  
  
  ;; build-or : (list-of re) cache -> re
  (define (build-or rs cache)
    (let ((rs 
           (filter
            (lambda (x) (not (eq? x z)))
            (do-simple-equiv (replace rs orR? orR-res null) re-index))))
      (cond
        ((null? rs) z)
        ((null? (cdr rs)) (car rs))
        ((memq (build-neg z cache) rs) (build-neg z cache))
        (else
         (cache (cons 'or (map re-index rs))
                (lambda ()
                  (make-orR (ormap re-nullable? rs) (get-index) rs)))))))
  
  ;; build-and : (list-of re) cache -> re
  (define (build-and rs cache)
    (let ((rs (do-simple-equiv (replace rs andR? andR-res null) re-index)))
      (cond
        ((null? rs) (build-neg z cache))
        ((null? (cdr rs)) (car rs))
        ((memq z rs) z)
        (else
         (cache (cons 'and (map re-index rs))
                (lambda ()
                  (make-andR (andmap re-nullable? rs) (get-index) rs)))))))
      
  ;; build-neg : re cache -> re
  (define (build-neg r cache)
    (cond
      ((negR? r) (negR-re r))
      (else
       (cache (cons 'neg (re-index r))
              (lambda ()
                (make-negR (not (re-nullable? r)) (get-index) r))))))
  
  ;; Tests for the build-functions
  (test-block ((c (make-cache))
               (isc is:integer-set-contents)
               (r1 (build-char-set (is:make-range (char->integer #\1)) c))
               (r2 (build-char-set (is:make-range (char->integer #\2)) c))
               (r3 (build-char-set (is:make-range (char->integer #\3)) c))
               (rc (build-concat r1 r2 c))
               (rc2 (build-concat r2 r1 c))
               (rr (build-repeat 0 +inf.0 rc c))
               (ro (build-or `(,rr ,rc ,rr) c))
               (ro2 (build-or `(,rc ,rr ,z) c))
               (ro3 (build-or `(,rr ,rc) c))
               (ro4 (build-or `(,(build-or `(,r1 ,r2) c)
                                 ,(build-or `(,r2 ,r3) c)) c))
               (ra (build-and `(,rr ,rc ,rr) c))
               (ra2 (build-and `(,rc ,rr) c))
               (ra3 (build-and `(,rr ,rc) c))
               (ra4 (build-and `(,(build-and `(,r3 ,r2) c)
                                  ,(build-and `(,r2 ,r1) c)) c))
               (rn (build-neg z c))
               (rn2 (build-neg r1 c)))
               
              ((isc (char-setR-chars r1)) (isc (is:make-range (char->integer #\1))))
              ((isc (char-setR-chars r2)) (isc (is:make-range (char->integer #\2))))
              ((isc (char-setR-chars r3)) (isc (is:make-range (char->integer #\3))))
              ((build-char-set (is:make-range) c) z)
              ((build-concat r1 e c) r1)
              ((build-concat e r1 c) r1)
              ((build-concat r1 z c) z)
              ((build-concat z r1 c) z)
              ((build-concat r1 r2 c) rc)
              ((concatR-re1 rc) r1)
              ((concatR-re2 rc) r2)
              ((concatR-re1 rc2) r2)
              ((concatR-re2 rc2) r1)
              (ro ro2)
              (ro ro3)
              (ro4 (build-or `(,r1 ,r2 ,r3) c))
              ((orR-res ro) (list rc rr))
              ((orR-res ro4) (list r1 r2 r3))
              ((build-or null c) z)
              ((build-or `(,r1 ,z) c) r1)
              ((build-repeat 0 +inf.0 rc c) rr)
              ((build-repeat 0 1 z c) e)
              ((build-repeat 0 0 rc c) e)
              ((build-repeat 0 +inf.0 z c) e)
              ((build-repeat -1 +inf.0 z c) e)
              ((build-repeat 0 +inf.0 (build-repeat 0 +inf.0 rc c) c)
               (build-repeat 0 +inf.0 rc c))
              ((build-repeat 20 20 (build-repeat 0 +inf.0 rc c) c)
               (build-repeat 0 +inf.0 rc c))
              ((build-repeat 20 20 (build-repeat 1 +inf.0 rc c) c)
               (build-repeat 20 +inf.0 rc c))
              ((build-repeat 1 1 rc c) rc)
              ((repeatR-re rr) rc)
              (ra ra2)
              (ra ra3)
              (ra4 (build-and `(,r1 ,r2 ,r3) c))
              ((andR-res ra) (list rc rr))
              ((andR-res ra4) (list r1 r2 r3))
              ((build-and null c) (build-neg z c))
              ((build-and `(,r1 ,z) c) z)
              ((build-and `(,r1) c) r1)
              ((build-neg r1 c) (build-neg r1 c))
              ((build-neg (build-neg r1 c) c) r1)
              ((negR-re (build-neg r2 c)) r2)
              ((re-nullable? r1) #f)
              ((re-nullable? rc) #f)
              ((re-nullable? (build-concat rr rr c)) #t)
              ((re-nullable? rr) #t)
              ((re-nullable? (build-repeat 0 1 rc c)) #t)
              ((re-nullable? (build-repeat 1 2 rc c)) #f)
              ((re-nullable? (build-repeat 1 2 (build-or (list e r1) c) c)) #t)
              ((re-nullable? ro) #t)
              ((re-nullable? (build-or `(,r1 ,r2) c)) #f)
              ((re-nullable? (build-and `(,r1 ,e) c)) #f)
              ((re-nullable? (build-and `(,rr ,e) c)) #t)
              ((re-nullable? (build-neg r1 c)) #t)
              ((re-nullable? (build-neg rr c)) #f))
              
  (test-block ((c (make-cache))
               (isc is:integer-set-contents)
               (r1 (->re #\1 c))
               (r2 (->re #\2 c))
               (r3-5 (->re '(char-range #\3 #\5) c))
               (r4 (build-or `(,r1 ,r2) c))
               (r5 (->re `(union ,r3-5 #\7) c))
               (r6 (->re #\6 c)))
              ((flatten-res null orR? orR-res is:union c) null)
              ((isc (char-setR-chars (car (flatten-res `(,r1) orR? orR-res is:union c))))
               (isc (is:make-range (char->integer #\1))))
              ((isc (char-setR-chars (car (flatten-res `(,r4) orR? orR-res is:union c))))
               (isc (is:make-range (char->integer #\1) (char->integer #\2))))
              ((isc (char-setR-chars (car (flatten-res `(,r6 ,r5 ,r4 ,r3-5 ,r2 ,r1)
                                                       orR? orR-res is:union c))))
               (isc (is:make-range (char->integer #\1) (char->integer #\7))))
              ((flatten-res `(,r1 ,r2) andR? andR-res (lambda (x y)
                                                        (let-values (((i _ __)
                                                                      (is:split x y)))
                                                          i))
                            c)
               (list z)))
  
  ;; ->re
  (test-block ((c (make-cache))
               (isc is:integer-set-contents)
               (r (->re #\a c))
               (rr (->re `(concatenation ,r ,r) c))
               (rrr (->re `(concatenation ,r ,rr) c))
               (rrr* (->re `(repetition 0 +inf.0 ,rrr) c)))
              ((isc (char-setR-chars r)) (isc (is:make-range (char->integer #\a))))
              ((->re "" c) e)
              ((->re "asdf" c) (->re `(concatenation #\a #\s #\d #\f) c))
              ((->re r c) r)
              ((->re `(repetition 0 +inf.0 ,r) c) (build-repeat 0 +inf.0 r c))
              ((->re `(repetition 1 +inf.0 ,r) c) (build-repeat 1 +inf.0 r c))
              ((->re `(repetition 0 1 ,r) c) (build-repeat 0 1 r c))
              ((->re `(repetition 0 1 ,rrr*) c) rrr*)
              ((->re `(union (union (char-range #\a #\c)
                                    (char-complement (char-range #\000 #\110)
                                                     (char-range #\112 ,(integer->char max-char-num))))
                         (union (repetition 0 +inf.0 #\2))) c)
               (build-or (list (build-char-set (is:union (is:make-range 73)
                                                         (is:make-range 97 99))
                                               c)
                               (build-repeat 0 +inf.0 (build-char-set (is:make-range 50) c) c))
                         c))
              ((->re `(union ,rr ,rrr) c) (build-or (list rr rrr) c))
              ((->re `(union ,r) c) r)
              ((->re `(union) c) z)
              ((->re `(intersection (intersection #\111 
                                                  (char-complement (char-range #\000 #\110)
                                                                   (char-range #\112 ,(integer->char max-char-num))))
                         (intersection (repetition 0 +inf.0 #\2))) c)
               (build-and (list (build-char-set (is:make-range 73) c)
                                (build-repeat 0 +inf.0 (build-char-set (is:make-range 50) c) c))
                          c))
              ((->re `(intersection (intersection #\000 (char-complement (char-range #\000 #\110)
                                                                         (char-range #\112 ,(integer->char max-char-num))))
                                    (intersection (repetition 0 +inf.0 #\2))) c)
               z)
              ((->re `(intersection ,rr ,rrr) c) (build-and (list rr rrr) c))
              ((->re `(intersection ,r) c) r)
              ((->re `(intersection) c) (build-neg z c))
              ((->re `(complement ,r) c) (build-neg r c))
              ((->re `(concatenation) c) e)
              ((->re `(concatenation ,rrr*) c) rrr*)
              (rr (build-concat r r c))
              ((->re `(concatenation ,r ,rr ,rrr) c)
               (build-concat r (build-concat rr rrr c) c))
              ((isc (char-setR-chars (->re `(char-range #\1 #\1) c))) (isc (is:make-range 49)))
              ((isc (char-setR-chars (->re `(char-range #\1 #\9) c))) (isc (is:make-range 49 57)))
              ((isc (char-setR-chars (->re `(char-range "1" "1") c))) (isc (is:make-range 49)))
              ((isc (char-setR-chars (->re `(char-range "1" "9") c))) (isc (is:make-range 49 57)))
              ((->re `(char-range "9" "1") c) z)
              ((isc (char-setR-chars (->re `(char-complement) c)))
               (isc (char-setR-chars (->re `(char-range #\000 ,(integer->char max-char-num)) c))))
              ((isc (char-setR-chars (->re `(char-complement #\001 (char-range #\002 ,(integer->char max-char-num))) c)))
               (isc (is:make-range 0)))
              )
  
  )
