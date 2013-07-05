#|

     DO NOT TABIFY THIS FILE

|#


;                                            
;                                            
;  ;;;;                                      
;   ;  ;                                ;    
;   ;   ;   ;;;         ;; ;;    ;;;   ;;;;; 
;   ;   ;  ;   ;         ;;  ;  ;   ;   ;    
;   ;   ;  ;   ;         ;   ;  ;   ;   ;    
;   ;   ;  ;   ;         ;   ;  ;   ;   ;    
;   ;  ;   ;   ;         ;   ;  ;   ;   ;   ;
;  ;;;;     ;;;         ;;; ;;;  ;;;     ;;; 
;                                            
;                                            
;                                            
;                                            
;                                            
;                                            
;                ;;        ;      ;;;        
;    ;            ;              ;           
;   ;;;;;   ;;;   ; ;;   ;;;    ;;;;; ;;; ;;;
;    ;     ;   ;  ;;  ;    ;     ;     ;   ; 
;    ;      ;;;;  ;   ;    ;     ;      ;  ; 
;    ;     ;   ;  ;   ;    ;     ;      ; ;  
;    ;   ; ;   ;  ;   ;    ;     ;       ;;  
;     ;;;   ;;;;;;;;;;   ;;;;;  ;;;;;    ;   
;                                        ;   
;                                      ;;;;  
;                                            
;                                            
;                                                                 
;                                                                 
;         ;;        ;                    ;;;    ;     ;;          
;    ;     ;                            ;              ;          
;   ;;;;;  ; ;;   ;;;     ;;;;         ;;;;;  ;;;      ;     ;;;  
;    ;     ;;  ;    ;    ;   ;          ;       ;      ;    ;   ; 
;    ;     ;   ;    ;     ;;;           ;       ;      ;    ;;;;; 
;    ;     ;   ;    ;        ;          ;       ;      ;    ;     
;    ;   ; ;   ;    ;    ;   ;          ;       ;      ;    ;     
;     ;;; ;;; ;;; ;;;;;  ;;;;          ;;;;;  ;;;;;  ;;;;;   ;;;; 
;                                                                 
;                                                                 
;                                                                 
;                                                                 


(module lw-test racket/base
  (require "test-util.rkt"
           redex/private/loc-wrapper
           "lw-test-util.rkt"
           (only-in redex/pict to-lw/stx))
  
  (reset-count)
  
  (test (normalize-lw (to-lw ()))
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw ")" 0 0 1 1))
                  0 0 0 2))
  
  (test (normalize-lw (to-lw "x"))
        (build-lw "“x”" 0 0 0 3))
  
  (test (normalize-lw (to-lw "#f"))
        (build-lw "“#f”" 0 0 0 4))
  
  (test (normalize-lw (to-lw #f))
        (build-lw "#f" 0 0 0 2))
  
  (test (normalize-lw (to-lw/uq ()))
        (make-lw (list (make-lw "(" 0 0 0 1 #t #f)
                       (make-lw ")" 0 0 1 1 #t #f))
                 0 0 0 2 #t #f))
  
  (test (normalize-lw (to-lw (a)))
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw 'a 0 0 1 1)
                        (build-lw ")" 0 0 2 1))
                  0 0 0 3))
  
  (test (normalize-lw (to-lw (a
                              b)))
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw 'a 0 0 1 1)
                        (build-lw 'b 1 0 1 1)
                        (build-lw ")" 1 0 2 1))
                  0 1 0 3))
  
  (test (normalize-lw (to-lw (a b)))
        (build-lw 
         (list (build-lw "(" 0 0 0 1)
               (build-lw 'a 0 0 1 1)
               (build-lw 'b 0 0 3 1)
               (build-lw ")" 0 0 4 1))
         0 0 0 5))
  
  
  (test (normalize-lw (to-lw (a
                              (b c)
                              d)))
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw 'a 0 0 1 1)
                        (build-lw 
                         (list (build-lw "(" 1 0 1 1)
                               (build-lw 'b 1 0 2 1)
                               (build-lw 'c 1 0 4 1)
                               (build-lw ")" 1 0 5 1))
                         1 0 1 5)
                        (build-lw 'd 2 0 1 1)
                        (build-lw ")" 2 0 2 1))
                  0 2 0 3))
  
  (test (normalize-lw (to-lw (abcdefghijkl
                              b)))
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw 'abcdefghijkl 0 0 1 12)
                        (build-lw 'b 1 0 1 1)
                        (build-lw ")" 1 0 2 1))
                  0 1 0 3))
  
  (test (normalize-lw (to-lw ((a b)
                              c)))
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw 
                         (list (build-lw "(" 0 0 1 1)
                               (build-lw 'a 0 0 2 1)
                               (build-lw 'b 0 0 4 1)
                               (build-lw ")" 0 0 5 1))
                         0 0 1 5)
                        (build-lw 'c 1 0 1 1)
                        (build-lw ")" 1 0 2 1))
                  0 1 0 3))
  
  (test (normalize-lw (to-lw (aaa bbb
                                  (ccc
                              ddd)))) ;; <--- the ddd should be lined up under the aaa
        (build-lw (list (build-lw "(" 0 0 0 1)
                                 (build-lw 'aaa 0 0 1 3)
                                 (build-lw 'bbb 0 0 5 3)
                                 (build-lw
                                  (list
                                   (build-lw "(" 1 0 5 1)
                                   (build-lw 'ccc 1 0 6 3)
                                   (build-lw 'ddd 2 0 1 3)
                                   (build-lw ")" 2 0 4 1))
                                  1 1 1 4)
                                 (build-lw ")" 2 0 5 1))
                           0 2 0 6))
  
  (test (normalize-lw          (to-lw (aaa bbb
                                           (ccc
                                       ddd          ;; <--- the ddd should be lined up under the aaa
                                            eee)))) ;; <--- the eee should be lined up under the ccc
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw 'aaa 0 0 1 3)
                        (build-lw 'bbb 0 0 5 3)
                        (build-lw
                         (list
                          (build-lw "(" 1 0 5 1)
                          (build-lw 'ccc 1 0 6 3)
                          (build-lw 'ddd 2 0 1 3)
                          (build-lw 'eee 3 0 6 3)
                          (build-lw ")" 3 0 9 1))
                         1 2 1 9)
                        (build-lw ")" 3 0 10 1))
                  0 3 0 11))
  
  (test (normalize-lw (to-lw ([{}])))
        (build-lw (list (build-lw "(" 0 0 0 1)
                        (build-lw
                         (list
                          (build-lw "[" 0 0 1 1)
                          (build-lw
                           (list
                            (build-lw "{" 0 0 2 1)
                            (build-lw "}" 0 0 3 1))
                           0 0 2 2)
                          (build-lw "]" 0 0 4 1))
                         0 0 1 4)
                        (build-lw ")" 0 0 5 1))
                  0 0 0 6))
  
  (test (normalize-lw (to-lw ,x))
        (make-lw
         (list 
          (make-lw "" 0 0 0 0 #f #f)
          'spring
          (make-lw 'x 0 0 1 1 #t #f))
         0 0 0 2 #f #f))
  
  (test (normalize-lw (to-lw ,@x))
        (make-lw
         (list 
          (make-lw "" 0 0 0 0 #f #f)
          'spring
          (make-lw 'x 0 0 2 1 #t #f))
         0 0 0 3 #f #f))
  
  (test (normalize-lw (to-lw 'x))
        (make-lw
         (list 
          (make-lw "'" 0 0 0 1 #f #f)
          'spring
          (make-lw 'x 0 0 1 1 #f #f))
         0 0 0 2 #f #f))
  
  (test (normalize-lw (to-lw ,(term x)))
        (make-lw
         (list 
          (make-lw "" 0 0 0 0 #f #f)
          'spring
          (make-lw
           (list 
            (make-lw "" 0 0 1 0 #t #f) 
            'spring
            (make-lw 'x 0 0 7 1 #f #f))
           0 0 1 7 #t #f))
         0 0 0 8 #f #f))

  (test (normalize-lw (to-lw (term x)))
        (build-lw 
         (list 
          (build-lw "(" 0 0 0 1)
          (build-lw 'term 0 0 1 4)
          (build-lw 'x 0 0 6 1)
          (build-lw ")" 0 0 7 1))
         0 0 0 8))
  
  (test (normalize-lw (to-lw '(term x)))
        (build-lw
         (list 
          (build-lw "'" 0 0 0 1)
          'spring
          (build-lw
           (list
            (build-lw "(" 0 0 1 1)
            (build-lw 'term 0 0 2 4)
            (build-lw 'x 0 0 7 1)
            (build-lw ")" 0 0 8 1))
           0
           0
           1
           8))
         0 0 0 9))
  
  (test (normalize-lw (to-lw ''x))
        (build-lw
         (list 
          (build-lw "'" 0 0 0 1)
          'spring
          (build-lw
           (list
            (build-lw "'" 0 0 1 1)
            'spring
            (build-lw 'x 0 0 2 1))
           0
           0
           1
           2))
         0 0 0 3))
  
  (test (normalize-lw (to-lw (|+1| x)))
        (build-lw
         (list (build-lw "(" 0 0 0 1)
               (build-lw '|+1| 0 0 1 4)
               (build-lw 'x 0 0 6 1)
               (build-lw ")" 0 0 7 1))
         0 0 0 8))
  
  ;; this one seems suspicious: why does the second comma start at 1 instead of 0?
  ;; rendering seems to work, however, so we'll go with it ..
  (test (normalize-lw (to-lw ,,x))
        (build-lw
         (list 
          (build-lw "" 0 0 0 0)
          'spring
          (make-lw
           (list
            (make-lw "," 0 0 1 1 #t #f)
            'spring
            (make-lw 'x 0 0 2 1 #t #f))
           0 0 1 2
           #t #f))
         0 0 0 3))
  
  (let ([from-str (λ (str) 
                    (define p (open-input-string str))
                    (port-count-lines! p)
                    (read-syntax #f p))])
    
    (test (normalize-lw (to-lw/stx (from-str "()")))
          (normalize-lw (to-lw ())))

    (test (normalize-lw (to-lw/stx (from-str "a")))
          (normalize-lw (to-lw a)))

    (test (normalize-lw (to-lw/stx (from-str "(c)")))
          (normalize-lw (to-lw (c))))
    
    (test (normalize-lw (to-lw/stx (from-str "((b))")))
          (normalize-lw (to-lw ((b)))))
    
    (test (normalize-lw (to-lw/stx (from-str "(a b c)")))
          (normalize-lw (to-lw (a b c))))
    
    (test (normalize-lw (to-lw/stx (from-str "1")))
          (normalize-lw (to-lw 1)))
    
    (test (normalize-lw (to-lw/stx (from-str "(#t)")))
          (normalize-lw (to-lw (#t))))
    
    (test (normalize-lw (to-lw/stx (from-str "#f")))
          (normalize-lw (to-lw #f)))
    
    (test (normalize-lw (to-lw/stx (from-str "(a b)")))
          (normalize-lw (to-lw (a b))))
    
    (test (normalize-lw (to-lw/stx (from-str "(a ((b)) c 1 #t)")))
          (normalize-lw (to-lw (a ((b)) c 1 #t))))
    
    (test (normalize-lw (to-lw/stx (from-str "(a b . c)")))
          (normalize-lw (to-lw (a b . c)))))
  
  (print-tests-passed "lw-test.rkt"))

