(require
 (lib "class.ss")
 (lib "mred.ss" "mred")
 (lib "etc.ss")
 (lib "list.ss")
 "../aligned-pasteboard.ss"
 "../aligned-editor-container.ss"
 "snip-dumper.ss")

;                                                          
;                                                          
;                                       ;;                 
;                                        ;                 
;                                        ;                 
;    ;;;;; ;;  ;;  ;;;;;;;;;;;;;; ;;;    ;     ;;;;;  ;;;; 
;   ;    ;  ;;;;      ; ;  ;  ; ;;  ;;   ;    ;    ; ;   ; 
;   ;;;;;;   ;;    ;;;; ;  ;  ; ;    ;   ;    ;;;;;; ;;;;  
;   ;        ;;   ;   ; ;  ;  ; ;    ;   ;    ;          ; 
;   ;;   ;  ;  ;  ;   ; ;  ;  ; ;;  ;;   ;    ;;   ; ;   ; 
;    ;;;;  ;;  ;;  ;;;;;;; ;; ;;; ;;;  ;;;;;   ;;;;  ;;;;  
;                               ;                          
;                              ;;;                         
;                                                          

(printf "running test2.ss~n")

(define frame
  (instantiate frame% ()
    (label "Frame")
    (width 400)
    (height 400)))

(define pasteboard
  (instantiate horizontal-pasteboard% ()))

(define canvas
  (instantiate aligned-editor-canvas% ()
    (parent frame)
    (editor pasteboard)))

(define vp1
  (instantiate vertical-pasteboard% ()))

(define ae-snip1
  (instantiate aligned-editor-snip% ()
    (editor vp1)))

(define vp2
  (instantiate vertical-pasteboard% ()))

(define ae-snip2
  (instantiate aligned-editor-snip% ()
    (editor vp2)))

(define vp3
  (instantiate vertical-pasteboard% ()))

(define ae-snip3
  (instantiate aligned-editor-snip% ()
    (editor vp3)))

(define vp4
  (instantiate vertical-pasteboard% ()))

(define ae-snip4
  (instantiate aligned-editor-snip% ()
    (editor vp4)))

(define vp5
  (instantiate vertical-pasteboard% ()))

(define ae-snip5
  (instantiate aligned-editor-snip% ()
    (editor vp5)))

(send pasteboard insert ae-snip1)
(send pasteboard insert ae-snip2)
(send pasteboard insert ae-snip5)
(send vp2 insert ae-snip3)
(send vp2 insert ae-snip4)
(send frame show true)

;                                     
;                                     
;                                     
;   ;                    ;            
;   ;                    ;            
;  ;;;;;    ;;;;;  ;;;; ;;;;;    ;;;; 
;   ;      ;    ; ;   ;  ;      ;   ; 
;   ;      ;;;;;; ;;;;   ;      ;;;;  
;   ;      ;          ;  ;          ; 
;   ;      ;;   ; ;   ;  ;      ;   ; 
;   ;;;;;   ;;;;  ;;;;   ;;;;;  ;;;;  
;                                     
;                                     
;                                     

(sleep/yield 1)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 120.0 368.0 0.0 0.0 empty)
  (make-snip-dump
   249.0
   368.0
   120.0
   0.0
   (list (make-snip-dump 117.0 178.0 0.0 0.0 empty) (make-snip-dump 117.0 356.0 0.0 178.0 empty)))
  (make-snip-dump 368.0 368.0 249.0 0.0 empty))
 )

(send frame resize 0 0)
(sleep/yield 1)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 10.0 30.0 0.0 0.0 empty)
  (make-snip-dump
   30.0
   30.0
   10.0
   0.0
   (list (make-snip-dump 10.0 10.0 0.0 0.0 empty) (make-snip-dump 10.0 19.0 0.0 9.0 empty)))
  (make-snip-dump 40.0 30.0 30.0 0.0 empty))
 )

(send frame resize 800 600)
(sleep/yield 1)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 253.0 568.0 0.0 0.0 empty)
  (make-snip-dump
   516.0
   568.0
   253.0
   0.0
   (list (make-snip-dump 251.0 278.0 0.0 0.0 empty) (make-snip-dump 251.0 556.0 0.0 278.0 empty)))
  (make-snip-dump 768.0 568.0 516.0 0.0 empty))
 )

(send pasteboard delete ae-snip5)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump
   389.0
   568.0
   0.0
   0.0
   (list (make-snip-dump 377.0 278.0 0.0 0.0 empty) (make-snip-dump 377.0 556.0 0.0 278.0 empty)))
  (make-snip-dump 768.0 568.0 389.0 0.0 empty))
 )

(send pasteboard insert ae-snip5)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 253.0 568.0 0.0 0.0 empty)
  (make-snip-dump
   516.0
   568.0
   253.0
   0.0
   (list (make-snip-dump 251.0 278.0 0.0 0.0 empty) (make-snip-dump 251.0 556.0 0.0 278.0 empty)))
  (make-snip-dump 768.0 568.0 516.0 0.0 empty))
 )

(send pasteboard delete ae-snip5)
(send pasteboard delete ae-snip1)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump
   768.0
   568.0
   0.0
   0.0
   (list (make-snip-dump 756.0 278.0 0.0 0.0 empty) (make-snip-dump 756.0 556.0 0.0 278.0 empty))))
 )

(send pasteboard erase)
(dump=?
 (dump-children pasteboard)
 empty
 )

(send frame show false)
(printf "done~n")
