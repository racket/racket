(require
 (lib "class.ss")
 (lib "mred.ss" "mred")
 (lib "etc.ss")
 "../aligned-pasteboard.ss"
 "../aligned-editor-container.ss")

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

(define t-snip1
  (instantiate editor-snip% ()
    (editor (instantiate text% ()))))

(define t-snip2
  (instantiate editor-snip% ()
    (editor (instantiate text% ()))))

(send pasteboard insert ae-snip1 false)
(send pasteboard insert ae-snip2 false)
(send pasteboard insert ae-snip5 false)
(send vp2 insert ae-snip3 false)
(send vp2 insert ae-snip4 false)
(send vp1 insert t-snip1 false)
(send vp5 insert t-snip2 false)
(send frame show true)