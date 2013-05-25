#lang racket/gui

(require "../aligned-pasteboard.rkt" "../aligned-editor-container.rkt"
         "snip-dumper.rkt")

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

(printf "running test2.rkt\n")

(define frame (new frame% [label "Frame"] [width 400] [height 400]))
(define pasteboard (new horizontal-pasteboard%))
(define canvas (new aligned-editor-canvas% [parent frame] [editor pasteboard]))
(define vp1      (new vertical-pasteboard%))
(define ae-snip1 (new aligned-editor-snip% [editor vp1]))
(define vp2      (new vertical-pasteboard%))
(define ae-snip2 (new aligned-editor-snip% [editor vp2]))
(define vp3      (new vertical-pasteboard%))
(define ae-snip3 (new aligned-editor-snip% [editor vp3]))
(define vp4      (new vertical-pasteboard%))
(define ae-snip4 (new aligned-editor-snip% [editor vp4]))
(define vp5      (new vertical-pasteboard%))
(define ae-snip5 (new aligned-editor-snip% [editor vp5]))

(send pasteboard insert ae-snip1)
(send pasteboard insert ae-snip2)
(send pasteboard insert ae-snip5)
(send vp2 insert ae-snip3)
(send vp2 insert ae-snip4)
(send frame show #t)


;   ;                    ;
;   ;                    ;
;  ;;;;;    ;;;;;  ;;;; ;;;;;    ;;;;
;   ;      ;    ; ;   ;  ;      ;   ;
;   ;      ;;;;;; ;;;;   ;      ;;;;
;   ;      ;          ;  ;          ;
;   ;      ;;   ; ;   ;  ;      ;   ;
;   ;;;;;   ;;;;  ;;;;   ;;;;;  ;;;;

;; Eli: Looks like these tests are supposed to return #t, so most are
;; failing (wasn't visible when this was running via "gracket -f")

(sleep/yield 0.5)
(dump=?
 (dump-children pasteboard)
 (list (make-snip-dump 120.0 368.0 0.0 0.0 '())
       (make-snip-dump 249.0 368.0 120.0 0.0
                       (list (make-snip-dump 117.0 178.0 0.0 0.0 '())
                             (make-snip-dump 117.0 356.0 0.0 178.0 '())))
       (make-snip-dump 368.0 368.0 249.0 0.0 '())))

(send frame resize 0 0)
(sleep/yield 0.5)
(dump=?
 (dump-children pasteboard)
 (list (make-snip-dump 10.0 30.0 0.0 0.0 '())
       (make-snip-dump 30.0 30.0 10.0 0.0
                       (list (make-snip-dump 10.0 10.0 0.0 0.0 '())
                             (make-snip-dump 10.0 19.0 0.0 9.0 '())))
       (make-snip-dump 40.0 30.0 30.0 0.0 '())))

(send frame resize 800 600)
(sleep/yield 0.5)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 253.0 568.0 0.0 0.0 '())
  (make-snip-dump 516.0 568.0 253.0 0.0
                  (list (make-snip-dump 251.0 278.0 0.0 0.0 '())
                        (make-snip-dump 251.0 556.0 0.0 278.0 '())))
  (make-snip-dump 768.0 568.0 516.0 0.0 '())))

(send pasteboard delete ae-snip5)
(dump=?
 (dump-children pasteboard)
 (list (make-snip-dump 389.0 568.0 0.0 0.0
                       (list (make-snip-dump 377.0 278.0 0.0 0.0 '())
                             (make-snip-dump 377.0 556.0 0.0 278.0 '())))
       (make-snip-dump 768.0 568.0 389.0 0.0 '())))

(send pasteboard insert ae-snip5)
(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 253.0 568.0 0.0 0.0 '())
  (make-snip-dump 516.0 568.0 253.0 0.0
                  (list (make-snip-dump 251.0 278.0 0.0 0.0 '())
                        (make-snip-dump 251.0 556.0 0.0 278.0 '())))
  (make-snip-dump 768.0 568.0 516.0 0.0 '())))

(send pasteboard delete ae-snip5)
(send pasteboard delete ae-snip1)
(dump=?
 (dump-children pasteboard)
 (list (make-snip-dump 768.0 568.0 0.0 0.0
                       (list (make-snip-dump 756.0 278.0 0.0 0.0 '())
                             (make-snip-dump 756.0 556.0 0.0 278.0 '())))))

(send pasteboard erase)
(dump=? (dump-children pasteboard) '())

(send frame show #f)
(printf "done\n")
