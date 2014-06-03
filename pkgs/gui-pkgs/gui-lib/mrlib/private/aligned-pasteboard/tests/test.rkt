#lang racket/gui

(require "../aligned-pasteboard.rkt" "../aligned-editor-container.rkt"
         "snip-dumper.rkt")


;                                     ;;
;                                      ;
;                                      ;
;  ;;;;; ;;  ;;  ;;;;;;;;;;;;;; ;;;    ;     ;;;;;  ;;;;
; ;    ;  ;;;;      ; ;  ;  ; ;;  ;;   ;    ;    ; ;   ;
; ;;;;;;   ;;    ;;;; ;  ;  ; ;    ;   ;    ;;;;;; ;;;;
; ;        ;;   ;   ; ;  ;  ; ;    ;   ;    ;          ;
; ;;   ;  ;  ;  ;   ; ;  ;  ; ;;  ;;   ;    ;;   ; ;   ;
;  ;;;;  ;;  ;;  ;;;;;;; ;; ;;; ;;;  ;;;;;   ;;;;  ;;;;
;                             ;
;                            ;;;

(printf "running test.rkt\n")

(define frame (new frame% [label "Frame"] [width 400] [height 400]))
(define pasteboard (new horizontal-pasteboard%))
(define canvas (new aligned-editor-canvas% [parent frame] [editor pasteboard]))
(define insider  (new vertical-pasteboard%))
(define insider2 (new vertical-pasteboard%))
(define insider3 (new vertical-pasteboard%))
(define insider4 (new vertical-pasteboard%))
(define insider5 (new vertical-pasteboard%))
(define insider6 (new vertical-pasteboard%))
(define insider7 (new vertical-pasteboard%))
(define pb-snip  (new aligned-editor-snip% [editor insider]))
(define pb-snip2 (new aligned-editor-snip% [editor insider2]))
(define pb-snip3 (new aligned-editor-snip% [editor insider3]))
(define pb-snip4 (new aligned-editor-snip% [editor insider4]))
(define pb-snip5 (new aligned-editor-snip% [editor insider5]))
(define pb-snip6 (new aligned-editor-snip% [editor insider6]))
(define pb-snip7 (new aligned-editor-snip% [editor insider7]))
(define t-snip   (new editor-snip% [editor (new text%)]))
(define i-snip   (new image-snip%))
(define i-snip2  (new image-snip%))
(define t-snip2  (new editor-snip% [editor (new text%)]))
(define t-snip3  (new editor-snip% [editor (new text%)]))

(send pasteboard begin-edit-sequence)
(send frame show #t)
(send pasteboard insert pb-snip)
(send pasteboard insert t-snip)
(send pasteboard insert i-snip)
(send pasteboard insert i-snip2)
(send pasteboard insert pb-snip2)
(send pasteboard insert t-snip2)
(send insider insert t-snip3)
(send insider2 insert pb-snip3)
(send insider2 insert pb-snip4)
(send pasteboard insert pb-snip5)
(send pasteboard insert pb-snip6)
(send pasteboard insert pb-snip7)
(send pasteboard end-edit-sequence)


; ;                    ;
; ;                    ;
;;;;;;;   ;;;;;  ;;;; ;;;;;    ;;;;
; ;      ;    ; ;   ;  ;      ;   ;
; ;      ;;;;;; ;;;;   ;      ;;;;
; ;      ;          ;  ;          ;
; ;      ;;   ; ;   ;  ;      ;   ;
; ;;;;;   ;;;;  ;;;;   ;;;;;  ;;;;

;; Eli: Looks like these tests are supposed to return #t, so most are
;; failing (wasn't visible when this was running via "gracket -f")

(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 57.0 368.0 0.0 0.0 '())
  (make-snip-dump 114.0 368.0 57.0 0.0 '())
  (make-snip-dump 171.0 368.0 114.0 0.0 '())
  (make-snip-dump 182.0 24.0 171.0 0.0 '())
  (make-snip-dump 249.0 368.0 182.0 0.0
                  (list (make-snip-dump 55.0 178.0 0.0 0.0 '())
                        (make-snip-dump 55.0 356.0 0.0 178.0 '())))
  (make-snip-dump 269.0 20.0 249.0 0.0 #f)
  (make-snip-dump 289.0 20.0 269.0 0.0 #f)
  (make-snip-dump 300.0 24.0 289.0 0.0 '())
  (make-snip-dump 368.0 368.0 300.0 0.0
                  (list (make-snip-dump 11.0 24.0 0.0 0.0 '())))))

(send frame resize 0 0)
(sleep/yield 0.5)

(dump=?
 (dump-children pasteboard)
 (list
  (make-snip-dump 10.0 34.0 0.0 0.0 '())
  (make-snip-dump 20.0 34.0 10.0 0.0 '())
  (make-snip-dump 30.0 34.0 20.0 0.0 '())
  (make-snip-dump 41.0 24.0 30.0 0.0 '())
  (make-snip-dump 61.0 34.0 41.0 0.0
                  (list (make-snip-dump 10.0 11.0 0.0 0.0 '())
                        (make-snip-dump 10.0 22.0 0.0 11.0 '())))
  (make-snip-dump 81.0 20.0 61.0 0.0 #f)
  (make-snip-dump 101.0 20.0 81.0 0.0 #f)
  (make-snip-dump 112.0 24.0 101.0 0.0 '())
  (make-snip-dump 133.0 34.0 112.0 0.0
                  (list (make-snip-dump 11.0 24.0 0.0 0.0 '())))))

(send frame resize 800 600)
(sleep/yield 0.5)

(dump=?
 (dump-children pasteboard)
 (list (make-snip-dump 137.0 568.0 0.0 0.0 '())
       (make-snip-dump 274.0 568.0 137.0 0.0 '())
       (make-snip-dump 411.0 568.0 274.0 0.0 '())
       (make-snip-dump 422.0 24.0 411.0 0.0 '())
       (make-snip-dump 569.0 568.0 422.0 0.0
                       (list (make-snip-dump 135.0 278.0 0.0 0.0 '())
                             (make-snip-dump 135.0 556.0 0.0 278.0 '())))
       (make-snip-dump 589.0 20.0 569.0 0.0 #f)
       (make-snip-dump 609.0 20.0 589.0 0.0 #f)
       (make-snip-dump 620.0 24.0 609.0 0.0 '())
       (make-snip-dump 768.0 568.0 620.0 0.0
                       (list (make-snip-dump 11.0 24.0 0.0 0.0 '())))))

(send frame resize 400 400)
(send pasteboard delete i-snip)
(send pasteboard delete i-snip2)

(dump=?
 (dump-children pasteboard)
 (list (make-snip-dump 65.0 368.0 0.0 0.0 '())
       (make-snip-dump 130.0 368.0 65.0 0.0 '())
       (make-snip-dump 195.0 368.0 130.0 0.0 '())
       (make-snip-dump 206.0 24.0 195.0 0.0 '())
       (make-snip-dump 281.0 368.0 206.0 0.0
                       (list (make-snip-dump 63.0 178.0 0.0 0.0 '())
                             (make-snip-dump 63.0 356.0 0.0 178.0 '())))
       (make-snip-dump 292.0 24.0 281.0 0.0 '())
       (make-snip-dump 368.0 368.0 292.0 0.0
                       (list (make-snip-dump 11.0 24.0 0.0 0.0 '())))))

(send pasteboard erase)
(dump=? (dump-children pasteboard) '())

(send frame show #f)
(printf "done\n")
