#lang racket/base
(require racket/class
         racket/gui/base
         racket/contract)

(provide/contract 
 [find-tag 
  (-> (is-a?/c text%)
      exact-nonnegative-integer?
      (or/c #f string?))])

;; finds the name of the XML tag just before `start' in `text'.
;; returns the tag name, with no trailing space of > or anything like that.
(define (find-tag text start)
  ;; loop iterates backwards, searching for #\<
  ;; when it finds it, it gets the string starting
  ;; there, forwards to last-space (if there was a space)
  ;; or start-1.
  ;; If there is a #\/ or a #\> just after the #\<, return #f
  ;; (in that case, they are typing a close tag or closing an empty tag)
  ;; this technique gleaned from the spec at:
  ;;  http://www.w3.org/TR/2000/REC-xml-20001006
  ;; If there are two hyphens after the #\<, then assume this is a
  ;; comment and just give up.
  (let loop ([pos (- start 2)]
             [last-space #f])
    (cond
      [(< pos 0) #f]
      [else
       (let ([char (send text get-character pos)])
         (case char
           [(#\>) #f]
           [(#\/) (if last-space
                      (loop (- pos 1) last-space)
                      #f)]
           [(#\<) 
            (cond
              [(or (char=? (send text get-character (+ pos 1)) #\/)
                   (char=? (send text get-character (+ pos 1)) #\>))
               ;;technique gleaned, as above
               #f]
              [(and (< (+ pos 3) (send text last-position))
                    (char=? (send text get-character (+ pos 1)) #\!)
                    (char=? (send text get-character (+ pos 2)) #\-)
                    (char=? (send text get-character (+ pos 3)) #\-))
               ;; comment, just give up
               #f]
              [else
               (send text get-text (+ pos 1) (or last-space (- start 1)))])]
           [(#\space #\return #\newline #\tab)
            (loop (- pos 1) pos)]
           [else (loop (- pos 1) last-space)]))])))
