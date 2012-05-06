#lang racket/base

(provide wrap-width wrap-line)

;; The default wrap width for `wrape-line'.
(define wrap-width (make-parameter 79))

;; `wrap-line' wraps the `line' string into a list of strings that fit `width'
;; characters.
;;
;; This is intended to be a building block for fancier wrappers -- for
;; examples, ones that have a prefix for all lines, that indent/outdent of the
;; first line, that treat some prefix regexp as a known indentation, that
;; "squeeze" all whitespaces, that can be used as part of some other output and
;; use the starting column to set the width, or even ones that take a multiline
;; string and identify paragraphs to wrap individually.  For now, it's not
;; intended to be used directly so it's only documented here -- this is likely
;; to change if the interface is stable enough.
;;
;; `width' can be either a width or an improper list of widths, where the last
;; element is used for the rest.  For example, (cons 72 70) indicates a width
;; of 72 characters for the first line, and 70 for the rest.
;;
;; `split-word' controls what happens when a word is split: it is invoked with
;; the word that is to be split, and an integer indicating how many characters
;; could fit on the first line.  In most cases the string will be just a word,
;; but if it was the first/last word with only spaces before/after it, then the
;; string will have these spaces too.  It is expected to return three values:
;; the first two are a string to put on the current line and a string that goes
;; first on the next one.  Either of these can be `#f' to indicate nothing
;; (this is different from returning "", since that will keep the spaces next
;; to it).  The third result is relevant when the second result is longer than
;; the wrapping width: #t indicates that the second result is considered a word
;; and should not be rewrapped, and #f indicates that it can.  Spaces from the
;; original string are preserved, even if there are more than one space between
;; words; spaces between a word that moves to the next line and the preceding
;; word are usually dropped, but they can be preserved too if the first result
;; is "" rather than #f (and same for the space between a word that stays on
;; the same line when the following word moves down).  If the first result is
;; `#f' and it was the only word on the line, then the line that would
;; otherwise be empty is dropped.  Note that depending on what `split-word'
;; chooses to do, the result may still have lines that are longer than `width'
;; characters.  The default `split-word' returns (values #f the-word #t).
;;
;; Side note: never returns an empty list.
;;
;; Caveats: considers only spaces as whitespace; not too efficient since it
;; constructs intermediate strings for its processing; the need for the third
;; boolean result from `split-word' is suspicious and might be dropped or
;; changed.
;;
(define (wrap-line line [width* (wrap-width)] [split-word #f])
  (define (loop str strs width*)
    (define width (if (pair? width*) (car width*) width*))
    (define strlen (string-length str))
    (define (do-word s1 w1 w2 s2) ; s1--spaces--w1--word--w2--spaces--s2
      (define w1* (if (eq? 0 s1) 0 w1))       ;\ w1*..w2* includes spaces
      (define w2* (if (eq? strlen s2) s2 w2)) ;/ for first/last word
      (define word (substring str w1* w2*))
      (define-values [r1 r2 r3]
        (if split-word (split-word word (- width w1*)) (values #f word #t)))
      (let* ([1st (cond [r1 (string-append (substring str 0 w1*) r1)]
                        [(eq? w1* 0) #f]
                        [else (substring str 0 s1)])]
             [strs (if 1st (cons 1st strs) strs)]
             [width* (cond [(not r1) width*]
                           [(pair? width*) (cdr width*)]
                           [else width*])]
             [2nd (and r2 r3
                       (>= (string-length r2)
                           (if (pair? width*) (car width*) width*))
                       r2)]
             [strs (if 2nd (cons 2nd strs) strs)]
             [width* (cond [(not r2) width*]
                           [(pair? width*) (cdr width*)]
                           [else width*])]
             [rst (cond [(and (not 2nd) r2)
                         (string-append r2 (substring str w2*))]
                        [(eq? w2* strlen) #f]
                        [else (substring str s2)])])
        (if rst (loop rst strs width*) (reverse strs))))
    (cond
      [(strlen . <= . width) (reverse (cons str strs))]
      ;; wrapping point is inside a word
      [(regexp-match-positions #rx"^([^ ][^ ]+) *" str (sub1 width))
       => (λ (m2)
            (define m1 (regexp-match-positions #rx" *([^ ]+)$" str 0 width))
            (do-word (caar m1) (caadr m1) (cdadr m2) (cdar m2)))]
      ;; wrapping point is probably between words
      [(regexp-match-positions #rx" *([^ ]+) *$" str 0 width)
       => (λ (m1)
            (define m2 (regexp-match-positions #rx"[^ ]" str width))
            (if m2 ; there is a following word, so we are between words
              (loop (substring str (caar m2))
                    (cons (substring str 0 (cdadr m1)) strs)
                    (if (pair? width*) (cdr width*) width*))
              ;; no following word, so the following spaces are part of a word
              ;; that does need to be split
              (do-word (caar m1) (caadr m1) strlen strlen)))]
      ;; all spaces in first line => part of the following word
      [(regexp-match-positions #rx"([^ ]+) *" str width)
       => (λ (m) (do-word 0 (caar m) (cdadr m) (cdar m)))]
      ;; no word at all => split the whole "word" (which is all spaces)
      [else (do-word 0 0 strlen strlen)]))
  (loop line '() width*))
