#lang racket/gui
(require wxme
         wxme/image)

#|

This file tests the wxme image-snip reader against the normal
image-snip reader (ie image-snip-class%'s read method)

It creates a bunch of different image-snip% objects 
(the try-perms and below functions)
and then feeds them thru both paths to get two new image snips 
(in the beginning of test-wxme-image-snip-reader/proc)
and compares a bunch of properties of them
(the end of that function).

|#

(define-syntax (test-wxme-image-snip-reader stx)
  (syntax-case stx ()
    [(_ is)
     (with-syntax ([line (syntax-line stx)])
       #'(test-wxme-image-snip-reader/proc line is))]))

(define tests 0)
(define (test-wxme-image-snip-reader/proc line is)
  (set! tests (+ tests 1))
  (define t (new text%))
  (send t insert is)
  (define sp (open-output-string))
  (void (send t save-port sp))
  (define wp (wxme-port->port (open-input-string (get-output-string sp))))
  (define wxme-is (read-char-or-special wp))
  
  (define t2 (new text%))
  (send t2 insert-port (open-input-string (get-output-string sp)))
  (define copy-is (send t2 find-first-snip))
  
  (define (warn . args)
    (eprintf (string-append (format "FAILED test-wxme-image-snip-reader.rkt line ~a: " line)
                            (apply format args))))
  
  (define-syntax-rule (cmp mtd) (cmp/proc (λ (x) (send x mtd)) 'mtd))
  (define (cmp/proc call-mtd mtd)
    (let ([is-ans (call-mtd is)]
          [wxme-is-ans (call-mtd wxme-is)]
          [copy-is-ans (call-mtd copy-is)])
      (unless (same? copy-is-ans wxme-is-ans)
        (warn "~a returned different results; copy-is: ~s wxme-is: ~s\n"
              mtd
              copy-is-ans 
              wxme-is-ans))
      #;
      (unless (same? is-ans copy-is-ans)
        (warn "~a returned different results; is: ~s copy-is: ~s\n"
              mtd
              is-ans 
              copy-is-ans))))
  
  (when (is-a? is image%)
    (warn "the input image-snip% is an image%\n"))
  
  (unless (is-a? wxme-is image%)
    (warn "new image snip is not an image%\n"))
  
  (cmp get-filename)
  (cmp get-filetype)
  (cmp get-bitmap)
  (cmp get-bitmap-mask))

(define (same? x y)
  (cond
    [(and (is-a? x bitmap%)
          (is-a? y bitmap%))
     (and (= (send x get-width)
             (send y get-width))
          (= (send x get-height)
             (send y get-height))
          (= (send x get-depth)
             (send y get-depth))
          (check? (bitmap->bytes x #f)
                  (bitmap->bytes y #f)
                  'bitmap/#f)
          (check? (bitmap->bytes x #t)
                  (bitmap->bytes y #t)
                  'bitmap/#t))]
    [else (equal? x y)]))


(define (check? a b what)
  (cond
    [(equal? a b) #t]
    [else 
     ;(eprintf "checking ~s, doesn't match\n~s\nvs\n~s\n\n" what a b)
     #f]))

(define (bitmap->bytes bmp alpha?)
  (define w (send bmp get-width))
  (define h (send bmp get-height))
  (define bytes (make-bytes (* 4 w h) 0))
  (send bmp get-argb-pixels 0 0 w h bytes alpha?)
  bytes)

(define (try-perms files kinds relative-path?s inline?s)
  (for* ([file (in-list files)]
         [kind (in-list kinds)]
         [relative-path? (in-list relative-path?s)]
         [inline? (in-list inline?s)])
    (test-wxme-image-snip-reader (make-object image-snip% file kind relative-path? inline?))))

(try-perms (list (collection-file-path "b-run.png" "icons"))
           '(unknown unknown/mask unknown/alpha
                     png png/mask png/alpha)
           '(#f)
           '(#f #t))

(parameterize ([current-directory (build-path (collection-file-path "b-run.png" "icons") "..")])
  (try-perms (list "b-run.png")
             '(unknown unknown/mask unknown/alpha
                       png png/mask png/alpha)
             '(#f)
             '(#f #t)))

(define (draw-circle bm)
  (define bdc (make-object bitmap-dc% bm))
  (send bdc set-smoothing 'smoothed)
  (send bdc set-brush "red" 'solid)
  (send bdc draw-ellipse 1 1 8 8)
  (send bdc set-bitmap #f))

(let ([bm (make-bitmap 10 10 #f)])
  (draw-circle bm)
  (test-wxme-image-snip-reader (make-object image-snip% bm))
  (test-wxme-image-snip-reader (make-object image-snip% bm #f))
  (test-wxme-image-snip-reader (make-object image-snip% bm bm)))

(let ([bm (make-bitmap 10 10)])
  (draw-circle bm)
  (test-wxme-image-snip-reader (make-object image-snip% bm)))

(printf "ran ~a tests\n" tests)
