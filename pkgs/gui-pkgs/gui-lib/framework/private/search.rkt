#lang scheme/base
(require racket/contract/base
         racket/class
         scheme/gui/base)

(provide/contract
 [find-string-embedded
  (->* ((is-a?/c text%)
        string?)
       ((symbols 'forward 'backward)
        (or/c (symbols 'start) number?)
        (or/c (symbols 'eof) number?)
        boolean?
        boolean?
        boolean?)
      (values (is-a?/c editor<%>)
              (or/c false/c number?)))])

(define find-string-embedded
  (lambda (edit
           str
           [direction 'forward]
           [start 'start]
           [end 'eof]
           [get-start #t]
           [case-sensitive? #t]
           [pop-out? #f])
    (let/ec k
      (let* ([start (if (eq? start 'start) 
                        (send edit get-start-position)
                        start)]
             [end (if (eq? 'eof end)
                      (if (eq? direction 'forward)
                          (send edit last-position)
                          0)
                      end)]
             [flat (send edit find-string str direction
                         start end get-start
                         case-sensitive?)]
             [pop-out
              (λ ()
                (let ([admin (send edit get-admin)])
                  (if (is-a? admin editor-snip-editor-admin<%>)
                      (let* ([snip (send admin get-snip)]
                             [edit-above (send (send snip get-admin) get-editor)]
                             [pos (send edit-above get-snip-position snip)]
                             [pop-out-pos (if (eq? direction 'forward) (add1 pos) pos)])
                        (find-string-embedded
                         edit-above
                         str
                         direction 
                         pop-out-pos
                         (if (eq? direction 'forward) 'eof 0)
                         get-start
                         case-sensitive?
                         pop-out?))
                      (values edit #f))))])
        (let loop ([current-snip (send edit find-snip start
                                       (if (eq? direction 'forward)
                                           'after-or-none
                                           'before-or-none))])
          (let ([next-loop
                 (λ ()
                   (if (eq? direction 'forward)
                       (loop (send current-snip next))
                       (loop (send current-snip previous))))])
            (cond
              [(or (not current-snip)
                   (and flat
                        (let* ([start (send edit get-snip-position current-snip)]
                               [end (+ start (send current-snip get-count))])
                          (if (eq? direction 'forward)
                              (and (<= start flat)
                                   (< flat end))
                              (and (< start flat)
                                   (<= flat end))))))
               (if (and (not flat) pop-out?)
                   (pop-out)
                   (values edit flat))]
              [(is-a? current-snip editor-snip%)
               (let-values ([(embedded embedded-pos)
                             (let ([media (send current-snip get-editor)])
                               (if (and media
                                        (is-a? media text%))
                                   (begin
                                     (find-string-embedded 
                                      media 
                                      str
                                      direction
                                      (if (eq? 'forward direction)
                                          0
                                          (send media last-position))
                                      'eof
                                      get-start case-sensitive?))
                                   (values #f #f)))])
                 (if (not embedded-pos)
                     (next-loop)
                     (values embedded embedded-pos)))]
              [else (next-loop)])))))))
