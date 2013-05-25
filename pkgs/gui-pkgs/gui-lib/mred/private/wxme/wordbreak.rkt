#lang racket/base
(require racket/class
         "../syntax.rkt"
         "cycle.rkt")

(provide editor-wordbreak-map%
         the-editor-wordbreak-map
         standard-wordbreak)

(defclass editor-wordbreak-map% object%
  (define char-map (make-hash))

  (super-new)

  (hash-set! char-map #\- '(line))

  (def/public (set-map [char? ch] [(make-list (symbol-in caret line selection user1 user2)) mask])
    (hash-set! char-map ch mask))

  (def/public (get-map [char? ch])
    (or (hash-ref char-map ch #f)
        (cond
         [(or (char-alphabetic? ch)
              (char-numeric? ch))
          '(caret line selection)]
         [(not (char-whitespace? ch))
          '(line)]
         [else null]))))

(define the-editor-wordbreak-map (new editor-wordbreak-map%))

(define MAX-DIST-TRY 30)

(define wb-get-map (generic editor-wordbreak-map% get-map))

(define (string-ref* str n)
  (if (n . >= . (string-length str))
      #\nul
      (string-ref str n)))

(define/top (standard-wordbreak [text% win] 
                                [(make-or-false (make-box exact-nonnegative-integer?)) startp]
                                [(make-or-false (make-box exact-nonnegative-integer?)) endp]
                                [(symbol-in caret line selection user1 user2) reason])
  (let ([wb (send win get-wordbreak-map)])
    (when wb
      (with-method ([get-map (wb get-map)])
          (define (nonbreak? ch) (memq reason (get-map ch)))

        (when startp
          (let* ([start (unbox startp)]
                 [pstart start]
                 [lstart (send win find-newline 'backward start 0)]
                 [lstart (if lstart
                             (if (eq? 'caret reason)
                                 (or (and (positive? lstart)
                                          (send win find-newline 'backward (sub1 lstart) 0))
                                     0)
                                 lstart)
                             0)]
                 [lend (min (+ start 1) (send win last-position))]
                 [tstart (if ((- start lstart) . > . MAX-DIST-TRY)
                             (- start MAX-DIST-TRY)
                             lstart)]
                 [text (send win get-text tstart lend)]
                 [start (- start tstart)]
                 [pstart (- pstart tstart)])

            (let ploop ([phase1-complete? #f]
                        [phase2-complete? #f]
                        [start start]
                        [pstart pstart]
                        [text text]
                        [tstart tstart])
              (let*-values ([(start phase1-complete?)
                             (if phase1-complete?
                                 (values start #t)
                                 (let ([start (if (and (positive? start)
                                                       (nonbreak? (string-ref* text start)))
                                                  (sub1 start)
                                                  start)])
                                   (values start
                                           (not (nonbreak? (string-ref* text start))))))]
                            [(start phase2-complete?)
                             (if (not (eq? 'selection reason))
                                 (if (not phase2-complete?)
                                     (let loop ([start start])
                                       (if (and (positive? start)
                                                (not (nonbreak? (string-ref* text start))))
                                           (loop (sub1 start))
                                           (if (nonbreak? (string-ref* text start))
                                               (values start #t)
                                               (values start #f))))
                                     (values start #t))
                                 (values start phase2-complete?))])
                (let loop ([start start])
                  (if (and (positive? start)
                           (nonbreak? (string-ref* text start)))
                      (loop (sub1 start))
                      (let ([start (if (and (start . < . pstart)
                                            (not (nonbreak? (string-ref* text start))))
                                       (add1 start)
                                       start)])
                        (if (and (zero? start)
                                 (not (= lstart tstart)))
                            (ploop phase1-complete?
                                   phase2-complete?
                                   (+ start (- tstart lstart))
                                   (+ pstart (- tstart lstart))
                                   (send win get-text lstart lend)
                                   lstart)
                            (set-box! startp (+ start tstart))))))))))
        
        (when endp
          (let* ([end (unbox endp)]
                 [lstart end]
                 [lend (send win find-newline 'forward end)]
                 [lend (if lend
                           (if (eq? 'caret reason)
                               (or (send win find-newline 'forward (+ lend 1))
                                   (send win last-position))
                               lend)
                           (send win last-position))]
                 [tend (if ((- lend end) . > . MAX-DIST-TRY)
                           (+ end MAX-DIST-TRY)
                           lend)]
                 [text (send win get-text lstart tend)]
                 [end (- end lstart)]
                 [lend (- lend lstart)]
                 [tend (- tend lstart)])
            
            (let ploop ([phase1-complete? #f]
                        [text text]
                        [tend tend]
                        [end end])
              (let-values ([(end phase1-complete?)
                            (if phase1-complete?
                                (values end #t)
                                (let loop ([end end])
                                  (if (and (end . < . tend) 
                                           (not (nonbreak? (string-ref* text end))))
                                      (loop (add1 end))
                                      (if (end . < . tend)
                                          (values end #t)
                                          (values end #f)))))])
                (let loop ([end end])
                  (if (and (end . < . tend)
                           (nonbreak? (string-ref* text end)))
                      (loop (add1 end))
                      (if (and (= tend end) (not (= lend tend)))
                          (ploop phase1-complete?
                                 (send win get-text lstart (+ lstart lend))
                                 lend
                                 end)
                          (set-box! endp (+ end lstart)))))))))))))
