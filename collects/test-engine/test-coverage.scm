#lang mzscheme

(require mzlib/class
         mred
         framework
         (prefix list: mzlib/list)
         mzlib/integer-set)

(provide (all-defined))

(define coverage-track%
  (class* object% ()

    (super-instantiate ())

    ;; interger-set
    (define covered (make-range))
    ;; [hashtable-of scheme-val -> integer-set]
    (define covered-from-src (make-hash-table 'weak))
    ;; (listof covered-from-src keys)
    (define current-coverage-srcs null)

    (define/public (covered-position start span)
      (let ([new-range (make-range start (+ start span))])
        (set! covered (union covered new-range))
        (for-each (lambda (key covered-set)
                    (hash-table-put! covered-from-src key
                                     (union covered-set new-range)))
                  current-coverage-srcs
                  (map (lambda (key)
                         (hash-table-get covered-from-src key (make-range)))
                       current-coverage-srcs))))

    (define/public (register-coverage-point src)
      (set! current-coverage-srcs (cons src current-coverage-srcs)))

    (define/public (unregister-coverage-point src)
      (set! current-coverage-srcs (list:remq src current-coverage-srcs)))

    (define/public (covers-span? start span)
      (zero? (card (difference (make-range start (+ start span)) covered))))

    (define/public (covers-spans? srcs)
      (andmap (lambda (s) (covers-span? (car s) (cdr s))) srcs))

    (define/public (display-coverage editor) 
      (highlight-covered editor covered))

    (define/public (display-covered-portion editor coverage-point)
      (highlight-covered editor (hash-table-get covered-from-src coverage-point
                                                (make-range))))


    (define/private (highlight-covered editor int-set)
      (let* ([style-list (editor:get-standard-style-list)]
             [uncovered-highlight (send style-list find-named-style
                                        "profj:syntax-colors:scheme:uncovered")]
             [covered-highlight (send style-list find-named-style
                                      "profj:syntax-colors:scheme:covered")])
        (letrec ([color-buff
                  (lambda ()
                    (cond
                      [(or (send editor is-locked?)
                           (send editor in-edit-sequence?))
                       (queue-callback color-buff)]
                      [else
                       (unless (send editor test-froze-colorer?)
                         (send editor freeze-colorer)
                         (send editor toggle-test-status))
                       (send editor begin-test-color)
                       (send editor change-style
                             uncovered-highlight 0
                             (send editor last-position) #f)
                       (let loop ([positions (integer-set-contents int-set)])
                         (unless (null? positions)
                           (send editor change-style covered-highlight 
                                 (sub1 (caar positions))
                                 (sub1 (cdar positions))
                                 #f)
                           (loop (cdr positions))))
                       (send editor end-test-color)]))])
          (queue-callback color-buff))))))

(define (test-coverage-button-mixin parent)
  (class* parent ()
    (super-instantiate ())

    (define/public (insert-covered-button dest coverage src src-editor partial?)
      (let* ([button-editor (new (editor:standard-style-list-mixin text%)
                                 [auto-wrap #t])]
             [snip (new editor-snip% (editor button-editor) (with-border? #t))]
             [start (send dest get-end-position)])
        (send snip set-style
              (send (send dest get-style-list) find-named-style "Standard"))
        (send button-editor insert
              (if partial?
                "Highlight covered expressions"
                "Highlight all covered expressions"))
        (send dest insert snip)
        (send button-editor set-clickback 0
              (send button-editor get-end-position)
              (cond
                [(and src-editor partial?)
                 (lambda (t s e)
                   (send coverage display-covered-portion src-editor src))]
                [src-editor
                 (lambda (t s e)
                   (send coverage display-coverage src-editor))]
                [else (lambda (t s e) (void))])
              #f #f)
        (send button-editor lock #t)
        (let ([c (new style-delta%)])
          (send c set-delta-foreground "royalblue")
          (send dest change-style c start (sub1 (send dest get-end-position))
                #f))))))

(define analysis<%>
  (interface ()
    register-test register-testcase 
    de-register-test de-register-testcase
    analyze provide-info))

(define coverage-analysis%
  (class* object% (analysis<%>)

    (define coverage-info (make-object coverage-track%))

    (define/public (register-test name src)
      (send coverage-info register-coverage-point src))
    (define/public (register-testcase name src)
      (send coverage-info register-coverage-point src))
    (define/public (de-register-test src)
      (send coverage-info unregister-coverage-point src))
    (define/public (de-register-testcase src)
      (send coverage-info unregister-coverage-point src))
    (define/public (analyze src vals)
      (send coverage-info covered-position (list-ref src 3) (list-ref src 4)))

    (define/public (provide-info) coverage-info)
    (super-instantiate ())))
