#lang racket

#|

This test checks:

  - if opening frames/tabs leaks,

  - if DrRacket writes to the filesystem during startup or
    while opening tabs/frames

  - if there are any adjacent separators in the menus

|#

(require "private/drracket-test-util.rkt"
         racket/gui/base
         framework)

(define (main)
  (parameterize ([current-security-guard
                  (make-security-guard
                   (current-security-guard)
                   (λ (who pth what)
                     (when (member 'write what)
                       (error who "Writing to the file system is not allowed"))
                     (when (member 'delete what)
                       (error who "Deleting files is not allowed")))
                   void
                   void)])
    (fire-up-drracket-and-run-tests 
     (λ ()
       (define drs-frame1 (wait-for-drracket-frame))
       (sync (system-idle-evt))
       
       (check-menus drs-frame1)
       
       (for ([tries (in-range 3)])
         (test:menu-select "File" "New Tab")
         (sync (system-idle-evt))
         
         (define drs-tabb (make-weak-box (send drs-frame1 get-current-tab)))
         (define tab-nsb (make-weak-box (send (send (send drs-frame1 get-current-tab) get-ints) get-user-namespace)))
         
         (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Tab"))
         (sync (system-idle-evt))
         
         (test:menu-select "File" "New")
         (sync (system-idle-evt))
         
         (define drs-frame2b (make-weak-box (wait-for-new-frame drs-frame1)))
         (define frame2-nsb (make-weak-box (send (send (send (weak-box-value drs-frame2b) get-current-tab) get-ints) get-user-namespace)))
         
         (queue-callback/res
          (λ () (send (send (send (weak-box-value drs-frame2b) get-current-tab) get-defs) load-file
                      (collection-file-path "unit.rkt" "drracket" "private"))))
         (sleep 2)
         (sync (system-idle-evt))
         
         (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Window"))
         (sync (system-idle-evt))
         
         (let loop ([n 30])
           (cond
             [(zero? n) 
              (when (weak-box-value drs-tabb)
                (eprintf "frame leak!\n"))
              (when (weak-box-value drs-frame2b)
                (eprintf "tab leak!\n"))
              (when (weak-box-value tab-nsb)
                (eprintf "tab namespace leak!\n"))
              (when (weak-box-value frame2-nsb)
                (eprintf "frame namespace leak!\n"))]
             [else
              (collect-garbage) (sync (system-idle-evt))
              (when (ormap weak-box-value 
                           (list drs-tabb
                                 tab-nsb
                                 drs-frame2b
                                 frame2-nsb))
                (loop (- n 1)))])))))))

(define (check-menus frame)
  
  (define (process-container container)
    (define sub-items (send container get-items))
    (unless (null? sub-items)
      (when (is-a? (car sub-items) menu-item-container<%>)
        (process-container (car sub-items)))
      (define printed? #f)
      (for ([prev-item (in-list sub-items)]
            [item (in-list (cdr sub-items))])
        (when (and (is-a? prev-item separator-menu-item%)
                   (is-a? item separator-menu-item%)
                   (not printed?))
          (set! printed? #t)
          (eprintf "found two adjacent separator items in: ~s:\n" (get-names container))
          (for ([item (in-list sub-items)])
            (eprintf "  ~a\n" (get-lab item)))
          (eprintf "\n"))
        (when (is-a? item menu-item-container<%>)
          (process-container item)))))
  
  (define (get-lab item)
    (cond
      [(is-a? item labelled-menu-item<%>)
       (send item get-label)]
      [(is-a? item separator-menu-item%)
       "---------"]
      [else #f]))
  
  (define (get-names item)
    (let loop ([item item])
      (cond
        [(is-a? item menu-item<%>)
         (cons (get-lab item)
               (loop (send item get-parent)))]
        [else
         '()])))
  
  (process-container (send frame get-menu-bar)))

(main)
