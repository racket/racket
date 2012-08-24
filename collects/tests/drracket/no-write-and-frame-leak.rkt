#lang racket

#|

This test checks:

  - if opening frames/tabs leaks,

  - if DrRacket writes to the filesystem during startup or
    while opening tabs/frames

  - if there are any adjacent separators in the menus

  - if there are any duplicate shortcuts in the menus

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
     #:prefs '([plt:framework-pref:drracket:online-compilation-default-on #f]) 
     (λ ()
       (check-menus (wait-for-drracket-frame))
       
       (try-to-find-leak "online compilation disabled:")
       
       (preferences:set 'drracket:online-compilation-default-on #t)
       
       (try-to-find-leak "online compilation enabled:")))))

(define (try-to-find-leak online-compilation-string)
  (define drs-frame1 (wait-for-drracket-frame))
  (sync (system-idle-evt))

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
           (eprintf "~a frame leak!\n" online-compilation-string))
         (when (weak-box-value drs-frame2b)
           (eprintf "~a tab leak!\n" online-compilation-string))
         (when (weak-box-value tab-nsb)
           (eprintf "~a tab namespace leak!\n" online-compilation-string))
         (when (weak-box-value frame2-nsb)
           (eprintf "~a frame namespace leak!\n" online-compilation-string))]
        [else
         (collect-garbage) (sync (system-idle-evt))
         (when (ormap weak-box-value 
                      (list drs-tabb
                            tab-nsb
                            drs-frame2b
                            frame2-nsb))
           (loop (- n 1)))]))))
  
(define (check-menus frame)
  
  (define shortcuts (make-hash))
  
  (define (process-container container)
    (define sub-items (send container get-items))
    (unless (null? sub-items)
      (record-shortcut (car sub-items))
      (when (is-a? (car sub-items) menu-item-container<%>)
        (process-container (car sub-items)))
      (define printed? #f)
      (for ([prev-item (in-list sub-items)]
            [item (in-list (cdr sub-items))])
        (record-shortcut item)
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
  
  (define (record-shortcut item)
    (when (is-a? item selectable-menu-item<%>)
      (when (send item get-shortcut)
        (define k (append (sort (send item get-shortcut-prefix)
                                string<=?
                                #:key symbol->string)
                          (list (send item get-shortcut))))
        (hash-set! shortcuts 
                   k
                   (cons (send item get-label)
                         (hash-ref shortcuts k '()))))))
  
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
  
  (define (check-shortcuts)
    (for ([(k v) (in-hash shortcuts)])
      (unless (= 1 (length v))
        (eprintf "found multiple menu items with the shortcut ~s: ~s\n"
                 k v))))
  
  (process-container (send frame get-menu-bar))
  (check-shortcuts))

(main)
