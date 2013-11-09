#lang racket

#|

This test checks:

  - if opening frames/tabs leaks,

  - if DrRacket writes to the filesystem during startup or
    while opening tabs/frames

  - if there are any adjacent separators in the menus

  - if there are any duplicate shortcuts in the menus

  - that the logger GUI is not completely broken (checks
    that it opens, that it catches a GC message, and that
    it closes)

  - that the reorder tabs method doesn't break

|#

(require "private/drracket-test-util.rkt"
         drracket/private/local-member-names
         racket/gui/base
         framework
	 rackunit
         string-constants)

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
     #:prefs '([plt:framework-pref:drracket:online-compilation-default-on #f]
               [plt:framework-pref:framework:autosaving-on? #f]) 
     (λ ()
       (define drr (wait-for-drracket-frame))
       (check-reorder-tabs drr)
       (check-log-panel drr)
       (check-menus drr)
       
       (try-to-find-leak "online compilation disabled:" void)
       
       (preferences:set 'drracket:online-compilation-default-on #t)
       
       (try-to-find-leak "online compilation enabled:" wait-for-online-compilation-to-finish)))))

(define (wait-for-online-compilation-to-finish frame) 
  (let loop ([i 0])
    (define current-colors (send frame get-online-expansion-colors))
    (unless (equal? current-colors '("forestgreen"))
      (sleep 1)
      (loop (+ i 1)))))

(define (try-to-find-leak online-compilation-string extra-waiting)
  (define drs-frame1 (wait-for-drracket-frame))
  (sync (system-idle-evt))

  (for ([tries (in-range 3)])
    (test:menu-select "File" "New Tab")
    (sync (system-idle-evt))
    
    (define drs-tabb (make-weak-box (send drs-frame1 get-current-tab)))
    (define tab-nsb (make-weak-box (send (send (send drs-frame1 get-current-tab) get-ints)
                                         get-user-namespace)))
    
    (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Tab"))
    (sync (system-idle-evt))
    
    (test:menu-select "File" "New")
    (sync (system-idle-evt))
    
    (define drs-frame2b (make-weak-box (wait-for-new-frame drs-frame1)))
    (define frame2-nsb (make-weak-box 
                        (send (send (send (weak-box-value drs-frame2b) get-current-tab) get-ints) 
                              get-user-namespace)))
    
    (queue-callback/res
     (λ () (send (send (send (weak-box-value drs-frame2b) get-current-tab) get-defs) load-file
                 (collection-file-path "rep.rkt" "drracket" "private"))))
    (sleep 2)
    (extra-waiting (weak-box-value drs-frame2b))
    (sync (system-idle-evt))
    
    (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Window"))
    (sync (system-idle-evt))
    
    (let loop ([n 30])
      (cond
        [(zero? n) 
         (when (weak-box-value drs-tabb)
           (eprintf "~a tab leak!\n" online-compilation-string))
         (when (weak-box-value drs-frame2b)
           (eprintf "~a frame leak!\n" online-compilation-string))
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

(define (check-reorder-tabs drr)
  (test:menu-select "File" "New")
  (define drr2 (wait-for-new-frame drr))

  (send drr2 create-new-tab)
  (send drr2 create-new-tab)
  (define tabs (send drr2 get-tabs))

  (send drr2 reorder-tabs (reverse (range (length tabs))))
  (define new-tabs (send drr2 get-tabs))
  (check-equal? new-tabs (reverse tabs))

  (send drr2 reorder-tabs (reverse (range (length tabs))))
  (define new-tabs2 (send drr2 get-tabs))
  (check-equal? new-tabs2 tabs)

  (cond
    [(eq? (system-type) 'unix)
     (for ([_ (in-range (length tabs))])
       (test:menu-select "File" "Close"))]
    [else
     (test:menu-select "File" "Close Window")])

  (wait-for-new-frame drr2))

(define (check-log-panel drr)
  
  (define (find-log-messages-message)
    (let loop ([p drr])
      (cond
        [(is-a? p area-container<%>)
         (for/or ([c (in-list (send p get-children))])
           (loop c))]
        [(is-a? p message%)
         (and (equal? (send p get-label) (string-constant log-messages))
              p)]
        [else #f])))
  
  (test:menu-select "View" "Show Log")
  
  ;; wait for the log window to show up.
  (poll-until
   (λ ()
     (queue-callback/res find-log-messages-message)))
  
  (collect-garbage)
  
  (define logger-string
    (poll-until
     (λ ()
       (define str
         (queue-callback/res
          (λ ()
            (define msg (find-log-messages-message))
            (define msg-parent-parent (send (send msg get-parent) get-parent))
            (let loop ([p msg-parent-parent])
              (cond
                [(is-a? p area-container<%>)
                 (for/or ([c (in-list (send p get-children))])
                   (loop c))]
                [(is-a? p editor-canvas%)
                 (send (send p get-editor) get-text)]
                [else #f])))))
       (and str
            
            ;; wait for a log message that doesn't look like a minor GC
            (for/or ([l (in-lines (open-input-string str))])
              (not (regexp-match? #rx"^GC: [0-9]*:min" l)))
            
            str))))
  
  (unless (regexp-match #rx"GC: [0-9]+:MAJ" logger-string)
    (error 'check-log-panel "didn't find GC log message: ~s" logger-string))
  
  (test:menu-select "View" "Hide Log")
  
  (poll-until
   (λ ()
     (not (queue-callback/res find-log-messages-message)))))

(main)
