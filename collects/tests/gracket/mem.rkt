#lang racket/gui

(define source-dir (current-load-relative-directory))

(define num-times 80)
(define num-threads 3)

(define dump-stats? #f)

(define edit? #t)
(define insert? #t)
(define load-file? #f) ; adds a lot of messy objects

(define menus? #t)
(define atomic? #t)
(define offscreen? #t)
(define frame? #t)

(define subwindows? #t)

(define allocated '())
(define (remember tag v)
  (set! allocated
        (cons (cons tag (make-weak-box v))
              allocated))
  v)

(define sub-collect-frame
  (and subwindows?
       (make-object frame% "sub-collect")))
(define sub-collect-panel
  (and subwindows?
       (make-object panel% sub-collect-frame)))

(define permanent-ready? #f)
(define mb-lock (make-semaphore 1))

(define htw (make-weak-hasheq))

(send sub-collect-frame show #t)

(define (get-panel% n)
  (case (modulo n 3)
    [(0) panel%]
    [(1) vertical-panel%]
    [(2) horizontal-panel%]))

(define (get-pane% n)
  (case (modulo n 6)
    [(0) pane%]
    [(1) vertical-pane%]
    [(2) horizontal-pane%]
    [else (get-panel% n)]))

(define (get-image n)
  (build-path (collection-path "icons")
   (case (modulo n 4)
     [(0) "mini-plt.xpm"]
     [(1) "lock.gif"]
     [(2) "help.bmp"]
     [(3) "return.xbm"])))

(define (maker id n)
  (sleep)
  (collect-garbage)
  (collect-garbage)
  (printf "Thread: ~s Cycle: ~s\n" id n)
  ; (dump-object-stats)
  ; (if (and dump-stats? (= id 1))
  ;    (dump-memory-stats))
  (unless (zero? n)
    (let ([tag (cons id n)])
      (let* ([edit (remember tag (make-object text%))]
             [ef (let ([f (make-object frame% "Editor Frame")])
                   (send (make-object editor-canvas% f) set-editor edit)
                   (remember tag f))]
             [c (make-custodian)]
             [es (parameterize ([current-custodian c])
                   (make-eventspace))])

        (when edit?
          (send ef show #t)
          (sleep 0.1))

        (parameterize ([current-eventspace es])
          (send (remember 
                 tag
                 (make-object
                  (class timer%
                    (init-rest args)
                    (override* [notify (lambda () (void))])
                    (apply super-make-object args))))
                start 100))

        (when frame?
            (let* ([f (remember tag
                       (make-object (if (even? n)
                                       frame% 
                                       dialog%)
                                   "Tester" #f 200 200))]
                   [cb (lambda (x y) f)]
                   [p (remember tag (make-object (get-pane% n) f))])
              (remember tag (make-object canvas% f))
              (when (zero? (modulo n 3))
                (thread (lambda () (send f show #t)))
                (let loop () (sleep) (unless (send f is-shown?) (loop))))
              (remember tag (make-object button% "one" p cb))
              (let ([class check-box%])
                (let loop ([m 10])
                  (unless (zero? m)
                    (remember (cons tag m)
                              (make-object class "another" p cb))
                    (loop (sub1 m)))))
              (remember tag (make-object check-box% "check" p cb))
              (remember tag (make-object choice% "choice" '("a" "b" "c") p cb))
              (remember tag (make-object list-box% "list" '("apple" "banana" "coconut")
                                         p cb))
              (remember tag (make-object button% "two" p cb))
              (send f show #f)))

        (when subwindows?
            (let ([p (make-object (get-panel% n) sub-collect-frame)]
                  [cv (make-object canvas% sub-collect-frame)]
                  [add-objects
                   (lambda (p tag hide?)
                     (let ([b (let* ([x #f]
                                     [bcb (lambda (a b) x)])
                                (set! x (make-object button% "one" p bcb))
                                x)]
                           [c (make-object check-box% "check" p void)]
                           [co (make-object choice% "choice" '("a" "b" "c") p void)]
                           [cv (make-object canvas% p)]
                           [lb (make-object list-box% "list" '("apple" "banana" "coconut") p void)])
                       (when hide?
                         (send p delete-child b)
                         (send p delete-child c)
                         (send p delete-child cv)
                         (send p delete-child co)
                         (send p delete-child lb))
                       (remember tag b)
                       (remember tag c)
                       (remember tag cv)
                       (remember tag co)
                       (remember tag lb)))])
              (add-objects sub-collect-panel (cons 'sc1 tag) #t)
              (add-objects p (cons 'sc2 tag) #f)
              (remember (cons 'sc0 tag) p)
              (remember (cons 'sc0 tag) cv)
              (send sub-collect-frame delete-child p)
              (send sub-collect-frame delete-child cv)))

        (when (and edit? insert?)
            (let ([e edit])
              (send e begin-edit-sequence)
              (when load-file?
                (send e load-file (build-path source-dir "mem.rkt")))
              (let loop ([i 20])
                (send e insert (number->string i))
                (unless (zero? i)
                  (loop (sub1 i))))
              (let ([s (make-object editor-snip%)])
                (send (send s get-editor) insert "Hello!")
                (send e insert s))
              (send e insert #\newline)
              (send e insert "done")
              (send e set-modified #f)
              (send e end-edit-sequence)))
        
        (when menus?
          (let ([f (remember tag (make-object frame% "MB Frame 0"))])
            (remember tag (make-object menu% "TM1" (remember (cons 'q tag) (make-object menu-bar% f)))))
          (let* ([mb (remember tag (make-object menu-bar% ef))]
                 [m (remember tag (make-object menu% "Ok" mb))])
            (remember tag (make-object menu-item% "Hi" m void))
            (remember tag (make-object menu-item% "There" m void #\t))
            (remember tag
                      (make-object checkable-menu-item%
                                   "Checkable"
                                   (remember tag (make-object menu% "Hello" m))
                                   void))
            (let ([i (remember tag (make-object menu-item% "Delete Me" m void))])
              (send i delete)))

          (when subwindows?
            (unless permanent-ready?
              (semaphore-wait mb-lock)
              (unless (send sub-collect-frame get-menu-bar)
                (let ([mb (make-object menu-bar% sub-collect-frame)])
                  (make-object menu% "Permanent" mb)))
              (set! permanent-ready? #t)
              (semaphore-post mb-lock))
            (let* ([mb (send sub-collect-frame get-menu-bar)]
                   [mm (car (send mb get-items))])
              (send (remember (cons 'm tag) (make-object menu-item% "Delete Me" mm void)) delete)
              (let ([m (remember tag (make-object menu% "Temporary" mb))])
                (remember (cons 't tag) (make-object menu-item% "Temp Hi" m void))
                (send m delete)))))

        (when atomic?
          (let loop ([m 8])
            (unless (zero? m)
              (remember (cons tag m) (make-object point% n m))
              (let ([br (make-object brush%)])
                (remember (cons tag m) br)
                (hash-set! htw br 'ok))
              (remember (cons tag m) (make-object pen%))
              (loop (sub1 m)))))
        
        (when offscreen?
          (let ([m (remember tag (make-object bitmap-dc%))]
                [b0 (remember (cons tag 'f) (make-object bitmap% (get-image n)))]
                [b (remember (cons tag 'u) (make-object bitmap% 100 100))]
                [b2 (remember (cons tag 'x) (make-object bitmap% 100 100))])
            (unless (send b0 ok?)
              (error "bitmap load error"))
            (send m set-bitmap b)))
        
        (when edit?
          (send ef show #f))
        
        (custodian-shutdown-all c)

        (collect-garbage)

        (maker id (sub1 n))))))

(define (still)
  (map (lambda (x)
         (let ([v (weak-box-value (cdr x))])
           (when v
             (printf "~s ~s\n" (car x) v))))
       allocated)
  (void))

(define (xthread f)
  (f))

(define (stw t n)
  '(thread-weight t (floor (/ (thread-weight t) n))))

(define (breakable t)
  (if #f
      (thread (lambda ()
                (read)
                (printf "breaking\n")
                (break-thread t)
                (thread-wait t)
                (printf "done\n")))
      (void)))

(define (do-test)
  (let ([sema (make-semaphore)])
    (let loop ([n num-threads])
      (unless (zero? n)
        (breakable
         (thread (lambda () 
                   (stw (current-thread) n)
                   (dynamic-wind
                    void
                    (lambda () (maker n num-times))
                    (lambda () (semaphore-post sema))))))
        (loop (sub1 n))))
    (let loop ([n num-threads])
      (unless (zero? n)
        (yield sema)
        (loop (sub1 n)))))

  (collect-garbage)
  (collect-garbage)
  (let loop ([n 100]) 
    (if (zero? n) 0 (sub1 (loop (sub1 n)))))
  (collect-garbage)
  (collect-garbage)
  (still)
  (when subwindows?
    (set! sub-collect-frame #f)
    (set! sub-collect-panel #f))
  (when dump-stats?
    (dump-memory-stats)
    (still)))

(do-test)
