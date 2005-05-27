;; NOTES:
;; 1. This is where the send/blah primtives are defined that get used by the
;;    servlet and servlet2 teachpacks.
;; 2. There's no chance that this code works anymore, and so I've excluded it
;;    from compilation in the info.ss until I have a chance to get the
;;    teachpacks going again.
;; 3. In the original server, send/blah were implemented twice, once for the
;;    normal server and then again in here for the teachpacks. Now that I've
;;    got the new continuation model in place, I will see if I can eliminate
;;    this redundancy.

(module servlet-primitives mzscheme
  (require "channel.ss"
           "configuration.ss"
           ;"configuration-structures.ss"
           "web-server-unit.ss"
           "min-servlet.ss"
           ;"servlet.ss"
           "servlet-tables.ss"
           "internal-structs.ss"
           ;(lib "xml.ss" "xml")
           (lib "url.ss" "net")
           (lib "external.ss" "browser")
           ; more here - use contracts when they support suitable error messages
           ;(lib "contracts.ss" "framework")
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           )
  (provide servlet@)

  ; the unit doesn't contain much since it's better to start as few servers as possible
  ; this is unitized so the servlet2.ss teachpack can work with either
  ; the development environment or with the actual server.
  ; more here - maybe this is not needed anymore with the parameter that
  ; affects send/suspend.
  (define servlet@
    (unit/sig servlet^
      (import)

      (define send/suspend the-send/suspend)
      (define send/finish the-send/finish)
      (define send/back the-send/back)
      (define send/forward the-send/forward)
      (define initial-request the-initial-request)
      (define adjust-timeout! the-adjust-timeout!)))

  ; : num -> void
  (define (the-adjust-timeout! n) (void))

  ; send/finish : response -> doesn't
  (define (the-send/finish page)
    (unless (response? page)
      (error 'send/finish "expected <response> as 1st argument, given: ~e"
             page))
    (output-page page)
    (kill-thread (current-thread))
    (set! *page-channel* #f))

  ; *page-channel* : #f | channel
  (define *page-channel* #f)

  ; update-channel! : channel -> void
  (define (update-channel! x)
    (set! *page-channel* x))

  (define *last-page-sent* #f)
  ;(define *open-new-window* #t)
  ; always re-use an exisiting window.
  (define *open-new-window* #f)

  ; output-page : page -> void
  (define (output-page page)
    (set! *last-page-sent* page)
    ;(unless *page-channel*
    ;  (init-channel))
    (init-channel)
    (async-channel-put *page-channel* page))

  ; : instance -> doesn't
  (define resume-next-request
    (gen-resume-next-request void update-channel!))

  ; init-channel : -> void
  (define (init-channel)
    ((gen-send/suspend uri invoke-id instances void resume-next-request)
     (lambda (url)
       (send-url url *open-new-window*)
       (set! *open-new-window* #f))))

  (define-values (listener port)
    (let loop ([port 8000])
      (with-handlers ([void (lambda (exn) (loop (add1 port)))])
        (values (tcp-listen port 10 #f "127.0.0.1")
                port))))

  (define instances (make-hash-table))
  (define uri (string->url (format "http://127.0.0.1:~a/servlets/" port)))
  (define invoke-id (string->symbol (symbol->string (gensym "id"))))

  ; : (str -> response) -> request
  (define the-send/suspend
    (lambda (k->page)
      (let ((s/s (gen-send/suspend uri invoke-id instances output-page resume-next-request)))
        (s/s (lambda (k-url)
               (let ([page (k->page k-url)])
                 (unless (response? page)
                   (error 'send/suspend "expected <~a> as 1st argument, given a function that produced: ~e"
                          "a function that produces a response"
                          page))
                 page))))))

  ; : (response -> doesn't)
  (define (the-send/back page)
    (the-send/suspend (lambda (not-used-k-url) page)))

  ; : (str -> response) -> request
  (define (the-send/forward page-maker)
    ;(set-servlet-instance-cont-table!
    ; (hash-table-get invoke-id instances)
    ; (make-hash-table))
    ; FIX - this is wrong.  The hash table must be cleared or the reference in the server must be reset, not our reference.
    ; try (set-config-instances! the-config (make-hash-table))
;    (set! instances (make-hash-table))
;    (add-new-instance invoke-id instances)
;    (the-send/suspend page-maker))
    (purge-table 'post uri instances invoke-id
                 (lambda (inst) (set-servlet-instance-cont-table! inst (make-hash-table))))
    (the-send/suspend page-maker))


  (define the-initial-request
    (make-request 'post uri null null "127.0.0.1" "127.0.0.1"))

  (add-new-instance invoke-id instances)

  ; override some configuration options
  (define the-configuration@
    (load-developer-configuration default-configuration-table-path))

  (thread (lambda ()
            (invoke-unit/sig
             (compound-unit/sig
              (import (T : net:tcp^))
              (link
               [c : web-config^ ((update-configuration the-configuration@ `((instances . ,instances))))]
               [s : web-server^ (web-server@ T C)]
               [m : () ((unit/sig ()
                          (import web-server^)
                          (server-loop (current-custodian)
                                       (lambda () (tcp-accept listener))
                                       (lambda ()
                                         (init-channel)
                                         (async-channel-put *page-channel* *last-page-sent*))))
                        s)])
              (export))
             net:tcp^))))
