#lang racket/base
(require racket/list
         racket/match
         racket/tcp
         racket/place
         racket/place/private/th-place
         racket/place/private/coercion
         racket/class
         racket/trait
         racket/udp
         racket/runtime-path
         racket/date
         syntax/location)

(provide ssh-bin-path
         racket-path
         distributed-launch-path

         ;; New Design Pattern 2 API
         message-router
         spawn-node-supervise-dynamic-place-at
         spawn-node-supervise-place-thunk-at
         spawn-node-with-dynamic-place-at
         spawn-node-with-place-thunk-at
         supervise-named-dynamic-place-at
         supervise-named-place-thunk-at
         supervise-place-thunk-at
         supervise-dynamic-place-at
         supervise-thread-at

         supervise-process-at
         every-seconds
         after-seconds
         restart-every

         connect-to-named-place

         spawn-remote-racket-node
         node-send-exit
         node-get-first-place
         dplace-put
         dplace-get

         ;; low-level API

         ll-channel-get
         ll-channel-put
         write-flush
         log-message
         start-spawned-node-router

         ;; Old Design Pattern 1 API
         dcg-get-cg
         dcg-send
         dcg-send-type
         dcg-recv
         dcg-kill

         dcg-send-new-dchannel
         dcg-spawn-remote-dplace

         dchannel-put
         dchannel-get

         launch-config
         startup-config
         (struct-out node-config)
         (struct-out dcg)

         ;classes
         event-container<%>
         spawned-process%
         place-socket-bridge%
         node%
         socket-connection%
         remote-node%
         remote-place%
         remote-connection%
         place%
         connection%
         respawn-and-fire%
         after-seconds%
         restarter%

         ;re-provides
         quote-module-path
         )

(define-runtime-path distributed-launch-path "distributed/launch.rkt")

(define DEFAULT-ROUTER-PORT 6340)

(define-syntax quote-module-path-bytes
  (syntax-rules ()
    [(_)
      (->module-path (quote-module-name))]
    [(_ path ... )
      (->module-path
        (let ([qmn (quote-module-name)])
          (cond 
            [(list? qmn) (append (list 'submod) qmn (list path ...))]
            [else (list 'submod qmn path ...)])))]))


; returns the path to the racket executable on the current machine.
(define (racket-path)
  (parameterize ([current-directory (find-system-path 'orig-dir)])
    (find-executable-path (find-system-path 'exec-file) #f)))

; find ssh-binary
(define (ssh-bin-path)
  (define (exists? paths)
    (and paths
         (for/or ([p paths]) (and (file-exists? p) p))))

  (define (fallback-paths)
    (exists?
      (case (system-type 'os)
        [(unix macosx)
          (list "/usr/local/bin/ssh" "/usr/bin/ssh" "/bin/ssh" "/opt/local/bin/ssh" "/opt/bin/ssh")]
        [(windows) #f])))

  (define (which cmd)
    (define path (getenv "PATH"))
    (and path
         (exists? (map (lambda (x) (build-path x cmd)) (regexp-split (case (system-type 'os)
                                    [(unix macosx) ":"]
                                    [(windows) "#:;"])
                                  path)))))
  (or (which "ssh")
      (fallback-paths)
      (raise "ssh binary not found")))



(define (write-flush msg [p (current-output-port)])
  (write msg p)
  (flush-output p))

(define (tcp-connect/backoff rname rport #:times [times 4] #:start-seconds [start-seconds 1])
  (let loop ([t 0]
             [wait-time start-seconds])
    (with-handlers ([exn? (lambda (e)
                  (cond [(t . < . times)
                         (log-debug (format "backing off ~a sec to ~a:~a" (expt 2 t) rname rport))
                         (sleep wait-time)
                         (loop (add1 t) (* 2 wait-time))]
                        [else (raise e)]))])
      (tcp-connect rname (->number rport)))))

(define (tcp-connect/retry rname rport #:times [times 10] #:delay [delay 1])
  (let loop ([t 0])
    (with-handlers ([exn? (lambda (e)
                  (cond [(t . < . times)
                         (log-debug (format "waiting ~a sec to retry connection to ~a:~a" delay rname rport))
                         (sleep delay)
                         (loop (add1 t))]
                        [else (raise e)]))])
      (tcp-connect rname (->number rport)))))

(define (format-log-message severity msg)
  (log-info (format "~a ~a ~a\n" (date->string (current-date) #t) severity msg))
  (flush-output))


;node configuration
(struct node-config (node-name node-port proc-count ssh-path racket-path distributed-path mod-path func-name conf-path conf-name) #:prefab)
;distributed communication group
(struct dcg (ch id n))
;distributed communication group message
(struct dcgm (type src dest msg) #:prefab)

(struct dchannel (ch) #:prefab)

;dcg types
(define DCGM-TYPE-NORMAL               0)
(define DCGM-TYPE-DIE                  1)
(define DCGM-TYPE-NEW-DCHANNEL         2)
(define DCGM-TYPE-NEW-INTER-DCHANNEL   3)
(define DCGM-TYPE-INTER-DCHANNEL       4)
(define DCGM-TYPE-KILL-DPLACE          5)
(define DCGM-TYPE-SPAWN-REMOTE-PROCESS 6)
(define DCGM-DPLACE-DIED               7)
(define DCGM-TYPE-LOG-TO-PARENT        8)
(define DCGM-TYPE-NEW-PLACE            9)
(define DCGM-TYPE-SET-OWNER           10)


(define (dchannel-put ch msg)
  (unless (or (dchannel? ch) (place-channel? ch))
    (raise-mismatch-error 'dchannel-get "expected dchannel?, got " ch))
  (if (dchannel? ch)
    (place-channel-put (dchannel-ch ch) msg)
    (place-channel-put ch msg)))

(define (dchannel-get ch)
  (unless (or (dchannel? ch) (place-channel? ch))
    (raise-mismatch-error 'dchannel-get "expected dchannel?, got " ch))
  (if (dchannel? ch)
    (place-channel-get (dchannel-ch ch))
    (place-channel-get ch)))

(define (dcg-send-type c type dest msg)
  (place-channel-put (dcg-ch c) (dcgm type (dcg-id c) dest msg)))

(define (dcg-send c dest msg)
  (dcg-send-type c DCGM-TYPE-NORMAL dest msg))

(define (dcg-get-cg ch) (apply dcg ch (place-channel-get ch)))

(define (dcg-kill c dest)
  (place-channel-put (dcg-ch c) (dcgm DCGM-TYPE-DIE (dcg-id c) dest "DIE")))

(define (dcg-send-new-dchannel c dest)
  (define-values (e1 e2) (place-channel))
  (dcg-send-type c DCGM-TYPE-NEW-DCHANNEL dest (dchannel e2))
  (dchannel e1))

;; Contract: start-node-router : VectorOf[ (or/c place-channel socket-connection)] -> (void)
;; Purpose: Forward messages between channels and build new point-to-point subchannels
;; Example:
(define (dcg-spawn-remote-dplace c hostname modpath funcname #:listen-port [listen-port 6432])
  (define-values (e1 e2) (place-channel))
  (dcg-send-type c DCGM-TYPE-SPAWN-REMOTE-PROCESS (list hostname listen-port modpath funcname) e2)
  e1)

(define (dcg-recv c)
  (dcgm-msg (place-channel-get (dcg-ch c))))

(define-syntax-rule (reduce-sum seq item body ...)
  (for/fold ([sum 0]) ([item seq]) (+ sum (begin body ...))))

(define (total-node-count conf) (reduce-sum conf item (node-config-proc-count item)))

;; Contract: start-node-router : VectorOf[ (or/c place-channel socket-connection)] -> (void)
;;
;; Purpose: Forward messages between channels and build new point-to-point subchannels
;;
;; Example:

(define (start-spawned-node-router listener)
  (define nc (new node% [listen-port listener]))
  (send nc sync-events))


(define (start-node-router chan-vec)
  (define nc (new node% [chan-vec chan-vec]))
  (send nc sync-events))

(define backlink
  (trait->mixin
    (trait
      (field [router #f])
      (define/public (remove-from-router)
        (and router (send router remove-ec this)))
      (define/public (get-router) router)
      (define/public (backlink _router)
        (set! router _router))
      )))

(define event-container<%>
  (interface ()
    register
  ))

(define-syntax-rule (for/filter/fold/cons tail xs body ...)
  (for/fold ([n tail]) xs
    (define r (let () body ...))
    (if r (cons r n) n)))

(define (filter/list* . lst)
  (define rl (reverse lst))
  (for/filter/fold/cons (car rl) ([x (cdr rl)]) x))

(define spawned-process%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field cmdline-list)
      (init-field [parent #f])
      (field [s #f]
             [i #f]
             [o #f]
             [e #f]
             [pid #f])

      (let-values ([(_s _o _i _e) (apply subprocess #f #f #f cmdline-list)])
        (set! pid (subprocess-pid _s))
        (set! s _s)
        (set! o (box _o))
        (set! i (box _i))
        (set! e (box _e)))
      (log-debug (format"SPAWNED-PROCESS:~a ~a" pid cmdline-list))

      (define (mk-handler _port desc)
        (define port (unbox _port))
        (if port
          (wrap-evt port (lambda (e)
            (define (print-out x) (log-debug (format "SPAWNED-PROCESS ~a:~a:~a ~a" pid desc (->length x) x))
              (flush-output))
            (cond
              [(not port) (print-out "IS #F")]
              [else
                (define bb (make-bytes 4096))
                (define bbl (read-bytes-avail!* bb port))
                (cond
                  [(eof-object? bbl)
                   (print-out "EOF")
                   (set-box! _port #f)]
                  [else
                   (print-out (subbytes bb 0 bbl))])])))
          #f))

      (define/public (get-pid) pid)
      (define/public (wait-for-die) (subprocess-wait s))
      (define/public (register nes)
        (for/filter/fold/cons nes ([x (list s (list o "OUT") (list e "ERR"))])
          (cond
            [(subprocess? x) (wrap-evt s (lambda (e)
                                             (log-debug (format "SPAWNED-PROCESS ~a DIED" pid))
                                             (and parent (send parent process-died this))))]
            [(list? x) (apply mk-handler x)]
            [else #f])))
      (super-new)
  )))

(define place-socket-bridge%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field pch
                  sch
                  id
                  node)
      (define/public (register nes)
        (cons
          (wrap-evt
            (if (dchannel? pch) (dchannel-ch pch) pch)
            (lambda (e)
              (match e
                [(dcgm 8 #;(== DCGM-TYPE-LOG-TO-PARENT) _ _ (list severity msg))
                  (send node log-from-child #:severity severity msg)]
                [else (put-msg e)])))
          nes))
      (define/public (get-sc-id) id)
      (define/public (get-raw-msg)
        (let loop ()
          (define msg (send sch read-message))
          (if (= (dcgm-type msg) DCGM-DPLACE-DIED)
            (loop)
          (dcgm-msg msg))))
      (define/public (put-msg msg)
        (sconn-write-flush sch (dcgm DCGM-TYPE-INTER-DCHANNEL id id msg)))
      (super-new)
  )))

(define node%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field [chan-vec #f])
      (init-field [listen-port #f])
      (init-field [socket-ports null])
      (init-field [sub-ecs null])
      (init-field [psbs null])
      (init-field [spawned-nodes null])
      (init-field [named-places (make-hash)])
      (init-field [beacon #f])
      (init-field [owner #f])
      (field [id 0])
      (define/public (nextid)
        (set! id (add1 id))
        id)
      (define (add-socket-port pair)
        (set! socket-ports (append socket-ports (list pair))))
      (define/public (add-sub-ec ec)
        (set! sub-ecs (append sub-ecs (list ec))))
      (define (add-spawned-node ec)
        (set! spawned-nodes (append spawned-nodes (list ec))))
      (define (add-psb ec)
        (set! psbs (append psbs (list ec))))
      (define (add-named-place name np)
        (hash-set! named-places (->string name) np))
      (define (named-place-lookup name)
        (hash-ref named-places (->string name) #f))
      (define (add-place-channel-socket-bridge pch sch id)
        (add-psb (new place-socket-bridge% [pch pch] [sch sch] [id id] [node this])))
      (define (forward-mesg m src-channel)
        (match m
          [(dcgm 1 #;(== DCGM-TYPE-DIE) src dest "DIE") (exit 1)]
          [(dcgm 2 #;(== DCGM-TYPE-NEW-DCHANNEL) src dest pch)
            (define d (vector-ref chan-vec dest))
            (cond
              [(is-a? d socket-connection%)
               (define ch-id (nextid))
               (sconn-add-subchannel d ch-id pch)
               (add-place-channel-socket-bridge pch d ch-id)
               (sconn-write-flush d (dcgm DCGM-TYPE-NEW-INTER-DCHANNEL src dest ch-id))]
              [(or (place-channel? d) (place? d))
               (place-channel-put d m)])]
          [(dcgm 9 #;(== DCGM-TYPE-NEW-PLACE) -1 (and place-exec (list-rest type rest)) ch-id)
           (match place-exec
             [(list 'connect name)
              (define np (named-place-lookup name))
              (cond
                [np
                  (define nc (new connection%
                                 [name-pl np]
                                 [ch-id ch-id]
                                 [sc src-channel]
                                 [node this]))
                  (add-sub-ec nc)]

                [else
                  (sconn-write-flush src-channel (dcgm DCGM-TYPE-INTER-DCHANNEL ch-id ch-id
                                                       (format "ERROR: name not found ~a" name)))])]
             [else
              (define np (new place%
                             [place-exec place-exec]
                             [ch-id ch-id]
                             [sc src-channel]
                             [node this]))
              (match place-exec
                [(list _ _ _ name) (add-named-place name np)]
                [else (add-sub-ec np)])])]
          [(dcgm 3 #;(== DCGM-TYPE-NEW-INTER-DCHANNEL) src dest ch-id)
            (define s src-channel)
            (define d (vector-ref chan-vec dest))
            (define-values (pch1 pch2) (place-channel))
            (sconn-add-subchannel s ch-id pch1)
            (add-place-channel-socket-bridge pch1 s ch-id)
            (place-channel-put d (dcgm DCGM-TYPE-NEW-DCHANNEL src dest pch2))]
          [(dcgm 4 #;(== DCGM-TYPE-INTER-DCHANNEL) _ ch-id msg)
            (define pch (sconn-lookup-subchannel src-channel ch-id))
            (cond
              [(place-channel? pch)
                (place-channel-put pch msg)]
              [(is-a? pch connection%)
               (send pch forward msg)]
              [(th-place-channel? pch)
               (th-place-channel-put pch msg)])]
          [(dcgm 6 #;(== DCGM-TYPE-SPAWN-REMOTE-PROCESS) src (list node-name node-port mod-path funcname) ch1)
           (define node
             (new remote-node%
                  [host-name node-name]
                  [listen-port node-port]
                  [cmdline-list (list (ssh-bin-path)  node-name (racket-path) "-tm" (->string distributed-launch-path) "spawn" (->string node-port))]))
           (add-spawned-node node)
           (send node launch-place
                    (list 'dynamic-place mod-path funcname)
                    ;#:initial-message initial-message
                    #:one-sided-place? ch1
                    ;#:restart-on-exit restart-on-exit
                    )]
          [(dcgm 7 #;(== DCGM-DPLACE-DIED) -1 -1 ch-id)
            (log-debug (format"PLACE ~a died" ch-id))]
          [(dcgm 8 #;(== DCGM-TYPE-LOG-TO-PARENT) _ _ (list severity msg))
            (log-from-child #:severity severity msg)]
          [(dcgm 10 #;(== DCGM-TYPE-SET-OWNER) -1 -1 msg)
            (log-debug (format "RECV DCGM-TYPE-SET-OWNER ~a" src-channel))
            (set! owner src-channel)]
          [(dcgm mtype srcs dest msg)
            (define d (vector-ref chan-vec dest))
            (cond
              [(is-a? d socket-connection%)
                (sconn-write-flush d m)]
              [(or (place-channel? d) (place? d))
                (place-channel-put d m)])]
          [(? eof-object?)
            (log-debug (format "connection died"))
            (flush-output)
            (exit 1)
            ]))

      (define us-buffer (make-bytes 4096))
      (define (register-beacon nes)
        (cond
          [(not beacon) nes]
          [(equal? #t beacon)
           (set! beacon (udp-open-socket))
           (udp-bind! beacon "255.255.255.255" DEFAULT-ROUTER-PORT)
           (wrap-evt (udp-receive!-evt beacon us-buffer)
            (match-lambda
              [(list count host port)
              (void)]))]))


      (define/public (log-from-child msg #:severity [severity 'info])
        ;(printf "Received Log Message ~a ~a\n" severity msg)
        (cond
          [owner
            ;(printf "Sent to owner\n")
            (sconn-write-flush owner (log-message severity msg))]
          [else (format-log-message severity msg)]))

      (define/public (register nes)
        (let*
          ([nes
            (if chan-vec
              (for/fold ([n nes]) ([x (in-vector chan-vec)])
                (cons
                  (cond
                    [(is-a? x socket-connection%)
                     (sconn-get-forward-event x forward-mesg)]
                    [(or (place-channel? x) (place? x))
                     (wrap-evt x (lambda (e)
                                     (forward-mesg e x)))])
                  n))
              nes)]
           [nes
            (if listen-port
              (cons
                (wrap-evt listen-port (lambda (e)
                  (define-values (in out) (tcp-accept listen-port))
                  (define-values (lh lp rh rp) (tcp-addresses in #t))
                  (log-debug (format "INCOMING CONNECTION ~a:~a <- ~a:~a" lh lp rh rp))
                  (define sp (new socket-connection% [in in] [out out]))
                  (add-socket-port sp)))
                nes)
              nes)]
           [nes
            (if socket-ports
              (for/fold ([n nes]) ([x socket-ports])
                (cons
                  (cond
                    [(is-a? x socket-connection%)
                     (sconn-get-forward-event x forward-mesg)]
                    [(or (place-channel? x) (place? x))
                     (wrap-evt x (lambda (e)
                                     ;(printf "SOCKET-PORT PLACE MESSAGE ~a\n" e)
                                     (forward-mesg e x)))])
                  n))
              nes)]
           [nes
            (if sub-ecs
              (for/fold ([n nes]) ([x sub-ecs])
                (send x register n))
              nes)]
           [nes
            (if psbs
              (for/fold ([n nes]) ([x psbs])
                (send x register n))
              nes)]
           [nes
            (if spawned-nodes
              (for/fold ([n nes]) ([x spawned-nodes])
                (send x register n))
              nes)]
           [nes (register-beacon nes)]
           [nes
            (cond
              [named-places
              (for/fold ([n nes]) ([x (in-hash-values named-places)])
                (send x register n))]
              [else nes])])
          nes))

      (define/public (sync-events)
        (let loop ()
          (define l (register null))
          (apply sync/enable-break l)

          (loop )))


      (super-new)
  )))


;socket channel
(define (sconn-add-subchannel s ch-id ch) (send s add-subchannel ch-id ch))
(define (sconn-lookup-subchannel s ch-id) (send s lookup-subchannel ch-id))
(define (sconn-write-flush s x) (send s _write-flush x))
(define (sconn-remove-subchannel s scid) (send s remove-subchannel scid))
(define (sconn-get-forward-event s forwarder) (send s get-forward-event forwarder))

(define socket-connection%
  (backlink
    (class* object% (event-container<%>)
      (init-field [host #f]
                  [port #f]
                  [retry-times 30]
                  [delay 1]
                  [background-connect #t]
                  [in #f]
                  [out #f])
      (field [subchannels null]
             [connecting #f]
             [ch #f])

      (define (forward-mesg x) (void))

      (define (tcp-connect/retry rname rport #:times [times 10] #:delay [delay 1])
        (let loop ([t 0])
          (with-handlers ([exn? (lambda (e)
                        (cond [(t . < . times)
                               (log-debug (format "try ~a waiting ~a sec to retry connection to ~a:~a" t delay rname rport))
                               (sleep delay)
                               (loop (add1 t))]
                              [else (raise e)]))])
            (tcp-connect rname (->number rport)))))

      (define (ensure-connected)
        (when connecting
          (match (channel-get ch)
            [(list _in _out)
             (set! in _in)
             (set! out _out)
             (set! connecting #f)])))

      (define/public (add-subchannel id pch)
        (set! subchannels (append subchannels (list (cons id pch)))))
      (define/public (lookup-subchannel id) (cdr (assoc id subchannels)))
      (define/public (_write-flush x)
        (when (equal? out #f) (ensure-connected))
        (write-flush x out))
      (define/public (remove-subchannel id)
        (set! subchannels (filter-map
                            (lambda (x) (and (not (= (car x) id)) x))
                            subchannels)))
      (define/public (addresses) (tcp-addresses in #t))
      (define/public (get-forward-event forwarder)
        (when (equal? out #f) (ensure-connected))
        (wrap-evt in (lambda (e)
                       (forwarder (read in) this))))

      (define/public (read-message)
        (when (equal? out #f) (ensure-connected))
        (read in))
      (define/public (register nes)
        (raise "Not-implemented/needed")
        (cons (wrap-evt in void) nes))

      (when (and host port background-connect)
        (set! connecting #t)
        (set! ch (make-channel))
        (thread
          (lambda ()
            (channel-put
              ch
              (call-with-values
                (lambda () (with-handlers ([exn:fail? (lambda (e)
                                                        (raise (format "socket error connecting to ~a:~a" host port)))])
                             (tcp-connect/retry host port #:times retry-times #:delay delay)))
                list)))))
      (when (and host port (not background-connect))
        (tcp-connect/retry host port #:times retry-times #:delay delay))
      (super-new)
  )))

(define remote-node%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field host-name)
      (init-field listen-port)
      (init-field [cmdline-list #f])
      (init-field [sc #f]) ;socket-connection
      (init-field [restart-on-exit #f])
      (field [sp #f]) ;spawned-process
      (field [id 0])
      (field [remote-places null])

      (define/public (nextid)
        (set! id (add1 id))
        id)

      (define (add-remote-place rp)
        (set! remote-places (append remote-places(list rp))))
      (define (spawn-node)
        (set! sp (new spawned-process% [cmdline-list cmdline-list] [parent this])))
      (define (setup-socket-connection)
        (set! sc (new socket-connection% [host host-name] [port listen-port]))
        (sconn-write-flush sc (dcgm DCGM-TYPE-SET-OWNER -1 -1 "")))
      (define (restart-node)
        (spawn-node)
        (setup-socket-connection))

      (when (and cmdline-list (not sc))
        (spawn-node))
      (unless sc
        (setup-socket-connection))

      (define (find-place-by-sc-id scid)
        (for/fold ([r #f]) ([rp remote-places])
          (if (= (send rp get-sc-id) scid)
            rp
            r)))

      (define (get-sp-pid)
        (cond
          [sp (send sp get-pid)]
          [else 'failed-to-launch ]))

      (define (on-socket-event it in-port)
        (match it
          [(dcgm 7 #;(== DCGM-DPLACE-DIED) -1 -1 ch-id)
            (log-debug (format "SPAWNED-PROCESS:~a PLACE DIED ~a:~a:~a" (get-sp-pid) host-name listen-port ch-id))
            (cond
              [(find-place-by-sc-id ch-id) => (lambda (rp)
                                                (send rp place-died))]
              [else (raise (format "remote-place for sc-id ~a not found\n" ch-id))])]
          [(dcgm 4 #;(== DCGM-TYPE-INTER-DCHANNEL) _ ch-id msg)
            (define pch (sconn-lookup-subchannel sc ch-id))
            (cond
              [(place-channel? pch)
                (place-channel-put pch msg)]
              [(is-a? pch connection%)
               (send pch forward msg)])]
          [(dcgm 8 #;(== DCGM-TYPE-LOG-TO-PARENT) _ _ (list severity msg))
            (define parent (send this get-router))
            (cond
              [parent
                (send parent log-from-child #:severity severity msg)]
              [else (format-log-message severity msg)])]

          [(? eof-object?)
           (define-values (lh lp rh rp) (send sc addresses))
           (log-debug (format "EOF on node socket connection pid to ~a ~a:~a CONNECTION ~a:~a -> ~a:~a" (get-sp-pid) host-name listen-port lh lp rh rp))
           (set! sc #f)]

          [else (log-debug (format"received message ~a" it))]))

      (define/public (get-log-prefix) (format "PLACE ~a:~a" host-name listen-port))
      (define/public (process-died child)
        (log-debug (format "Remote node pid ~a ~a:~a died" (get-sp-pid) host-name listen-port))
        (set! sp #f)
        (cond
          [restart-on-exit
            (cond
              [cmdline-list
                (if (equal? restart-on-exit #t)
                  (restart-node)
                  (send restart-on-exit restart restart-node))]
              [else
                (log-debug (format "No restart cmdline arguments for ~a" (get-log-prefix)))])]
          [else
            (log-debug (format "No restart condition for ~a" (get-log-prefix)))]))

      (define/public (get-first-place)
        (car remote-places))
      (define/public (get-first-place-channel)
        (send (car remote-places) get-channel))

      (define/public (drop-sc-id scid)
        (sconn-remove-subchannel sc scid))

      (define/public (launch-place place-exec #:restart-on-exit [restart-on-exit #f] #:one-sided-place? [one-sided-place? #f])
        (define rp (new remote-place% [node this] [place-exec place-exec] [restart-on-exit restart-on-exit]
                        [one-sided-place? one-sided-place?]))
        (add-remote-place rp)
        rp)

      (define/public (remote-connect name #:restart-on-exit [restart-on-exit #f])
        (define rp (new remote-connection% [node this] [name name] [restart-on-exit restart-on-exit]))
        (add-remote-place rp)
        rp)

      (define/public (spawn-remote-place place-exec dch)
        (define ch-id (nextid))
        (sconn-add-subchannel sc ch-id dch)
        (sconn-write-flush sc (dcgm DCGM-TYPE-NEW-PLACE -1 place-exec ch-id))
        (new place-socket-bridge% [pch dch] [sch sc] [id ch-id] [node this]))

      (define/public (spawn-remote-connection name dch)
        (define ch-id (nextid))
        (sconn-add-subchannel sc ch-id dch)
        (sconn-write-flush sc (dcgm DCGM-TYPE-NEW-PLACE -1 (list 'connect name) ch-id))
        (new place-socket-bridge% [pch dch] [sch sc] [id ch-id] [node this]))

      (define/public (send-exit)
        (sconn-write-flush sc (dcgm DCGM-TYPE-DIE -1 -1 "DIE")))

      (define/public (wait-for-die)
        (send sp wait-for-die))

      (define/public (register es)
        (let* ([es (if sp (send sp register es) es)]
               [es (for/fold ([nes es]) ([rp remote-places])
                             (send rp register nes))]
               [es (if sc (cons (sconn-get-forward-event sc on-socket-event) es) es)]
               [es (if (and restart-on-exit
                            (not (equal? restart-on-exit #t)))
                       (send restart-on-exit register es)
                       es)])
          es))

      (super-new)
      )))

(define (node-send-exit node) (send node send-exit))
(define (node-get-first-place node) (send node get-first-place))

(define remote-place%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field node)
      (init-field [place-exec #f])
      (init-field [restart-on-exit #f])
      (init-field [one-sided-place? #f])
      (init-field [on-channel #f])
      (field [psb #f])
      (field [pc #f])
      (field [rpc #f])
      (field [running #f])
      (field [k #f])
      (field [handle-channel #t])

      (cond
        [one-sided-place?
          (set! rpc one-sided-place?)]
        [else
          (define-values (pch1 pch2) (place-channel))
          (set! rpc pch1)
          (set! pc pch2)])

      (set! psb (send node spawn-remote-place place-exec rpc))

      (define (restart-place)
        (send node drop-sc-id (send psb get-sc-id))
        (set! psb (send node spawn-remote-place place-exec rpc)))

      (define/public (stop) (void))
      (define/public (get-channel) pc)
      (define/public (set-on-channel! proc) (set! on-channel proc))
      (define/public (get-sc-id) (send psb get-sc-id))
      (define/public (set-handle-channel! x) (set! handle-channel x))
      (define/public (place-died)
        (cond
          [restart-on-exit
                (if (equal? restart-on-exit #t)
                  (restart-place)
                  (send restart-on-exit restart restart-place))]
          [else
            (log-debug (format "No restart condition for ~a:~a"
                    (send node get-log-prefix)
                    (send psb get-sc-id)))]))
      (define (on-channel-event e)
        (log-debug (format "~a ~a" (send node get-log-prefix) e)))
      (define/public (register es)
        (let* ([es (if (and handle-channel pc)
                       (cons (wrap-evt pc
                                            (cond
                                              [k
                                               (lambda (e)
                                                 (call-with-continuation-prompt (lambda ()
                                                   (begin0
                                                     (k e)
                                                     (set! k #f)))))]
                                              [on-channel
                                               (lambda (e)
                                                 (on-channel pc e))]
                                              [else
                                               on-channel-event])) es)
                       es)]
               [es (send psb register es)]
               [es (if (and restart-on-exit
                            (not (equal? restart-on-exit #t)))
                       (send restart-on-exit register es)
                       es)])
          es))
      (define/public (set-continuation _k) (set! k _k))

      (define/public (get-raw-msg) (send psb get-raw-msg))
      (define/public (get-msg)
        (call-with-composable-continuation
          (lambda (_k)
            (set! k _k)
            (abort-current-continuation (default-continuation-prompt-tag) void))))

      (define/public (put-msg msg) (send psb put-msg msg))

      (super-new)
      )))

(define (dplace-get dest)
  (cond
    [(place-channel? dest) (place-channel-get dest)]
    [else (send dest get-msg)]))

(define (dplace-put dest msg)
  (cond
    [(place-channel? dest) (place-channel-put dest msg)]
    [else (send dest put-msg msg)]))

(define remote-connection%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field node)
      (init-field name)
      (init-field [restart-on-exit #f])
      (init-field [on-channel #f])
      (field [psb #f])
      (field [pc #f])
      (field [running #f])
      (field [k #f])

      (define-values (pch1 pch2) (place-channel))
      (set! psb (send node spawn-remote-connection name pch1))
      (set! pc pch2)

      (define/public (stop) (void))
      (define/public (get-channel) pc)
      (define/public (set-on-channel! proc) (set! on-channel proc))
      (define/public (get-sc-id) (send psb get-sc-id))
      (define/public (place-died)
            (log-debug (format "No restart condition for ~a:~a"
                    (send node get-log-prefix)
                    (send psb get-sc-id))))
      (define (on-channel-event e)
        (log-debug (format "~a ~a" (send node get-log-prefix) e)))
      (define/public (register es)
        (let* ([es (if pc (cons (wrap-evt pc
                                            (cond
                                              [k
                                               (lambda (e)
                                                 (call-with-continuation-prompt (lambda ()
                                                   (begin0
                                                     (k e)
                                                     (set! k #f)))))]
                                              [on-channel
                                                (lambda (e)
                                                  (on-channel pc e))]
                                              [else
                                               on-channel-event])) es) es)]
               [es (send psb register es)])
          es))
      (define/public (set-continuation _k) (set! k _k))

      (define/public (get-raw-msg) (send psb get-raw-msg))
      (define/public (get-msg)
        (call-with-composable-continuation
          (lambda (_k)
            (set! k _k)
            (abort-current-continuation (default-continuation-prompt-tag) void))))
      (define/public (put-msg msg) (send psb put-msg msg))

      (super-new)
      )))

(define place%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field place-exec
                  ch-id
                  sc
                  node)
      (field [pd #f])
      (field [psb #f])
      (field [running #f])
      (define (default-on-place-dead e)
        (set! pd #f)
        (set! psb #f)
        (sconn-write-flush sc (dcgm DCGM-DPLACE-DIED -1 -1 ch-id))
        (sconn-remove-subchannel sc ch-id))

      (init-field [on-place-dead default-on-place-dead])

      (set! pd
        (match place-exec
          ;place% is a named place
          [(list 'dynamic-place place-path place-func name)
            (dynamic-place (->module-path place-path) place-func)]
          [(list 'place place-path place-func name)
            ((dynamic-require (->module-path place-path) place-func))]
          ;place% is a single connected place
          [(list 'dynamic-place place-path place-func)
            (dynamic-place (->module-path place-path) place-func)]
          [(list 'place place-path place-func)
            ((dynamic-require (->module-path place-path) place-func))]
          [(list 'thread place-path place-func)
           (define-values (ch1 ch2) (th-place-channel))
           (define th 
             (thread
               (lambda ()
                 ((dynamic-require (->module-path place-path) place-func) ch1))))
           (th-place th ch2 null)]))

      (sconn-add-subchannel sc ch-id pd)
      (set! psb (new place-socket-bridge% [pch pd] [sch sc] [id ch-id] [node node]))
      (define/public (get-channel) pd)
      (define/public (stop)
        (cond
          [(place? pd)
            (place-kill pd)
            (set! pd #f)]
          [(th-place? pd)
           (th-place-kill pd)]
          [else (void)])) ;send place not running message

      (define/public (register es)
        (let* ([es (if pd (cons (wrap-evt 
                                  (cond 
                                    [(place? pd) (place-dead-evt pd)]
                                    [(th-place? pd) (th-place-dead-evt pd)]) on-place-dead)
                                es) es)]
               [es (if psb (send psb register es) es)])
          es))
      (super-new)
      )))

(define connection%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field name-pl
                  ch-id
                  sc
                  node)
      (field [psb #f])

      (define-values (pch1 pch2) (place-channel))

      (define name-ch (send name-pl get-channel))

      (init-field [on-place-dead #f])

      (sconn-add-subchannel sc ch-id this)
      (set! psb (new place-socket-bridge% [pch pch1] [sch sc] [id ch-id] [node node]))

      (define/public (forward msg)
        (place-channel-put name-ch (list msg pch2)))

      (define/public (put msg)
        (sconn-write-flush sc (dcgm DCGM-TYPE-INTER-DCHANNEL ch-id ch-id msg)))
      (define/public (register es) (send psb register es))

      (super-new)
      )))


(define (ll-channel-put ch msg) (send ch put-msg msg))
(define (ll-channel-get ch) (send ch get-raw-msg))

(define respawn-and-fire%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field seconds)
      (init-field thunk)
      (field [fire-time (current-inexact-milliseconds)])

      (define/public (register es)
        (if fire-time
          (cons
            (wrap-evt (alarm-evt fire-time)
                        (lambda (x)
                          (set! fire-time (+ (current-inexact-milliseconds) (* seconds 1000)))
                          (thunk)))
            es)
          es))

      (super-new)
      )))

(define after-seconds%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field seconds)
      (init-field thunk)
      (init-field [fire-time (+ (current-inexact-milliseconds) (* seconds 1000))])

      (define/public (register es)
        (if fire-time
          (cons
            (wrap-evt (alarm-evt fire-time)
                        (lambda (x)
                          (set! fire-time #f)
                          (call-with-continuation-prompt thunk)))
            es)
          es))

      (super-new)
      )))

(define restarter%
    (class*
      after-seconds% (event-container<%>)
      (inherit-field seconds thunk fire-time)
      (init-field [retry #f])
      (init-field [on-final-fail #f])
      (super-new [fire-time #f] [thunk #f])
      (init-field [retry-reset (* 2 seconds)])
      (field [last-attempt 0])
      (field [retries 0])
      (define/public (restart restart-func)
        (cond
          [(and retry (>= retries retry))
           (log-debug (format "Already retried to restart ~a times" retry))
           (and on-final-fail (on-final-fail))]
          [(> (- (current-inexact-milliseconds) last-attempt) (* seconds 1000))
           (when (> (- (current-inexact-milliseconds) last-attempt) (* retry-reset 1000))
             (set! retries 0))
           (set! last-attempt (current-inexact-milliseconds))
           (set! retries (+ 1 retries))
           (set! fire-time #f)
           (restart-func)]
          [else
            (set! thunk (lambda () (restart restart-func)))
            (set! fire-time (+ (current-inexact-milliseconds) (* seconds 1000)))]))
      ))


(define (startup-config conf conf-idx)
  (start-node-router
    (cond
      ;master
      [(= 0 conf-idx)
        (define t-n-c (total-node-count conf))
        (define cv (make-vector t-n-c null))
        (build-down (node-config-node-port (first conf)) cv conf 0)
        cv]
      ;slave
      [else
        (listen/init-channels (node-config-node-port (list-ref conf conf-idx)))])))

;; Contract: build-down : port channel-vector conf conf-idx -> (void)
;;
;; Purpose: build up channel-vector by connecting to nodes greater than my-id
;;
;; Example: (build-down 6432 channel-vector conf 0)
;;
(define (build-down port cv conf conf-idx)
  (define t-n-c (total-node-count conf))
  (match-define (node-config node-name _ node-cnt _ _ _ modpath funcname config-path confname) (list-ref conf conf-idx))
  (define (isself? rid) (equal? rid conf-idx))

  (for/fold ([my-id #f]
             [next-node-id 0])
            ([item conf]
             [curr-conf-idx (in-naturals)])
    (match-define (node-config rname rport rcnt _ _ _ modpath funcname conf-path confname) item)

    (define (loopit my-id)
      (values my-id (+ next-node-id rcnt)))
    (define (remote-spawn)
      (define sp (new socket-connection% [host rname] [port rport]))
      (define msg (list my-id node-name node-cnt curr-conf-idx next-node-id rname rcnt conf))
      (sconn-write-flush sp msg)
      (for ([i (in-range rcnt)])
        (vector-set! cv (+ next-node-id i) sp))
      (loopit my-id))
    (define (local-spawn)
      (for ([i (in-range rcnt)])
        (define sp (dynamic-place (->path modpath) funcname))
        (vector-set! cv (+ next-node-id i) sp)
        (place-channel-put sp (list (+ next-node-id i) t-n-c)))
      (loopit next-node-id))

   (cond
     [my-id  (remote-spawn)]
     [(isself? curr-conf-idx) (local-spawn)]
     [(not my-id) (loopit my-id)])))


;; Contract: listen/init-channels : port -> VectorOf[ socket-connection%]
;;
;; Purpose: build up channel-vector by listening for connect requests for nodes less than
;; myid. Spawn thread to build channel-vector by connecting to nodes greater than myid.
;;
;; Example: (listen/init-channels 6432)
;;
;; Node 1  Node 2  Node 3
;; 1 2     3 4     5 6
;;
;;
(define (listen/init-channels port)
  (define listener (tcp-listen (->number port) 4 #t))
  (let loop ([cnt #f]
             [thr #f]
             [cv #f])
    (define-values (in out) (tcp-accept listener))
    (define sp (new socket-connection% [in in] [out out]))
    (match-define (list sid sname scnt myidx myid myname mycnt conf) (read in))
    ;(printf "Listen ~a\n" (list sid sname scnt myidx myid myname mycnt conf))
    (let*
      ([cnt (or cnt (- myidx 1))]
       [cv  (or cv  (make-vector (total-node-count conf) null))]
       [thr (or thr (thread (lambda () (build-down port cv conf myidx))))])

      (for ([i (in-range scnt)])
        (vector-set! cv (+ sid i) sp))

      (if (= 0 cnt)
        (begin (thread-wait thr) cv)
        (loop (sub1 cnt) thr cv)))))

(define (supervise-process-at host
                            #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:restart-on-exit [restart-on-exit #f]
                            . command-line-list)
  (void)
  )

(define (supervise-named-place-thunk-at node name place-path place-func
                            #:initial-message [initial-message #f]
                            #:restart-on-exit [restart-on-exit #f])
    (send node launch-place
          (list 'place (->module-path-bytes place-path) place-func (->string name))
          ;#:initial-message initial-message
          #:restart-on-exit restart-on-exit
          ))

(define (supervise-named-dynamic-place-at node name place-path place-func
                            #:initial-message [initial-message #f]
                            #:restart-on-exit [restart-on-exit #f])
    (send node launch-place
          (list 'dynamic-place (->module-path-bytes place-path) place-func (->string name))
          ;#:initial-message initial-message
          #:restart-on-exit restart-on-exit
          ))

(define (spawn-node-with-dynamic-place-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath (->module-path-bytes distributed-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
    (define-values (node pl)
      (spawn-node-supervise-place-at/exec host (list 'dynamic-place (->module-path-bytes place-path) place-func) #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath
                            #:restart-on-exit restart-on-exit))
    node)

(define (spawn-node-with-place-thunk-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath (->module-path-bytes distributed-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
    (define-values (node pl)
      (spawn-node-supervise-place-at/exec host (list 'place (->module-path-bytes place-path) place-func) #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath
                            #:restart-on-exit restart-on-exit))
    node)

(define (spawn-node-supervise-dynamic-place-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath (->module-path-bytes distributed-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
    (spawn-node-supervise-place-at/exec host (list 'dynamic-place (->module-path-bytes place-path) place-func) #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath
                            #:restart-on-exit restart-on-exit))

(define (spawn-node-supervise-place-thunk-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath (->module-path-bytes distributed-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
    (spawn-node-supervise-place-at/exec host (list 'place (->module-path-bytes place-path) place-func) #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath
                            #:restart-on-exit restart-on-exit))

(define (spawn-node-supervise-place-at/exec host place-exec #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath (->module-path-bytes distributed-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
  (define node (spawn-remote-racket-node host
                            #:listen-port listen-port
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath))
  (define dp
    (send node launch-place
        place-exec
        ;#:initial-message initial-message
        #:restart-on-exit restart-on-exit
        ))

  (values node dp))

(define (message-router #:node [_nc #f] #:listen-port [listen-port DEFAULT-ROUTER-PORT] . event-containers)
  (define listener (tcp-listen listen-port 4 #t))
  (define nc (or _nc (new node% [listen-port listener])))
  (for ([ec event-containers])
    (send nc add-sub-ec ec)
    (send ec backlink nc))
  (send nc sync-events))


(define (spawn-remote-racket-node host #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                                       #:racket-path [racketpath (racket-path)]
                                       #:ssh-bin-path [sshpath (ssh-bin-path)]
                                       #:distributed-launch-path [distributedlaunchpath (->module-path-bytes distributed-launch-path)])
  (new remote-node%
       [host-name host]
       [listen-port listen-port]
       [cmdline-list (list sshpath host racketpath "-tm" distributedlaunchpath "spawn" (->string listen-port))]))

(define (supervise-dynamic-place-at remote-node place-path place-func)
  (send remote-node launch-place (list 'dynamic-place (->module-path-bytes  place-path) place-func)))

(define (supervise-place-thunk-at remote-node place-path place-func)
  (send remote-node launch-place (list 'place (->module-path-bytes place-path) place-func)))

(define (supervise-thread-at remote-node place-path place-func)
  (send remote-node launch-place (list 'thread (->module-path-bytes place-path) place-func)))

(define-syntax-rule (every-seconds _seconds _body ...)
  (new respawn-and-fire% [seconds _seconds] [thunk (lambda () _body ...)]))

(define-syntax-rule (after-seconds _seconds _body ...)
  (new after-seconds% [seconds _seconds] [thunk (lambda () _body ...)]))

(define (connect-to-named-place node name)
  (send node remote-connect name))

(define (restart-every seconds #:retry [retry #f] #:on-fail-email [fail-email-address #f]
                       #:on-final-fail [on-final-fail #f])
  (new restarter% [seconds seconds] [retry retry]
       [on-final-fail on-final-fail]))

(define (log-message severity msg)
  (dcgm DCGM-TYPE-LOG-TO-PARENT -1 -1 (list severity msg)))




;; Contract: node-config -> (void)
;;
;; Purpose: use ssh to launch remote nodes of distributed places
;;
;; Example:
(define (launch-config config)
  ;FIXME kill safety
  (define nodes
    (for/list ([c config]
               [i (in-naturals)])
      (list
        (call-with-values
          (lambda ()
            (match-define (node-config node-name node-port _ ssh-path racket-path distributed-path mod-path func-name config-path conf-name) c)
            (subprocess #f #f #f (ssh-bin-path) node-name racket-path "-tm"
                        distributed-launch-path
                        "launch"
                        config-path
                        (symbol->string conf-name)
                        (number->string i)))
          list)
        c)))

  (define bb (make-bytes 4096))
  (define handlers
    (let ()
      (define (mkhandler port config)
        (let ()
          (define self
            (wrap-evt port
              (lambda (x)
                (define bbl (read-bytes-avail!* bb x))
                (define (print-out x)
                  (log-debug (format "~a:~a:~a ~a" (node-config-node-name config) (node-config-node-port config) bbl x))
                  (flush-output))
                (cond [(eof-object? bbl)
                       (print-out "EOF")
                       (set! handlers (remove self handlers))]
                      [else
                       (print-out (subbytes bb 0 bbl))]))))
          self))

        (for/fold ([r null]) ([n nodes])
          (list* (mkhandler (second (first n)) (second n))
                 (mkhandler (fourth (first n)) (second n))
                 r))))
  (define normal-finish #f)
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (let loop ()
        (apply sync/enable-break handlers)
        (unless (null? handlers)
          (loop)))
      (set! normal-finish #t))
    (lambda ()
      (unless normal-finish
        (for ([n nodes])
          (log-debug (format "Killing ~a" n))
          (define out (third (first n)))
          (with-handlers ([exn:fail? (lambda (e) (log-debug (format "Error sending Ctrl-C: ~a" e)))])
            (write-byte 3 out)
            (flush-output out)
            (sleep))
          (subprocess-kill (first (first n)) #f))))))
