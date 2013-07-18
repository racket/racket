#lang racket/base
(require racket/list
         racket/match
         racket/tcp
         racket/place
         racket/place/private/prop
         racket/place/private/th-place
         racket/place/private/coercion
         racket/place/private/async-bi-channel
         racket/class
         racket/trait
         racket/udp
         racket/runtime-path
         racket/date
         racket/contract
         syntax/location)

(define-syntax define/provide
  (syntax-rules ()
    [(_ (name x ...) body ...)
     (begin (provide name)
            (define (name x ...) body ...))]
    [(_ name val)
     (begin (provide name)
            (define name val))]))

(provide ssh-bin-path
         racket-path

         DEFAULT-ROUTER-PORT

         ;; New Design Pattern 2 API
         message-router
         spawn-node-supervise-place-at
         spawn-node-with-place-at
         supervise-place-at
         supervise-thread-at

         supervise-process-at
         every-seconds
         after-seconds
         restart-every

         connect-to-named-place

         spawn-remote-racket-node
         node-send-exit
         node-get-first-place

         ;; without message router api
         create-place-node
         distributed-place-wait

         ;; low-level API
         write-flush
         printf/f
         displayln/f
         log-message
         start-spawned-node-router ;not documented

         ;; Old Design Pattern 1 API
         ;; not documented
         startup-config
         (struct-out node-config)

         ;v3 api
         build-distributed-launch-path
         #;build-node-args
         *channel-get
         *channel-put
         send-new-place-channel-to-named-dest

         mr-spawn-remote-node
         mr-supervise-named-dynamic-place-at
         mr-connect-to
         start-message-router/thread
         spawn-node-at
         spawn-nodes/join
         spawn-nodes/join/local
 
         ;classes
         event-container<%>
         spawned-process%
         place-socket-bridge%
         node%
         socket-connection%
         remote-node%
         remote-connection%
         place%
         connection%
         respawn-and-fire%
         after-seconds%
         restarter%

         ;re-provides
         quote-module-path
         place-channel-get
         place-channel-put

         ;named-place-typed-channel
         named-place-typed-channel%
         tc-get

         ;contracts
         *channel?
         port-no?
         )

;(define klogger displayln)
(define klogger (lambda (x) (log-debug x)))

(define in-message-router-mark (cons #f #f))
(define (call-in-message-router thunk)
  (with-continuation-mark in-message-router-mark #f
                           (call-with-continuation-prompt thunk)))

(define-runtime-path distributed-launch-path "distributed/launch.rkt")
(define (build-distributed-launch-path [collects-path
                                         (simplify-path (find-executable-path (find-system-path 'exec-file) 
                                                                              (find-system-path 'collects-dir)))])
   (path->string (build-path collects-path "racket/place/distributed/launch.rkt")))

(define DEFAULT-ROUTER-PORT 6340)

; returns the path to the current racket executable on the current machine.
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
         (exists? (map (lambda (x) (build-path x cmd))
                       (regexp-split (case (system-type 'os)
                                       [(unix macosx) ":"]
                                       [(windows) "#:;"])
                                     path)))))
  (or (which "ssh")
      (fallback-paths)
      (raise "ssh binary not found")))



(define (write-flush msg [p (current-output-port)])
;  (write msg (current-output-port))
;  (newline)
  (flush-output)
  (write msg p)
  (flush-output p))

(define (printf/f . args)
  (apply printf args)
  (flush-output))

(define (displayln/f . args)
  (apply displayln args)
  (flush-output))


(define (tcp-connect/backoff rname rport #:times [times 4] #:start-seconds [start-seconds 1])
  (let loop ([t 0]
             [wait-time start-seconds])
    (with-handlers ([exn? (lambda (e)
                            (cond [(t . < . times)
                                   (klogger (format "backing off ~a sec to ~a:~a" (expt 2 t) rname rport))
                                   (sleep wait-time)
                                   (loop (add1 t) (* 2 wait-time))]
                                  [else (raise e)]))])
      (tcp-connect rname (->number rport)))))

(define (tcp-connect/retry rname rport #:times [times 10] #:delay [delay 1])
  (let loop ([t 0])
    (with-handlers ([exn? (lambda (e)
                            (cond [(t . < . times)
                                   (klogger (format "waiting ~a sec to retry connection to ~a:~a" delay rname rport))
                                   (sleep delay)
                                   (loop (add1 t))]
                                  [else (raise e)]))])
      (tcp-connect rname (->number rport)))))

(define (format-log-message severity msg)
  (define l (current-logger))
  (when (log-level? l severity)
    (log-message l severity (format "~a ~a" (date->string (current-date) #t) msg) #f)))

;node configuration
(struct node-config (node-name 
                     node-port
                     proc-count
                     ssh-path
                     racket-path
                     distributed-path
                     mod-path
                     func-name
                     conf-path
                     conf-name) #:prefab)
;distributed communication group message
(struct dcgm (type src dest msg) #:prefab)

;dcgm types
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
(define DCGM-TYPE-NEW-CONNECTION      10)
(define DCGM-TYPE-SET-OWNER           11)

(define DCGM-NEW-NODE-CONNECT         50)
(define DCGM-NEW-PLACE-CHANNEL        51)

(define DCGM-CONTROL-NEW-NODE        100)
(define DCGM-CONTROL-NEW-PLACE       101)
(define DCGM-CONTROL-NEW-CONNECTION  102)

(define (send-new-place-channel-to-named-dest ch src-id dest-list)
  (define-values (e1 e2) (place-channel))
  (place-channel-put ch (dcgm DCGM-NEW-PLACE-CHANNEL (list 'new-place-channel src-id) dest-list e2))
  e1)

(define-syntax-rule (reduce-sum seq item body ...)
  (for/fold ([sum 0]) ([item seq]) (+ sum (begin body ...))))

(define (total-node-count conf) (reduce-sum conf item (node-config-proc-count item)))

;; Contract: start-node-router : VectorOf[ (or/c place-channel socket-connection)] -> (void)
;; Purpose: Forward messages between channels and build new point-to-point subchannels
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

(define place<%>
  (interface* ()
              ([prop:place
                 (lambda (method obj . args)
                   (case method
                     [(place-channel-get)
                      (*channel-get obj)]
                     [(place-channel-put)
                      (apply *channel-put obj args)]
                     [(place-wait)
                      (distributed-place-wait obj)]
                     [(place-channel?) #t]
                     [(place?) #t]
                     [else
                       (raise (format "Error in place<%> ~v ~v ~v" obj method args))]))])
  ))

(define event<%>
  (interface* ()
              ([prop:evt
                 (lambda (x)
                   (apply choice-evt (send x register null)))])))


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
      (init-field [s #f]
                  [i #f]
                  [o #f]
                  [e #f])
      (field [pid #f])

      (let-values ([(_s _o _i _e) (apply subprocess o i e cmdline-list)])
        (set! pid (subprocess-pid _s))
        (set! s _s)
        (set! o (box _o))
        (set! i (box _i))
        (set! e (box _e)))
      (klogger (format"SPAWNED-PROCESS:~a ~a" pid cmdline-list))

      (define (mk-handler _port desc)
        (define port (unbox _port))
        (if port
          (wrap-evt port (lambda (e)
            (define (print-out x) (klogger (format "SPAWNED-PROCESS ~a:~a:~a ~a" pid desc (->length x) x))
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
      (define/public (kill [force #t]) (subprocess-kill s force))
      (define/public (wait-for-die) (subprocess-wait s))
      (define/public (register nes)
        (for/filter/fold/cons nes ([x (list s (list o "OUT") (list e "ERR"))])
          (cond
            [(subprocess? x) (wrap-evt s (lambda (e)
                                             (klogger (format "SPAWNED-PROCESS ~a DIED" pid))
                                             (and parent (send parent process-died this))))]
            [(list? x) (apply mk-handler x)]
            [else #f])))
      (super-new)
  )))

(define place-socket-bridge%
  (backlink
    (class*
      object% (event-container<%> event<%>)
      (init-field pch
                  sch
                  id
                  node)
      (field [msg-queue null])

      (define (queue-had-died-message?)
        (for/or ([msg msg-queue])
          (= (dcgm-type msg) DCGM-DPLACE-DIED)))

      (define/public (register nes)
        (cons
          (wrap-evt
            pch
            (lambda (e)
              (match e
                [(dcgm #;8 (== DCGM-TYPE-LOG-TO-PARENT) _ _ (list severity msg))
                  (send node log-from-child #:severity severity msg)]
                [(dcgm #;51 (== DCGM-NEW-PLACE-CHANNEL) _ _ _)
                  (send node forward-mesg e pch)]
                [(dcgm #;102 (== DCGM-CONTROL-NEW-CONNECTION) dest -1 (list name ch))
                  (send node forward-mesg e pch)]
                [else (put-msg e)])))
          nes))
      (define/public (drain-place-channel)
        (let loop ()
          (when (apply sync/timeout/enable-break 0 (register null))
            (loop))))
      (define/public (get-sc-id) id)
      (define/public (get-raw-msg)
        (let loop ()
          (define msg (send sch read-message))
          (cond
            [(eof-object? msg)
              (printf "EOF from remote end during place-socket-bridge% get-raw-msg\n")
              msg]
            [(= (dcgm-type msg) DCGM-DPLACE-DIED)
              (set! msg-queue (append msg-queue (list msg)))
              (loop)]
            [else
              (dcgm-msg msg)])))
      (define/public (wait-to-die)
        (or
          (queue-had-died-message?)
          (let loop ()
            (define msg (send sch read-message))
            (unless (= (dcgm-type msg) DCGM-DPLACE-DIED)
              (loop))))
        (void))
      (define/public (put-msg msg)
        ;(printf/f "PSB3 ~a ~a ~a\n" sch id msg)
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
      (init-field [spawned-nodes (make-hash)])
      (init-field [solo-nodes (make-hash)])
      (init-field [named-places (make-hash)])
      (init-field [beacon #f])
      (init-field [owner #f])
      (init-field [nodes #f])
      (field [id 0])

      (define/public (nextid)
        (set! id (add1 id))
        id)
      (define (add-socket-port pair)
        (set! socket-ports (append socket-ports (list pair))))
      (define/public (add-sub-ec ec)
        (set! sub-ecs (append sub-ecs (list ec))))
      (define (add-spawned-node key ec)
        (hash-set! spawned-nodes key ec))
      (define (find-spawned-node key)
        (hash-ref spawned-nodes key #f))
      (define (add-solo-node key ec)
        (hash-set! solo-nodes key ec))
      (define (find-solo-node key)
        (hash-ref solo-nodes key #f))
      (define (add-psb ec)
        (set! psbs (append psbs (list ec))))
      (define (add-named-place name np)
        (hash-set! named-places (->string name) np))
      (define (named-place-lookup name)
        (hash-ref named-places (->string name) #f))
      (define (add-place-channel-socket-bridge pch sch id)
        (add-psb (new place-socket-bridge% [pch pch] [sch sch] [id id] [node this])))
      (define/public (forward-mesg m src-channel)
        ;(printf/f "FORWARD MESSAGE ~a ~a\n" src-channel m)
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
               (place-channel-put d m)]
              [else (raise (format "Unexpected channel type1 ~a" d))])]
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
             [(list 'channel-connect name src-id)
              (define np (named-place-lookup name))
              (cond
                [np
                  (define nc (new connection%
                                 [name-pl np]
                                 [ch-id ch-id]
                                 [sc src-channel]
                                 [node this]
                                 [channel-connection src-id]))
                  (add-sub-ec nc)]

                [else
                  (sconn-write-flush src-channel (dcgm DCGM-TYPE-INTER-DCHANNEL ch-id ch-id
                                                       (format "ERROR: name not found ~a" name)))])]
             [else  
              (with-handlers ([exn? (lambda (e) 
                                      (printf/f "Error starting place command ~a ~a\n" place-exec e)
                                      #;(raise e))])
                (define np (new place%
                             [place-exec place-exec]
                             [ch-id ch-id]
                             [sc src-channel]
                             [node this]))
                (match place-exec
                  [(list _ _ _ name) (add-named-place name np)]
                  [else (add-sub-ec np)]))])]
          [(dcgm 3 #;(== DCGM-TYPE-NEW-INTER-DCHANNEL) src dest ch-id)
            (define s src-channel)
            (define d (vector-ref chan-vec dest))
            (define-values (pch1 pch2) (place-channel))
            (sconn-add-subchannel s ch-id pch1)
            (add-place-channel-socket-bridge pch1 s ch-id)
            (place-channel-put d (dcgm DCGM-TYPE-NEW-DCHANNEL src dest pch2))]
          [(dcgm 4 #;(== DCGM-TYPE-INTER-DCHANNEL) _ ch-id msg)
            (define pch (sconn-lookup-subchannel src-channel ch-id))
            ;(printf/f "4 ~a ~a ~a ~a\n" src-channel ch-id pch msg)
            (match pch
              [#f (raise (format "Unknown channel ch-id ~a in message ~a" ch-id m))]
              [else
                (cond
                  [(place-channel? pch)
                    (place-channel-put pch msg)]
                  [(is-a? pch connection%)
                   (send pch forward msg)]
                  [(th-place-channel? pch)
                   (th-place-channel-put pch msg)]
                  [else (raise (format "Unexpected channel type2 ~a" pch))])])]
          [(dcgm 6 #;(== DCGM-TYPE-SPAWN-REMOTE-PROCESS) src (list node-name node-port mod-path funcname) ch1)
           (define node (spawn-remote-racket-node node-name #:listen-port node-port))
           (for ([x (in-hash-values spawned-nodes)])
             (send x notify-of-new-node node-name node-port))
           (add-spawned-node (list node-name node-port) node)
           (send node launch-place
                    (list 'dynamic-place mod-path funcname)
                    ;#:initial-message initial-message
                    #:one-sided-place? ch1
                    ;#:restart-on-exit restart-on-exit
                    )]
          [(dcgm 7 #;(== DCGM-DPLACE-DIED) -1 -1 ch-id)
            (klogger (format"PLACE ~a died" ch-id))]
          [(dcgm 8 #;(== DCGM-TYPE-LOG-TO-PARENT) _ _ (list severity msg))
            (log-from-child #:severity severity msg)]
          [(dcgm 11 #;(== DCGM-TYPE-SET-OWNER) -1 -1 msg)
            (klogger (format "RECV DCGM-TYPE-SET-OWNER ~a" src-channel))
            (set! owner src-channel)]
          [(dcgm #;50 (== DCGM-NEW-NODE-CONNECT) -1 -1 (list node-name node-port))
            (define node (find-spawned-node (list node-name node-port)))
            (unless node
              (add-spawned-node (list node-name node-port) (new remote-node% [host-name node-name] [listen-port node-port])))]

          [(dcgm #;51 (== DCGM-NEW-PLACE-CHANNEL) src-id (and dest (list host port name)) pch)
            ;(printf/f "DCGM-NEW-PLACE-CHANNEL ~a ~a\n" src-id dest)
            (define node (find-spawned-node (list host port)))
            (unless node (raise (format "1DCGM-CONTROL-NEW-CONNECTION Node ~a not found in ~a" dest spawned-nodes)))
            (send node connect-channel src-id name #:one-sided pch)]

          [(dcgm #;100 (== DCGM-CONTROL-NEW-NODE) -1 solo (list node-name node-port))
           (define node (spawn-remote-racket-node node-name #:listen-port node-port))
           (cond
             [solo
               (add-solo-node (list node-name node-port) node)]
             [else
               (for ([x (in-hash-values spawned-nodes)])
                 (send x notify-of-new-node node-name node-port))
               (add-spawned-node (list node-name node-port) node)])]
          [(dcgm #;101 (== DCGM-CONTROL-NEW-PLACE) dest -1 place-exec)
           (define node (find-spawned-node dest))
           (unless node (raise (format "2DCGM-CONTROL-NEW-PLACE Node ~a not found in ~a" dest spawned-nodes)))
           (send node launch-place place-exec)]
          [(dcgm #;102 (== DCGM-CONTROL-NEW-CONNECTION) dest -1 (list name ch))
           (define node
             (or (find-spawned-node dest)
                 (let ()
                   (define node (new remote-node% [host-name (first dest)] [listen-port (second dest)]))
                   (add-spawned-node dest node)
                   node)))
           (send node remote-connect name #:one-sided ch)]

          [(dcgm mtype srcs dest msg)
;            (printf/f "DEFAULT ACTION ~a ~a ~a ~a\n" mtype srcs dest msg)
            (define d (vector-ref chan-vec dest))
            (cond
              [(is-a? d socket-connection%)
                (sconn-write-flush d m)]
              [(or (place-channel? d) (place? d))
                (place-channel-put d m)])]
          [(? eof-object?)
            (klogger (format "connection died"))
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
                     (sconn-build-forward-event x (lambda (e x) (forward-mesg e x)))]
                    [(or (place-channel? x) (place? x))
                     (wrap-evt x (lambda (e) (forward-mesg e x)))]
                    [(channel? x)
                     (wrap-evt x (lambda (e) (forward-mesg e x)))]
                    [else (raise (format "Unexpected channel type3 ~a" x))])

                  n))
              nes)]
           [nes
            (if listen-port
              (cons
                (wrap-evt listen-port (lambda (e)
                  (define-values (in out) (tcp-accept listen-port))
                  (define-values (lh lp rh rp) (tcp-addresses in #t))
                  (klogger (format "INCOMING CONNECTION ~a:~a <- ~a:~a" lh lp rh rp))
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
                     (sconn-build-forward-event x (lambda (e x) (forward-mesg e x)))]
                    [(or (place-channel? x) (place? x))
                     (wrap-evt x (lambda (e)
                                     ;(printf "SOCKET-PORT PLACE MESSAGE ~a\n" e)
                                     (forward-mesg e x)))]
                    [else (raise (format "Unexpected channel type4 ~a" x))])
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
              (for/fold ([n nes]) ([x (in-hash-values spawned-nodes)])
                (send x register n))
              nes)]
           [nes
            (if solo-nodes
              (for/fold ([n nes]) ([x (in-hash-values solo-nodes)])
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

      (when nodes
        (for ([n nodes])
          (define n-host-name (send n get-host-name))
          (define n-port (send n get-listen-port))

          (for ([sn (in-hash-values spawned-nodes)])
            (send sn notify-of-new-node n-host-name n-port))
          (add-spawned-node (list n-host-name n-port) n)))


      (super-new)
  )))


;socket channel
(define (sconn-add-subchannel s ch-id ch)     (send s add-subchannel ch-id ch))
(define (sconn-lookup-subchannel s ch-id)     (send s lookup-subchannel ch-id))
(define (sconn-write-flush s x)               (send s _write-flush x))
(define (sconn-remove-subchannel s scid)      (send s remove-subchannel scid))
(define (sconn-build-forward-event s forwarder) (send s build-forward-event forwarder))

(define socket-connection%
  (backlink
    (class* object% (event-container<%>)
      (init-field [host #f]
                  [port #f]
                  [retry-times 30]
                  [delay 1]
                  [background-connect? #t]
                  [in #f]
                  [out #f]
                  [remote-node #f])
      (field [subchannels null]
             [connecting #f]
             [ch #f])

      (define (forward-mesg x) 
        (raise (format "Getting forwarded ~a" x))
        (void))

      (define (tcp-connect/retry rname rport #:times [times 10] #:delay [delay 1])
        (let loop ([t 0])
          (with-handlers ([exn? (lambda (e)
                        (cond [(t . < . times)
                               (klogger (format "try ~a waiting ~a sec to retry connection to ~a:~a" t delay rname rport))
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

      (define (handle-error e)
        (cond
          [remote-node => (lambda (n)
                                      (send n tcp-connection-died host port e))]
          [else (raise (format "TCP connection to ~a:~a failed ~a\n" host port e))]))

      (define/public (add-subchannel id pch)
        (set! subchannels (append subchannels (list (cons id pch)))))
      (define/public (lookup-subchannel id)
        (match (assoc id subchannels)
          [(cons _id _pch) _pch]
          [else #f]))
      (define/public (_write-flush x)
        (when (equal? out #f) (ensure-connected))
        ;(printf/f "SC ~a ~a\n" x out)
        (with-handlers ([exn:fail? handle-error])
          (write-flush x out)))
      (define/public (remove-subchannel id)
        (set! subchannels (filter-map
                            (lambda (x) (and (not (= (car x) id)) x))
                            subchannels)))
      (define/public (addresses) (tcp-addresses in #t))
      (define/public (build-forward-event forwarder)
        (when (equal? out #f) (ensure-connected))
        (wrap-evt in (lambda (e)
                       (forwarder
                          (with-handlers ([exn:fail? handle-error])
                            (define r (read in))
                            ;(printf/f "SC IN ~a\n" r)
                            r)
                          this))))

      (define/public (read-message)
        (when (equal? out #f) (ensure-connected))
        (define m (read in))
        ;(printf/f "MESSAGE ~a\n" m)
        m)
      (define/public (register nes)
        (raise "Not-implemented/needed")
        (cons (wrap-evt in void) nes))

      (when (and host port background-connect?)
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
      (when (and host port (not background-connect?))
        (tcp-connect/retry host port #:times retry-times #:delay delay))
      (super-new)
  )))

(define place-location<%>
  (interface* 
   ()
   ([prop:place-location
     (lambda (remote-node place-path place-func named)
       (supervise-place-at remote-node place-path place-func
                           #:named named))])))

(define remote-node%
  (backlink
    (class* object% (event-container<%> event<%> place-location<%>)
      (init-field host-name)
      (init-field listen-port)
      (init-field [cmdline-list #f])
      (init-field [sc #f]) ;socket-connection
      (init-field [restart-on-exit #f])
      (init-field [use-current-ports #f])
      (field [sp #f]) ;spawned-process
      (field [id 0])
      (field [remote-places null])

      (define/public (get-host-name) host-name)
      (define/public (get-listen-port) listen-port)
      (define/public (nextid)
        (set! id (add1 id))
        id)

      (define (add-remote-place rp)
        (set! remote-places (append remote-places(list rp))))
      (define (spawn-node)
        (and cmdline-list
          (set! sp 
            (if use-current-ports
              (new spawned-process% [cmdline-list cmdline-list] [parent this]
                   [o (current-output-port)]
                   [e (current-error-port)])
              (new spawned-process% [cmdline-list cmdline-list] [parent this])))))
      (define (setup-socket-connection)
        (set! sc (new socket-connection% [host host-name] [port listen-port] [remote-node this]))
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
            (klogger (format "SPAWNED-PROCESS:~a PLACE DIED ~a:~a:~a" (get-sp-pid) host-name listen-port ch-id))
            (cond
              [(find-place-by-sc-id ch-id) => (lambda (rp)
                                                (send rp place-died))]
              [else (raise (format "remote-place for sc-id ~a not found\n" ch-id))])]
          [(dcgm 4 #;(== DCGM-TYPE-INTER-DCHANNEL) _ ch-id msg)
            (define pch (sconn-lookup-subchannel sc ch-id))
            ;(printf/f "44 ~a ~a ~a ~a\n" in-port ch-id pch msg)
            (match pch
              [#f (raise (format "Unknown channel ch-id ~a in message ~a" ch-id it))]
              [else
                (cond
                  [(place-channel? pch)
                    (place-channel-put pch msg)]
                  [(is-a? pch connection%)
                   (send pch forward msg)]
                  [(th-place-channel? pch)
                   (th-place-channel-put pch msg)]
                  [(async-bi-channel? pch)
                   (async-bi-channel-put pch msg)]
                  [else (raise (format "Unexpected channel type ~a" pch))])])]
          [(dcgm 8 #;(== DCGM-TYPE-LOG-TO-PARENT) _ _ (list severity msg))
            (define parent (send this get-router))
            (cond
              [parent
                (send parent log-from-child #:severity severity msg)]
              [else (format-log-message severity msg)])]

          [(? eof-object?)
           (define-values (lh lp rh rp) (send sc addresses))
           (klogger (format "EOF on node socket connection pid to ~a ~a:~a CONNECTION ~a:~a -> ~a:~a" (get-sp-pid) host-name listen-port lh lp rh rp))
           (set! sc #f)]

          [else (klogger (format"received message ~a from ~a" it in-port))]))

      (define/public (get-log-prefix) (format "PLACE ~a:~a" host-name listen-port))
      (define/public (tcp-connection-died host port e)
        (klogger (format "TCP connection ~a:~a died, ~a, restarting node/connection" host-name listen-port e))
        (and sp (send sp kill))
        (set! sp #f)
        (cond
          [cmdline-list (process-died null)]
          [restart-on-exit
            (if (equal? restart-on-exit #t)
                (restart-node)
                (send restart-on-exit restart restart-node))]
          [else
            (klogger (format "No restart condition for ~a" (get-log-prefix)))]))

      (define/public (process-died child)
        (klogger (format "Remote node pid ~a ~a:~a died" (get-sp-pid) host-name listen-port))
        (set! sp #f)
        (cond
          [restart-on-exit
            (cond
              [cmdline-list
                (if (equal? restart-on-exit #t)
                    (restart-node)
                    (send restart-on-exit restart restart-node))]
              [else
                (klogger (format "No restart cmdline arguments for ~a" (get-log-prefix)))])]
          [else
            (klogger (format "No restart condition for ~a" (get-log-prefix)))]))

      (define/public (get-first-place)
        (car remote-places))
      (define/public (get-first-place-channel)
        (send (car remote-places) get-channel))

      (define/public (drop-sc-id scid)
        (sconn-remove-subchannel sc scid))

      (define/public (launch-place place-exec #:restart-on-exit [restart-on-exit #f] #:one-sided-place? [one-sided-place? #f])
        (define rp (new remote-connection% [node this] [place-exec place-exec] [restart-on-exit restart-on-exit]
                        [one-sided one-sided-place?]))
        (add-remote-place rp)
        rp)

      (define/public (remote-connect name #:restart-on-exit [restart-on-exit #f] #:one-sided [one-sided #f])
        (define rp (new remote-connection% [node this] [name name] [restart-on-exit restart-on-exit]
                        [one-sided one-sided]))
        (add-remote-place rp)
        rp)

      (define/public (connect-channel src-id name #:restart-on-exit [restart-on-exit #f] #:one-sided [one-sided #f])
        (define rp (new remote-connection% [node this] [name name] [src-id src-id] [restart-on-exit restart-on-exit]
                        [one-sided one-sided]))
        (add-remote-place rp)
        rp)

      (define/public (spawn-remote-connection msg-gen dch)
        (define ch-id (nextid))
        (sconn-add-subchannel sc ch-id dch)
        (sconn-write-flush sc (msg-gen ch-id))
        (new place-socket-bridge% [pch dch] [sch sc] [id ch-id] [node this]))

      (define/public (send-exit)
        (sconn-write-flush sc (dcgm DCGM-TYPE-DIE -1 -1 "DIE")))

      (define/public (wait-for-die)
        (send sp wait-for-die))

      (define/public (register es)
        (let* ([es (if sp (send sp register es) es)]
               [es (for/fold ([nes es]) ([rp remote-places])
                             (send rp register nes))]
               [es (if sc (cons (sconn-build-forward-event sc on-socket-event) es) es)]
               [es (if (and restart-on-exit
                            (not (equal? restart-on-exit #t)))
                       (send restart-on-exit register es)
                       es)])
          es))

      (define/public (notify-of-new-node node-name node-port)
        (sconn-write-flush sc (dcgm DCGM-NEW-NODE-CONNECT -1 -1 (list node-name node-port))))

      (super-new)
      )))

(define (node-send-exit node) (send node send-exit))
(define (node-get-first-place node) (send node get-first-place))
(define (distributed-place-wait p) (send p place-wait))

(define remote-connection%
  (backlink
    (class*
      object% (event-container<%> place<%> event<%>)
      (init-field node)
      (init-field [place-exec #f])
      (init-field [name #f])
      (init-field [src-id #f])
      (init-field [one-sided #f])
      (init-field [restart-on-exit #f])
      (init-field [on-channel #f])
      (init-field [on-channel/2 #f])
      (field [psb #f])
      (field [pc #f])
      (field [rpc #f])
      (field [running #f])
      (field [k #f])

      (unless (or place-exec name)
        (raise "for new places place-exec must be set, for named-place connections the named argument must be supplied"))
      (when (and place-exec name)
        (raise "only one of the place-exec or the named arguements can be set at a time."))
      (when (and name restart-on-exit)
        (raise "named place connections that restart on exit are not possible"))

      (cond
        [one-sided
          (set! rpc one-sided)]
        [else
          (define-values (pch1 pch2) (place-channel))
          (set! rpc pch1)
          (set! pc pch2)])

      (set! psb
        (send node spawn-remote-connection 
              (cond
                [place-exec (lambda (ch-id) (dcgm DCGM-TYPE-NEW-PLACE -1 place-exec ch-id))]
                [src-id     (lambda (ch-id) (dcgm DCGM-TYPE-NEW-PLACE -1 (list 'channel-connect name src-id) ch-id))]
                [else       (lambda (ch-id) (dcgm DCGM-TYPE-NEW-PLACE -1 (list 'connect name) ch-id))])
              rpc))

      (define (restart-place)
        (send node drop-sc-id (send psb get-sc-id))
        (set! psb (send node spawn-remote-connection (lambda (ch-id) (dcgm DCGM-TYPE-NEW-PLACE -1 place-exec ch-id)) rpc)))

      (define/public (stop) (void))
      (define/public (get-channel) pc)
      (define/public (set-on-channel! proc) (set! on-channel proc))
      (define/public (get-sc-id) (send psb get-sc-id))
      (define/public (set-handle-channel! x) (set! on-channel x))
      (define/public (place-died)
        (cond
          [restart-on-exit
                (cond 
                  [(equal? restart-on-exit #t) (restart-place)]
                  [(procedure? restart-on-exit) (restart-on-exit)]
                  [else (send restart-on-exit restart restart-place)])]
          [else
            (klogger (format "No restart condition for ~a:~a"
                    (send node get-log-prefix)
                    (send psb get-sc-id)))]))
      (define (on-channel-event e)
        (klogger (format "~a ~a" (send node get-log-prefix) e)))
      (define/public (register es)
        (let* ([es (if pc (cons (wrap-evt pc
                                            (cond
                                              [k
                                               (lambda (e)
                                                 (call-in-message-router (lambda ()
                                                   (begin0
                                                     (k e)
                                                     (set! k #f)))))]
                                              [on-channel/2
                                                (lambda (e)
                                                  (on-channel/2 pc e))]
                                              [on-channel on-channel]
                                              [else
                                               on-channel-event])) es) es)]
               [es (send psb register es)]
               [es (if (and restart-on-exit
                            (not (equal? restart-on-exit #t))
                            (not (procedure? restart-on-exit)))
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
      (define/public (place-wait)
        (send psb wait-to-die))

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
        ;drain operation might be needed if sync returns dead place evt before ready place channel
        ;(send psb drain-place-channel)
        ;(set! psb #f)
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
                  node
                  [channel-connection #f]
                  [on-place-dead #f])
      (field [psb #f])

      (define-values (pch1 pch2) (place-channel))

      (define forward-ch 
        (if channel-connection
            pch1
            (send name-pl get-channel)))

      (define control-ch 
        (if channel-connection
            (send name-pl get-channel)
            #f))


      (sconn-add-subchannel sc ch-id this)
      (set! psb (new place-socket-bridge% [pch pch1] [sch sc] [id ch-id] [node node]))
      
      (when channel-connection
        (place-channel-put control-ch (list channel-connection pch2)))

      (define/public (forward msg)
        (place-channel-put forward-ch 
                           (if channel-connection
                               msg
                               (list msg pch2))))

      (define/public (put msg)
        (sconn-write-flush sc (dcgm DCGM-TYPE-INTER-DCHANNEL ch-id ch-id msg)))
      (define/public (register es) (send psb register es))

      (super-new)
      )))

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
                          (call-in-message-router  thunk)))
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
                          (call-in-message-router thunk)))
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
           (klogger (format "Already retried to restart ~a times" retry))
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

(define on-exit%
  (class*
    object% ()
    (init-field thunk)
    (define/public (restart restart-func)
      (thunk))))



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
  (new spawned-process% [cmdline-list command-line-list]))

(define (mk-place-creation-addr place-path place-func name thunk)
  (list* (if thunk 'place 'dynamic-place) 
         (->writeable-module-path place-path) 
         place-func 
         (if name (list (->string name)) null)))

(define (spawn-node-with-place-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath #f]
                            #:restart-on-exit [restart-on-exit #f]
                            #:named [named #f]
                            #:thunk [thunk #f])
    (define-values (node pl)
      (spawn-node-supervise-place-at/exec host 
                            (mk-place-creation-addr (->writeable-module-path place-path) place-func #f thunk) 
                            #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath
                            #:restart-on-exit restart-on-exit))
    node)

(define (spawn-node-supervise-place-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath #f]
                            #:restart-on-exit [restart-on-exit #f]
                            #:named [named #f]
                            #:thunk [thunk #f])
    (spawn-node-supervise-place-at/exec host 
                            (mk-place-creation-addr (->writeable-module-path place-path) place-func #f thunk) 
                            #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath
                            #:restart-on-exit restart-on-exit))

(define (spawn-node-supervise-place-at/exec host place-exec #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath #f]
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
                                       #:distributed-launch-path [distributedlaunchpath #f]
                                       #:use-current-ports [use-current-ports #f])
  (new remote-node%
       [host-name host]
       [listen-port listen-port]
       [cmdline-list (if distributedlaunchpath
                       (list sshpath host racketpath "-tm" distributedlaunchpath "spawn" (->string listen-port))
                       (list sshpath host racketpath "-lm racket/place/distributed/launch spawn" (->string listen-port)))]
       [use-current-ports use-current-ports]))

(define (create-place-node host #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                                       #:racket-path [racketpath (racket-path)]
                                       #:ssh-bin-path [sshpath (ssh-bin-path)]
                                       #:distributed-launch-path [distributedlaunchpath #f]
                                       #:use-current-ports [use-current-ports #t])
  (spawn-remote-racket-node host
                            #:listen-port listen-port
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:distributed-launch-path distributedlaunchpath
                            #:use-current-ports use-current-ports))

(define (supervise-place-at remote-node place-path place-func
                            ;;#:initial-message [initial-message #f]
                            #:restart-on-exit [restart-on-exit #f]
                            #:named [named #f]
                            #:thunk [thunk #f])
  
  (send remote-node launch-place 
        (mk-place-creation-addr place-path place-func named thunk)
        #:restart-on-exit restart-on-exit
        ;#:initial-message initial-message
        ))

(define (supervise-thread-at remote-node place-path place-func)
  (send remote-node launch-place (list 'thread (->writeable-module-path place-path) place-func)))

(define-syntax-rule (every-seconds _seconds _body ...)
  (new respawn-and-fire% [seconds _seconds] [thunk (lambda () _body ...)]))

(define-syntax-rule (after-seconds _seconds _body ...)
  (new after-seconds% [seconds _seconds] [thunk (lambda () _body ...)]))

;; -> node name -> 
(define (connect-to-named-place node name)
  (send node remote-connect name))

(define (restart-every seconds #:retry [retry #f] #:on-fail-email [fail-email-address #f]
                       #:on-final-fail [on-final-fail #f])
  (new restarter% [seconds seconds] [retry retry]
       [on-final-fail on-final-fail]))

(define (on-place-exit thunk)
  (new (on-exit% [thunk thunk])))

(define (log-message severity msg)
  (dcgm DCGM-TYPE-LOG-TO-PARENT -1 -1 (list severity msg)))

;;
;; API Version 3
;;

;;;(define (start-message-router/place #:listen-port [listen-port DEFAULT-ROUTER-PORT])
;;;  (define mr
;;;    (place ch
;;;      (match (place-channel-get ch)
;;;             [(list listen-port)
;;;              (define listener (tcp-listen listen-port 4 #t))
;;;              (define mrn (new node% [listen-port listener]
;;;                                     [chan-vec (vector ch)]))
;;;              (send mrn sync-events)])))
;;;  (place-channel-put mr (list listen-port)))
(define (*channel? ch)
  (or (place-channel? ch)
      (async-bi-channel? ch)
      (channel? ch)
      (is-a? ch remote-connection%)))

(define (*channel-put ch msg)
  (cond
    [(is-a? ch remote-connection%) (send ch put-msg msg)]
    [(place-channel? ch) (place-channel-put ch msg)]
    [(async-bi-channel? ch) (async-bi-channel-put ch msg)]
    [(channel? ch) (channel-put ch msg)]
    [else (raise (format "unknown channel type ~a" ch))]))

(define (*channel-get ch)
  (cond
    [(is-a? ch remote-connection%) 
     (cond
       [(continuation-mark-set-first #f in-message-router-mark)
         (send ch get-msg)]
       [else
         (send ch get-raw-msg)])]
    [(place-channel? ch) (place-channel-get ch)]
    [(async-bi-channel? ch) (async-bi-channel-get ch)]
    [(channel? ch) (channel-get ch)]
    [else (raise (format "unknown channel type ~a" ch))]))

(define/provide (mr-spawn-remote-node mrch host #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                                      #:solo [solo #f])
  (*channel-put mrch (dcgm DCGM-CONTROL-NEW-NODE -1 solo (list host listen-port))))

(define/provide (mr-supervise-named-dynamic-place-at mrch dest name path func)
  (*channel-put mrch (dcgm DCGM-CONTROL-NEW-PLACE dest -1 (list 'dynamic-place path func name))))

(define/provide (mr-connect-to mrch dest name)
  (define-values (ch1 ch2)
    (cond
      [(channel? mrch) (make-async-bi-channel)]
      [(place-channel? mrch) (place-channel)]
      [else (raise (format "Unexpected channel type6 ~a" mrch))]))
  (*channel-put mrch (dcgm DCGM-CONTROL-NEW-CONNECTION dest -1 (list name ch2)))
  ch1)

(define (port-no? x)  
  (and (exact-nonnegative-integer? x)
                ((integer-in 0 65535) x)))

(define/provide (start-message-router/thread #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                                             #:nodes [nodes null])
  (define ch (make-channel))
  (define mr
    (thread
      (lambda ()
        (define listener (tcp-listen listen-port 4 #t))
        (define mrn (new node% [listen-port listener]
                               [nodes nodes]
                               [chan-vec (vector ch)]))
        (send mrn sync-events))))
  (values mr ch))

(define (spawn-node-at host #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:distributed-launch-path [distributedlaunchpath #f])

  (define ch (make-channel))
  (thread
    (lambda () 
      (with-handlers ([exn:fail? (lambda (e) (channel-put #f))])
        (channel-put ch (spawn-remote-racket-node host #:listen-port listen-port
                                                       #:racket-path racketpath
                                                       #:ssh-bin-path sshpath
                                                       #:distributed-launch-path distributedlaunchpath)))))
  ch)

(define/provide (spawn-nodes/join nodes-descs)
  (for/list ([x
               (for/list ([n nodes-descs])
                 (apply keyword-apply spawn-node-at n))])
    (unless x
      (raise "Failed to connect to a remotely spawned node"))
    (channel-get x)))

#;(define build-node-args
  (make-keyword-procedure (lambda (kws kw-args . rest)
                            (list kws kw-args rest))))

(define/provide (spawn-nodes/join/local nodes-descs)
    (spawn-nodes/join
      (for/list ([c nodes-descs])
        (match-define (list-rest host port _rest) c)
        (define rest 
          (cond 
            [(null? _rest)
             (list (make-immutable-hash (list (cons "listen-port" port))))]
            [else
              (list
                (hash-set (car _rest) "listen-port" port))]))
        (define-values (k v) 
          (let loop ([keys (list "racket-path" "listen-port" "distributed-launch-path")]
                     [k null]
                     [v null])
            (cond 
             [(pair? keys)
              (cond
                [(hash-ref (car rest) (car keys) #f) => (lambda (x) 
                  (loop (cdr keys)
                        (cons (string->keyword (car keys)) k)
                        (cons x v)))]
                [else
                  (loop (cdr keys) k v)])]
             [else
               (values k v)])))
        (list k v (list host)))))

(define named-place-typed-channel%
  (class*
    object% ()
    (init-field ch)
    (field [msgs null])
    (define/public (get-evt type)
      (cond
        [(has-type type) => (lambda (x)
                              (wrap-evt always-evt (lambda (e) x)))]
        [else
          (wrap-evt ch (lambda (v)
                         (set! msgs (append msgs (list v)))
                         (cond 
                           [(has-type type) => (lambda (x) x)]
                           [else #f])))]))
                     
    (define (has-type type)
      (let loop ([l msgs]
                 [nl null])
        (cond
          [(null? l) #f]
          [(equal? type (caaar l))
           (set! msgs (append (reverse nl) (cdr l)))
           (car l)]
          [else
           (loop (cdr l) (cons (car l) nl))])))

    (define/public (get type)
      (let loop ([l msgs]
                 [nl null])
        (cond
          [(null? l)
           (define nm (place-channel-get ch))
           (set! msgs (append msgs (list nm)))
           (loop msgs null)]
          [(equal? type (caaar l))
           (set! msgs (append (reverse nl) (cdr l)))
           (car l)]
          [else
           (loop (cdr l) (cons (car l) nl))])))
    (super-new)
    ))

(define (tc-get type ch) (send ch get type))

