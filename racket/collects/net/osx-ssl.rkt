#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/nsstring
         ffi/unsafe/alloc
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         racket/port
         racket/format
         openssl)

(provide osx-ssl-connect
         osx-ssl-abandon-port
         osx-ssl-output-port?
         osx-old-openssl?)

(define (osx-old-openssl?)
  (and (eq? 'macosx (system-type))
       (or (not ssl-available?)
           (not (memq 'tls12 (supported-client-protocols))))))

(define cf-lib
  (and (eq? 'macosx (system-type))
       (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")))
(define net-lib
  (and (eq? 'macosx (system-type))
       (ffi-lib
        "/System/Library/Frameworks/CFNetwork.framework/CFNetwork"
        #:fail (lambda ()
                 ;; Path inside "CoreServices.framework" needed for OS X 10.5
                 (ffi-lib "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/CFNetwork.framework/CFNetwork")))))

(define-ffi-definer define-cf cf-lib
  #:default-make-fail make-not-available)
(define-ffi-definer define-net net-lib
  #:default-make-fail make-not-available)
(define-ffi-definer define-racket #f
  #:default-make-fail make-not-available)

(define _CFReadStreamRef (_cpointer/null 'CFReadStreamRef))
(define _CFWriteStreamRef (_cpointer/null 'CFWriteStreamRef))

(define _CFRunLoopRef (_cpointer/null 'CFRunLoopRef))

(define _CFDictionaryRef (_cpointer/null 'CFDictionaryRef))

(define _Boolean _bool)
(define _CFIndex _long)

(define-cf CFRelease (_fun _pointer -> _void)
  #:wrap (deallocator))

(define retain
  ((allocator CFRelease) (lambda (p) p)))

;; Call in atomic mode to ensure `retain` calls:
(define-cf CFStreamCreatePairWithSocketToHost
  (_fun (_pointer = #f)
        _NSString
        _int32
        (in : (_ptr o _CFReadStreamRef))
        (out : (_ptr o _CFWriteStreamRef))
        -> _void
        -> (values (and in (retain in)) (and out (retain out)))))

(define-cf CFReadStreamScheduleWithRunLoop (_fun _CFReadStreamRef _CFRunLoopRef _pointer -> _void))
(define-cf CFWriteStreamScheduleWithRunLoop (_fun _CFWriteStreamRef _CFRunLoopRef _pointer -> _void))

(define-cf CFReadStreamOpen (_fun _CFReadStreamRef -> _Boolean))
(define-cf CFWriteStreamOpen (_fun _CFWriteStreamRef -> _Boolean))

(define-cf CFReadStreamClose (_fun _CFReadStreamRef -> _void))
(define-cf CFWriteStreamClose (_fun _CFWriteStreamRef -> _void))

(define-cf CFReadStreamHasBytesAvailable (_fun _CFReadStreamRef -> _Boolean))
(define-cf CFReadStreamRead (_fun _CFReadStreamRef _pointer _CFIndex -> _CFIndex))

(define-cf CFWriteStreamCanAcceptBytes (_fun _CFWriteStreamRef -> _Boolean))
(define-cf CFWriteStreamWrite (_fun _CFWriteStreamRef _pointer _CFIndex -> _CFIndex))

(define-cf kCFRunLoopDefaultMode _pointer)

(define-cf CFRunLoopStop (_fun _CFRunLoopRef -> _void))

(define-cstruct _CFStreamError ([domain _int]
                                [error _int32]))

(define-cf CFReadStreamGetError (_fun _CFReadStreamRef -> _CFStreamError))
(define-cf CFWriteStreamGetError (_fun _CFWriteStreamRef -> _CFStreamError))

(define-cf NSStreamSocketSecurityLevelNegotiatedSSL _pointer)
(define-cf NSStreamSocketSecurityLevelKey _pointer)

(define-net kCFStreamPropertySSLSettings _pointer)
(define-net kCFStreamSSLValidatesCertificateChain _pointer)
(define-net kCFStreamSSLLevel _pointer)

(define-cf kCFBooleanFalse _pointer)
(define-cf kCFBooleanTrue _pointer)

(define-net kCFStreamSocketSecurityLevelSSLv2 _pointer)
(define-net kCFStreamSocketSecurityLevelSSLv3 _pointer)
(define-net kCFStreamSocketSecurityLevelTLSv1 _pointer)
(define-net kCFStreamSocketSecurityLevelNegotiatedSSL _pointer)

(define-cf CFReadStreamSetProperty (_fun _CFReadStreamRef _pointer _pointer -> _Boolean))
(define-cf CFWriteStreamSetProperty (_fun _CFWriteStreamRef _pointer _pointer -> _Boolean))

(define-cstruct _CFStreamClientContext ([version _CFIndex]
                                        [info _pointer]
                                        [retain _pointer]
                                        [release _pointer]
                                        [copy _pointer]))

(define-cf CFReadStreamSetClient (_fun _CFReadStreamRef
                                       _int
                                       (_fun #:atomic? #t
                                             #:async-apply (lambda (f) (f))
                                             _CFReadStreamRef _int _pointer -> _void)
                                       _CFStreamClientContext-pointer
                                       -> _Boolean))
(define-cf CFWriteStreamSetClient (_fun _CFWriteStreamRef
                                        _int
                                        (_fun #:atomic? #t
                                              #:async-apply (lambda (f) (f))
                                              _CFWriteStreamRef _int _pointer -> _void)
                                        _CFStreamClientContext-pointer
                                        -> _Boolean))

(define kCFStreamEventNone 0)
(define kCFStreamEventOpenCompleted 1)
(define kCFStreamEventHasBytesAvailable 2)
(define kCFStreamEventCanAcceptBytes 4)
(define kCFStreamEventErrorOccurred 8)
(define kCFStreamEventEndEncountered 16)

(define all-evts (bitwise-ior
                  kCFStreamEventOpenCompleted
                  kCFStreamEventHasBytesAvailable
                  kCFStreamEventCanAcceptBytes
                  kCFStreamEventErrorOccurred
                  kCFStreamEventEndEncountered))

(define _CFStreamStatus
  (_enum '(kCFStreamStatusNotOpen
           kCFStreamStatusOpening
           kCFStreamStatusOpen
           kCFStreamStatusReading
           kCFStreamStatusWriting
           kCFStreamStatusAtEnd
           kCFStreamStatusClosed
           kCFStreamStatusError)))

(define-cf CFReadStreamGetStatus
  (_fun _CFReadStreamRef -> _CFStreamStatus))
(define-cf CFWriteStreamGetStatus
  (_fun _CFWriteStreamRef -> _CFStreamStatus))


(define-cf CFDictionaryCreate
  (_fun (_pointer = #f)
        (keys : (_list i _pointer))
        (vals : (_list i _pointer))
        (_CFIndex = (length keys))
        (_pointer = #f)
        (_pointer = #f)
        -> _CFDictionaryRef)
  #:wrap (allocator CFRelease))

;; ----------------------------------------

(define-cstruct _Scheme_Proc_Sequence ([num_procs _racket]
                                       [data _pointer]
                                       [proc1 _pointer]
                                       [proc2 (_fun #:atomic? #t #:async-apply (lambda (f) (f)) _pointer -> _pointer)]
                                       [proc3 _pointer]
                                       [proc4 (_fun #:atomic? #t #:async-apply (lambda (f) (f)) -> _pointer)])
  #:malloc-mode 'nonatomic)

(define-racket scheme_signal_received (_fun -> _void))

(define _pthread (_cpointer/null 'pthread))

(define-racket pthread_create
  (_fun (p : (_ptr o _pthread)) _pointer _pointer _pointer
        -> (r : _int)
        -> (and (zero? r)
                p)))
(define-racket pthread_detach
  (_fun _pointer -> _int))

(define-racket scheme_call_sequence_of_procedures-ptr _fpointer
  #:c-id scheme_call_sequence_of_procedures)
  
(define-cf CFRunLoopRun-ptr _fpointer
  #:c-id CFRunLoopRun)
(define-cf CFRunLoopGetCurrent-ptr _fpointer
  #:c-id CFRunLoopGetCurrent)

(define stop-and-release
  ((deallocator)
   (lambda (run-loop)
     (CFRunLoopStop run-loop)
     (CFReleaseRunLoop run-loop))))

(define-cf CFRetainRunLoop (_fun _CFRunLoopRef -> _CFRunLoopRef)
  #:c-id CFRetain
  #:wrap (allocator stop-and-release))
(define-cf CFReleaseRunLoop (_fun _pointer -> _void)
  #:c-id CFRelease)

(define (launch-run-loop-in-pthread init-reg more-retain)
  (define run-loop #f)
  (define done (make-semaphore))
  (define (setup r)
    ;; Called in atomic mode in arbitrary Racket thread:
    (set! run-loop (CFRetainRunLoop (cast r _pointer _CFRunLoopRef)))
    (init-reg run-loop)
    (semaphore-post done)
    (scheme_signal_received)
    #f)
  (define (finished)
    (free-immobile-cell retainer)
    #f)
  ;; Retains callbacks until the thread is done:
  (define retainer (malloc-immobile-cell
                    (vector setup finished more-retain)))
  (define seq (make-Scheme_Proc_Sequence 4
                                         #f
                                         CFRunLoopGetCurrent-ptr
                                         ;; `#:aync-apply` moves the following
                                         ;; back to the main thread (in atomic mode):
                                         setup
                                         CFRunLoopRun-ptr
                                         ;; `#:async-apply` here, too:
                                         finished))
  (define pth (pthread_create #f scheme_call_sequence_of_procedures-ptr seq))
  (unless pth (error "could not start run-loop thread"))
  (pthread_detach pth)
  
  (semaphore-wait done)
  (set! done seq) ; retains `seq` until here

  run-loop)

;; ----------------------------------------

(define (osx-ssl-connect host port [protocol 'auto])
  (define-syntax-rule (check-ok (op arg ...))
    (unless (op arg ...)
      (error 'op "failed")))
  
  (define-values (in out)
    (call-as-atomic
     (lambda ()
       (CFStreamCreatePairWithSocketToHost host port))))

  (check-ok (CFReadStreamSetProperty in
                                     NSStreamSocketSecurityLevelKey
                                     NSStreamSocketSecurityLevelNegotiatedSSL))
  (check-ok (CFWriteStreamSetProperty out
                                      NSStreamSocketSecurityLevelKey
                                      NSStreamSocketSecurityLevelNegotiatedSSL))
  
  (unless (eq? protocol 'secure)
    (define d (CFDictionaryCreate
               (list kCFStreamSSLValidatesCertificateChain
                     kCFStreamSSLLevel)
               (list kCFBooleanFalse
                     (case protocol
                       [(sslv2) kCFStreamSocketSecurityLevelSSLv2]
                       [(sslv3) kCFStreamSocketSecurityLevelSSLv3]
                       [(tls tls11 tls12) kCFStreamSocketSecurityLevelTLSv1]
                       [else kCFStreamSocketSecurityLevelNegotiatedSSL]))))
    (check-ok (CFReadStreamSetProperty in kCFStreamPropertySSLSettings d))
    (check-ok (CFWriteStreamSetProperty out kCFStreamPropertySSLSettings d))
    (CFRelease d))
  
  (define in-ready (make-semaphore))
  (define out-ready (make-semaphore 1))
  
  ;; These callback must be retained so that they're not GCed
  ;; until the run loop is terminated:
  (define in-callback (lambda (_in evt _null)
                        (void (semaphore-try-wait? in-ready))
                        (semaphore-post in-ready)
                        (scheme_signal_received)))
  (define out-callback (lambda (_out evt _null)
                         (void (semaphore-try-wait? out-ready))
                         (semaphore-post out-ready)
                         (scheme_signal_received)))

  (define context (make-CFStreamClientContext 0 #f #f #f #f))
  (check-ok (CFReadStreamSetClient in all-evts in-callback context))
  (check-ok (CFWriteStreamSetClient out all-evts out-callback context))

  (define run-loop
    (launch-run-loop-in-pthread
     ;; This function will be called as atomic within the scheduler:
     (lambda (run-loop)
       (CFReadStreamScheduleWithRunLoop in run-loop kCFRunLoopDefaultMode)
       (CFWriteStreamScheduleWithRunLoop out run-loop kCFRunLoopDefaultMode))
     (list in-callback out-callback)))
  
  (check-ok (CFWriteStreamOpen out))
  (check-ok (CFReadStreamOpen in))
  
  (let loop ()
    (when (or (eq? (CFReadStreamGetStatus in) 'kCFStreamStatusOpening)
              (eq? (CFWriteStreamGetStatus out) 'kCFStreamStatusOpening))
      (sync in-ready out-ready)
      (loop)))
  
  (when (or (eq? (CFReadStreamGetStatus in) 'kCFStreamStatusError)
            (eq? (CFWriteStreamGetStatus out) 'kCFStreamStatusError))
    (raise
     (exn:fail:network
      (~a "osx-ssl-connect: connection failed\n"
          "  address: " host "\n"
          "  port number: " port)
      (current-continuation-marks))))
  
  (define open-count 2)
  (define skip-close-out? #f)
  
  (define in-cust-reg (register-custodian-shutdown in (lambda (v) (close!))))
  (define out-cust-reg (register-custodian-shutdown out (lambda (v) (close!))))
  
  (define (close!)
    (call-as-atomic
     (lambda ()
       (set! open-count (sub1 open-count))
       (when (zero? open-count)
         (unregister-custodian-shutdown in in-cust-reg)
         (unregister-custodian-shutdown out out-cust-reg)
         (stop-and-release run-loop)
         (CFRelease in)
         (CFRelease out)))))

  (define-values (in-buffer-in in-buffer-out) (make-pipe))
  (define IN-BUFFER-SIZE 4096)
  (define in-buffer (make-bytes IN-BUFFER-SIZE))
    
  (define lock (make-semaphore 1))

  ;; Callbacks used below (written here so that they're allocated once):
  (define (lock-unavailable/read) (wrap-evt lock (lambda () 0)))
  (define (lock-unavailable/write) (wrap-evt lock (lambda () #f)))

  (define (read-in bstr)
    (define n (read-bytes-avail!* bstr in-buffer-in))
    (cond
     [(positive? n) n]
     [(zero? n)
      (void (semaphore-try-wait? in-ready))
      (cond
       [(CFReadStreamHasBytesAvailable in)
        (define use-bstr
          (if ((bytes-length bstr) . < . IN-BUFFER-SIZE)
              in-buffer
              bstr))
        (define n (CFReadStreamRead in use-bstr (bytes-length use-bstr)))
        (cond
         [(zero? n) eof]
         [(negative? n)
          (raise-osx-ssl-network-error 'read-bytes
                                       (CFReadStreamGetError in))]
         [else
          (cond
           [(eq? use-bstr in-buffer)
            (write-bytes in-buffer in-buffer-out 0 n)
            ;; Try again:
            0]
           [else n])])]
       [(equal? (CFReadStreamGetStatus in)
                'kCFStreamStatusError)
        (raise-osx-ssl-network-error 'read-bytes
                                     (CFReadStreamGetError in))]
       [else
        (wrap-evt (semaphore-peek-evt in-ready) (lambda (v) 0))])]))
  
  (define (write-out bstr start end buffer? breakable?)
    (cond
     [(= start end) 0]
     [else
      (void (semaphore-try-wait? out-ready))
      (cond
       [(CFWriteStreamCanAcceptBytes out)
        (let ([n (CFWriteStreamWrite out
                                     (if (zero? start)
                                         bstr
                                         (substring bstr start end))
                                     (- end start))])
          (cond
           [(zero? n)
            (wrap-evt always-evt (lambda (v) #f))]
           [(negative? n)
            (raise-osx-ssl-network-error 'write-bytes
                                         (CFWriteStreamGetError out))]
           [else n]))]
       [(equal? (CFWriteStreamGetStatus out)
                'kCFStreamStatusError)
        (raise-osx-ssl-network-error 'write-bytes
                                     (CFWriteStreamGetError out))]
       [else
        (wrap-evt (semaphore-peek-evt out-ready) (lambda (v) #f))])]))

  (values (make-input-port/read-to-peek
           'osx-ssl
           ;; read:
           (lambda (bstr)
             (call-with-semaphore
              lock
              read-in
              lock-unavailable/read
              bstr))
           ;; peek:
           (lambda (bstr offset slow)
             ;; Try fast peek on buffer port:
             (define n (peek-bytes-avail!* bstr offset #f in-buffer-in))
             (if (zero? n)
                 (slow bstr offset)
                 n))
           (lambda ()
             (call-with-semaphore
              lock
              (lambda ()
                (CFReadStreamClose in)
                (close!)))))
          
          (osx-ssl-output-port
           (make-output-port
            'osx-ssl
            (semaphore-peek-evt out-ready)
            ;; write
            (lambda (bstr start end non-block? enable-break?)
              (call-with-semaphore
               lock
               write-out                  
               lock-unavailable/write
               bstr start end non-block? enable-break?))
            ;; close
            (lambda ()
              (call-with-semaphore
               lock
               (lambda () 
                 (unless skip-close-out?
                   (CFWriteStreamClose out))
                 (close!)))))
           ;; abandon:
           (lambda (self)
             (set! skip-close-out? #t)
             (close-output-port self)))))

(struct osx-ssl-output-port (port abandon)
        #:property prop:output-port 0)

(define (osx-ssl-abandon-port p)
  (if (osx-ssl-output-port? p)
      ((osx-ssl-output-port-abandon p) p)
      (close-output-port p)))

(define (raise-osx-ssl-network-error who err)
  (raise
   (exn:fail:network
    (~a who ": failed " (CFStreamError->list err))
    (current-continuation-marks))))
