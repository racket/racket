#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class
         "pool.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "../common/queue.rkt"
         "../../lock.rkt"
         "../common/freeze.rkt")
(unsafe!)
(objc-unsafe!)

(provide app
         cocoa-start-event-pump
         cocoa-install-event-wakeup
         queue-event
         set-eventspace-hook!
         set-front-hook!
         set-menu-bar-hooks!
         post-dummy-event

         ;; from common/queue:
         current-eventspace
         queue-event
         yield)

(import-class NSApplication NSAutoreleasePool)
(import-protocol NSApplicationDelegate)

(define app (tell NSApplication sharedApplication))

(define-objc-class MyApplicationDelegate NSObject #:protocols (NSApplicationDelegate)
  []
  [-a _BOOL (applicationShouldTerminate: [_id app])
      (queue-quit-event)
      #f])

(tellv app finishLaunching)

(define app-delegate (tell (tell MyApplicationDelegate alloc) init))
(tellv app setDelegate: app-delegate)
(tellv app activateIgnoringOtherApps: #:type _BOOL #t)

#|
(import-class NSNotificationCenter)
(define-cocoa NSMenuDidBeginTrackingNotification _id)
(tellv (tell NSNotificationCenter defaultCenter)
       addObserver: app-delegate
       selector: #:type _SEL (selector trackingMenuNow:)
       name: NSMenuDidBeginTrackingNotification
       object: #f)
|#

;; ------------------------------------------------------------
;; Create an event to post when MzScheme has been sleeping but is
;;  ready to wake up

(import-class NSEvent)
(define NSApplicationDefined 15)
(define wake-evt
  (tell NSEvent 
        otherEventWithType: #:type _int NSApplicationDefined
        location: #:type _NSPoint-pointer (make-NSPoint 0.0 0.0)
        modifierFlags: #:type _NSUInteger 0 
        timestamp: #:type _double 0.0
        windowNumber: #:type _NSUInteger 0
        context: #:type _pointer #f
        subtype: #:type _short 0
        data1: #:type _NSInteger 0
        data2: #:type _NSInteger 0))
(define (post-dummy-event)
  (tell #:type _void app postEvent: wake-evt atStart: #:type _BOOL YES))

;; This callback will be invoked by the CoreFoundation run loop
;; when data is available on `ready_sock', which is used to indicate
;; that MzScheme would like to wake up (and posting a Cocoa event
;; causes the event-getting function to unblock).
(define (socket_callback)
  (read2 ready_sock read-buf 1)
  (post-dummy-event))

;; ------------------------------------------------------------
;; Create a pipe's pair of file descriptors, used to communicate
;; from the MzScheme-sleep thread to the CoreFoundation run loop.

(define pipe2 (get-ffi-obj 'pipe #f (_fun _pointer -> _int)))
(define write2 (get-ffi-obj 'write #f (_fun _int _pointer _long -> _long)))
(define read2 (get-ffi-obj 'read #f (_fun _int _pointer _long -> _long)))
(define read-buf (make-bytes 1))
(define-values (ready_sock write_sock)
  (let ([s (malloc 'raw 2 _int)])
    (unless (zero? (pipe2 s))
      (error "pipe didn't create fds"))
    (let ([r (ptr-ref s _int 0)]
          [w (ptr-ref s _int 1)])
      (free s)
      (values r w))))

;; ------------------------------------------------------------
;; Register the event-posting callback on `ready_sock' with
;; the CoreFoundation run loop

(define _CFIndex _uint)
(define _CFStringRef _NSString)
(define-cstruct _CFSocketContext ([version _CFIndex]
                                  [info _pointer]
                                  [retain (_fun _pointer -> _pointer)]
                                  [release (_fun _pointer -> _void)]
                                  [copyDescription (_fun _pointer -> _CFStringRef)]))
(define (sock_retain v) #f)
(define (sock_release v) (void))
(define (sock_copy_desc v) "sock")
(define sock-context (make-CFSocketContext 0 #f sock_retain sock_release sock_copy_desc))

(define _CFRunLoopRef _pointer)
(define _CFAllocatorRef _pointer)
(define _CFSocketRef _pointer)
(define _CFRunLoopSourceRef _pointer)
(define _CFSocketNativeHandle _int)
(define _CFOptionFlags _uint)
(define _CFSocketCallBack (_fun -> _void))
(define-cf CFAllocatorGetDefault (_fun -> _pointer))
(define-cf CFSocketCreateWithNative (_fun _CFAllocatorRef
                                          _CFSocketNativeHandle
                                          _CFOptionFlags
                                          _CFSocketCallBack
                                          _CFSocketContext-pointer
                                          -> _CFSocketRef))
(define-cf CFSocketCreateRunLoopSource (_fun _CFAllocatorRef
                                             _CFSocketRef
                                             _CFIndex
                                             -> _CFRunLoopSourceRef))
(define-cf CFRunLoopAddSource (_fun _CFRunLoopRef
                                    _CFRunLoopSourceRef
                                    _CFStringRef
                                    -> _void))
(define-cf kCFRunLoopDefaultMode _CFStringRef)

(define kCFSocketReadCallBack 1)

(import-class NSRunLoop)
(let* ([rl (tell #:type _CFRunLoopRef (tell NSRunLoop currentRunLoop) getCFRunLoop)]
       [cfs (CFSocketCreateWithNative (CFAllocatorGetDefault) ready_sock kCFSocketReadCallBack
                                      socket_callback sock-context)]
       [source (CFSocketCreateRunLoopSource (CFAllocatorGetDefault) cfs 0)])
  (CFRunLoopAddSource rl source kCFRunLoopDefaultMode))

;; ------------------------------------------------------------
;; Cocoa event pump

(define-cocoa NSDefaultRunLoopMode _id) ; more specifically an _NSString, but we don't need a conversion

(import-class NSDate)
(define distantFuture (tell NSDate distantFuture))

(define eventspace-hook (lambda (e) #f))
(define (set-eventspace-hook! proc) (set! eventspace-hook proc))

(define front-hook (lambda () (values #f #f)))
(define (set-front-hook! proc) (set! front-hook proc))

(define in-menu-bar-range? (lambda (p) #f))
(define suspend-menu-bar (lambda (suspend?) (void)))
(define (set-menu-bar-hooks! r? s) 
  (set! in-menu-bar-range? r?)
  (set! suspend-menu-bar s))

(define events-suspended? #f)

(define (check-menu-bar-click evt)
  (when (and evt 
             (= 14 (tell #:type _NSUInteger evt type))
             (= 7 (tell #:type _short evt subtype))
             (not (tell evt window))
             (in-menu-bar-range? (tell #:type _NSPoint evt locationInWindow)))
    ;; Mouse down in the menu bar:
    (let-values ([(f e) (front-hook)])
      (when e
        ;; Don't handle further events until we've made an effort
        ;; at on-demand notifications.
        (set! events-suspended? #t)
        (let ([t (thread (lambda ()
                           (sleep 2)
                           ;; on-demand took too long, so disable the menu bar
                           ;; until the application can catch up
                           (suspend-menu-bar #t)
                           (set! events-suspended? #f)))])
          (queue-event e (lambda ()
                           (send f on-menu-click)
                           (set! events-suspended? #f)
                           (kill-thread t))))))))

;; Call this function only in atomic mode:
(define (check-one-event wait? dequeue?)
  (pre-event-sync wait?)
  (let ([pool (tell (tell NSAutoreleasePool alloc) init)])
    (when (and events-suspended? wait?)
      (suspend-menu-bar #t)
      (set! events-suspended? #f))
    (begin0
     (let ([evt (if events-suspended?
                    #f
                    (tell app nextEventMatchingMask: #:type _NSUInteger NSAnyEventMask
                          untilDate: (if wait? distantFuture #f)
                          inMode: NSDefaultRunLoopMode
                          dequeue: #:type _BOOL dequeue?))])
       (when evt (check-menu-bar-click evt))
       (and evt
            (or (not dequeue?)
                (let ([e (eventspace-hook (tell evt window))])
                  (if e
                      (begin
                        (retain evt)
                        (queue-event e (lambda () 
                                         (call-as-unfreeze-point
                                          (lambda ()
                                            (tellv app sendEvent: evt)
                                            (release evt))))))
                      (tellv app sendEvent: evt)))
                #t)))
     (tellv pool release))))

;; Call this function only in atomic mode:
(define (dispatch-all-ready)
  (when (check-one-event #f #t)
    (dispatch-all-ready)))

(define (cocoa-start-event-pump)
  (thread (lambda ()
            (let loop ()
              (sync queue-evt)
              (as-entry dispatch-all-ready)
              (loop)))))

(set-check-queue!
 ;; Called through an atomic callback:
 (lambda () (check-one-event #f #f)))

;; ------------------------------------------------------------
;; Install an alternate "sleep" function (in the PLT Scheme core)
;; that wakes up if any Cocoa event is ready.
  
(define-mz scheme_start_sleeper_thread (_fun _fpointer _float _pointer _int -> _void))
(define-mz scheme_end_sleeper_thread (_fun -> _void))

(define-mz scheme_sleep _pointer)

;; Called through an atomic callback:
(define (sleep-until-event secs fds)
  (scheme_start_sleeper_thread scheme_sleep secs fds write_sock)
  (check-one-event #t #f) ; blocks until an event is ready
  (scheme_end_sleeper_thread))

(define (cocoa-install-event-wakeup)
  (post-dummy-event) ; why do we need this? 'nextEventMatchingMask:' seems to hang if we don't use it
  (set-ffi-obj! 'scheme_sleep #f _pointer (function-ptr sleep-until-event 
                                                        (_fun #:atomic? #t 
                                                              _float _pointer -> _void))))
