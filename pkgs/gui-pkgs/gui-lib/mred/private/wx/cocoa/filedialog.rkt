#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         racket/path
         "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "queue.rkt"
         "frame.rkt")

(provide (protect-out file-selector))

(import-class NSOpenPanel NSSavePanel NSURL NSArray)

(define (nsurl->string url)
  (string->path (tell #:type _NSString url path)))

(define (file-selector message directory filename 
                       extension
                       filters style parent)
  (promote-to-gui!)
  (let ([ns (as-objc-allocation-with-retain
             (if (memq 'put style)
                (tell NSSavePanel savePanel)
                (tell NSOpenPanel openPanel)))]
        [parent (and parent
                     (not (send parent get-sheet))
                     parent)])

    (let* ([globs (apply append
                         (map (lambda (f) (regexp-split #rx" *; *" (cadr f)))
                              filters))]
           ;; get suffixes from "*.foo" globs (and *only* such globs)
           [extensions
            (for/list ([g (in-list globs)]
                       #:when (and (regexp-match #rx"[*][.][^.]+$" g)
                                   (not (equal? g "*.*"))))
              (car (regexp-match #rx"[^.]+$" g)))]
           [extensions
            (if (memq 'packages style) (cons "app" extensions) extensions)]
           [extensions
            (if (and extension (not (equal? "" extension)))
              (cons extension extensions) extensions)])
      (unless (null? extensions)
        (when (memq 'put style)
          (tellv ns setCanSelectHiddenExtension: #:type _BOOL #t))
        (let ([allow-any? (member "*.*" globs)])
          (when (or (not allow-any?)
                    (memq 'put style))
            (let ([a (tell NSArray
                           arrayWithObjects: #:type (_list i _NSString) extensions
                           count: #:type _NSUInteger (length extensions))])
              (tellv ns setAllowedFileTypes: a))
            (tellv ns setAllowsOtherFileTypes: #:type _BOOL allow-any?)))))

    (cond
     [(memq 'multi style)
      (tellv ns setAllowsMultipleSelection: #:type _BOOL #t)]
     [(memq 'dir style)
      (tellv ns setCanChooseDirectories: #:type _BOOL #t)
      (tellv ns setCanChooseFiles: #:type _BOOL #f)])

    (when (or (memq 'put style)
              (memq 'dir style))
      (tellv ns setCanCreateDirectories: #:type _BOOL #t))

    (when message
      (tellv ns setMessage: #:type _NSString message))
    (when directory
      (let ([dir (if (string? directory)
                     directory
                     (path->string directory))])
        (if (version-10.6-or-later?)
            (tellv ns setDirectoryURL: (tell NSURL 
                                             fileURLWithPath: #:type _NSString dir
                                             isDirectory: #:type _BOOL #t))
            (tellv ns setDirectory: #:type _NSString dir))))
    (when filename
      (when (version-10.6-or-later?)
        (tellv ns setNameFieldStringValue: #:type _NSString (path->string
                                                             (file-name-from-path filename)))))
    
    (when (memq 'enter-packages style)
      (tellv ns setTreatsFilePackagesAsDirectories: #:type _BOOL #t))

    (let ([result 
           ;; We run the file dialog completely modally --- shutting out
           ;; all other eventspaces and threads. It would be nice to improve
           ;; on this, but it's good enough.
           (atomically
            (let ([front (get-front)]
                  [parent (and (version-10.6-or-later?)
                               parent)])
              (when parent
                (tellv ns beginSheetModalForWindow: (send parent get-cocoa-window)
                       completionHandler: #f))
              (begin0
               (tell #:type _NSInteger ns runModal)
               (when parent (tell app endSheet: ns))
               (when front (tellv (send front get-cocoa-window)
                                  makeKeyAndOrderFront: #f)))))])
      (begin0
       (if (zero? result)
           #f
           (if (memq 'multi style)
               (let ([urls (tell ns URLs)])
                 (for/list ([i (in-range (tell #:type _NSUInteger urls count))])
                   (nsurl->string (tell urls objectAtIndex: #:type _NSUInteger i))))
               (let ([url (tell ns URL)])
                 (nsurl->string url))))
       (release ns)))))

