#lang racket/base
(require racket/class
         racket/snip
         "private/class-help.rkt")

(provide image%)

#|

This code is a bit strange in order to attempt to
preserve backwards compatibility with pre-5.1 versions.

The old version is:

  (define image%
    (class object%
      (init-accessible filename data w h dx dy)
      (super-new))))

The things I attempted to preserve:

 - image% as a class whose objects can be tested with is-a?

 - the get-* methods that init-accessible provides; with the exception
   of get-filename, which is now the image-snip% method, these are done
   still with init-accessible

   The get-filename method changed, tho: it now returns a path (it returned
   bytes before)

 - the constructor arity (there are now additional optional arguments that
   wxme supplies to be able to call super-make-object)

The main change is to make this file depend on racket/snip so that
image% can be a subclass of image-snip% and thus work with things like
the 2htdp/universe libraries (in executables)


|#

(define image%
  (class image-snip%
    (init filename)
    (init-accessible data w h dx dy)
    (init [relative 1] [type 'unknown])
    ;; the call to super-make-object is intended to mimic the way that racket/snip/private/snip.rkt
    ;; creates an image-snip% object in the image-snip-class% class's read method
    (let ([data (get-data)])
      (super-make-object
       (if data
           (let-values ([(in out) (make-pipe)])
             (thread (λ () (display data out) (close-output-port out)))
             in)
           (if (bytes? filename)
               (bytes->path filename)
               #f))
       (if data 'unknown/alpha type)
       (positive? relative) 
       (and data #t)))
    (inherit resize set-offset)
    (resize (get-w) (get-h))
    (set-offset (get-dx) (get-dy))))
