#lang scribble/doc
@(require scribble/manual
          (except-in "guide-utils.rkt" log-message)
          scribble/eval
          scriblib/figure
          racket/port
          racket/contract
          (for-label racket/place/distributed
                     racket/match
                     racket/place/define-remote-server))

@(define (codeblockfromfile filename)
   (call-with-input-file
     filename
     (lambda (i)
       (codeblock0 (port->string i)))))

@title[#:tag "distributed-places"]{Distributed Places}

The @racketmodname[racket/place/distributed] library provides support for
distributed programming.

The example bellow demonstrates how to launch a remote racket node instance,
launch remote places on the new remote node instance, and start an
event loop that monitors the remote node instance.

The example code can also be found in
@filepath{racket/distributed/examples/named/master.rkt}.



@figure["named-example-master" "examples/named/master.rkt"]{
@codeblockfromfile[(path->string (collection-file-path "master.rkt" "racket/place/distributed/examples/named"))]
}



The @racket[spawn-remote-racket-node] primitive connects to
@tt{"localhost"} and starts a racloud node there that listens on port
6344 for further instructions.  The handle to the new racloud node is
assigned to the @racket[remote-node] variable. Localhost is used so that
the example can be run using only a single machine.  However localhost
can be replaced by any host with ssh publickey access and racket.  The
@racket[supervise-named-dynamic-place-at] creates a new place on the
@racket[remote-node].  The new place will be identified in the future by
its name symbol @racket['tuple-server].  A place descriptor is
expected to be returned by invoking @racket[dynamic-place] with the
@racket[tuple-path] module path and the @racket['make-tuple-server]
symbol.

The code for the tuple-server place exists in the file
@filepath{tuple.rkt}.  The @filepath{tuple.rkt} file contains the use of
@racket[define-named-remote-server] form, which defines a RPC server
suitiable for invocation by @racket[supervise-named-dynamic-place-at].



@figure["named-example" "examples/named/tuple.rkt"]{
@codeblockfromfile[(path->string (collection-file-path "tuple.rkt" "racket/place/distributed/examples/named"))]
}



The @racket[define-named-remote-server] form takes an identifier and a
list of custom expressions as its arguments.  From the identifier a
place-thunk function is created by prepending the @tt{make-} prefix.
In this case @racket[make-tuple-server].  The
@racket[make-tuple-server] identifier is the
@racket[place-function-name] given to the
@racket[supervise-named-dynamic-place-at] form above. The
@racket[define-state] custom form translates into a simple
@racket[define] form, which is closed over by the @racket[define-rpc]
form.

The @racket[define-rpc] form is expanded into two parts. The first
part is the client stubs that call the rpc functions. The client
function name is formed by concatenating the
@racket[define-named-remote-server] identifier, @tt{tuple-server},
with the RPC function name @tt{set} to form @racket[tuple-server-set].
The RPC client functions take a destination argument which is a
@racket[remote-connection%] descriptor and then the RPC function
arguments. The RPC client function sends the RPC function name,
@racket[set], and the RPC arguments to the destination by calling an
internal function @racket[named-place-channel-put]. The RPC client
then calls @racket[named-place-channel-get] to wait for the RPC
response.

The second expansion part of @racket[define-rpc] is the server
implementation of the RPC call.  The server is implemented by a match
expression inside the @racket[make-tuple-server] function.  The match
clause for @racket[tuple-server-set] matches on messages beginning
with the @racket['set] symbol. The server executes the RPC call with
the communicated arguments and sends the result back to the RPC
client.

The @racket[define-cast] form is similar to the @racket[define-rpc] form
except there is no reply message from the server to client

@figure["define-named-remote-server-expansion" "Expansion of define-named-remote-server"]{
@codeblock0{
(module tuple racket/base
  (require racket/place
           racket/match)
  (define/provide
   (tuple-server-set dest k v)
   (named-place-channel-put dest (list 'set k v))
   (named-place-channel-get dest))
  (define/provide
   (tuple-server-get dest k)
   (named-place-channel-put dest (list 'get k))
   (named-place-channel-get dest))
  (define/provide
   (tuple-server-hello dest)
   (named-place-channel-put dest (list 'hello)))
  (define/provide
   (make-tuple-server ch)
    (let ()
      (define h (make-hash))
      (let loop ()
        (define msg (place-channel-get ch))
        (define (log-to-parent-real 
                  msg 
                  #:severity (severity 'info))
          (place-channel-put 
            ch 
            (log-message severity msg)))
        (syntax-parameterize
         ((log-to-parent (make-rename-transformer 
                           #'log-to-parent-real)))
         (match
          msg
          ((list (list 'set k v) src)
           (define result (let () (hash-set! h k v) v))
           (place-channel-put src result)
           (loop))
          ((list (list 'get k) src)
           (define result (let () (hash-ref h k #f)))
           (place-channel-put src result)
           (loop))
          ((list (list 'hello) src)
           (define result
             (let () 
               (printf "Hello from define-cast\n") 
               (flush-output)))
           (loop))))
        loop))))
}
}





