#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]
@require["contracts-utils.ss"]
@(require (for-label scheme/contract))

<section title="Gotchas" tag="gotchas" />

<question> What about @scheme[set!] on variables provided via @scheme[provide/contract]?
</question>

<p>
The contract library assumes that variables exported
via @scheme[provide/contract] are not assigned to, but
does not enforce it. Accordingly, if you try
to @scheme[set!] those variables, you may find
unexpected behavior. As an example, consider this program:

<scheme>
(module x mzscheme
  (require (lib "contract.ss"))
  (define (inc-x!) (set! x (+ x 1)))
  (define x 0)
  (provide/contract [inc-x! (-> void?)]
                    [x integer?]))

(module client mzscheme
  (require x)
  (define (print-latest) (printf "x is ~s\n" x))

  (print-latest)
  (inc-x!)
  (print-latest))

(require client)
</scheme>

When it runs, both calls to @scheme[print-latest]
print @scheme[0], even though the value
of @scheme[x] has been incremented (and the change is
visible inside the module @scheme[x]).
</p>

<p>
To work around this, export accessor functions, rather than
exporting the function directly, like this:
<scheme>
(module x mzscheme
  (require (lib "contract.ss"))
  (define (get-x) x)
  (define (inc-x!) (set! x (+ x 1)))
  (define x 0)
  (provide/contract [inc-x! (-> void?)]
                    [get-x (-> integer?)]))

</scheme>
</p>

<p>
This is a bug we hope to address in a future release.
</p>
