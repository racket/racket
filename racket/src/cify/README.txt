The "cify" compiler takes a linklet with no imports as produced by
"schemify" and compiles it to C code suitable for including as part of
the Racket virtual machine's implementation.

The schmeify compiler is lightly customized by a `for-cify?` flag to
make it produce output friendlier for cify. In cify mode, schemify
avoids obscuring structure-type creation, and it lifts all constants
to the top.

Compilation by cify is UNSAFE. For example:

 * cify assumes that `car` is applied to a pair, `vector-ref` is
   applied to a vector with an in-bounds argument, a struct selector
   is applied to an instance of the struct, and so on.

 * cify assumes that a variable is not referenced before its
   defintion.

 * cify assumes that a function defined within the linklet is called
   within the linklet with the right number of arguments.

The cify pass relies on the schemify pass lift functions to avoid
closure allocation whenevr possible. The schemify pass also performs
some basic inlining, constant propoagation, and compy propagation ---
but it's designed to defer significantly to the back end to perform
more of that. So, cify includes some copy propagation support,
especially for bindings that are outside of any `lambda` form.
(There's not much extra inling, so that's a direction for
improvement.)

When a function A tail-calls a known function B, cify generates a
single "vehicle" C function to hold A and B, so the tail cal can be
implemented as a `goto`. The same vehicle holds anything else that
tail-calls A and B or any other function that A or B tail-calls. That
strategy can generate a big function. For tail cals to unknown
functions or non-immediate primitives, cify uses the Racket virtual
machine's protocol for trampolined tail calls.

The output of cify uses the same Racket-value stack (internally called
"runstack") as the virtual machine's bytecode interpreter and JIT.
Along the same lines as the bytecode compiler, cify must determine the
liveness of a binding and clear the stack slot of any dead variable
before a potential GC. If a value's lifetime doesn't span a GC
boundary, then cify tries to keep it in a C variable (so the C
compiler can allocate it to a register, etc.). Most of cify's
complexity is to put variables in the right place and take advantage
of global runstack invariants to minimize value shuffling.
