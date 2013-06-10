#lang racket/base

#|
Here's the idea:

- Take a module's bytecode
- Recursively get all the bytecode for modules that the target requires
- After reading it, prune everything that isn't at phase 0 (the runtime phase)

- Now that we have all the modules, the next step is to merge them into a single
  module
-- Although actually we collapse them into the top-level, not a module
- To do that, we iterate through all the modules doing two things as we go:
-- Incrementing all the global variable references by all the references in all
   the modules
--- So if A has 5, then B's start at index 5 and so on
-- Replacing module variable references with the actual global variables
   corresponding to those variables
--- So if A's variable 'x' is in global slot 4, then if B refers to it, it 
    directly uses slot 4, rather than a module-variable slot

- At that point we have all the module code in a single top-level, but many
  toplevels won't be used because a library function isn't really used
- So, we do a "garbage collection" on elements of the prefix
- First, we create a dependency graph of all toplevels and the initial scope
- Then, we do a DFS on the initial scope and keep all those toplevels, throwing
  away the construction of everything else
  [XXX: This may be broken because of side-effects.]

- Now we have a small amount code, but because we want to go back to source,
  we need to fix it up a bit; because different modules may've used the same
  names
- So, we do alpha-renaming, but it's easy because names are only used in the
  compilation-top prefix structure

[TODO]

- Next, we decompile
- Then, it will pay to do dead code elimination and inlining, etc.
|#

(require racket/cmdline
         racket/set
         raco/command-name
         "main.rkt")


(let ([output-file (make-parameter #f)])
  (command-line #:program (short-program+command-name)
                #:multi
                [("-e" "--exclude-modules") path "Exclude <path> from flattening"
                 (current-excluded-modules (set-add (current-excluded-modules) path))]
                #:once-each
                [("-o") dest-filename "Write output as <dest-filename>"
                 (output-file (string->path dest-filename))]
                [("-g" "--garbage-collect") "Garbage-collect final module (unsound)"
                 (garbage-collect-toplevels-enabled #t)]                         
                #:args (filename) 
                (demodularize filename (output-file))))
