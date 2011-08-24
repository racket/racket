To run the tests using the model:
---------------------------------

 1. Open "test.rkt" in DrRacket

 2. Change DrRacket's current language to "Use the language declared
    in the source"

 3. Click "Run"

Afterward, in the REPL window that shows whether the tests passed, you
can try your own expressions using the `show' function. The program
`call/cc-loop' is helpfully defined, so that you can try

 (show call/cc-loop)


To read the program:
--------------------

After a quick look at "grammar.rkt", read "reduce.rkt". If you become
interested in a metafunction, see "meta.rkt"
