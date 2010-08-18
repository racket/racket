#lang at-exp s-exp "shared.rkt"

(define name i)

(define (url str) (tt (a href: str str)))

(define styles
  @style/inline{
    .nestedheading {
      text-align: center;
      font-weight: bold;
      font-size: large;
    }
    .nested {
      width: 80%;
      margin-left: auto;
      margin-right: auto;
      margin-top: 1em;
      margin-bottom: 1em;
    }
    .faqsection {
      margin-left: auto;
      margin-right: auto;
      margin-top: 2em;
      width: 90%;
    }
    .faqques {
      font-weight: bold;
      margin-top: 1em;
      margin-bottom: 1em;
    }
    .faqans {
      margin-left: 1em;
    }})

(define ((FAQ tag . ques) . answer)
  @div[class: 'faqentry]{
    @div[class: 'faqques]{@a[name: tag]{@ques}}
    @div[class: 'faqans]{@answer}})

(define (heading . text)
  (apply div class: 'nestedheading text))
(define (nested . text)
  (apply div class: 'nested text))

(define guidelines
  @page[#:title "Coding Guidelines"]{
    @styles

    @heading{Coding Guidelines}

    @nested{Our philosopher-in-chief has written, "[Programming] is about making sure that
            you know how and why your code works, and that you and your readers will so 
            in the future." Every revision to the Racket code base should be consistent
            with this epigram by providing:
    
            @ul{ @li{external documentation for users,}
                 @li{internal documentation for maintainers,}
                 @li{automated tests, and}
                 @li{automated stress tests.}}}
    
    @nested{Code that is not externally documented is not usable.}
    @nested{Code that is not internally documented is not maintainable.}
    @nested{Code that is not tested is not trustworthy.}
    @nested{Code that is not stressed is not reliable.}

    @div[class: 'faqsection]{
                             
      @@FAQ['perfection]{This standard of perfection is too much.}{
      @p*{As long as your code fulfills its promises and meets each of these
          criteria, you can push incrementally.
          @~
          For example, I may be working on adding an exhaustive queue library to
          Racket. I imagine supporting 30 different functions across 5 different
          queue implementations. I don't have to wait to push until all 150 function
          implementations are documented, tested, and stressed. Instead, I can push
          whenever I provide documentation and tests for a new set of functions.
          @~
          It is better to make a little bit of progress on each of functionality, tests,
          and documentation than lots of functionality with no tests or documentation.}}
                             
      @heading{Documentation}
      
      @@FAQ['docs]{What is the proper documentation style?}{
      @p*{
        First, refer to the @a[href: "http://docs.racket-lang.org/scribble/how-to-doc.html#%28part._reference-style%29"]{style guide}
        in the Scribble manual.
        @~
        Next, consider splitting your manual into a "Guide" section, that suggests
        use cases and explains the purpose of the code, and a traditional
        "Reference" section that presents the minutae. You may also consider
        adding examples to each function in your "Reference" section.
        @~
        Finally, ensure you have all the correct @code{for-label} @code{require}s and
        make use of other useful cross-references.}}
                             
      @heading{Tests}
      
      @@FAQ['bugs]{When fixing a mistake...}{
      @p*{
        First, write an automated test that exposes the mistake in the existing
        implementation. Put this in the software's test suite so it will never
        be accidentally introduced again.
        @~
        Second, modify the code to fix the mistake. Do this second to be sure
        you didn't introduce a mistake in your tests; it is all too easy to
        think you've fixed a mistake when in reality your new test just doesn't
        properly reveal the old mistake.
        @~
        Last, re-run the test suite to ensure that the mistake is fixed and no
        existing tests fail.}}
       
      @@FAQ['oldcode]{When changing code without an existing test suite...}{
       @p*{Your first task when changing old code is to build an adequate test suite
           to ensure you do not introduce new mistakes as you attempt to improve it.
           Thank you for improving the world for future generations!}}
      
      @heading{Stress Tests}
      
      @@FAQ['stress]{What is the difference between a normal test and a stress test?}{
      @p*{
        A test case ensures that functions fulfill their purpose and have the correct error
        behavior. A stress test exposes your function's performance and complexity.
        @~
        For example, a test case for a queue library ensures that it deals correctly with
        enqueue and dequeue using small queues (say 3 or 4) and tests the corner cases. In
        contrast, a stress test for the same library would run the queue operations on a variety
        of queue sizes, including very large queues.
        @~
        Stress tests don't normally have an expected output, so they never "pass". The practice of
        writing stress tests exposes implementation flaws or provides comparative data to be used
        when choosing between two APIs. Just writing them and keeping them around reminds us that
        things can go bad and we can detect when performance degrades through some other door.}}
      
      @@FAQ['how-to-stress]{How can I easily construct stess tests?}{
       @p*{
         Refer to the @code{tests/stress/stress} library.
         @~
         It contains a macro for running experiments that should behave the same but
         may have different performance. After running the experiments in isolation
         (through custodians and periodic garbage collection), it reports sorted
         timing information for comparative purposes.
         @~
         It also contains a function for running a function on larger and larger
         numbers and computes their relative timings.}}
      
      }})
