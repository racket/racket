#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme
                     teachpack/htdp/convert))

@teachpack["convert"]{Converting Temperatures}

@declare-exporting[teachpack/htdp/convert]

The teachpack @scheme[convert.ss] provides three functions for
converting Fahrenheit temperatures to Celsius. It is useful for a single
exercise in HtDP. Its purpose is to demonstrate the independence of
``form'' (user interface) and ``function'' (also known as ``model''). 

@defproc[(convert-gui [convert (-> number? number?)]) true]{Consumes a
conversion function from Fahrenheit to Celsius and creates a graphical user
interface with two rulers, which users can use to convert temperatures
according to the given temperature conversion function.}

@defproc[(convert-repl [convert (-> number? number?)]) true]{Consumes a
conversion function from Fahrenheit to Celsius and then starts a
read-evaluate-print loop. The loop prompts users to enter a number and then
converts the number according to the given temperature conversion function.
A user can exit the loop by entering ``x.''}

@defproc[(convert-file [in string?][convert (-> number? number?)][out string?]) true]{Consumes a
file name @scheme[in], a
conversion function from Fahrenheit to Celsius, and a string
@scheme[out]. The program then reads all the number from
@scheme[in], converts them according to @scheme[convert], and prints the
results to the newly created file @scheme[out].

@bold{Warning}: If @scheme[out] already exists, it is deleted.}

Example: Create a file with name @scheme["in.dat"] with some numbers in
it, using your favorite text editor on your computer.  Define a function
@scheme[f2c] in the Definitions window and set teachpack to ``convert.ss''
and click RUN. Then evaluate 
@(begin
#reader scribble/comment-reader
(schemeblock
 (convert-gui f2c)
 ;; and 
 (convert-file "in.dat" f2c "out.dat")
 ;; and 
 (convert-repl f2c)
))
Finally inspect the file @scheme["out.dat"] and use the repl to check the
answers. 
