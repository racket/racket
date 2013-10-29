# Title

## 1. Section

This is a top-level section.

### 1.1. Subsection

This is a subsection.

#### 1.1.1. Subsubsection

This is a subsubsection.

Here is an itemize:

* Item 1.

* Item 2.

Here is a hyperlink:

[I am a hyperlink to Racket.](http://racket-lang.org/)

_Italic_. \_Just underlines\_.

**Bold**. \*Just asterisks.\*

“Dobule quoted”. ‘Single quoted’.

This should NOT be ‘code‘ in Markdown.

Example of vebatim:

`Hi, world.`  
`A “quote”.`  
`Second line.`
`Last line.`  

Another example of verbatim, with ticks/quotes:

`THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS`   
`“AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT`     
`LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR` 
`A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT`  
`HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,`
`SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT`      
`LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,` 
`DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY` 
`THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT`   
`(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE` 
`OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.`  

Example of a defmodule:

```racket
 (require racket/string)
```

Example of a defproc:

```racket
(make-string k [char]) -> string?
  k : exact-nonnegative-integer? 
  char : char? = #\nul           
```

Returns a new mutable string of length `k` where each position in the
string is initialized with the character `char`

Blah blah `(or/c string? bytes?)`.

Example of Scribble `examples`:

```racket
Examples:      
> (define x 0) 
               
> (displayln x)
0              
               
```

Example of Scribble `interaction`:

```racket
> (define x 0)
              
> x           
0             
```

> Note: This is a note. Let’s make it long enough that the markdown output
> will have to line-wrap, to make sure the > mark starts each line
> properly.
