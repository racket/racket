#lang scribble/doc

@(require scribble/manual
          (for-label racket/base racket/contract racket/string))

@defmodule[racket/string]

@itemize[
@item{Item 1.}
@item{Item 2.}
]

@section{Section}

@hyperlink["http://www.racket-lang.org/" "I am a hyperlink to Racket."]

@italic{Italic}.
_Just underlines_.

@bold{Bold}.
*Just asterisks.*

``Dobule quoted''.
`Single quoted'.

@verbatim{
Hi, world.
A ``quote''.
Second line.
Last line.
}

The end.

@verbatim{
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

@defproc[(make-string [k exact-nonnegative-integer?][char char? #\nul]) string?]{

Returns a new mutable string of length @racket[k] where each position in the
string is initialized with the character @racket[char]

}
