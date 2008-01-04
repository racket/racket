NOTE: this section needs the files in contract-examples/*ss
to be added to it (presumably programmatically).

<section title="Examples" tag="text-examples" />

<blockquote>
<p>
 This section illustrates the current state of PLT Scheme's contract
 implementation with a series of examples from Mitchell and McKim's text
 book "Design by Contract, by Example" [Addison and Wesley, 2002]. 
</p>
</blockquote>

<question title="How do I design data structures with contracts?" tag="0">

<p>Mitchell and McKim's principles for design by contract DbC are derived
  from the 1970s style algebraic specifications. The overall goal of DbC is
  to specify the constructors of an algebra in terms of its
  observers. While we reformulate Mitchell and McKim's terminology, we
  retain their terminology of "classes" and "objects" even though we
  reformulate their examples in an applicative style (as well as an
  imperative one):
</p>

<ol>
<li><b>Separate queries from commands.</b>

    <p>A <em>query</em> returns a result but does not change the observable
    properties of an object. A <em>command</em> changes the visible
    properties of an object, but does not return a result. In applicative
    implementation a command typically returns an new object of the same
    class.</p>
</li>

<li><b>Separate basic queries from derived queries.</b>

    <p>A <em>derived query</em> returns a result that is computable in
    terms of basic queries.</p>
</li>

<li><b>For each derived query, write a post-condition contract that
    specifies the result in terms of the basic queries.</b>

    <p />
</li>

<li><b>For each command, write a post-condition contract that specifies the
    changes to the observable properties in terms of the basic queries.</b>

    <p />
</li>

<li><b>For each query and command, decide on suitable pre-condition contract.</b>

    <p />
</li>
</ol>
</question>

<blockquote>
<p>Each of the followong questions (sections) corresponds to a chapter in
 Mitchell and McKim's book (but not all chapters show up here). The
 contracts have a colored background and appear in <font
 color="purple">font</font> so that they are easy to find. Implementations
 are presented ahead of the interface in <font color="navy">navy.</font>
 Finally, tests are included in <font "a0a0aa">a light shade.</font> We
 recommend that you read the contracts first, then the implementation, and
 then the test module.
</p>

<p>
 Mitchell and McKim use Eiffel as the underlying programming language and
 employ a conventional imperative programming style. Our long-term goal is
 to transliterate their examples into applicative Scheme,
 structure-oriented imperative Scheme, and PLT Scheme's class system (when
 contracts for those become available). </p>

<p>Note: To mimic Mitchell and McKim's informal notion of parametericity
 (parametric polymorphism), we use first-class contracts. At several
 places, this use of first-class contracts improves on Mitchell and McKim's
 design (see comments in interfaces).
</p>

</blockquote>
