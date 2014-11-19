<p><strong>Project Description</strong><br />Unified collection of Monads (M, unit, *) implemented in the Microsoft F# Language.<br /><br />In functional programming, a monad is a kind of abstract data type used to represent computations (instead of data in the domain model). Monads allow the programmer to chain actions together to build a pipeline, in which each action is decorated with additional processing rules provided by the monad. Programs written in functional style can make use of monads to structure procedures that include sequenced operations, or to define arbitrary control flows (like handling concurrency, continuations, or exceptions).<br />Formally, a monad is constructed by defining two operations (bind and return) and a type constructor M that must fulfill several properties to allow the correct composition of monadic functions (i.e. functions that use values from the monad as their arguments). The return operation puts a value from a plain type into a monadic container of type M. The bind operation performs the reverse process, extracting the original value from the container and passing it to the associated next function in the pipeline. <br />(From Wikipedia. <a href="http://en.wikipedia.org/wiki/Monad_(functional_programming)">http://en.wikipedia.org/wiki/Monad_(functional_programming)</a>)<br /><br /><strong>Monads Catalog:</strong><br />Done:</p>
<ul>
<li>Identity</li>
<li>Maybe</li>
<li>State</li>
<li>List</li>
<li>Error</li>
<li>Continuation</li>
</ul>
<p><br />In Progress:</p>
<ul>
<li>Parser</li>
</ul>
<p><br />To do:</p>
<ul>
<li>IO</li>
<li>....</li>
</ul>
<p><br />The monads are implemented in F#, but they can be used from both C# (LINQ) and F# (Computation expression) <br /><br />References:<br /><a href="http://www.disi.unige.it/person/MoggiE/ftp/abs-view.pdf">An Abstract View of Programming Languages</a> by Eugenio Moggi<br /><br /><a href="http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf">Monads for functional programming</a> Philip Wadler<br /><a href="http://www.amazon.co.uk/gp/product/3540594515?ie=UTF8&amp;tag=httpfsharpcbl-21&amp;linkCode=as2&amp;camp=1634&amp;creative=6738&amp;creativeASIN=3540594515">Merging Monads and Folds for Functional Programming</a> by Erik Meijer , Johan Jeuring<br /><a href="http://coblitz.codeen.org:3125/citeseer.ist.psu.edu/cache/papers/cs/4583/http:zSzzSzcm.bell-labs.comzSzwhozSzwadlerzSztopicszSz..zSzpaperszSzessencezSzessence.pdf/wadler92essence.pdf">The essence of functional programming</a><br /><a href="http://www.inf.uni-konstanz.de/dbis/publications/download/monad-comprehensions.pdf">Monad Comprehensions: A Versatile Representation for Queries</a> Torsten Grust<br /><a href="http://www.amazon.co.uk/Real-World-Haskell-Code-Believe/dp/0596514980?&amp;camp=2486&amp;linkCode=wey&amp;tag=httpfsharpcbl-21&amp;creative=20370">Real World Haskell: Code You Can Believe In (Paperback)</a><br /><br /><a href="http://www.cs.nott.ac.uk/~gmh/pearl.pdf">Monadic Parsing in Haskell</a> by Graham Hutton and Erik Meijer<br /><a href="http://www.cs.nott.ac.uk/~gmh/monparsing.pdf">Monadic Parser Combinators</a> by Graham Hutton and Erik Meijer<br /><br /><strong>Any help on this project would be really appreciated since it is a potentially tricky one to get right.</strong></p>
