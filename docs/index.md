## lucille

### Usage

This library is currently available for Scala binary versions 2.12, 2.13, and 3.2.

Additionally, it's available for the JVM, Scala.js, and Scala Native.

To use the latest version, include the following in your `build.sbt`:

```scala
// use this snippet for the JVM
libraryDependencies += "pink.cozydev" %% "lucille" % "@VERSION@"

// use this snippet for JS, Native, or cross-building
libraryDependencies += "pink.cozydev" %%% "lucille" % "@VERSION@"
```

### Parsing

Lucille offers a `parseQ` function to parse all of a string into a Lucille `MultiQuery` structure:

```scala mdoc
import pink.cozydev.lucille.Parser

Parser.parseQ("cats OR dogs")
```

### Last Query Rewriting

To enable a better interactive search experience, it can be helpful to rewrite the last term as a
prefix term to enable partial matching on terms.

We'll write a helper function `expandQ` to rewrite `Term` queries into a query that matches either
that term `OR` a `Prefix` query:

```scala mdoc:silent
import pink.cozydev.lucille.Query

def expandQ(q: Query): Query =
  q match {
    case Query.Term(t) => Query.Or(Query.Term(t), Query.Prefix(t))
    case _ => q
  }
```

We can now use `expandQ` along with `mapLast` to rewrite the last term of a `MultiQuery` into our
expanded term + prefix:

```scala mdoc
Parser.parseQ("cats do").map(mq => mq.mapLast(expandQ))
```

However, this might not work as expected when the last term is part of a boolean or field query.
For such use cases there is also `mapLastTerm` which will pass the function `f` down through boolean and field queries if they are in the last position.

```scala mdoc
Parser.parseQ("cats OR do").map(mq => mq.mapLastTerm(expandQ))
```