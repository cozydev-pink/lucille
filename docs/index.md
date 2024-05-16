## lucille

Lucille is a small library for parsing and representing queries using the [Lucene query syntax](https://lucene.apache.org/core/9_8_0/queryparser/org/apache/lucene/queryparser/flexible/standard/StandardQueryParser.html).


### Usage

This library is currently available for Scala binary versions 2.12, 2.13, and 3.

Additionally, it's available for the JVM, Scala.js, and Scala Native.

To use the latest version, include the following in your `build.sbt`:

```scala
// use this snippet for the JVM
libraryDependencies += "pink.cozydev" %% "lucille" % "@VERSION@"

// use this snippet for JS, Native, or cross-building
libraryDependencies += "pink.cozydev" %%% "lucille" % "@VERSION@"
```

### Parsing

Lucille offers a `parse` function to parse a whole string into a Lucille `MultiQuery` structure:

```scala mdoc
import pink.cozydev.lucille.QueryParser

QueryParser.parse("cats OR dogs")
```

### Printing

Lucille offers a `printer` to format `Query`s as Lucene query strings:

```scala mdoc
import pink.cozydev.lucille.Query
import pink.cozydev.lucille.QueryPrinter

QueryPrinter.print(Query.And(Query.Term("cats"), Query.Term("dogs")))
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

We can now use `expandQ` along with `mapLastTerm` to rewrite the last term of a `MultiQuery` into our
expanded term + prefix:

```scala mdoc
QueryParser.parse("cats meo").map(mq => mq.mapLastTerm(expandQ))
```

This also works when the last term is part of a boolean or field query.

```scala mdoc
QueryParser.parse("cats AND do").map(mq => mq.mapLastTerm(expandQ))
```
