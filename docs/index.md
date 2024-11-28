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

Lucille offers a `parse` function to parse a whole string into a Lucille `Query` structure:

```scala mdoc
import pink.cozydev.lucille.QueryParser

QueryParser.default.parse("cats OR dogs")
```

The default `QueryParser` automatically inserts an `OR` operation inbetween consecutive terms.

```scala mdoc
QueryParser.withDefaultOperatorOR.parse("cats dogs")
```

This can be changed to an `AND` operation via `withDefaultOperatorAND`:

```scala mdoc
QueryParser.withDefaultOperatorAND.parse("cats dogs")
```

### Printing

Lucille offers a `printer` to format `Query`s as Lucene query strings:

```scala mdoc
import pink.cozydev.lucille.Query
import pink.cozydev.lucille.QueryPrinter

QueryPrinter.print(Query.And(Query.Term("cats"), Query.Term("dogs")))
```

Because the numeric value of a query boost parameter is modelled as a `Float`, the query printer
has a `precision` parameter it uses to round the boost parameter for pretty printing:

```scala mdoc
val queryWithBoost = Query.Boost(Query.Phrase("apple pi"), 3.14159265f)

// the default precision is 2
QueryPrinter.print(queryWithBoost)

QueryPrinter.print(queryWithBoost, precision=5)
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

We can now use `expandQ` along with `mapLastTerm` to rewrite the last term of a `Query` into our
expanded term + prefix:

```scala mdoc
QueryParser.parse("cats meo").map(mq => mq.mapLastTerm(expandQ))
```

This also works when the last term is part of a boolean or field query.

```scala mdoc
QueryParser.parse("cats AND do").map(mq => mq.mapLastTerm(expandQ))
```

### Associativity

Queries may contain a mix of AND/OR operators, e.g. `cats AND dogs OR fish`.
It is best to add parenthesis to help indicate your intent, either `(cats AND dogs) OR fish` or `cats AND (dogs OR fish)`, as both of these queries could evaluate differently.
In the absence of clarifying parenthesis, Lucille parses according to the precedence of the boolean operators.
The highest and most immediately binding operator is `NOT`, then `AND` and finally `OR`.

Consider the following examples:

```
NOT a AND b         ->  (NOT A) AND b
a AND NOT b         ->  A AND (NOT b)
a AND b OR x        ->  (a AND b) OR x
a AND b OR x AND y  ->  (a AND b) OR (x AND y)
a AND b AND c OR x  ->  (a AND b AND c) OR x
```

It's worth noting that the last example could equivalently be written as `((a AND b) AND c) OR x` or `(a AND (b AND c)) OR x`.
However, Lucille parses sequences of repeated operators into a single `Query.And` or `Query.Or` node to avoid unnecessary nesting.
