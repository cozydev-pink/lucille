//> using scala "2.13.10"
//> using lib "org.apache.lucene:lucene-core:9.5.0"
//> using lib "org.apache.lucene:lucene-analysis-common:9.5.0"
//> using lib "org.apache.lucene:lucene-memory:9.5.0"
//> using lib "org.apache.lucene:lucene-queryparser:9.5.0"

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.memory.MemoryIndex
import org.apache.lucene.queryparser.flexible.standard.StandardQueryParser


case class Book(title: String, author: String)
val defaultField = "title"

val books: List[Book] = List(
  Book("The Tale of Peter Rabbit", "Beatrix Potter"),
  Book("The Tale of Two Bad Mice", "Beatrix Potter"),
  Book("One Fish, Two Fish, Red Fish, Blue Fish", "Dr. Suess"),
  Book("Green Eggs and Ham", "Dr. Suess"),
)

val analyzer = new StandardAnalyzer()
val parser = new StandardQueryParser(analyzer)
val index = new MemoryIndex()

val q = "(Two Tale Fish)@2"
println(s"q: $q")

books.foreach { book =>
  index.addField("title", book.title, analyzer)
  index.addField("author", book.author, analyzer)
  val score = index.search(parser.parse(q, defaultField))
  index.reset()
  val res = f"$score%1.4f"
  println(s"""$res: "${book.title}" by ${book.author}""")
}

println("FIN\n")
