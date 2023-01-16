
val fib: LazyList[BigInt] =
  BigInt(0) #:: BigInt(1) #:: (fib zip fib.tail).map(p=>p._1 + p._2)

def fib2(): Iterator[BigInt] =
  Iterator.iterate((BigInt(0),BigInt(1))){case (a, b) => (b, a+b)}.map(_._1)

@main
def hello(): Unit =
  println("Hello world!")
  println(s"The first 10 Fibonacci numbers are ${fib.take(10).to(List)}")
  println(s"An alternative method produces the same: ${fib2().take(10).to(List)}")

