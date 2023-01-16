@main def zipWithIndexDemo(): Unit =
  val input = 'a' to 'k'
  val output = input.zipWithIndex
  println(output)
  println(myZipWithIndex(input))

def myZipWithIndex[T, CC[T2] <: Iterable[T2]](seq: CC[T]): Iterable[(T, Int)] = seq.scanLeft(Option.empty[(T, Int)]) {
  case (Some((_, idx)), elem) => Some((elem, idx + 1))
  case (None, elem) => Some((elem, 0))
}.flatten

def extractNumbers(s: String): IndexedSeq[Int] =
  case class State(output: Option[Int] = None, partialNo: Option[Int] = None)
  val stateMachineOutput = s.scanLeft(State()) {
    case (State(_, None), char) if char.isDigit => State(None, Some(char.asDigit))
    case (State(_, Some(n)), char) if char.isDigit => State(None, Some(10 * n + char.asDigit))
    case (State(_, n), char) => State(n, None)
  }
  stateMachineOutput.flatMap(_.output) ++ stateMachineOutput.last.partialNo

@main def extractNumbersDemo(): Unit =
  println(extractNumbers("abc 123 def 456 #@$@ 789"))
  println(extractNumbers("abc 123 def 456 #@$@ 789==="))
  println(extractNumbers(""))

class ScanLeftPattern // Keep IntelliJ happy, one class name must be same as file name.
