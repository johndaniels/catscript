import catscript.interpret.Interpretor

object main {
  def main(args: Array[String]) {
	val interpretor = new Interpretor
	while (true) {
	  val line = readLine();
	  val result = Parser.apply(line);
	  result match {
	    case Parser.Success(result, value) => interpretor.run(result)
	    case Parser.Failure(_, _) =>{ println("ERROR") }
	  }
	}
  }
}