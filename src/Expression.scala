class Expression
{
    val helloWorld: String = "What is up with it my people!!!!";
    def twelve(): Stream[Int] = 2 #:: Stream.cons(1, Stream.empty)
    lazy val nineynine: Stream[Int] = 9 #:: Stream.cons(9, Stream.empty)
    lazy val pminus1s: Stream[Int] = Stream.cons(p - 1,pminus1s)

    //-----------------------------------------------------------------------------------------

    var p:Int = 10;

    var pO:PadicOperations = new PadicOperations

    val amountToIterate = 1000
    var iterateCount:Int = amountToIterate

    val add1 = pO.add(pminus1s,pminus1s)
    val sub2 = pO.sub(pminus1s,pminus1s)

    val addAll = pO.add(add1,sub2)

    val expression = pO.practiceMult(twelve(),nineynine).iterator

    def exampleIt() =
    {
        for(i <- 1 to amountToIterate) yield
        {
            if(expression.hasNext)
            {
                println(expression.next())

            }
            else println("No more")
        }

        iterateCount+=amountToIterate

        println("Done!")
    }
}
