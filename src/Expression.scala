class Expression
{
    val helloWorld: String = "What is up with it my people!!!!";
    def twelve(): Stream[Int] = 2 #:: Stream.cons(1, Stream.empty)
    lazy val nineynine: Stream[Int] = 9 #:: Stream.cons(9, Stream.empty)
    lazy val pminus1s: Stream[Int] = Stream.cons(p - 1,pminus1s)

    //-----------------------------------------------------------------------------------------

    var p:Int = 10;

    var pA:PadicAddition = new PadicAddition(p)
    var pS:PadicSubtraction = new PadicSubtraction(p)
    var pM:PadicMultiplication = new PadicMultiplication(p)

    val amountToIterate = 1000
    var iterateCount:Int = amountToIterate

    lazy val add1 = pA.add(pminus1s,pminus1s)
    lazy val add2 = pA.add(pminus1s,pminus1s)

    lazy val addAll = pA.add(add1,add2)

    lazy val expression = pM.practiceMult(pminus1s,pminus1s).iterator

    def exampleIt() =
    {
        for(i <- 1 to amountToIterate) yield
        {
            if(expression.hasNext)
            {
                println(expression.next())

            }
            else println("No more! I can't take it anymore !!")
        }

        iterateCount+=amountToIterate

        println("Done!")
    }
}
