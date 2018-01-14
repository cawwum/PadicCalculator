class Expression
{
    val helloWorld: String = "Calculate!";
    def twelve: Stream[Int] = 2 #:: Stream.cons(1, zeroes)
    def nineynine: Stream[Int] = 9 #:: Stream.cons(9, zeroes)
    def pminus1s: Stream[Int] = Stream.cons(p - 1,pminus1s)
    def zeroes:Stream[Int] = Stream.cons(0,zeroes)

    //-----------------------------------------------------------------------------------------

    var p:Int = 10;

    var pO:PadicOperations = new PadicOperations

    val amountToIterate = 1000
    var iterateCount:Int = amountToIterate

    //Have to append zeroes to results....

    val expression = pO.mult(pminus1s,pminus1s,100).iterator

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
