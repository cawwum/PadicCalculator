class Expression
{
    val helloWorld: String = "What is up with it my people!!!!";
    def twelve(): Stream[Int] = 2 #:: Stream.cons(1, Stream.empty)
    def nineynine(): Stream[Int] = 9 #:: Stream.cons(9, Stream.empty)

    //-----------------------------------------------------------------------------------------

    var p:Int = 10;

    var pA:PadicAddition = new PadicAddition(p)
    var pS:PadicSubtraction = new PadicSubtraction(p)

    val expression = pA.add(pS.sub(twelve(),nineynine()),nineynine()).iterator

    def exampleIt() =
    {
        if(expression.hasNext)println(expression.next())
        else println("No more! I can't take it anymore !!")
    }
}
