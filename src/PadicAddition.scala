class PadicAddition(p:Int)
{
    //An infinite stream of zeroes
    def zeroes(): Stream[Int] = 0 #:: zeroes()

    //Adds 2 infinite streams of digits together
    def add(n1: Stream[Int], n2: Stream[Int]): Stream[Int] =
    {
        def adding(n1: Stream[Int], n2: Stream[Int], carry: Int) = (n1, n2) match
        {
            //only used in the addhelper method so does not need to match on anything else
            case (h1#::n1s, h2#::n2s) =>
            {
                //gets the total of the heads and carry.
                val total: Int = carry + h1 + h2
                //if that value is under p, take the digit with no carry
                if (total < p) total #:: addHelper(n1s, n2s, 0)
                //If that value is over p then take last digit and give the rest over p as a carry
                else (total % p) #:: addHelper(n1s, n2s, total / p)
            }
        }

        //helper so carry can be passed as a parameter
        def addHelper(n1: Stream[Int], n2: Stream[Int], carry: Int): Stream[Int] = (n1, n2,carry) match
        {
            //If both empty and carry of 0, the rest of the digits must all be 0
            case (Stream.Empty,Stream.Empty,0) => Stream.empty
            //If both empty, just handle the carry
            case (Stream.Empty, Stream.Empty,_) => adding(Stream.cons(0, Stream.empty),Stream.cons(0, Stream.empty),carry)
            //if a stream is empty, replace it with a 0 and then continue
            case (Stream.Empty, n2,_) => adding(Stream.cons(0, Stream.empty), n2, carry)
            case (n1, Stream.Empty,_) => adding(n1, Stream.cons(0, Stream.empty), carry)
            // if 2 values are given, perform addition with them.
            case (n1, n2,_) => adding(n1, n2, carry)
        }

        //call the helper method
        addHelper(n1, n2, 0)
    }
}