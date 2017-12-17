class PadicSubtraction(p:Int)
{
    //An infinite stream of p-1
    def pminus1s(): Stream[Int] = (p - 1) #:: pminus1s()

    //n1 minus n2
    def sub(n1: Stream[Int], n2: Stream[Int]): Stream[Int] = (n1, n2) match
    {
        //If nothing left to subtract .... then
        case (Stream.Empty, Stream.Empty) => Stream.empty
        //If the minuend is empty but the subtrahend isn't, then treat the next value as 0
        case (Stream.Empty, _) => sub(Stream.cons(0,Stream.empty), n2)
        //If there is no more to subtract then just return the rest of the minuend
        case (_, Stream.Empty) => n1
        //Check if the head of the top is bigger than the head of the bottom
        //if it is, then just subtract and continue
        //otherwise add p and subtract; then continue with the minuend borrowing from the next digit
        case (h1 #:: n1s, h2 #:: n2s) =>
        {
            if (h1 >= h2) (h1 - h2) #:: sub(n1s, n2s)
            else (p + h1 - h2) #:: sub(borrow(n1s), n2s)
        }
    }

    //possibly could make tail recursive buuut idk
    def borrow(n1: Stream[Int]): Stream[Int] = n1 match
    {
        //If no more digits to borrow from, borrow infinitely
        case Stream.Empty => pminus1s()
        //if there is something to borrow from then just borrow
        case h #:: t =>
        {
            if (h > 0) (h - 1) #:: t
            //if the next digit is 0 then make that p-1 and borrow from the next digit
            else (p - 1) #:: borrow(t)
        }
    }
}
