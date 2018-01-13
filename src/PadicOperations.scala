class PadicOperations
{
    //base p to be used
    def p:Int = 10
    //max usable length of p-adic integer
    def lengthcap:Int = 100

    //An infinite stream of p-1
    def pminus1s(): Stream[Int] = (p - 1) #:: pminus1s()
    //An infinite stream of zeroes
    def zeroes(): Stream[Int] = 0 #:: zeroes()

    def capInfiniteHelper(xs:Stream[Int],count:Int):Stream[Int] = xs match
    {
        case Stream.Empty => Stream.empty
        case (x#::xs) => if(count == lengthcap)Stream.empty else Stream.cons(x,capInfiniteHelper(xs,count+1))
    }

    def capInfinite(xs:Stream[Int]):Stream[Int] =
    {
        capInfiniteHelper(xs,0)
    }


    //---------- ADDITION ------------//

    //Adds 2 infinite streams of digits together
    def add(n1: Stream[Int], n2: Stream[Int]): Stream[Int] =
    {
        def adding(n1: =>Stream[Int], n2: =>Stream[Int], carry: Int) = (n1, n2) match
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
        def addHelper(n1: => Stream[Int], n2: => Stream[Int], carry: Int):Stream[Int] = (n1, n2,carry) match
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

    //-------- SUBTRACTION ----------//

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

    //possibly could make tail recursive
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

    //-------- MULTIPLICAION -----------//

    //...Should probably use the other one which cross multiplies...?

    def addLoop(n:Int,ns:Stream[Int]):Stream[Int] = (n,ns) match
    {
        case (0,_) => Stream.empty
        case (_,ns) => add(ns,addLoop(n-1,ns))
    }

    def addShift(n1:Stream[Int], n2:Stream[Int]):Stream[Int] = n1 match
    {
        case Stream.Empty => Stream.empty
        case h#::t => Stream.cons(h,add(t,n2))
    }

    def practiceMult(n1: Stream[Int], n2: Stream[Int]): Stream[Int] = n1 match
    {
        case Stream.Empty => Stream.empty
        case (h #:: t) => addShift(addLoop(h,n2),practiceMult(t,n2))
    }

    //--------- DIVISION ---------//

}
