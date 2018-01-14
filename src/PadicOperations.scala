class PadicOperations
{
    //base p to be used
    def p:Int = 10
    //max usable length of p-adic integer
    def lengthcap:Int = 100

    //An infinite stream of p-1
    def pminus1s: Stream[Int] = (p - 1) #:: pminus1s
    //An infinite stream of zeroes
    def zeroes: Stream[Int] = 0 #:: zeroes

    def capInfinite(xs:Stream[Int],cap:Int):Stream[Int] = xs match
    {
        case Stream.Empty => Stream.empty
        case (x#::xs) => if(cap <= 0)Stream.empty else Stream.cons(x,capInfinite(xs,cap-1))
    }


    //---------- ADDITION ------------//

    //Adds 2 infinite streams of digits together
    def add(n1: Stream[Int], n2: Stream[Int],cap:Int): Stream[Int] =
    {
        def adding(n1:Stream[Int], n2:Stream[Int], carry: Int,cap:Int) = (n1, n2) match
        {
            //only used in the addhelper method so does not need to match on anything else
            case (h1#::n1s, h2#::n2s) =>
            {
                //gets the total of the heads and carry.
                val total: Int = carry + h1 + h2
                //if that value is under p, take the digit with no carry
                if (total < p) Stream.cons(total,addHelper(n1s, n2s, 0,cap-1))
                //If that value is over p then take last digit and give the rest over p as a carry
                else Stream.cons((total % p),addHelper(n1s, n2s, total / p,cap-1))
            }
        }

        //helper so carry can be passed as a parameter
        def addHelper(n1:Stream[Int], n2:Stream[Int],carry:Int,cap:Int):Stream[Int] = (n1, n2,carry,cap) match
        {
            case (_,_,_,0) => Stream.empty
            //If both empty and carry of 0, the rest of the digits must all be 0
            case (Stream.Empty,Stream.Empty,0,_) => Stream.empty
            //If both empty, just handle the carry
            case (Stream.Empty, Stream.Empty,_,_) => adding(Stream.cons(0, Stream.empty),Stream.cons(0, Stream.empty),carry,cap)
            //if a stream is empty, replace it with a 0 and then continue
            case (Stream.Empty, n2,_,_) => adding(Stream.cons(0, Stream.empty), n2, carry,cap)
            case (n1, Stream.Empty,_,_) => adding(n1, Stream.cons(0, Stream.empty), carry,cap)
            // if 2 values are given, perform addition with them.
            case (n1, n2,_,_) => adding(n1, n2, carry,cap)
        }

        //call the helper method
        addHelper(n1, n2, 0,cap)
    }

    //-------- SUBTRACTION ----------//

    //n1 minus n2
    def sub(n1:Stream[Int], n2:Stream[Int],cap:Int): Stream[Int] = (n1, n2,cap) match
    {
        case (_,_,0) => Stream.empty
        //If nothing left to subtract .... then
        case (Stream.Empty, Stream.Empty,_) => Stream.empty
        //If the minuend is empty but the subtrahend isn't, then treat the next value as 0 (I don't think I reduce the cap here...?)
        case (Stream.Empty, _, _) => sub(Stream.cons(0,Stream.empty), n2,cap)
        //If there is no more to subtract then just return the rest of the minuend
        case (_, Stream.Empty,_) => n1
        //Check if the head of the top is bigger than the head of the bottom
        //if it is, then just subtract and continue
        //otherwise add p and subtract; then continue with the minuend borrowing from the next digit
        case (h1 #:: n1s, h2 #:: n2s,_) =>
        {
            if (h1 >= h2) (h1 - h2) #:: sub(n1s, n2s,cap-1)
            else (p + h1 - h2) #:: sub(borrow(n1s), n2s,cap-1)
        }
    }

    def borrow(n1: Stream[Int]): Stream[Int] = n1 match
    {
        //If no more digits to borrow from, borrow infinitely
        case Stream.Empty => pminus1s
        //if there is something to borrow from then just borrow
        case h #:: t =>
        {
            if (h > 0) (h - 1) #:: t
            //if the next digit is 0 then make that p-1 and borrow from the next digit
            else (p - 1) #:: borrow(t)
        }
    }

    //-------- MULTIPLICAION -----------//

    def tailReverse(list:Stream[Int]):Stream[Int] =
    {
        def reverseHelper(list: Stream[Int], acc: Stream[Int]): Stream[Int] = list match
        {
            case Stream.Empty => acc
            case hd #:: tl => reverseHelper(tl, hd#::acc)
        }

        reverseHelper(list, Stream.empty)
    }

    def zeroHeads(xs:Stream[Int],n:Int):Stream[Int] = n match
    {
        case 0 => xs
        case n => zeroHeads(Stream.cons(0,xs),n-1)
    }

    def nzeroes(n:Int):Stream[Int] = n match
    {
        case 0 => Stream.empty
        case n => Stream.cons(0,nzeroes(n-1))
    }

    /*
    def lineMultiply(n1:Stream[Int],n2:Stream[Int],addacc:Int,carry:Int):(Int,Int) = (n1,n2) match
    {
        case (Stream.Empty,Stream.Empty) => ((addacc+carry)%p,(addacc+carry)/p)
        case (Stream.Empty,h2#::t2) => lineMultiply(Stream.empty,t2,addacc,carry)
        case (h1#::t1,Stream.Empty) => lineMultiply(t1,Stream.empty,addacc,carry)
        case (h1#::t1,h2#::t2) => lineMultiply(t1,t2,addacc+h1*h2,carry)
    }

    //call with count starting at 1
    def streamMultiply(n1:Stream[Int],n2:Stream[Int],count:Int,carry:Int,cap:Int):Stream[Int] =
    {
        if(count >= cap)
        {
            Stream.empty
        }
        else
        {
            val tempn1:Stream[Int] = if(count > n1.length)(n1 take count)#:::nzeroes(count - n1.length)else n1 take count
            val tempn2:Stream[Int] = if(count > n2.length)zeroHeads(n2 take count reverse,count - n2.length) else n2 take count reverse

            //not tail recursive in here somewhere! probably reverse...
            lineMultiply(tempn1, tempn2, 0, carry) match
            {
                case (a, b) => Stream.cons(a, streamMultiply(n1, n2, count + 1, b, cap))
            }
        }
    }
*/
    def mult(n1:Stream[Int],n2:Stream[Int],cap:Int):Stream[Int] =
    {
        streamMultiply(n1,n2,1,0,cap)
    }

    def lineMultiply(n1:Stream[Int],n2:Stream[Int],addacc:Int,carry:Int):(Int,Int) = (n1,n2) match
    {
        case (Stream.Empty,Stream.Empty) => ((addacc+carry)%p,(addacc+carry)/p)
        case (Stream.Empty,h2#::t2) => lineMultiply(Stream.empty,t2,addacc,carry)
        case (h1#::t1,Stream.Empty) => lineMultiply(t1,Stream.empty,addacc,carry)
        case (h1#::t1,h2#::t2) => lineMultiply(t1,t2,addacc+h1*h2,carry)
    }

    //call with count starting at 1
    def streamMultiply(n1:Stream[Int],n2:Stream[Int],count:Int,carry:Int,cap:Int):Stream[Int] =
    {
        if(count >= cap)
        {
            Stream.empty
        }
        else
        {
            //not tail recursive in here somewhere! probably reverse...
            lineMultiply(n1 take count, tailReverse(n2 take count), 0, carry) match
            {
                case (a, b) => Stream.cons(a, streamMultiply(n1, n2, count + 1, b,cap-1))
            }
        }
    }

    //...Should probably use the other one which cross multiplies...?

/*    def addLoop(n:Int,ns:Stream[Int],cap:Int):Stream[Int] = (n,ns) match
    {
        case (0,_) => Stream.empty
        case (_,ns) => addCap(ns,addLoop(n-1,ns,cap),cap)
    }

    def addShift(n1:Stream[Int], n2:Stream[Int],cap:Int):Stream[Int] = n1 match
    {
        case Stream.Empty => Stream.empty
        case h#::t => Stream.cons(h,addCap(t,n2,cap))
    }

    def multCap(n1: Stream[Int], n2: Stream[Int],cap:Int): Stream[Int] = n1 match
    {
        case Stream.Empty => Stream.empty
        case (h #:: t) => addShift(addLoop(h,n2,cap),multCap(t,n2,cap),cap)
    }
*/

    //--------- DIVISION ---------//

    def findMatchHelper(topDigit:Int,botDigit:Int,counter:Int):Int =
    {
        if(counter < p)
        {
            if((counter*botDigit) % p == topDigit)counter else findMatchHelper(topDigit,botDigit,counter+1)
        }
        else
        {
            println("Counter reached p when finding a match during division")
            0
        }
    }

    def findMatch(topDigit:Int,botDigit:Int):Int =
    {
        findMatchHelper(topDigit,botDigit,0)
    }

    def removeLastDigit(xs:Stream[Int]):Stream[Int] = xs match
    {
        case Stream.Empty => Stream.empty
        case (x#::xs)=> xs
    }

    //maybe append zeroes instead of empty stream through subtraction??
    def divn(n1:Stream[Int],n2:Stream[Int],cap:Int):Stream[Int] = (n1,n2,cap) match
    {
        case (_,_,0) => Stream.empty
        case (Stream.Empty,y#::ys,_) => Stream.empty
        case (_,Stream.Empty,_) =>
        {
            println("Dividing by zero!")
            Stream.empty
        }
        case (x#::xs,y#::ys,_) => Stream.cons(findMatch(x,y),divn(removeLastDigit(sub(n1,mult(Stream.cons(findMatch(x,y),zeroes),n2,100),100)),n2,cap-1))
    }
}
