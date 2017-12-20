class PadicMultiplication(p: Int)
{
    var pA: PadicAddition = new PadicAddition(p);

    def addLoop(n:Int,ns:Stream[Int]):Stream[Int] = (n,ns) match
    {
        case (0,_) => Stream.empty
        case (_,ns) => pA.add(ns,addLoop(n-1,ns))
    }

    def addShift(n1:Stream[Int], n2:Stream[Int]):Stream[Int] = n1 match
    {
        case Stream.Empty => Stream.empty
        case h#::t => Stream.cons(h,pA.add(t,n2))
    }

    def practiceMult(n1: Stream[Int], n2: Stream[Int]): Stream[Int] = n1 match
    {
        case Stream.Empty => Stream.empty
        case (h #:: t) => addShift(addLoop(h,n2),practiceMult(t,n2))
    }
}
