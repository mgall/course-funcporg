package funcprog.week03

trait TConsList[T] {
	
	def head: T

	def tail: TConsList[T]

	def isEmpty: Boolean

	def get(i: Int): T = 
		if(isEmpty) throw new IndexOutOfBoundsException
		else if(i==0) head
		else tail.get(i-1)
}


class EmptyConList[T] extends TConsList[T] {
	
	def head: Nothing = throw new NoSuchElementException("Empty.Head")

	def tail: TConsList[T] = null

	def isEmpty: Boolean = true

	override def toString = "."
}

class ConsList[T](val head: T, val tail: TConsList[T]) extends TConsList[T] {

	def isEmpty: Boolean = false

	override def toString = head.toString + ", " + tail.toString
}

object TestList {

	def GetRangeList(i: Int): TConsList[Int] = 
		if(i <= 0) new EmptyConList[Int]
		else new ConsList(i-1, GetRangeList(i-1))

	def test(count: Int, msg: String , action: TConsList[Int]=>Any) = { 
		val l = GetRangeList(count); 
		println( "["+ l +"] " + msg + " --> " + action(l))
	}

	
	def main(args: Array[String]) {
		test(-1, "Is Empty", l=>l.isEmpty)
		test(04, "Is Empty", l=>l.isEmpty)
		test(04, "Elem at 0", l=>l.get(0))
		test(04, "Elem at 2", l=>l.get(2))
	}

}