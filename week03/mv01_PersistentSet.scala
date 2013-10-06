package funcprog.week03

abstract class IntSet {

	def incl(x: Int):IntSet

	def contains(y: Int): Boolean 

	def U (that: IntSet): IntSet
}

object EmptyBTree extends IntSet {

	def incl(x: Int): IntSet = new BTree(x, EmptyBTree, EmptyBTree)

	def contains(x: Int): Boolean = false

	def U (that: IntSet): IntSet = that

	override def toString: String = "."
}

class BTree(rootVal: Int, left: IntSet, right: IntSet) extends IntSet {
	
	def incl(x: Int): IntSet =
		if(x > rootVal) new BTree(rootVal, left, right.incl(x)) 
		else if (x < rootVal) new BTree(rootVal, left.incl(x), right)
		else this

	def contains(x: Int): Boolean = 
		if (x > rootVal) right.contains(x)
		else if(x < rootVal) left.contains(x) 
		else true

	def U (that: IntSet): IntSet = (left U right U that).incl( rootVal )

	override def toString: String = left.toString + rootVal.toString + right.toString
}

object TestSet {

	def CheckAndLog(a: Int, b: Int)(s: IntSet): Unit = 
		if(a<=b) {println("Exist " + a + " in set -> " + s.contains(a)); CheckAndLog(a+1,b)(s)}

	def listAndLog(name: String, s: IntSet) = println( name + "={" + s.toString + "}" )

	def main(args: Array[String]) = {

		def test(name: String, s: IntSet) = { listAndLog(name,s); CheckAndLog(0,7)(s) }
		
		// EMPTY
		val a = EmptyBTree
		test("A", a)

		// INSERT Some elements
		val b = EmptyBTree incl 5 incl 4  incl 6
		test("B", b)

		// UNION
		val c = EmptyBTree incl 1 incl 2 incl 3
		listAndLog("C", c)
		
		val d = b U c
		test("B U C", d)
	}
}


