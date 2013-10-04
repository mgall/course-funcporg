package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1: Pascal Triangle
   */
  def pascal(c: Int, r: Int): Int = 
		if (c%r == 0) 1 
		else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2: Brackets balance
   */
    def balance(chars: List[Char]): Boolean = {
    
		def state(c: Char):Int =
			if(c=='(') 1
			else if(c==')') -1
			else 0
			
		def bCount(e: List[Char], c: Int): Int =
			if(e.isEmpty) c
				else if(c < 0) -1
				else bCount(e.tail, c + state(e.head))
				
		bCount(chars, 0) == 0
    } 
  
  /**
   * Exercise 3
   */
  	def countChange(money: Int, coins: List[Int]): Int =
		if(coins.isEmpty || money < coins.head) 0
		else if (coins.head == money) 1
		else countChange(money-coins.head, coins) + countChange(money, coins.tail)
}
