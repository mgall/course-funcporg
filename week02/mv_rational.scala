package funcprog.week02

import math.abs

class Rational(x: Int, y: Int) {
	private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a%b) 
	private val sign = if((x > 0 && y < 0) || (x < 0 && y > 0)) -1 else 1
	private val g = abs( gcd(x,y) )

	// FIELDS
	private val num = sign *( abs(x) / g)
	private val den = abs(y) / g

	override def toString(): String = if(num==den || den == 1) num.toString else  num + "/" + den;

	def unary_- : Rational = new Rational( -this.num, this.den )

	def + (that: Rational) = new Rational( this.num*that.den + that.num*this.den, this.den*that.den )

	def - (that: Rational) = this + -that

	def * (that: Rational) = new Rational( this.num*that.num, this.den*that.den )

	def * (i: Int) = new Rational( i*this.num, this.den )
}

object TestRational {

    def main(args: Array[String]) = {
        val x = new Rational(1,2)

        val y = new Rational(1,2)

        println( x*x*x - y*3 )
    }
}
