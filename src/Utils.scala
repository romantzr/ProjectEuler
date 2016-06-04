/**
  * Created by Roman_ on 23/05/2016.
  */
object Utils {
  def gcd(a: BigInt, b: BigInt): BigInt = if(a % b == 0) b else gcd(b, a - (a / b) * b)

  def lcm(a: BigInt, b: BigInt): BigInt = abs(a * b) / gcd(a, b)

  def abs(a: BigInt): BigInt = if (a >= 0) a else a * (-1)

  def factorize(n: BigInt): List[BigInt] = Sequences.naturals.tail.takeWhile(j => j * j <= n).find(n % _ == 0).
    fold(List(n))(i => i :: factorize(n/i))

  def isPalindrome(n: BigInt): Boolean = n.toString.equals(n.toString.reverse)

  def power(a: BigInt, b: BigInt): BigInt = if(b == 0) 1 else a * power(a, b-1)

  def getPrimes(count: Int): List[BigInt] = {
    var primeLst = List[BigInt]()
    def getNext(current: BigInt): BigInt = {
      if (primeLst.takeWhile(n => n * n <= current).forall(n => current % n != 0)) current else getNext(current + 1)
    }
    var i = 0
    var curr = BigInt(2)
    while (i < count) {
      primeLst = primeLst :+ curr
      curr = getNext(curr + 1)
      i += 1
    }
    primeLst
  }
}

object Sequences {
  def naturals: Stream[BigInt] = Stream.cons(BigInt(1), naturals.map(_ + 1))

  def primes: Stream[BigInt] = BigInt(2) #:: Stream.from(3).map(i => BigInt(i)).filter(i =>
    primes.takeWhile(j => j * j <= i).forall(i % _ > 0))

  def sieve(s: Stream[BigInt]): Stream[BigInt] = s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val primes2 = sieve(naturals.tail)

  //def fibonacci: Stream[BigInt] = Stream.cons(BigInt(0), Stream.cons(BigInt(1), fibonacci.zip(fibonacci.tail).map(n => n._1 + n._2)))

  def fibonacci: Stream[BigInt] = BigInt(0) #:: fibonacci.scanLeft(BigInt(1))(_ + _)
}

case class Fraction(var numerator: BigInt, var denominator: BigInt){
  simplify

  def simplify = {
    val gcd = Utils.gcd(numerator, denominator)
    if(gcd > 1) {
      numerator = numerator / gcd
      denominator = denominator / gcd
    }
  }

  def +(other: Fraction) = {
    val lcm = Utils.lcm(denominator, other.denominator)
    Fraction(numerator * (lcm / denominator) + other.numerator * (lcm / other.denominator), lcm)
  }

  override def toString: String = numerator + " / " + denominator
}