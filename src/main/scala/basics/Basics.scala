package basics

object Basics {
  def lcm(a: Int, b: Int): Int = if (b == 0) 0 else Math.abs(a * b) / gcd(a, b)

  def gcd(a: Int, b: Int): Int = if (b == 0) Math.abs(a) else gcd(b, a % b)
}
