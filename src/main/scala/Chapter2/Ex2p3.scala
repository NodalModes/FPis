package Chapter2

object Ex2p3 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
  
}
