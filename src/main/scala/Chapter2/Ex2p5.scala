package Chapter2

object Ex2p5 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}
