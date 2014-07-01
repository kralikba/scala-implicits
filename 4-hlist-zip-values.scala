sealed trait HList
case class C[H, T <: HList](hd : H, tl : T) extends HList
case object N extends HList

sealed abstract class Zip[A,B,R] {
  def do_zip(a : A, b : B) : R
}

object Zip {
  implicit val basecase 
    = new Zip[N.type, N.type, N.type] {
        def do_zip(n1 : N.type, n2 : N.type) = N
    }

  implicit def reccase
    [X, XS <: HList, Y, YS <: HList, ZS <: HList]
    (implicit z : Zip[XS, YS, ZS])
    = new Zip[C[X, XS], C[Y, YS], C[(X,Y), ZS]] {
        def do_zip(c1 : C[X, XS], c2 : C[Y,YS]) =
        C((c1.hd, c2.hd), z.do_zip(c1.tl, c2.tl))
      }

  def apply
    [A <: HList, B <: HList, R <: HList]
    (a : A, b : B)
    (implicit z : Zip[A,B,R]) : R 
  = z.do_zip(a,b)
}