sealed trait HList
case class C[H, T <: HList](hd : H, tl : T) extends HList
case object N extends HList

sealed class Zip[A,B,R]
object Zip {
  implicit val basecase = new Zip[N.type, N.type, N.type]

  implicit def reccase[X, XS <: HList, Y, YS <: HList, ZS <: HList]
    (implicit z : Zip[XS, YS, ZS])
    = new Zip[C[X, XS], C[Y, YS], C[(X,Y), ZS]]
}

def f[A,B,R](a : Manifest[A], b : Manifest[B])
            (implicit z : Zip[A,B,R], r : Manifest[R])
            = r

f(manifest[C[String, C[Int, N.type]]], manifest[C[Int, C[Boolean, N.type]]])
f(manifest[C[String, C[Int, N.type]]], manifest[C[Int, N.type]])