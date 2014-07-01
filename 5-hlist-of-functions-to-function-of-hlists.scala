import scala.language.implicitConversions
import scala.annotation.implicitNotFound

sealed trait HList 
case class C[H, T <: HList](hd : H, tl : T) extends HList
case object N extends HList

@implicitNotFound("${I} contains types other than that of functions with exactly one parameter")
sealed abstract class LF2FL[I <: HList, F] {
  def transform(input : I) : F
}

object LF2FL {
  implicit val basecase
    = new LF2FL[N.type, N.type => N.type] {
        def transform(n : N.type) = _ => N
      }

  implicit def reccase
    [X, Y, XS <: HList, YS <: HList, L <: HList]
    (implicit r : LF2FL[L, XS => YS]) 
    : LF2FL[C[X => Y, L], C[X, XS] => C[Y, YS]]
    = new LF2FL[C[X => Y, L], C[X, XS] => C[Y, YS]] {
        def transform(c : C[X => Y, L]) =
          ((i : C[X,XS]) => C(c.hd(i.hd), r.transform(c.tl)(i.tl)))
      }
      
    def apply[I <: HList, F]
        (i : I)
        (implicit lf2fl : LF2FL[I, F])
        : F
        = lf2fl.transform(i)
}

val f = (_:Int).toString
val g = (_:String).toInt
val h = (_:Float) > 0.0f

