// The database

class Joshua
class Joe
class Johann
class Joshi

class Son[S,F]

implicit val s0 = new Son[Joshua, Joe]
implicit val s1 = new Son[Joe, Johann]
implicit val s2 = new Son[Johann, Joshi]

// A compile-time test

def sonOf[S,F](implicit ev : Son[S,F]) = r

sonOf[Joshua,Joe]
sonOf[Johann, Joe]

// A compile-time query

sonOf[Johann, _]

fatherOf[S,F](sonType : Manifest[S])
             (implicit ev : Son[S,F], fatherType : Manifest[F])
			 = fatherType

fatherOf(manifest[Johann])

// Resolution order problems

def fatherOfR[S,F](sonType : Manifest[S])
                  (implicit fatherType : Manifest[F], ev : Son[S,F])
                  = fatherType
				  
fatherOfR(manifest[Johann])

implicit val s3 = new Son[Johann, Nothing] // this shall be removed later
fatherOfR(manifest[Johann])

// Composing implicits

class Father[F,S]
object Father {
  implicit def father_evidence[F,S](implicit ev : Son[S,F]) = new Father[F,S]
}

class Grandfather[S,F]
object Grandfather {
  implicit def grandfather_evidence[S,F,G]
               (implicit ev_f : Father[F,S], ev_s : Son[F,G])
               = new Grandfather[S,G]
			   
  def Of[S,G](s : Manifest[S])
             (implicit ev : Grandfather[S,G], m : Manifest[G])
             = m
}

Grandfather.Of(manifest[Joe])

// Recursion

class Descendant[D,A]
object Descendant {
  def direct[D,A](implicit s : Son[D,A]) = new Descendant[D,A]
  implicit def indirect[D,C,A]
               (implicit s : Son[D,C], d : Descendant[C,A])
               = new Descendant[D,A]
}

implicitly[Descendant[Joe,Johann]]
implicitly[Descendant[Joe, Joshi]]
implicitly[Descendant[Joe, Joshua]]

// Negation

class Not[X]
object Not {
  implicit def always[X] = new Not[X]
  implicit def never[X](implicit x : X) = new Not[X]
}

// The last item

class Forefather[D,A]
object Forefather {
  implicit def next[D,C,A]
    (implicit s : Son[D,C], d : Forefather [C,A])
    = new Forefather[D,A]

  implicit def last[D,A]
    (implicit s : Son[D,A],
     n : Not[Son[A,_]]) = new Forefather [D,A]

  def Of[D,A]
      (d : Manifest[D])
      (implicit f : Forefather[D,A], m : Manifest[A])
      = m
}

Forefather.Of(manifest[Joe])
Forefather.Of(manifest[Johann])
Forefather.Of(manifest[Joshua])