class Son[S,F]
class Male[P]

implicit val s0 = new Son[Joe, Johann]
implicit val s1 = new Son[Joe, Jill]

implicit val m0 = new Male[Joe]
implicit val m1 = new Male[Johann]

def father[S,F](m : Manifest[S])(implicit s : Son[S,F], p : Male[F]) = p