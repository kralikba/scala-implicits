#Implicit metaprogramming in Scala

{toc}

This is a short tutorial-like text based on my talk on abusing Scala's type inference & implicit resolution algorithms so that we get a kind of compile-time, type-level metaprogramming that resembles logic programming. The tutorial assumes that the reader has some basic knowledge about the Scala language and generics. If you already know about implicits, you could as well skip the first chapter. Knowledge of logic programming is not required, but on the long run, useful.

Firing up a Scala interpreter and playing around with the code snippets is deemed a good idea by 9 out of 10 Scala experts. Do note that in the interpreter, classes and their companion objects have to be input in the same go. For this, using `:paste` is recommended.

Note: "iff" does not equal "if". I use with the meaning of "if, and only if".

---

This text is loosely based on Scala 2.10.4. There might be statements which do not reflect the current language standard (be it the one at the time of writing or something newer). Please notify the author if You find any discrepancies between this tutorial and reality. Thanks.

Any other comments are also warm-heartedly welcome (e.g. "I think topic X should not be left out", "paragraph X should be left out", "warm-heartedly is not a word of the English language", "code snippets should not contain the noise of the scala console", etc.)


Copyright 2014, Barnabás Králik. Distributed under the 2-clause BSD license. The author can be reached at kralikba at elte dot hu.

Greets go out to the [Vienna Scala User Group](http://github.com/scala-vienna).

---

## A primer on implicits

In Scala, functions may have multiple formal parameter lists. The last - and possibly only - one may be marked `implicit`. By this, we tell the Scala compiler to try to automatically find a value of the given type. Eligible are those `val`s and `def`s without explicit parameters that are marked `implicit`. 

```scala
scala> def f(x : Int)(implicit b : Int) = x * b

scala> implicit val intimplval : Int = 9

scala> f(5)
res1: Int = 45
```

If we do not wish to use to introduce or use implicit values, we can still specify explicit ones for the implicit parameters at the call site:

```scala
scala> f(5)(6)
res1: Int = 30
```

If no implicit value has been found in the current scope, including imported items, the type's companion object's public members are searched to find an appropriate value.

```scala
scala> :paste
class A
object A {
  implicit val a_impl = new A
}

scala> implicitly[A]
res1: A = A@7a30635f

scala> A.a_impl
res2: A = A@7a30635f
```

> #### `implicitly[X]`

> A handy function for testing implicits is the built-in generic function `implicitly[X]`, which tries to resolve the implicit value belonging to the type `X`. It can be defined as follows:

> ```scala
def implicitly[X](implicit inst : X) : X = inst
```

### Unambiguity required

If either attempt yields more than one appropriate implicit, implicit resolution fails, and results in a compile time error - even if these values are equal.

```scala
scala> def f(implicit x : Int) = x * 2

scala> implicit val x1 = 5

scala> implicit val x2 = 5

scala> f
<console>:12: error: ambiguous implicit values:
 both value x1 of type => Int
 and value x2 of type => Int
 match expected type Int
              f
              ^
```

### Type parameter inference

Implicit resolution is able to find an implicit value for generic types even if they are underspecified, i.e. there is not enough information about their type parameters.

```scala
scala> class A[X]
defined class A

scala> implicit val a_impl = new A[Int]
a_impl: A[Int] = A@275acd57

scala> def f[X](implicit a : A[X]) = a
f: [X](implicit a: A[X])A[X]

scala> f
res0: A[Int] = A@275acd57
```

The compiler automatically infers that `X` equals `Int` and is able to use it when resolving implicits and deducing types later on.

```scala
scala> class B[Y]
scala> implicit val b_int = new B[Int]
b_int: B[Int] = B@4defff82

scala> implicit val b_float = new B[Float]
b_float: B[Float] = B@40f13bf0

scala> def g[X](implicit a : A[X], b : B[X]) = b
scala> g
res1: B[Int] = B@4defff82
```

## Representing data with types

Let us now see how notions of logic programming can be expressed using Scala's type system. Suppose that we want to store data about some members of a family; they are the objects of our discussion. These people can be represented as types:

```scala
class Joshua
class Joe
class Johann
class Joshi
```

The statement _`S` is the son of `F`_ can be represented using a generic type:

```scala
class Son[S,F]
```

The fact, that `S` is actually the son of `F` corresponds to the fact that `Son[S,F]` is implicitly instantiable for the given two types, for example

```scala
implicit val s0 = new Son[Joshua, Joe]
implicit val s1 = new Son[Joe, Johann]
implicit val s2 = new Son[Johann, Joshi]
```

represents that Joshua is the son of Joe, who is the son of Johann, who is in turn the son of Joshi. (Note, that actually `s0`, `s1` and `s2` could all be `null` as long as their static type remains the same as in the example.)

### A compile-time test

Now, we can define a following function:

```scala
def sonOf[S,F](implicit ev : Son[S,F]) = ev
```

Observe, that not the result or callability, but already the compilability of a function call with a given `S,F` pair correspond to a truth value - whether `S` is the son of `F` or not:

```scala
scala> sonOf[Joshua,Joe]
res1: Son[Joshua,Joe] = Son@1596e5d4

scala> sonOf[Johann, Joe]
<console>:17: error: could not find implicit value for parameter ev: Son[Johann,Joe]
              sonOf[Johann, Joe]
```

### A compile-time query

In logic programming languages, such as Prolog, we can leave wildcards in some statements. The execution environment will then tell us for what objects would the statement be true.

So it would be reasonable to expect Scala to guess who the father of Johann is if we tell it the following:

```scala
scala> sonOf[Johann, _]
```

Unfortunately this is not the case:

```scala
<console>:1: error: unbound wildcard type
       sonOf[Johann, _]
                     ^
```

Scala expects us to either specify all type parameters exactly or leave the inference of all of them to the compiler. So we need to give an explicit hint about the `S` without too much overhead. For example, we could pass the `Manifest` of the type in question as an explicit parameter.

> #### Manifests

> The (now deprecated) way of overcoming Java's type erasure of generic type arguments in Scala is to pass around type information as references to instances of `Manifest[X]`. The type is visible by default and is implicitly instantiable for all types.

> ```scala
scala> implicitly[Manifest[Johann]]
res1: Manifest[Johann] = Johann
```

> A shorthand for `implicitly[Manifest[X]]` is the function `manifest[X]`:

> ```scala
scala> manifest[Johann]
res2: Manifest[Johann] = Johann
```

> The non-deprecated way is to use `TypeTag`s but that would introduce unnecessary complexity to this tutorial.

We can thus define a function which is practically a compile-time query:

```scala
scala> def fatherOf[S,F](sonType : Manifest[S])
     |                  (implicit ev : Son[S,F], fatherType : Manifest[F])
     |                  = fatherType
scala> fatherOf(manifest[Johann])
res8: Manifest[Joshi] = Joshi
```

... and indeed, our initial facts show that Johann's father is Joshi. There are two key points to observe on this example.

One is that no type parameters have been explicitly passed; the compiler has deduced for which `S` and `F` is it possible to instantiate `fatherOf`. Implicit resolution and type inference both did some work. From `sonType`'s static type, it could be easily deduced that `S` is `Johann`. Then, `Son[Johann, F]` only has an implicit value iff `F` is `Joshi`. `fatherType` is then easily looked up as its type is already complete.

The other point is that the function call's parameters are all constants, it does not have any side effects and the result is also constant. This could give a smart compiler ample opportunities to generate no-overhead code, should this function be used in some part of a program.

### Resolution order

We have been talking about notions of logic; it would then be reasonable to expect that type inference is independent on the order of parameters. Thus, the following definition of `fatherOf` should be equivalent to the previous one in terms of type inference and implicit resolution:

```scala
scala> def fatherOf[S,F](sonType : Manifest[S])
     |                  (implicit fatherType : Manifest[F], ev : Son[S,F])
     |                  = fatherType
```

Unfortunately, this is not the case.

```scala
scala> fatherOf(manifest[Johann])
<console>:17: error: could not find implicit value for parameter ev: Son[Johann,F]
              fatherOf(manifest[Johann])
```

In Scala, type inference is done left-to-right, in the order of the parameters. Furthermore, it is done eagerly - if at some point, we only have a wild guess about some unknown type parameter, this wild guess would be considered as its concrete value for the rest of the expression.

In this concrete case, nothing is known about `F` when first encountering it. Scala will then look for the most special type for which `Manifest[X]` is implicitly instantiable. `Nothing` is the subtype of all types in Scala, thus `F` will be considered to be `Nothing`. When encountering `ev`, it won't be able to find an implicit value for `Son[Johann, Nothing]`, thus implicit resolution fails.

Unfortunately the error message is not too informative. To see that `F` has really been deduced to be `Nothing`, we can introduce an appropriate implicit value for `Son`:

```scala
scala> implicit val s3 = new Son[Johann, Nothing]

scala> fatherOf(manifest[Johann])
res10: Manifest[Nothing] = Nothing
```

> Do note that having a `Son[Johann, Nothing]` fact might hurt us later on. You have been warned. We will also disregard the existence of `s3` later on.

### Composing implicits

Let us now see how the grandfather of a person could be computed (but not in a most straightforward way). The definition of _grandfather_ is simple: `G` is the grandfather of `S` iff there exists some `F` who is the father of `S` and the son of `G`.

To aid us, we could define the statement _`F` is the father of `S`_:
```scala
class Father[F,S]
```

Now, let us define the rule that this statement holds for some `S` and `F` if _`S` is the son of `F`_. It is good practice to put these `implicit def`s in its return type's companion object. This way, the consumer code does not have to have any extra imports as the companion object's exported implicits are usable wherever the type is referenced, i.e. not even `Father` has to be imported.

```scala
object Father {
  implicit def father_evidence[F,S](implicit ev : Son[S,F]) = new Father[F,S]
}
```

Using `Father` and `Son`, we can define the _grandfather-of_ relation, again using a fact and defining a rule based on formerly known ones.

```scala
class Grandfather[S,F]
object Grandfather {
  implicit def grandfather_evidence[S,F,G]
               (implicit ev_f : Father[F,S], ev_s : Son[F,G])
               = new Grandfather[S,G]
}
```

We can create a function to try it out and see that this really works:

```scala
scala> def grandfatherOf[S,G](s : Manifest[S])
     |                       (implicit ev : Grandfather[S,G], m : Manifest[G])
     |                       = m
grandfatherOf: [S, G](s: Manifest[S])(implicit ev: Grandfather[S,G], implicit m: Manifest[G])Manifest[G]

scala> grandfatherOf(manifest[Joe])
res7: Manifest[Joshi] = Joshi
```

### Recursion

No computational method would be practical without some possibility for infinity. In logic programming and functional languages, this means recursion.

Fortunately, implicit resolution is willing to recursively instantiate generic `implicit def`s for different parameters.

Let us implement a test that tells us whether some person is the descendant of another.

```scala
class Descendant[D,A]
```

The relation can be defined recursively:

1. The base case: `D` is a descendant of `A` if `D` is the son of `A`.
```scala
implicit def direct[D,A](implicit s : Son[D,A]) = new Descendant[D,A]
```

2. The recursive case: `D`'s father is known and he is a descendant of `A`.
```scala
implicit def indirect[D,C,A]
                (implicit s : Son[D,C], d : Descendant[C,A])
                = new Descendant[D,A]
```

We can check that this test works:


```scala
scala> implicitly[Descendant[Joe,Johann]]
res2: Descendant[Joe,Johann] = Descendant@e3d1e03

scala> implicitly[Descendant[Joe, Joshi]]
res3: Descendant[Joe,Joshi] = Descendant@5d20d627

scala> implicitly[Descendant[Joe, Joshua]]
<console>:29: error: could not find implicit value for parameter e: Descendant[Joe,Joshua]
              implicitly[Descendant[Joe, Joshua]]
```

### The last item

In the previous section, we managed to decide whether two people are direct descendants of one other or not. But what if we are interested in something more open-ended - such as: who is the furthest known forefather of a given person? Let's see what a query on descendants returns:

```scala
scala> def descQuery[D,A](dm : Manifest[D])(implicit da : Descendant[D,A]) = da
scala> descQuery(manifest[Joshua])
res0: Descendant[Joshua,Joe] = Desc@4874bec5
```

The compiler notices that there is an implicit, which does not require resolving further implicits - thus this first possible solution is _the_ solution. We would need to express somehow, that the recursion should only be stopped when there is no possibility to continue it. For example, we could say that the base case is when there is an appropriate `Son[D,A]` but there is no appropriate `Son[D,_]`.

Observe the usage of the wildcard type. `implicit e : Son[F,_]` means that _there exists some `X`, for which an implicit `Son[F,X]` exists_.


```scala
class Forefather[D,A]

implicit def next[D,C,A]
  (implicit s : Son[D,C], d : Forefather [C,A])
  = new Forefather[D,A]

implicit def last[D,A]
  (implicit s : Son[D,A],
   n : Not[Son[A,_]]) = new Forefather [D,A]
```

#### Negation

Unfortunately, Scala has only minimal support for implicit metaprogramming; thus, we ourselves have to implement `Not[X]` as an implicit metafunction.

The most straightforward way to tell Scala to abandon trying to instantiate a generic implicit for a given type parameter is to create ambiguity. This will let it still explore other possiblities, if not that specific instantiation was top-level. We can exploit this by making `Not[X]` such that it can unambiguously instantiated implicitly only iff `X` cannot be instantiated implicitly.

```scala
class Not[X]
object Not {
  implicit def always[X] = new Not[X]
  implicit def never[X](implicit x : X) = new Not[X]
}
```

## A practical example

Up till now, our examples were mostly toy examples. You might have asked - who on Earth would want to do graph algorithms on regular data using type-level metaprogramming. This question is left open.

In this section, we will see how a tuple of functions could be transformed into a function on tuples.
I.e. Instead of `(f(a),g(b),h(c),...)` we wish to use `(f,g,h,...)(a,b,c...)` and get the same result.

### Tuples and heterogeneous lists

In Scala, there is a handy feature: we have anonymous types and an easy-to-use syntax for cross products of an arbitrary number of types. (actually, at most 22 without manual intervention). These are called tuples and are represented by the generic types `Tuple1[T1], Tuple2[T1,T2], ..., Tuple22[T1,T2, ..., T22]`.

Scala even provides a trait to treat them and other types, such as case classes, uniformly: `Product`. Its two most important members are `def productArity: Int`, which returns the current item's arity (number of members); the other one is `def productElement(n : Int) : Any`, which returns the value of the *n*th item. This could easily be used to convert our tuple of functions into a function of tuples, but as `productElement` does not give any static information about the types of the items, we lose compile-time type safety. As in many cases, this is our main motivation for doing compile-time metaprogramming; to introduce advanced features without compromising static type safety.

To be able to treat tuples uniformly and keep type safety, we use _heterogeneous lists_ (or HLists in some literature). These are fixed-length linked lists the members of which are of different types, yet these types are statically known.

A simplest definition of heterogeneous lists would be:
```scala
sealed trait HList
case class C[H, T <: HList](hd : H, tl : T) extends HList
case object N extends HList
```

For example, a 3-tuple of an `Int`, a `String` and a `Boolean` could be represented as:
```scala
C[Int, C[String, C[Boolean, N.type]]]
```

Also, type inference works well:
```scala
scala> C(5, C("5", C(false, N)))
res0: C[Int,C[String,C[Boolean,N.type]]] = C(5,C(5,C(false,N)))
```

Do note, that due to the type signature containing the type of each item, one-by-one, the signature itself defines the length of the list. (You might ask: why use `N.type`. `N` is an `object`, thus the name `N` refers to an actual value, which is the singleton instance of an unnamed type. Thus, we can only indirectly refer to its type, e.g. using the expression `N.type`)

### Zip on types

Let us play around a bit with heterogeneous lists. Take the `zip` function. This takes two lists of equal lengths and produces a third list of pairs, so that the result list's *n*th item is the pair of the two input lists' *n*th items. This could be defined for example in the following manner in Scala:

```scala
def zip[A,B](a : List[A], b : List[B]) : List[(A,B)]
    = (a,b) match {
      case (x :: xs, y :: ys) => (x,y) :: zip(xs, ys)
      case (Nil, Nil) => Nil
    }
```

This function throws a run-time exception if the lists are of different lengths - as the length of `List[_]`s is unknown at compile time. How do we make this type safe at compile time?

We can introduce an implicit metafunction, which works on heterogeneous lists, of course. At first, let us just compute the type of the output. Let us denote this `Zip[A,B,R]`, where `A` and `B` are the inputs' types and R is the result's type.

```scala
class Zip[A <: HList, B <: HList, R <: HList]
```

In terms of logic programming, the above can be considered a statement: iff zipping two heterogeneous lists of type A and B results in a list of type R, then `Zip` can be implicitly instantiated as `Zip[A,B,R]`.

The base case is when both input lists are empty:

```scala
implicit val nilcase = new Zip[N.type, N.type, N.type]
```

The recursive case is when the input lists are both non-empty. Then, the current output type will be a pair of these input lists' heads' types; the current output value a pair of the respective items.

```scala
implicit def reccase[X, XS <: HList, Y, YS <: HList, ZS <: HList]
    (implicit z : Zip[XS, YS, ZS])
    = new Zip[C[X, XS], C[Y, YS], C[(X,Y), ZS]]
```

`X` and `Y` denote the heads' types; `XS` and `YS` denote the tails'. `ZS` denotes the resulting list's tail, which is computed by the compiler when looking for an appropriate implicit value for `z`. The essence of the implicit above can be read out loud as:

*If zipping the heterogeneous lists of types `XS` and `YS` yields one of type `ZS`, then zipping two lists, the first elements of which are of types `X` and `Y`, and the remaining parts of which are of type `XS` and `YS`, respectively, yields a list, the first element of which is a pair of `X` and `Y` and the remaining part of which is `ZS`.*

We can test this using a function that is similar to what we have used in the previous sections:

```scala
def f[A,B,R](a : Manifest[A], b : Manifest[B])
            (implicit z : Zip[A,B,R], r : Manifest[R])
            = r
```

```scala
scala> f(manifest[C[String, C[Int, N.type]]], manifest[C[Int, C[Boolean, N.type]]])
res0: Manifest[C[(String, Int),C[(Int, Boolean),N.type]]] = C[scala.Tuple2[java.lang.String, Int], C[scala.Tuple2[Int, Boolean], N.type]]

scala> f(manifest[C[String, C[Int, N.type]]], manifest[C[Int, N.type]])
<console>:22: error: could not find implicit value for parameter z: Zip[C[String,C[Int,N.type]],C[Int,N.type],R]
              f(manifest[C[String, C[Int, N.type]]], manifest[C[Int, N.type]])
               ^
```

### Zip on values as well

We already have a correct type for the zipped list; let us now associate a value to it! Our goal is to be able to write something along the lines of the following expression

```scala
scala> Zip(C("5", C(5, N)), C(true, C(9.0f, N)))
```

and receive a well-typed and correct answer:

```scala
res0: C[(String, Boolean), C[(Int, Float), N.type]] = C((5,true), C((5, 9.0), N))
```

(And of course, receive a compilation error for lists of different lengths.)

As we have seen, `implicit`s may depend on type parameters - but they cannot depend on values. Up till now, we did not care too much about the actual value of the implicits - only their type. But that does not need to stay so. We can return arbitrary values of the given type. For example, instead of a simple placeholder `class Zip`, we can have one such class, that has a member that does the actual zipping

```scala
abstract class Zip[A,B,R] {
  def do_zip(a : A, b : B) : R
}
```

and define such implicits that implement this abstract method. (Do put them in the companion objects in real code!)

The base case is that of empty lists:
```scala
implicit val basecase = new Zip[N.type, N.type, N.type] {
        def do_zip(n1 : N.type, n2 : N.type) = N
    }
```

The recursive case doesn't simply do a recursive call, but receives the appropriate implicit `Zip` for the remainder of the list. It creates a tuple of the heads at first. Then, it calls this instance's `do_zip` method for zipping the remainder of the lists.
```scala
implicit def reccase[X, XS <: HList, Y, YS <: HList, ZS <: HList]
         (implicit z : Zip[XS, YS, ZS])
         = new Zip[C[X, XS], C[Y, YS], C[(X,Y), ZS]] {
             def do_zip(c1 : C[X, XS], c2 : C[Y,YS]) =
                 C((c1.hd, c2.hd), z.do_zip(c1.tl, c2.tl))
         }
```

Now, we can define an auxiliary function which can be used without explicit type parameters for zipping heterogeneous lists:

```scala
def apply[A <: HList, B <: HList, R <: HList](a : A, b : B)(implicit z : Zip[A,B,R]) : R = z.do_zip(a,b)
```

If we define this inside the companion object of `Zip`, we will be able to use it as a function object, and thus use the function literal syntax we have set sight upon. Let's check that the results are as expected for correct input

```scala
scala> Zip(C(5, C(9.0f, N)), C("5", C(true, N)))
res0: C[(Int, String),C[(Float, Boolean),N.type]] = C((5,5),C((9.0,true),N))
```

and for incorrect input as well:
```scala
scala> Zip(C(5, C(9.0f, N)), C("5", N))
<console>:14: error: could not find implicit value for parameter z: Zip[C[Int,C[Float,N.type]],C[String,N.type],R]
              Zip(C(5, C(9.0f, N)), C("5", N))
                 ^
```

### From lists of functions to functions of lists

Now, we have nearly everything on our toolbelt for reaching our set goal. Let us begin by defining an abstract type for such transformations:

```scala
@scala.annotation.implicitNotFound("${I} contains types other than that of functions with exactly one parameter")
abstract class LF2FL[I <: HList, F] {
  def transform(input : I) : F
}
```

Note the annotation. Its effect on the compilation is that it will make `could not find implicit` messages for the annotated type more user friendly. It can even contain references to the type parameters which are then substituted with concrete types if known - here we only have a reference to `I`.

`I` will be a heterogeneous list. Implicits will only exist for lists which fit the above criteria of only having items of arity-one functions.

`F` will be a function from lists to lists. Its bounds should be `>: HList => Nothing <: Nothing => HList`. Remember the subtyping law for functions: iff `A <: C` and `B >: D`, then `A => B <: C => D`. Unfortunately, specifying these bounds explicitly prevents implicit resolution from working. Still, implicit metaprogramming will yield us types conforming to these bounds.

The base case is that of empty lists, of course:
```scala
implicit val basecase
  = new LF2FL[N.type, N.type => N.type] {
      def transform(n : N.type) = _ => N
    }
```

The recursive case is similar to the inverse of `Zip`. Just replace the ordered pair's (`Tuple2`) type and data constructors with those of the arity-one function (`=>` or `Function1`)!
```scala
implicit def reccase
  [X, Y, XS <: HList, YS <: HList, L <: HList]
  (implicit r : LF2FL[XS => YS, L])
  = new LF2FL[C[X => Y, L], C[X, XS] => C[Y, YS]] {
      def transform(c : C[X => Y, L]) =
        i : C[X,XS] => C(c.hd(i.hd), r.transform(c.tl)(i.tl))
    }
```

Let us now introduce an appropriate helper function
```scala
def apply[I <: HList, F]
    (i : I)
    (implicit lf2fl : LF2FL[I, F])
    : F
    = lf2fl.transform(i)
```

If the above functions are correctly placed inside the companion object of `LF2FL`, we can observe expected functionality for both incorrect and correct inputs:

```scala
scala> LF2FL(N)
res0: N.type => N.type = <function1>

scala> res0(N)
res1: N.type = N

scala> LF2FL(C(Int, C(f, N)))
<console>:17: error: C[Int.type,C[Int => String,N.type]] contains types other than that of functions with exactly one parameter
              LF2FL(C(Int, C(f, N)))
                   ^

scala> LF2FL(C(f, C(g, C(h, N))))
res3: C[Int,C[String,C[Float,N.type]]] => C[String,C[Int,C[Boolean,N.type]]] = <function1>

scala> res3(C(1,C("1", C(10.0f,N))))
res4: C[String,C[Int,C[Boolean,N.type]]] = C(1,C(1,C(true,N)))
```

(Let us define `f`, `g` and `h` such that
```scala
val f = (_:Int).toString
val g = (_:String).toInt
val h = (_:Float) > 0.0f
```
)

---

_This concludes the body of the tutorial. Happy hacking!_

---

## Related work

* [shapeless](https://github.com/milessabin/shapeless/) has a comprehensive `HList` implementation
* Searching for "prolog" and "scala" in Google yields an interesting hit; a talk titled [There is a Prolog in your Scala!](https://speakerdeck.com/folone/theres-a-prolog-in-your-scala)

## Appendices

These are some snippets that do not fit well into the flow of the tutorial, but might be of interest for the fellow metaprogrammer and could save some Google-time and to-the-hell-with-this-I'm-becoming-a-stripper time. Some of them are simple "fun facts".

#### Missing parameters vs. not found implicits

Do note that not missing explicit parameters and implicit ones are two completely different kinds of mistakes:

```scala
scala> def f(x : Int)(implicit b : Int) = x * b
f: (x: Int)(implicit b: Int)Int

scala> f
<console>:9: error: missing arguments for method f;
follow this method with `_' if you want to treat it as a partially applied function
              f
              ^

scala> f(5)
<console>:9: error: could not find implicit value for parameter b: Int
              f(5)
               ^
```

#### More than one implicit parameter

The keyword `implicit` is to be used only once in an parameter list; in front of the first implicit parameter; e.g.

```scala
scala> def f(x : Int)(implicit b : Int, q : String) = ???
f: (x : Int)(implicit b : Int, implicit q : String)Nothing
```

#### Subtyping and implicits

If an implicit value exists both for the type in question and some of its subtypes, the most special subtype takes precedence. Ambiguity errors arise only if there are multiple implicit values of this subtype.

```scala
scala> class A
defined class A

scala> class B extends A
defined class B

scala> implicit val b_impl = new B
b_impl: B = B@2cac0ca8

scala> implicitly[A]
res0: A = B@2cac0ca8

scala> implicit val a_impl = new A
a_impl: A = A@32d8cf89

scala> implicitly[A]
res1: A = B@2cac0ca8

scala> implicit val a_impl2 = new A
a_impl2: A = A@f7532b5

scala> implicitly[A]
res2: A = B@2cac0ca8

scala> implicit val b_impl2 = new B
b_impl2: B = B@4f2943d1

scala> implicitly[A]
<console>:14: error: ambiguous implicit values:
 both value b_impl of type => B
 and value b_impl2 of type => B
 match expected type A
              implicitly[A]
                        ^
```

#### User-friendly `could not find implicit`

Annotating a type with `scala.annotation.implicitNotFound(String)` results in nicer messages if implicit resolution cannot find any matching values for a given type. If the type is generic, type parameters' actual values may be included by name in the message by escaping them as `${TypeParamName}` in the error message.

```scala
scala> import scala.annotation.implicitNotFound

scala> @implicitNotFound("${B} and ${C} are not too similar") class AreSimilar[B,C]

scala> def f[G,H](implicit ev : AreSimilar[G,H]) = ???

scala> f[Int, String]
<console>:15: error: Int and String are not too similar
              f[Int, String]
               ^
```

#### Debugging implicit resolution

Passing the `-Xlog-implicits` parameters to Scala results in a detailed log of attempts at resolving implicits - be them successfully resolvable or not.

#### Some useful built-in implicits

* ` =:=[A,B] ` is implicitly instantiable iff the types `A` and `B` equal
* ` <:<[A,B]` is implicitly instantiable iff the type `A` is the subtype of or equal to `B`

Operator syntax works for type constructors as well, e.g. the following is valid:

```scala
implicitly[Int =:= Int]
implicitly[String <:< Any]
```

#### Be careful with context and view bounds!

> **Context bounds** are annotations on type parameters in a generic's formal type parameter bounding some type by the availability of some implicit value, e.g.

> ```scala
scala> def f[X : Manifest](g : Int) = g
```

> means that there must be a `Manifest[X]` implicitly resolvable. Instead of `Manifest`, any one-parameter generic type can be written. A type parameter may have multiple context bounds and multiple type parameters may have context bounds.

> These are desugared as implicit parameters, e.g. the previous definition yields the following signature:

> ```scala
scala> def f[X : Manifest](g:Int) =  g
f: [X](g: Int)(implicit evidence$1: Manifest[X])Int
```

> **View bounds** require that the annotated type be implicitly convertible into some other one. The same rules apply as for context bounds. It is also simply syntactic sugar, which is desugared as follows:

> ```scala
scala> def f[X <% Int](g:Int) =  g
f: [X](g: Int)(implicit evidence$1: X => Int)Int
```

If type parameters of a generic function are constrained by context or type bounds and we would like to have the type inference algorithm deduce such type parameters through implicit metaprogramming, then we are definitely going to run into problems. Take for example the `fatherOf` example:

```scala
scala> def fatherOf[S,F](sonType : Manifest[S])
     |                  (implicit ev : Son[S,F], fatherType : Manifest[F])
     |                  = fatherType
scala> fatherOf(manifest[Johann])
res1: Manifest[Joshi] = Joshi
```

It would be reasonable to believe that the following is equivalent:

```scala
scala> def fatherOf[S,F : Manifest](sonType : Manifest[S])
     |                  (implicit ev : Son[S,F])
     |                  = manifest[F]
```

However, the context bound is desugared as an implicit parameter which is _prepended_ to the implicit parameter list:

```scala
fatherOf: [S, F](sonType: Manifest[S])(implicit evidence$1: Manifest[F], implicit ev:
 Son[S,F])Manifest[F]
```

Thus, the type inference algorithm, working eagerly from left to right, will deduce that `F` is `Nothing` and thus not work as expected.

#### There is no Prolog in your Scala

As much as we would love it, full-blown logic programming cannot be done directly using implicit metaprogramming. This is due to the way Scala handles ambiguities. In Prolog, it is perfectly OK to have multiple positive answers to a query. Scala, on the other hand, terminates search and backtracks as soon as an ambiguity is found - even if that means only nominal ambiguity and the values are in fact, the same.

Take for example the following example:

> We have information about some people and their family relations, Joe, Johann and Jill. We know that Joe is the son of Johann and the son of Jill; we know that Joe an Johann are male. Who is the father of Joe?

In Prolog, these facts are:

```prolog
son(joe,johann).
son(joe,jill).
male(joe).
male(johann).
```

With implicit metaprogramming in Scala:
```scala
implicit val s0 = new Son[Joe, Johann]
implicit val s1 = new Son[Joe, Jill]

implicit val m0 = new Male[Joe]
implicit val m1 = new Male[Johann]
```

In Prolog, we can directly express that `F` is the father of `S` if `S` is the son of `F` and `F` is male. Prolog handles ambiguity well.

```prolog
father(S,F) :- son(S, F), male(F).
```

```prolog
?- father(joe, F).
F = johann ?
yes
```

After processing the subclause `son(S,F)`, there will be two possible answers: `F = johann` and `F = jill`. Then, `male(F)` is evaluated, for which only `F = johann` remains.

Let us try transcribing this to implicits and making a query:

```scala
scala> def father[S,F](m : Manifest[S])(implicit s : Son[S,F], p : Male[F]) = p

scala> father(manifest[Joe])
<console>:18: error: ambiguous implicit values:
 both value s0 of type => Son[Joe,Johann]
 and value s1 of type => Son[Joe,Jill]
 match expected type Son[Joe,F]
              father(manifest[Joe])
                    ^
```

As the Scala compiler is trying to deduce a possible concrete type for `F`, the first (leftmost) expression encountered is `Son[Joe, F]`. As this in itself is ambiguous, implicit resolution fails. `Male[F]` is not even tried.

If we switch the order of the parameters, something similar happens as there are multiple known males.