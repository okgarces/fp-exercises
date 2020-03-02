package exercises.function

import exercises.function.HttpClientBuilder
import exercises.function.HttpClientBuilder._

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Random

// you can run and print things here
object FunctionApp extends App {
  import FunctionExercises._

  println("Hello World!")
}

object FunctionExercises {

  ////////////////////////////
  // 1. first class functions
  ////////////////////////////

  // 1a. Implement `isEven` a function that checks if a number is even
  // such as isEven(2) == true
  // but     isEven(3) == false
  // Note: You can use `x % 2` for x modulo 2
  def isEven(x: Int): Boolean =
    x % 2 == 0

  // 1b. Now, we are going to experiment with functions val syntax.
  // Implement `isEvenVal` which behaves exactly like `isEven`.
  val isEvenVal: Int => Boolean =
  (x: Int) => x % 2 == 0


  // 1c. Implement `isEvenDefToVal` by transforming `isEven` def function into a val.
  // Note: This transformation (def to val) is called eta expansion. There is a syntax for it.
  val isEvenDefToVal: Int => Boolean = isEven


  // 1d. Implement `keepEvenNumbers` which removes all the odd numbers from a list
  // such as keepEvenNumbers(List(1,2,3,4)) == List(2,4)
  // Note: You can use `filter` method from `List`
  def keepEvenNumbers(xs: List[Int]): List[Int] =
    xs.filter(isEven)

  // 1e. Implement `keepNumbersSmallThan` which removes all the numbers above a threshold
  // such as keepNumbersSmallThan(List(1,6,3,10))(3) == List(1,3)
  // Try to define a predicate function inline, e.g. xs.filter(x => x == 0)
  def keepNumbersSmallThan(xs: List[Int])(threshold: Int): List[Int] =
    xs.filter((x: Int) => x < threshold)

  // 1f. Implement `move` which increases or decreases a number based on a `Direction` (enumeration)
  // such as move(Up)(5) == 6
  // but     move(Down)(5) == 4
  sealed trait Direction
  case object Up   extends Direction
  case object Down extends Direction

  def move(direction: Direction)(x: Int): Int =
    direction match {
      case Up => x + 1
      case Down => x - 1
      case _ => x
    }

  // 1g. Implement `increment` and `decrement` by reusing `move`
  // such as increment(10) == 11
  // such as decrement(10) == 9
  val increment: Int => Int = (x) => move(Up)(x)

  val decrement: Int => Int = (x) => move(Down)(x)

  ////////////////////////////
  // 2. polymorphic functions
  ////////////////////////////
  case class Pair[A](first: A, second: A) {
    // 2a. Implement `map` which applies a function to `first` and `second`
    // such as Pair("John", "Doe").map(_.length) == Pair(4,3)
    def map[B](f: A => B): Pair[B] =
      Pair(f(first), f(second))
  }

  val zero: Pair[Int]        = Pair(0, 0)
  val fullName: Pair[String] = Pair("John", "Doe")

  // 2b. Implement `mapOption` which applies a function to an Option if it is a `Some`.
  // Use patter matching on Option (see `sizeOption`) instead of using Option API
  // such as mapOption(Some(2), isEven)    == Some(true)
  //         mapOption(Some(2), increment) == Some(3)
  // but     mapOption(Option.empty[Int], increment) == None
  // Note: Option is a enumeration with two constructors `Some` and `None`.
  def mapOption[A, B](option: Option[A], f: A => B): Option[B] =
    option match {
      case Some(value) => Option(f(value))
      case None => None
    }

  def sizeOption[A](option: Option[A]): Int =
    option match {
      case None    => 0
      case Some(a) => 1
    }

  // 2c. What is the difference between `mapOption` and `mapOption2`?
  // Which one should you use?
  // I should use mapOption2 due to group of parameters. You can apply partial functions
  def mapOption2[A, B](option: Option[A])(f: A => B): Option[B] =
    mapOption(option, f)

  // 2d. Implement `identity` which returns its input unchanged
  // such as identity(1) == 1
  //         identity("foo") == "foo"
  def identity[A](x: A): A = x

  // 2e. Implement `identityVal` a function which behaves like `identity` but it is a val instead of a def.
  // This works because the eta-abstraction
  val identityVal2 = (x: Any) => identity(x)
  val identityVal = identity _

  // 2f. Implement `const` which returns its first input unchanged and discards its second input
  // such as const(5)("foo") == 5
  // For example, you can use const in conjunction with `map` to set the values in a List or String:
  // List(1,2,3).map(const(0)) == List(0,0,0)
  // "FooBar86".map(const(*))  == "********"
  def const[A, B](a: A)(b: B): A = a

  // 2g. Implement `andThen` and `compose` which pipes the result of one function to the input of another function
  // such as compose(isEven, increment)(10) == false
  // and     andThen(increment, isEven)(10) == false
  def andThen[A, B, C](f: A => B, g: B => C): A => C = {
    a => g(f(a))
  }
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  // 2h. Implement `doubleInc` using `inc`, `double` with `compose` or `andThen`
  // such as `doubleInc` is equivalent to the maths function: f(x) = (2 * x) + 1
  val double: Int => Int = x => 2 * x

  val doubleInc: Int => Int = compose(increment, double)
  // 2i. Implement `incDouble` using `inc`, `double` with `compose` or `andThen`
  // such as `incDouble` is equivalent to the maths function: f(x) = 2 * (x + 1)
  val incDouble: Int => Int = andThen(increment, double)

  // 2j. inc and double are a special case of function where the input and output type is the same.
  // These functions are called endofunctions.
  // Endofunctions are particularly convenient for API because composing two endofunctions give you an endoufunction
  // Can you think of a common design pattern that relies on endofunctions?
  type Endo[A] = A => A
  def composeEndo[A](f: Endo[A], g: Endo[A]): Endo[A] = f compose g

  ///////////////////////////
  // 3. Recursion & Laziness
  ///////////////////////////

  // 3a. Implement `sumList` using an imperative approach (while, for loop)
  // such as sumList(List(1,5,2)) == 8
  def sumList(xs: List[Int]): Int = {
    var toReturn = 0
    xs.foreach((x: Int) => toReturn = toReturn + x)
    toReturn
  }

  // 3b. Implement `mkString` using an imperative approach (while, for loop)
  // such as mkString(List('H', 'e', 'l', 'l', 'o')) == "Hello"
  def mkString(xs: List[Char]): String = {
    var toReturn = ""
    xs.foreach((x: Char) => toReturn = toReturn.appended(x))
    toReturn
  }

  // 3c. Implement `sumList2` using recursion (same behaviour than `sumList`).
  // Does your implementation work with a large list? e.g. List.fill(1000000)(1)
  def sumList2(xs: List[Int]): Int = {
    @scala.annotation.tailrec
    def func(xsApplied: List[Int], acc: Int): Int ={
      xsApplied match {
        case ::(head, next) => func(next, acc + head)
        case Nil => acc
      }
    }
    func(xs,0)
  }

  ///////////////////////
  // GO BACK TO SLIDES
  ///////////////////////
  // foldLeft Outside to Inside => ((( true && true) && true ) && false)
  def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = {
    var acc = b
    fa.foreach((a) => acc = f(acc, a))
    acc
  }

  @scala.annotation.tailrec
  def foldLeftRec[A, B](xs: List[A], b: B)(f: (B, A) => B): B =
    xs match {
      case Nil    => b
      case h :: t => foldLeftRec(t, f(b, h))(f)
    }

  def sumList3(xs: List[Int]): Int =
    foldLeft(xs, 0)(_ + _)

  // 3d. Implement `mkString2` using `foldLeft` (same behaviour than `mkString`)
  def mkString2(xs: List[Char]): String = foldLeft(xs, "")(_+_)

  // 3e. Implement `multiply` using `foldLeft`
  // such as multiply(List(3,2,4)) == 3 * 2 * 4 = 24
  // and     multiply(Nil) == 1
  def multiply(xs: List[Int]): Int = foldLeft(xs, 1)(_*_)

  // 3f. Implement `forAll` which checks if all elements in a List are true
  // such as forAll(List(true, true , true)) == true
  // but     forAll(List(true, false, true)) == false
  // does your implementation terminate early? e.g. forAll(List(false, false, false)) does not go through the entire list
  // does your implementation work with a large list? e.g. forAll(List.fill(1000000)(true))
  def forAll(xs: List[Boolean]): Boolean = foldLeft(xs, true)(_ && _)

  // 3g. Implement `find` which returns the first element in a List where the predicate function returns true
  // such as find(List(1,3,10,2,6))(_ > 5) == Some(10)
  // but     find(List(1,2,3))(_ > 5) == None
  // does your implementation terminate early? e.g. find(List(1,2,3,4)(_ == 2) stop iterating as soon as it finds 2
  // does your implementation work with a large list? e.g. find(1.to(1000000).toList)(_ == -1)
  def find[A](xs: List[A])(predicate: A => Boolean): Option[A] = {
    foldLeft(xs, Option.empty[A]) { (x: Option[A], y: A) =>
      if (predicate(y)) Option.apply[A](y)
      else x
    }
  }
  // Now the find method returns early
  @scala.annotation.tailrec
  def findEarly[A](xs: List[A])(predicate: A => Boolean): Option[A] = {
    xs match {
      case ::(head, next) => if(predicate(head)) Option.apply(head) else findEarly(next)(predicate)
      case Nil => Option.empty
    }
  }

  ///////////////////////
  // GO BACK TO SLIDES
  ///////////////////////

  def foldRight[A, B](xs: List[A], b: B)(f: (A, => B) => B): B =
    xs match {
      case Nil    => b
      case h :: t => f(h, foldRight(t, b)(f))
    }

  // FoldRight -> From Inside to Outside (true && ( true && ( true && nil)))
  // 3h. Implement `forAll2` using `foldRight` (same behaviour than `forAll`)
  def forAll2(xs: List[Boolean]): Boolean = foldRight(xs, true)((a, acc) => a && acc)

  // 3i. Implement `headOption` using `foldRight`.
  // `headOption` returns the first element of a List if it exists
  // such as headOption(List(1,2,3)) == Some(1)
  // but     headOption(Nil) == None
  // In worksheet does not work None and Some
  def headOption[A](xs: List[A]): Option[A] =
    foldRight(xs, Option.empty[A]){
      (a, acc) =>
        a match {
          case Nil => acc
          case value => Option.apply(value)
        }
    }

  // 3j. What fold (left or right) would you use to implement `min`? Why?
  def minFoldLeft(xs: List[Int]): Option[Int] =
    foldLeft(xs, Option.empty[Int]){
      (a: Option[Int], acc: Int) =>
        a match {
          case None => Option.apply(acc)
          case value if value.get >= acc => Option.apply(acc)
          case value if value.get < acc => value
        }
    }
  def minFoldRight(xs: List[Int]): Option[Int] =
    foldRight(xs, Option.empty[Int]){
      (a: Int, acc) =>
        a match {
          case value if acc == Option.empty[Int] => Option.apply(value)
          case value if value < acc.get => Option.apply(value)
          case _ => acc
        }
    }

  // 3k. Run `isEven` or `isOdd` for small and large input.
  // Search for mutual tail recursion in Scala.
  // Could be used using methods such as trampolines and continuations
  def isEvenRec(x: Int): Boolean =
    if (x > 0) isOddRec(x - 1)
    else if (x < 0) isOddRec(x + 1)
    else true

  def isOddRec(x: Int): Boolean =
    if (x > 0) isEvenRec(x - 1)
    else if (x < 0) isEvenRec(x + 1)
    else false

  // 3l. What happens when we call `foo`? Search for General recursion
  // or read https://www.quora.com/Whats-the-big-deal-about-recursion-without-a-terminating-condition
  @scala.annotation.tailrec
  def foo: Int = foo

  ////////////////////////
  // 4. PurInstropectiore functions
  ////////////////////////

  // 4a. is `plus` a pure function? why?
  def plus(a: Int, b: Int): Int = a + b

  // 4b. is `div` a pure function? why?
  def div(a: Int, b: Int): Int =
    if (b == 0) sys.error("Cannot divide by 0")
    else a / b

  // 4c. is `times2` a pure function? why?
  var counterTimes2 = 0
  def times2(i: Int): Int = {
    counterTimes2 += 1
    i * 2
  }

  // 4d. is `boolToInt` a pure function? why?
  def boolToInt(b: Boolean): Int =
    if (b) 5
    else Random.nextInt() / 2

  // 4e. is `mapLookup` a pure function? why?
  def mapLookup(map: Map[String, Int], key: String): Int =
    map(key)

  // 4f. is `times3` a pure function? why?
  def times3(i: Int): Int = {
    println("do something here") // could be a database access or http call
    i * 3
  }

  // 4g. is `circleArea` a pure function? why?
  val pi = 3.14
  def circleArea(radius: Double): Double =
    radius * radius * pi

  // 4h. is `inc` or inc_v2 a pure function? why?
  def inc_v2(xs: Array[Int]): Unit =
    for { i <- xs.indices } xs(i) = xs(i) + 1

  // 4i. is `incAll` a pure function? why?
  def incAll(value: Any): Any = value match {
    case x: Int    => x + 1
    case x: Long   => x + 1
    case x: Double => x + 1
  }

  // 4j. is `incAll_v`2 a pure function? why?
  def incAll_v2(value: Any): Any = value match {
    case x: Int    => x + 1
    case x: Long   => x + 1
    case x: Double => x + 1
    case _         => 0
  }

  // 4k. is `sum` a pure function? why?
  def sum(xs: List[Int]): Int = {
    var acc = 0
    xs.foreach(x => acc += x)
    acc
  }

  ////////////////////////
  // 5. Memoization
  ////////////////////////

  // 5a. Implement `memoize` such as
  // val cachedInc = memoize((_: Int) + 1)
  // cachedInc(3) // 4 calculated
  // cachedInc(3) // from cache
  // see https://medium.com/musings-on-functional-programming/scala-optimizing-expensive-functions-with-memoization-c05b781ae826
  // or https://github.com/scalaz/scalaz/blob/series/7.3.x/tests/src/test/scala/scalaz/MemoTest.scala
  def memoize[A, B](f: A => B): A => B = ???

  // 5b. How would you adapt `memoize` to work on recursive function e.g. fibonacci
  // can you generalise the pattern?
  def memoize2 = ???

}
