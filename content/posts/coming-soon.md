---
title: Coming Soon! 
date: 2019-03-09
---

Blog posts are coming soon! Check my [other writings](/writings.html) in the meantime.

```scala
import cats._, implicits._
import cats.effect._, concurrent._
import cats.effect.implicits._
import fs2._
import scala.concurrent.duration._

object Playground extends IOApp {
  def run(args: List[String]) = ExitCode.Success.pure[IO]

  implicit class Runner[A](s: Stream[IO, A]) {
    def yolo: Unit = s.compile.drain.unsafeRunSync
  }

  def put[A](a: A): IO[Unit] = IO(println(a))

  def yo =
    Stream
      .repeatEval(put("hello"))
      .interruptAfter(2.seconds)
      .yolo
}
```

```haskell
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

fizzBuzz = mapM_ (print . f) [1..100]
  where f a = case (a `rem` 3, a `rem` 5) of
          (0,0) -> "FizzBuzz"
          (0,_) -> "Fizz"
          (_,0) -> "Buzz"
          _ -> show a


-- This works for any number of mappings.
genericFizzBuzz =
  let divs = (3,"Fizz") : (5,"Buzz") : (7,"Boom") : []
      list = [1..200]
  in mapM_ print $ fb divs list


fb :: [(Integer, String)] -> [Integer] -> [String]
fb spec = fmap $ fromMaybe <$> show <*> matcher
  where
    matcher = foldMap toMatcher spec
    toMatcher (i, s) x = s <$ guard (x `rem` i == 0)
```
