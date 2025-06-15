
alternative title: infix notation is tail recursion for your brain

Make some disclaimers about non-science.

bunch of interesting thoughts:
x.f() vs f(x) is largely a matter of taste, but:

f xs =
    filter isEven
  $ map increment
  $ fold ...
  $ xs

vs

f xs =
  xs
   |> fold 
   |> map increment
   |> filter isEven

is not. Assuming you read left-to-write, top-to-bottom.
The former means you keep intermediate results in your brain.
The latter does not.
Compare with tail recursion.
You can avoid the growing stack effect by reading write-to-left, bottom-to-top.
State that obviously you can still learn how to read anything.

Interesting point: APL's brevity makes it less of a problem.

second point, names.

objection: infix doesn't matter because you can name things.
If there's a natural name, sure. If not, significant brain drain.
Less names are better.
Point-free.
It ends up sucking: why?
when you don't need linear chains, you're back to the "brain stack" problem.
Names help you anchoring that, but you don't have them in point-free.
Maybe make some reference to forth.

