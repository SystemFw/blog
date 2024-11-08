```scala
// Thing
trait Animal
case class Cat() extends Animal
case class Dog() extends Animal

// Contravariant Thing
case class Vet[-A]() {
 def cures(a: A): Unit = ()
}

/*
 Thing that takes a contravariant thing, this is key to show the
 subtyping relationship that contravariance expresses, i.e
 A >: B ==> F[A] <: F[B]
*/

def dogClinic(vet: Vet[Dog], d: Dog): Unit = vet.cures(d)

val drDoLittle: Vet[Animal] = Vet[Animal]()

drDoLittle.cures(Cat())

dogClinic(drDoLittle, Dog())

// we were able to pass a `Vet[Animal]` to a place that expects `Vet[Dog]`,
// i.e. `Vet[Animal] <: Vet[Dog], although `Dog <: Animal`
```
or, in layfolk logic: if I have a pack of dogs, I also have a pack of animals, but if I have a pack of animals, it's not a given that I have a pack of dogs .That's covariance.
If I can raise all animals, I can also raise dogs, but just because I can raise dogs, it doesn't mean I can raise all animals. That's contravariance. 

did you mean to say A >: B ==> F[A] <: F[B]

yes, Animal is a supertype of Dog, and Vet[Animal] is a subtype of Vet[Dog], in the sense that all vets that can cure all animals are also vets that can cure dogs 
SystemFw — Yesterday at 21:46
you can also say:
A <: B ==> F[B] <: F[A]

if you want it to be symmetric with covariance

SystemFw — Yesterday at 21:55
so reading it in this form it would be:
vet being contravariant means that all dogs are animals implies vets that can cure all animals can cure dogs 
as opposed to vets that can cure dogs can cure all animals, which is covariance (and doesn't make sense in this example) 
★☆ uli :3 ☆★ — Yesterday at 22:15
okay i get it
