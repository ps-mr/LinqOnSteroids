import scala.util.continuations._
trait Exp[T]

/*
 * Show how it is possible to capture functions operating on expressions.
 * @author Paolo Giarrusso
 */
object Lifting {
    //type Return[T1, Res] = T1 @cpsParam[Res, Exp[Res]]
    //implicit def project[T1, Res](t: Exp[T1]): Return[T1, Res] = shift {
    //implicit def inject[T1, Res](t: Return[T1, Res]): Exp[T1] = reset(t)
    //type Return[T1, Res] = T1 @cpsParam[Res, Call1[T1, Res]]
    implicit def project[T1, Res](t: Exp[T1]): T1 @cpsParam[Res, Call1[T1, Res]] = shift {
        k: (T1 => Res) => Call1(k, t)
    }
    //Kind-of-compiles (crashes), wrong type:
    //implicit def inject[T1, Res](t: T1 @cpsParam[T1, Call1[T1, Res]]): Exp[Res] = reset(t)
    implicit def inject[T1, Res](t: Res @cpsParam[Res, Call1[T1, Res]]): Exp[Res] = reset(t)
    //?
    //implicit def lift1[T1, Res](f: T1 => Res): Exp[T1 => Res] = 
    //implicit def callToFunc
}
object Using {
    import Lifting._
    val one: Exp[Int] = new AnyRef with Exp[Int]
    def plus1(a: Int) = a + 1
    //Code the user writes
    //def test = plus1(one) //fails
    //def test = inject(plus1(one)) //Error!

    //Code which should be produced through injection of implicits:
    def test = inject(plus1(project(one))) //'Compiles' (no error, crash).
    def test2 = inject(1 + (project(one))) //'Compiles' (no error, crash).

    //This code allows functions to be projected.
    //def test = (plus1 _)(one)
}

case class Call1[T1, Res](callfunc: T1 => Res, arg0: Exp[T1]) extends Exp[Res]


// vim: set ts=4 sw=4 et:
