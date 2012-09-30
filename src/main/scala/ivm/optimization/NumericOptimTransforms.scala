package ivm
package optimization

import expressiontree._
import Lifting._

import Numeric.Implicits._

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait NumericOptimTransforms {
  // Reassociation similarly to what is described in "Advanced Compiler Design and Implementation", page 337-...,
  // Sec 12.3.1 and Fig 12.6. We refer to their reduction rules with their original codes, like R1, R2, ..., R9.
  // We don't apply distributivity, nor (yet) rules not involving just Plus (i.e., involving Minus or Times).
  // XXX: We should abstract this code to also apply on other commutative and associative operations. However, there's only * at the moment.
  // Note that we don't check whether computation is being done on floating-point numbers - for them, we should perform
  // no simplification (Sec. 12.3.2).
  //
  // We make sure that right children are never Plus expressions and that constants are moved to the left (using rules R2 and R7).
  // All `Const` nodes are then constant-folded together (using rules R1 and R9).
  //
  // For instance, both (1 + x) + 2 and 1 + (x + 2) are rewritten to (1 + 2) + x
  // and then to 3 + x, conceptually (in practice,
  // such steps are fused together).
  //
  // Note: keep in mind that the transformation is applied bottom-up; moreover, whenever a new expression is built, new sums
  // are created by recursive invocation of buildSum, except in the default case.
  // Note 2: in their picture, "t_x" represents an arbitrary node, even in rule R7.
  // Note 3: rule R2 and R7 together seem not to form a terminating rewrite system; note however that
  // rule R7 does not just swap children but performs a tree rotation.
  // Note 4: we omit rules on subtractions (R5, R6, etc.) because NumericOps.- just always represent subtraction through
  // addition (XXX which is a pessimization if no constants are involved)
  def buildSum[T: Numeric](l: Exp[T], r: Exp[T]): Exp[T] = {
    r match {
      case rc @ Const(rV) =>
        implicit val cTag = rc.cTag.asInstanceOf[ClassTag[T]]
        implicit val tTag = rc.tTag.asInstanceOf[TypeTag[T]]
        l match {
          case Const(a) => //R1
            a + rV
          case Sym(Plus(Const(a), b)) => //R9 - must be before R2!
            buildSum(a + rV, b)
          case _ => //R2 - must be after R1!
            buildSum(r, l)
        }
      case Sym(Plus(rl, rr)) => //R7
        buildSum(buildSum(l, rl), rr)
      case _ =>
        l + r
    }
    /*
     * Rules R1 and R9 reduce the tree size by one, thus it is easy to prove that their application causes
     * well-founded recursion. Rules R2 and R7 are instead more problematic.
     * Why does the above recursive invocation in R2 terminate? This is obvious by case analysis unless l is a Plus
     * node. Otherwise, we need to prove that we either terminate recursion or that we reach either of rule R1 or
     * R9, reducing the input size. We must do the proof by induction, assuming that all calls to buildSum with
     * total input size smaller than the current one terminate.
     *
     * Let us assume (l, r) matches (Plus(l1, l2), Const(rV)); buildSum(b, a) will match
     * (r, l) against (Const(rV), Plus(l1, l2)) (R7), and rewrite it to buildSum(buildSum(Const(rV), l1), l2).
     * If l2 is not a Const node, it will not match again this case (rule R7) without further reduction.
     * If l2 is a Const node and the inner buildSum(Const(rV), l1) just returns Plus(Const(rV), l1), rule R9 applies
     * and reduces the input size.
     * If l2 is a Const node and the inner buildSum(Const(rV), l1) returns something else, then it must terminate
     * by the inductive hypothesis.
     */
  }

  //Copy-n-paste of buildSum. We don't use the distributive rule currently.
  def buildProd[T: Numeric](l: Exp[T], r: Exp[T]): Exp[T] = {
    r match {
      case rc @ Const(rV) =>
        implicit val cTag = rc.cTag.asInstanceOf[ClassTag[T]]
        implicit val tTag = rc.tTag.asInstanceOf[TypeTag[T]]
        l match {
          case Const(a) => //R1
            a * rV
          case Sym(Times(Const(a), b)) => //R9 - must be before R2!
            buildProd(a * rV, b)
          case _ => //R2 - must be after R1!
            buildProd(r, l)
        }
      case Sym(Times(rl, rr)) => //R7
        buildProd(buildProd(l, rl), rr)
      case _ =>
        l * r
    }
  }

  val reassociateOps: Exp[_] => Exp[_] = {
    case Sym(n@Negate(constNode@Const(c))) =>
      implicit val cTag = constNode.cTag
      implicit val tTag = constNode.tTag
      n.isNum.negate(c)
    case Sym(p@Plus(l, r)) =>
      buildSum(l, r)(p.isNum)
    case Sym(t@Times(l, r)) =>
      buildProd(l, r)(t.isNum)
    case e => e
  }
}
