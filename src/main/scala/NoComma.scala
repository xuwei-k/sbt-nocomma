package sbtnocomma

import sbt._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object NoComma {
  def nocomma(a: SettingsDefinition): Vector[SettingsDefinition] =
    macro nocommaImpl

  def nocommaImpl(c: Context)(
      a: c.Expr[SettingsDefinition]): c.Expr[Vector[SettingsDefinition]] = {
    import c.universe._
    val items: List[Tree] = a.tree match {
      case Block(stats, x) => stats ::: List(x)
      case x               => List(x)
    }
    c.Expr[Vector[SettingsDefinition]](
      Apply(Select(reify(Vector).tree, TermName("apply")), items))
  }

  /*
  def nocomma[A](a: A): Vector[A] = macro nocommaImpl[A]

  def nocommaImpl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]) : c.Expr[Vector[A]] = {
    import c.universe._
    val items: List[Tree] = a.tree match {
      case Block(stats, x) => stats ::: List(x)
      case x               => List(x)
    }
    c.Expr[Vector[A]](
      Apply(
        Select(reify(Vector).tree, TermName("apply")),
        items))
  }
 */
}
