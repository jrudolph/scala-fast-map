/* Scala Fast Map Plugin
 * Copyright 2010 Johannes Rudolph
 */

package net.virtualvoid.scala

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.{ Transform, TypingTransformers }
import nsc.symtab.Flags

class FastMapPlugin(val global: Global) extends Plugin {
  import global._

  val name = "fast-map"
  val description = "A simple syntax to lift methods to the monad level"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with Transform with TypingTransformers {
    import global._
    import global.definitions._

    val global = FastMapPlugin.this.global
    override val runsAfter = List("parser")
    /** The phase name of the compiler plugin
     *  @todo Adapt to specific plugin.
     */
    val phaseName = "fast-map"

    def newTransformer(unit: CompilationUnit) = new FastMapTransformer(unit)
    
    val ext = "$up"
    val longExt = "_"+ext
    
    import java.util.regex.Pattern
    val SymbolicWithExt = ("(\\$.*)"+Pattern.quote(ext)).r
    val WordWithExt = ("(\\w+)"+Pattern.quote(longExt)).r
    object Extended {
      def unapply(nme: Name): Option[String] = nme.toString match {
        case SymbolicWithExt(inner) => Some(inner)
	case WordWithExt(inner) => Some(inner)
	case _ => None
      }
    }

    class FastMapTransformer(unit: CompilationUnit) extends Transformer {
      import unit.error

      def param(name: String) = ValDef(Modifiers(Flags.PARAM), name, TypeTree(), EmptyTree)
      
      override def transform(tree: Tree): Tree = tree match {
        case Apply(Select(exp, Extended(name)), Nil) =>
          Apply(Select(transform(exp), "map"), List(Function(List(param("it")), Select(Ident("it"), name))))
        case Select(exp, Extended(name)) =>
          println("Found "+tree)
          Apply(Select(transform(exp), "map"), List(Function(List(param("it")), Select(Ident("it"), name))))
        case Apply(Select(exp, Extended(name)), List(op2)) =>
          Apply(Select(transform(exp), "flatMap"),           // a.flatMap
                      List(Function(List(param("av")),       // av =>
                        Apply(Select(transform(op2), "map"), // b.map
                          List(Function(List(param("bv")),   // bv =>
                            Apply(Select(Ident("av"), name), List(Ident("bv")))))))))
        case _ => super.transform(tree)
      }
    }
  }
}
