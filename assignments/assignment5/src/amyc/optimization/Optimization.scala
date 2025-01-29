import amyc.codegen.{Utils => CodeGenUtils}
import amyc.utils.{Context, Pipeline}
import amyc.wasm.Instructions._
import amyc.wasm._

import scala.io.StdIn

object Optimization extends Pipeline[Module, Unit] {
  override def run(ctx: Context)(m: Module): Unit = {

    val (mainMethod, methods) = (m.functions.filter(_.isMain).head, m.functions.filter(_.isMain) ::: m.functions.filter(!_.isMain))
    val instMem: Array[Instructions.Instruction] = methods.flatMap(function => (function.code <:> Return).instructions).toArray

    
  }
}