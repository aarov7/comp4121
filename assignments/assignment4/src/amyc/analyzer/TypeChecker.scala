package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: TypeOrVar, expected: TypeOrVar, pos: Position)

    type TypeOrVar = Type | TypeVariable

    // Represents a type variable.
    // It is meant only for internal type checker use,
    // since no Amy value can have such type.
    case class TypeVariable private (id: Int)
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: TypeOrVar)(implicit env: Map[Identifier, TypeOrVar]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: TypeOrVar): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case StringLiteral(_) =>
          topLevelConstraint(BooleanType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)

        case Plus(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Minus(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Div(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Times(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Mod(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case LessThan(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case LessEquals(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case And(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case Or(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)

        case Call(qname, args) =>
          val (signature,constructor) = (table.getFunction(qname),table.getConstructor(qname)) match {
            case (Some(signature), None) => (signature, Constraint(signature.retType,expected,e.position))
            case (None,Some(signature)) => (signature, Constraint(ClassType(signature.parent), expected, e.position))
            case _ => throw new scala.MatchError(e)
          }
          constructor :: args.zip(signature.argTypes).flatMap(pair => genConstraints(pair._1, pair._2))

        case Sequence(e1, e2) =>
          val typeVariable = TypeVariable.fresh()
          topLevelConstraint(typeVariable) ++ genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, typeVariable)
        case Let(df,value, body) =>
          val typeVariable = TypeVariable.fresh()
          topLevelConstraint(typeVariable) ++ genConstraints(value, df.tt.tpe) ++ genConstraints(body, typeVariable)(env + (df.name -> df.tt.tpe))
        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          val typeVariable = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, typeVariable) ++ genConstraints(rhs, typeVariable)

        case Concat(lhs, rhs) =>
          topLevelConstraint(StringType) ++ genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType)

        case Variable(name) =>
          topLevelConstraint(env(name))
        case Not(e) =>
          topLevelConstraint(BooleanType) ++ genConstraints(e, BooleanType)
        case Neg(e) =>
          topLevelConstraint(IntType) ++ genConstraints(e, IntType)
        
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: TypeOrVar):
            (List[Constraint], Map[Identifier, TypeOrVar]) = //maybe change Type to TypeOrVar?
          {
            pat match {
              case WildcardPattern() => (List(), Map())
              case IdPattern(name) =>
                val fresh = TypeVariable.fresh()
                (List(Constraint(fresh, scrutExpected, pat.position)), Map(name -> fresh))
              case LiteralPattern(lit) => {
                lit match {
                  case IntLiteral(_) =>
                    (List(Constraint(IntType, scrutExpected, pat.position)), Map())
                  case BooleanLiteral(_) =>
                    (List(Constraint(BooleanType, scrutExpected, pat.position)), Map())
                  case StringLiteral(_) =>
                    (List(Constraint(StringType, scrutExpected, pat.position)), Map())
                  case UnitLiteral() =>
                    (List(Constraint(UnitType, scrutExpected, pat.position)), Map())
                }
              }
              case CaseClassPattern(constructor, args) =>
                val sig = table.getConstructor(constructor).get
                val parent = ClassType(sig.parent)
                val expected = sig.argTypes
                val patterns = args zip expected map (x => handlePattern(x._1, x._2))
                (Constraint(parent, scrutExpected, pat.position) :: patterns.unzip._1.flatten, patterns.unzip._2.flatMap(_.toList).toMap)
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: TypeOrVar): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case Error(msg) =>
          topLevelConstraint(expected) ++ genConstraints(msg, StringType)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: TypeOrVar): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: TypeOrVar, from: Int, to: TypeOrVar): TypeOrVar = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found,expected) match {
            case (TypeVariable(id), type2: TypeVariable) =>
              if(id == type2.id) {
                solveConstraints(more)
              } else {
                solveConstraints(subst_*(constraints, id, type2))
              }
            case (tpe, TypeVariable(id)) =>
              solveConstraints(subst_*(constraints,id, tpe))
            case (TypeVariable(_), _) =>
              solveConstraints(Constraint(expected, found, pos) :: more)
            case (type1, type2) if type1 == type2 =>
              solveConstraints(more)
            case _ =>
              error(s"Type error; expected $expected, found $found", pos)
              solveConstraints(more)
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
