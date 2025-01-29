package amyc
package parsing

import scala.language.implicitConversions
import amyc.ast.NominalTreeModule.*
import amyc.utils.*
import Token.*
import TokenKind.*
import scallion.*

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
  with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)

  def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }

  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr1) ~ "}").map {
    case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
  }

  lazy val definition: Syntax[ClassOrFunDef] = abstractClassDef | caseClassDef | funDef

  lazy val abstractClassDef: Syntax[ClassOrFunDef] =
    (kw("abstract") ~ kw("class") ~ identifier).map {
      case kw ~ KeywordToken("class") ~ id => AbstractClassDef(id).setPos(kw)
    }

  lazy val caseClassDef: Syntax[ClassOrFunDef] =
    (kw("case") ~ kw("class") ~ identifier ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifier).map {
      case kw ~ _ ~ name ~ _ ~ fields ~ _ ~ _ ~ parent => CaseClassDef(name, fields.map(p => p.tt), parent).setPos(kw)
    }

  lazy val funDef: Syntax[ClassOrFunDef] =
    (kw("def") ~ identifier ~ "(" ~ parameters ~ ")" ~ ":" ~ typeTree ~ "=" ~ "{" ~ expr1 ~ "}").map {
      case kw ~ id ~ _ ~ params ~ _ ~ _ ~ retType ~ _ ~ _ ~ body ~ _ => FunDef(id, params, retType, body).setPos(kw)
    }

  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  lazy val parameter: Syntax[ParamDef] = (identifier ~ ":" ~ typeTree).map {
    case id ~ _ ~ ty => ParamDef(id, ty)
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] = (identifier ~ opt("." ~ identifier)).map {
    case id1 ~ None => TypeTree(ClassType(QualifiedName(None, id1)))
    case id1 ~ Some(_ ~ id2) => TypeTree(ClassType(QualifiedName(Some(id1), id2)))
  }

  lazy val expr1: Syntax[Expr] = recursive {
    (kw("val") ~ parameter ~ "=" ~ expr2 ~ ";" ~ expr1).map {
      case kw ~ x ~ _ ~ e1 ~ _ ~ e2 => Let(x, e1, e2).setPos(kw)
    } | (expr2 ~ opt(moreExpr1)).map{
      case e1 ~ None => e1.setPos(e1)
      case e1 ~ Some(next) => Sequence(e1, next).setPos(e1)
    }
  }

  lazy val moreExpr1: Syntax[Expr] = (";" ~ expr1).map{ case _ ~ expr => expr}

  lazy val oneCase: Syntax[MatchCase] = (kw("case") ~ pattern ~ "=>" ~ expr1).map {
    case kw ~ p ~ DelimiterToken("=>") ~ e1 => MatchCase(p, e1).setPos(kw)
  }

  lazy val cases: Syntax[List[MatchCase]] = many1(oneCase).map(l => l.toList)

  lazy val expr2: Syntax[Expr] = recursive {
    (expr3 ~ many(moreExpr2)).map {
      case e3 ~ rest => rest.foldLeft(e3){
        case (acc, next) => Match(acc, next._1).setPos(next._2)
      }
    } | (kw("if") ~ "(" ~ expr1 ~ ")" ~ "{" ~ expr1 ~ "}" ~ kw("else") ~ "{" ~ expr1 ~ "}" ~ many(moreExpr2)).map{
      case s ~ _ ~ c1 ~ _ ~ _ ~ c2 ~ _ ~ _ ~ _ ~ c3 ~ _ ~ rest => rest.foldLeft(Ite(c1, c2, c3).setPos(s).asInstanceOf[Expr]){
        case (acc, next) => Match(acc, next._1).setPos(next._2)
      }
    }
  }

  lazy val moreExpr2: Syntax[(List[MatchCase], Token)] = (kw("match") ~ "{" ~ cases ~ "}").map{ case kw ~ DelimiterToken("{") ~ cases ~ DelimiterToken("}")  => (cases, kw)}
  //
  //  lazy val basic: Syntax[Expr] = identifier.up[Expr] | variableOrCall | literal.up[Expr] | "(".skip ~ intermediateExpr ~ ")".skip
  //
  //
  //  val binaryOp: Syntax[Token] = accept(OperatorKind(_)) {
  //    case tk@
  //  }
  //  lazy val left: Syntax[Expr] = infixLeft(basic, binaryOp)({
  //    case (l, "*", r) => Times(l, r)
  //    case (l, "/", r) => Div(l, r)
  //    case (l, "%", r) => Mod(l, r)
  //    case (l, "+", r) => Plus(l, r)
  //    case (l, "-", r) => Minus(l, r)
  //    case (l, "++", r) => Concat(l, r)
  //    case (l, "<", r) => LessThan(l, r)
  //    case (l, "<=", r) => LessEquals(l, r)
  //    case (l, "==", r) => Equals(l, r)
  //    case (l, "&&", r) => And(l, r)
  //    case (l, "||", r) => Or(l, r)
  //  })
  //
  //  lazy val intermExpr: Syntax[Expr] = recursive {
  //    operators(left)(
  //      times | div | mod is LeftAssociative
  //      plus | minus | concat is LeftAssociative
  //      less | lessEquals is LeftAssociative
  //      equals is LeftAssociative
  //      and is LeftAssociative
  //      or is LeftAssociative
  //    )({
  //      case (l, "*", r) => Times(l, r)
  //      case (l, "/", r) => Div(l, r)
  //      case (l, "%", r) => Mod(l, r)
  //      case (l, "+", r) => Plus(l, r)
  //      case (l, "-", r) => Minus(l, r)
  //      case (l, "++", r) => Concat(l, r)
  //      case (l, "<", r) => LessThan(l, r)
  //      case (l, "<=", r) => LessEquals(l, r)
  //      case (l, "==", r) => Equals(l, r)
  //      case (l, "&&", r) => And(l, r)
  //      case (l, "||", r) => Or(l, r)
  //    })
  //  }


  lazy val expr3: Syntax[Expr] = recursive {
    (expr4 ~ many(moreExpr3)).map{
      case e4 ~ rest => rest.foldLeft(e4) {
        case (acc, next) => Or(acc, next).setPos(acc)
      }
    }
  }
  lazy val moreExpr3: Syntax[Expr] = (op("||") ~ expr4).map{ case "||" ~ expr => expr}

  lazy val expr4: Syntax[Expr] = recursive {
    (expr5 ~ many(moreExpr4)).map{
      case e5 ~ rest => rest.foldLeft(e5) {
        case (acc, next) => And(acc, next).setPos(acc)
      }
    }
  }

  lazy val moreExpr4: Syntax[Expr] = (op("&&") ~ expr5).map{ case "&&" ~ expr => expr}

  lazy val expr5: Syntax[Expr] = recursive {
    (expr6 ~ many(moreExpr5)).map {
      case e6 ~ rest => rest.foldLeft(e6) {
        case (acc, next) => Equals(acc, next).setPos(acc)
      }
    }
  }

  lazy val moreExpr5: Syntax[Expr] = (op("==") ~ expr6).map{ case "==" ~ expr => expr}

  lazy val expr6: Syntax[Expr] = recursive {
    (expr7 ~ many(moreExpr6)).map{
      case e7 ~ rest => rest.foldLeft(e7) {
        case (acc, ("<", next) ) => LessThan(acc, next).setPos(acc)
        case (acc, ("<=", next)) => LessEquals(acc, next).setPos(acc)
      }
    }
  }

  lazy val moreExpr6: Syntax[(String, Expr)] =
    (op("<") ~ expr7).map{ case "<" ~ e7 => ("<", e7)}
      | (op("<=") ~ expr7).map{ case "<=" ~ e7 => ("<=", e7)}

  lazy val expr7: Syntax[Expr] = recursive {
    (expr8 ~ many(moreExpr7)).map{
      case e8 ~ rest => rest.foldLeft(e8) {
        case (acc, ("+", next)) => Plus(acc, next).setPos(acc)
        case (acc, ("-", next)) => Minus(acc, next).setPos(acc)
        case (acc, ("++", next)) => Concat(acc, next).setPos(acc)
      }
    }
  }

  lazy val moreExpr7: Syntax[(String, Expr)] =
    (op("+") ~ expr8).map { case "+" ~ e8 => ("+", e8) }
      | (op("-") ~ expr8).map { case "-" ~ e8 => ("-", e8) }
      | (op("++") ~ expr8).map { case "++" ~ e8 => ("++", e8) }

  lazy val expr8: Syntax[Expr] = recursive {
    (expr9 ~ many(moreExpr8)).map{
      case e9 ~ rest => rest.foldLeft(e9) {
        case (acc, ("*", next)) => Times(acc, next).setPos(acc)
        case (acc, ("/", next)) => Div(acc, next).setPos(acc)
        case (acc, ("%", next)) => Mod(acc, next).setPos(acc)
      }
    }
  }

  lazy val moreExpr8: Syntax[(String, Expr)] =
    (op("*") ~ expr9).map { case "*" ~ e9 => ("*", e9) }
      | (op("/") ~ expr9).map { case "/" ~ e9 => ("/", e9) }
      | (op("%") ~ expr9).map { case "%" ~ e9 => ("%", e9) }

  lazy val expr9: Syntax[Expr] = expr10
    | (op("-") ~ expr10).map{ case "-" ~ expr => Neg(expr).setPos(expr)}
    | (op("!") ~ expr10).map{ case "!" ~ expr => Not(expr).setPos(expr)}

  //  lazy val moreExpr9: Syntax[(Option[String], Expr)] =
  //    expr10.map{e10 => (None, e10)}
  //    | (op("-")~expr10).map { case op ~ e10 => (Some("-"), e10) }
  //    | (op("!") ~ expr10).map { case op ~ e10 => (Some("!"), e10) }

  lazy val variableOrCall: Syntax[Expr] = (identifierPos ~ opt(opt("." ~ identifier) ~ "(" ~ repsep(expr1, ",") ~ ")")).map {
    case id1 ~ None => Variable(id1._1).setPos(id1._2)
    case id1 ~ Some(None ~ DelimiterToken("(") ~ args ~ DelimiterToken(")")) => Call(QualifiedName(None, id1._1), args.toList).setPos(id1._2)
    case id1 ~ Some(Some(DelimiterToken(".") ~ id2) ~ DelimiterToken("(") ~ args ~ DelimiterToken(")")) => Call(QualifiedName(Some(id1._1), id2), args.toList).setPos(id1._2)
  }

  lazy val expr10: Syntax[Expr] = {
    (kw("error") ~ "(" ~ expr1 ~ ")").map{
      case kw ~ DelimiterToken("(") ~ e ~ DelimiterToken(")") => Error(e).setPos(kw)
    }
      | ("(" ~ opt(expr1) ~ ")").map{
      case o ~ Some(e) ~ DelimiterToken(")") => e.setPos(o)
      case o ~ None ~ DelimiterToken(")") => UnitLiteral().setPos(o)
    }
      | nonUnitLiteral.up[Expr]
      | variableOrCall
  }

  // A literal expression.
  lazy val literal: Syntax[Literal[_]] = accept(LiteralKind){
    case IntLitToken(x) => IntLiteral(x)
    case StringLitToken(x) => StringLiteral(x)
    case BoolLitToken(x) => BooleanLiteral(x)
    case  _ => UnitLiteral()
  }

  lazy val nonUnitLiteral: Syntax[Literal[_]] = accept(LiteralKind) {
    case IntLitToken(x) => IntLiteral(x)
    case StringLitToken(x) => StringLiteral(x)
    case BoolLitToken(x) => BooleanLiteral(x)
  }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | morePattern
  }

  lazy val morePattern: Syntax[Pattern] = recursive {
    (identifierPos ~ opt(opt("." ~ identifier)  ~ "(" ~ repsep(pattern, ",") ~ ")")).map{
      case id1 ~ Some(None ~ DelimiterToken("(") ~ patterns ~ DelimiterToken(")")) => CaseClassPattern(QualifiedName(None, id1._1), patterns.toList).setPos(id1._2)
      case id1 ~ Some(Some(DelimiterToken(".") ~ id2) ~ DelimiterToken("(") ~ patterns ~ DelimiterToken(")")) => CaseClassPattern(QualifiedName(Some(id1._1), id2), patterns.toList).setPos(id1._2)
      case id1 ~ None => IdPattern(id1._1).setPos(id1._2)
    }
  }

  lazy val literalPattern: Syntax[Pattern] = literal.map(l => LiteralPattern(l))

  lazy val wildPattern: Syntax[Pattern] = kw("_").map(kw => WildcardPattern().setPos(kw))


  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  //  lazy val simpleExpr: Syntax[Expr] = literal.up[Expr] | variableOrCall | ???
  //
  //  lazy val variableOrCall: Syntax[Expr] = ???


  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.


  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = true
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
