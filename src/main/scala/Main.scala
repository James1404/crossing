import scala.collection.mutable.ArrayBuffer
import scala.compiletime.ops.double

type ReservedSymbol = '=' | ':' | ';' | '(' | ')' | '{' | '}'
type ReservedKeyword = "fn" | "type" | "if" | "else" | "match"
  
enum TokenType {
  case Invalid
  case Identifier(id: String)
  case Number(v: Int | Float)
  case Keyword(ty: ReservedKeyword)
  case Symbol(ty: ReservedSymbol)
  case EOF
}

case class Token(val line: Int, val loc: Int, val ty: TokenType):
  def precedence: Int = ty match
    case _ => -1

@main def start: Unit =
  val src = io.Source.fromFile("./test.crs").mkString;
  val lexer = Lexer(src);
  lexer.run;
  lexer.tokens.foreach((t) => println(t.ty));
  
  println("Running parser")
  val parser = Parser(lexer.tokens);
  val ast = parser.run;
  println(ast);

class Lexer(val source: String) {
  private var idx: Int = 0;
  private var line: Int = 0;
  val tokens = ArrayBuffer[Token]();
  
  private def safe = idx < source.length;
  private def current: Char = if safe then source.charAt(idx) else 0.toChar;
  private def peek: Char = if safe then source.charAt(idx + 1) else 0.toChar;
  private def advance = idx += 1;
  
  private def isAlpha(c: Char) = !(c.isDigit || c.isWhitespace) && !c.isInstanceOf[ReservedSymbol]
  
  def run =
    while safe do {
      val start = idx;
      current match
        case '/' if peek == '/' => while current != '\n' do advance // single line comment
        case c if c.isWhitespace => advance // skip whitespace
        case c if c.isDigit =>
          while current.isDigit || current == '.' do advance
          tokens.append(Token(line, idx, TokenType.Number(source.substring(start, idx).toFloat)))
        case c if isAlpha(c) =>
          while isAlpha(current) do advance
          val id = source.substring(start, idx)
          
          if id.isInstanceOf[ReservedKeyword] then
            tokens.append(Token(line, idx, TokenType.Keyword(id.asInstanceOf[ReservedKeyword])))
          else
            tokens.append(Token(line, idx, TokenType.Identifier(id)))
        case c =>
          advance

          if c.isInstanceOf[ReservedSymbol] then
            tokens.append(Token(line, idx, TokenType.Symbol(c.asInstanceOf[ReservedSymbol])))
          else
            tokens.append(Token(line, idx, TokenType.Identifier(c.toString)))
    }

    tokens.append(Token(line, idx, TokenType.EOF))
}

enum AST {
  case Invalid

  case Global(items: Array[AST])
  case Scope(items: Array[AST])

  case NumberLiteral(num: Token)
  case StringLiteral(str: Token)
  case Identifier(id: Token)

  case List(items: Array[AST])
  
  case InfixOperator(lhs: AST, op: AST, rhs: AST)
  
  case FnDecleration(id: AST, params: Array[AST], body: AST)
  
  case TypeDecleration(variants: Array[AST])
  case TypeRecord(variants: Array[(AST, AST)])
  case TypeAlias(ty: AST)
  
  case IfExpression(cond: AST, true_block: AST, false_block: AST)
  
  case MatchCase(pattern: AST, value: AST)
  case MatchExpression(value: AST, patterns: Array[MatchCase])
}

class LangError(msg: String);
val errors = ArrayBuffer[LangError]();

class Parser(tokens: ArrayBuffer[Token]) {
  var idx = 0;

  private def safe = idx < tokens.length - 1;
  private def current: Token = tokens(idx);
  private def peek: Token = tokens(idx + 1);
  private def advance: Unit = if safe then idx += 1;
  private def advance_if(expected: TokenType) =
    if safe then
      val b = current.ty == expected;
      if b then advance
      b
    else false
  
  def value: AST = 
    val c = current
    c.ty match
      case TokenType.Number(_) => advance; AST.NumberLiteral(c)
      case TokenType.Identifier(_) =>
        advance
        AST.Identifier(c)
      case TokenType.Symbol('(') => advance; expr(TokenType.Symbol(')'))
      case _ =>
        errors.addOne(LangError("Invalid Token"))
        AST.Invalid
  
  // def expression(node: AST, min_precedence: Int): AST =
  //   var lhs = node;
  //   var lookahead = current;
  //   while lookahead.precedence >= min_precedence do
  //     var op = lookahead
  //     advance
  //     var rhs = value
  //     lookahead = current;
  //     while lookahead.precedence > op.precedence do
  //       rhs = expression(rhs, op.precedence + (if lookahead.precedence > op.precedence then 1 else 0))
  //       lookahead = current
  //     lhs = AST.Binary(lhs, op, rhs)
  //   lhs
  
  def identifier =
    val id = AST.Identifier(current)
    advance
    id
  
  def expr(until: TokenType) =
    var list = ArrayBuffer[AST]();
    while current.ty != until do
      list.addOne(value)    
    
    advance
    AST.List(list.toArray)

  def fn_decleration: AST =
    if current.ty == TokenType.Keyword("fn") then
      advance
      val id = identifier;
      var params = ArrayBuffer[AST]();
      while current.ty != TokenType.Symbol('=') do params.addOne(identifier)
      advance
      val body = expr;
      
      AST.FnDecleration(id, params.toArray, expr(TokenType.Symbol(';')))
    else
      AST.Invalid
  
  def type_decleration: AST = AST.Invalid
    if current.ty == TokenType.Keyword("type") then
      advance
      val id = identifier;
      AST.Invalid
    else
      AST.Invalid
  
  def toplevel: AST =
    val scope = ArrayBuffer[AST]();
    while safe do
      current.ty match
        case TokenType.Keyword("fn") => scope.addOne(fn_decleration)
        case TokenType.Keyword("type") => scope.addOne(type_decleration)
        case _ => ()
    
    AST.Global(scope.toArray);
    
  def run = toplevel
}

class Typechecker(tree: AST) {
}