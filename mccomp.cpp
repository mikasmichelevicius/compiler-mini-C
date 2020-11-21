#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::sys;

FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns one of these for known things.
enum TOKEN_TYPE {

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  // TRUE   = -12,     // "true"
  // FALSE   = -13,     // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than




  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
  int type = -100;
  std::string lexeme;
  int lineNo;
  int columnNo;
};

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

extern std::string indentation = "";

static TOKEN returnTok(std::string lexVal, int tok_type) {
  TOKEN return_tok;
  return_tok.lexeme = lexVal;
  return_tok.type = tok_type;
  return_tok.lineNo = lineNo;
  return_tok.columnNo = columnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok() {

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    if (LastChar == '\n' || LastChar == '\r') {
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(LastChar) ||
      (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    IdentifierStr = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
      IdentifierStr += LastChar;
      columnNo++;
    }

    if (IdentifierStr == "int")
      return returnTok("int", INT_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "float")
      return returnTok("float", FLOAT_TOK);
    if (IdentifierStr == "void")
      return returnTok("void", VOID_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "extern")
      return returnTok("extern", EXTERN);
    if (IdentifierStr == "if")
      return returnTok("if", IF);
    if (IdentifierStr == "else")
      return returnTok("else", ELSE);
    if (IdentifierStr == "while")
      return returnTok("while", WHILE);
    if (IdentifierStr == "return")
      return returnTok("return", RETURN);
    if (IdentifierStr == "true") {
      BoolVal = true;
      return returnTok("true", BOOL_LIT);
    }
    if (IdentifierStr == "false") {
      BoolVal = false;
      return returnTok("false", BOOL_LIT);
    }

    return returnTok(IdentifierStr.c_str(), IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", EQ);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", COMMA);
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, FLOAT_LIT);
      } else { // Integer : [0-9]+
        IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", AND);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", int('&'));
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", OR);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", int('|'));
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", NE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", LE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", GE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if (LastChar == '/') { // definitely a comment
      do {
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    columnNo++;
    return returnTok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, int(ThisChar));
}

static std::string boolToString(bool b) {
  if (b) {
    return "true";
  }
  return "false";
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const {};
};

/// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"IntegerLiteral 'int', value: "+std::to_string(Val);
    indentation.resize(indentation.size()-3);
    return ret;
    // return std::to_string(Val);
  }
};

class FloatASTnode : public ASTnode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTnode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"FloatLiteral 'float', value: "+std::to_string(Val);
    indentation.resize(indentation.size()-3);
    return ret;
    // return std::to_string(Val);
  }
};

class BoolASTnode : public ASTnode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTnode(TOKEN tok, bool val) : Val(val), Tok(tok), Name(boolToString(val)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    //return std::to_string(Val);
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"BooleanLiteral 'bool', value: "+Name;
    indentation.resize(indentation.size()-3);
    return ret;

    // return Name;
  }
};

class SemicolASTnode : public ASTnode {
  std::string Semicol;

public:
  SemicolASTnode(TOKEN tok) : Semicol(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return "";
    // return Semicol+"\n";
  }
};

class IdentASTnode : public ASTnode {
  TOKEN Tok;
  std::string Name;
public:
  IdentASTnode(TOKEN tok) : Tok(tok), Name(tok.lexeme.c_str()) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"FuncOrVarIdentity 'ident', value: '"+Name+"'";
    indentation.resize(indentation.size()-3);
    return ret;
    // return Name;
  }

  std::string get_id_name() const {
    return Name;
  }
  // const std::string &getName() const {return Name;}
};

class NegativeASTnode : public ASTnode {
  char Op;
  TOKEN Tok;
  std::unique_ptr<ASTnode> Expr;
  std::string Name;

public:
  NegativeASTnode(TOKEN tok, char op, std::unique_ptr<ASTnode> expr) : Op(op), Tok(tok), Expr(std::move(expr)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"NegOfExpression, prefix: '" + std::string(1, Op) +"'"+Expr->to_string().c_str()+std::string(1,')');
    indentation.resize(indentation.size()-3);
    return ret;
    // return std::string(1,'(')+std::string(1, Op)+Expr->to_string().c_str()+std::string(1,')');
  }
};

class ExpressionASTnode : public ASTnode {
  std::string Op;
  std::unique_ptr<ASTnode> LHS;
  std::unique_ptr<ASTnode> RHS;
  std::string Name;

public:
  ExpressionASTnode(TOKEN op, std::unique_ptr<ASTnode> lhs, std::unique_ptr<ASTnode> rhs) : Op(op.lexeme.c_str()), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"ArithmeticExpr, binary operator: '"+Op+"'";
    indentation = indentation + "   ";
    ret = ret + "\n"+indentation+"├── "+"LeftHandSide of expression: "+LHS->to_string()+"\n"+indentation+"├── "+"RightHandSide of expression: "+RHS->to_string().c_str();
    indentation.resize(indentation.size()-6);
    return ret;
    // return LHS->to_string().c_str()+Op+RHS->to_string().c_str();

  }
};

class ExprEnclosedASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;

public:
  ExprEnclosedASTnode(std::unique_ptr<ASTnode> expr) : Expr(std::move(expr)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"Expression in parantheses: " + Expr->to_string();
    indentation.resize(indentation.size()-3);
    return ret;
    // return std::string(1,'(')+Expr->to_string().c_str()+std::string(1,')');
  }
};

class ExprSemicolASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;
  std::unique_ptr<ASTnode> Semicol;
public:
  ExprSemicolASTnode(std::unique_ptr<ASTnode> expr, std::unique_ptr<ASTnode> semicol) : Expr(std::move(expr)), Semicol(std::move(semicol)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Expr->to_string().c_str();
    // return Expr->to_string().c_str()+std::string(Semicol->to_string().c_str());
  }
};

class AssignASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Id;
  std::unique_ptr<ASTnode> Assignment;

public:
  AssignASTnode(std::unique_ptr<ASTnode> ident_node, std::unique_ptr<ASTnode> assignment) : Id(std::move(ident_node)), Assignment(std::move(assignment)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"AssignmentExpr, binary operator: '" + std::string(1,'=') + "'";
    indentation = indentation + "   ";
    ret = ret + "\n"+indentation+"├── "+"IdentityOfAssignment: " + Id->to_string().c_str() + "\n"+indentation+"├── "+"Assignment expression:" + Assignment->to_string().c_str();
    indentation.resize(indentation.size()-6);
    return ret;
    // return Id+std::string(1,'=')+Assignment->to_string().c_str();
  }
};

class FunCallASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Id;
  std::vector<std::unique_ptr<ASTnode>> Args;
  std::string Callee;
public:
  FunCallASTnode(std::unique_ptr<ASTnode> ident_node, std::vector<std::unique_ptr<ASTnode>> args, TOKEN tok) : Id(std::move(ident_node)), Args(std::move(args)), Callee(tok.lexeme.c_str()) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"FunctionCall Expression. Function call identity: '" + Id->to_string() + "'";

    if (Args.size()>0) {
      ret = ret + "\n"+indentation+"├── "+"Arguments of function call:";
      indentation = indentation + "   ";
      for (unsigned i=0; i<Args.size(); i++) {
        ret = ret + Args.at(i)->to_string().c_str();
      }
    }
    indentation.resize(indentation.size()-6);

    return ret;
  }
};

class VarTypeASTnode : public ASTnode {
  std::string Type;
public:
  VarTypeASTnode(TOKEN tok) : Type(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"VarOrFuncType, value: '" + Type + "'";
    indentation.resize(indentation.size()-3);
    return ret;
    // return Type+" ";
  }
  std::string get_var_type() const {
    return Type;
  }
};

class VoidASTnode : public ASTnode {
  std::string Type;
public:
  VoidASTnode(TOKEN tok) : Type(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"VoidFuncType, value: '" + Type + "'";
    indentation.resize(indentation.size()-3);
    return ret;
    // return Type;
  }
};

class LocalDeclASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Type;
  std::unique_ptr<ASTnode> Id;
  std::unique_ptr<ASTnode> Semicol;
public:
  LocalDeclASTnode(std::unique_ptr<ASTnode> type, std::unique_ptr<ASTnode> ident_node, std::unique_ptr<ASTnode> semicol) : Type(std::move(type)), Id(std::move(ident_node)), Semicol(std::move(semicol)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"LocalDeclOfVar:"+Type->to_string()+Id->to_string();
    indentation.resize(indentation.size()-3);
    return ret;
    // return Type->to_string().c_str()+Id+std::string(Semicol->to_string().c_str());
  }
};

class BlockASTnode : public ASTnode {
  std::vector<std::unique_ptr<ASTnode>> Decls;
  std::vector<std::unique_ptr<ASTnode>> Stmts;
public:
  BlockASTnode(std::vector<std::unique_ptr<ASTnode>> decls, std::vector<std::unique_ptr<ASTnode>> stmts) : Decls(std::move(decls)), Stmts(std::move(stmts)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    // indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"BlockExpression: ";
    if (Decls.size()>0) {
      indentation = indentation + "   ";
      ret = ret + "\n"+indentation+"├── "+"LocalDeclarations within the block:";
      for (unsigned i=0; i<Decls.size(); i++) {
        ret = ret + std::string(Decls.at(i)->to_string().c_str());
      }
      indentation.resize(indentation.size()-3);
    }
    if (Stmts.size()>0) {
      indentation = indentation + "   ";
      ret = ret + "\n"+indentation+"├── "+"StatementList within the block:";
      for (unsigned i=0; i<Stmts.size(); i++) {
        ret = ret + std::string(Stmts.at(i)->to_string().c_str());
      }
      indentation.resize(indentation.size()-3);
    }
    // indentation.resize(indentation.size()-3);
    return ret;
  }
};

class WhileASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;
  std::unique_ptr<ASTnode> Stmt;
public:
  WhileASTnode(std::unique_ptr<ASTnode> expr, std::unique_ptr<ASTnode> stmt) : Expr(std::move(expr)), Stmt(std::move(stmt)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {

    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"WhileStmt:";
    indentation = indentation + "   ";
    ret = ret + "\n"+indentation+"├── "+"Expression in While Statement:";
    ret = ret + Expr->to_string().c_str();
    ret = ret + "\n"+indentation+"├── "+"StatementsExpr in the While block:";
    ret = ret + Stmt->to_string().c_str();
    indentation.resize(indentation.size()-6);
    // std::string ret = "while (";
    // ret = ret + std::string(Expr->to_string().c_str()) + ")";
    // ret = ret + std::string(Stmt->to_string().c_str());
    return ret;
  }
};

class IfASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;
  std::unique_ptr<ASTnode> Block;
  std::unique_ptr<ASTnode> Else;
public:
  IfASTnode(std::unique_ptr<ASTnode> expr, std::unique_ptr<ASTnode> block, std::unique_ptr<ASTnode> else_stmt) : Expr(std::move(expr)), Block(std::move(block)), Else(std::move(else_stmt)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {

    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"IfStatement:";
    indentation = indentation + "   ";
    ret = ret + "\n"+indentation+"├── "+"If statement expression: " + Expr->to_string() + "\n"+indentation+"├── "+"Block of If statement: " + Block->to_string().c_str();
    if (Else) {
      ret = ret + "\n"+indentation+"├── "+"Else block of If Statement: " + Else->to_string().c_str();
    }
    indentation.resize(indentation.size()-6);
    // std::string ret = "if (";
    // ret = ret + std::string(Expr->to_string().c_str()) + ")";
    // ret = ret + std::string(Block->to_string().c_str());
    // if (Else) {
    //   ret = ret + std::string(Else->to_string().c_str());
    // }
    return ret;
  }
};

class ElseASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Block;
public:
  ElseASTnode(std::unique_ptr<ASTnode> block) : Block(std::move(block)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n"+indentation+"├── "+"ElseBlock of If statement: " + Block->to_string();
    indentation.resize(indentation.size()-3);
    // std::string ret = "else ";
    // ret = ret + std::string(Block->to_string().c_str());
    return ret;
  }
};

class ReturnASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;
  std::unique_ptr<ASTnode> Semicol;
public:
  ReturnASTnode(std::unique_ptr<ASTnode> expr, std::unique_ptr<ASTnode> semicol) : Expr(std::move(expr)), Semicol(std::move(semicol)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n" +indentation+"├── "+"ReturnStatement";
    if (Expr) {
      indentation = indentation + "   ";
      ret = ret + "\n "+indentation +"├── "+"ReturnExpression:" + Expr->to_string().c_str();
      indentation.resize(indentation.size()-3);
    }
    indentation.resize(indentation.size()-3);
    // std::string ret = "return ";
    // if (Expr) {
    //   ret = ret + std::string(Expr->to_string().c_str());
    // }
    // ret = ret + std::string(Semicol->to_string().c_str());
    return ret;
  }
};

class ParamASTnode : public ASTnode {
  std::unique_ptr<VarTypeASTnode> Type;
  std::unique_ptr<IdentASTnode> Id;
public:
  ParamASTnode(std::unique_ptr<VarTypeASTnode> type, std::unique_ptr<IdentASTnode> ident_node) : Type(std::move(type)), Id(std::move(ident_node)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    indentation = indentation + "   ";
    std::string ret = "\n" +indentation +"├── "+"Parameter of Function: ";
    if (Type->get_var_type().compare("void")==0) {
      ret = ret + Type->to_string();
    } else {
      ret = ret + Type->to_string() + Id->to_string().c_str();
    }

    indentation.resize(indentation.size()-3);
    return ret;
  }
  std::string get_type() const {
    return Type->get_var_type();
  }
  std::string get_name() const {
    return Id->get_id_name();
  }
};

class PrototypeAST {
  std::unique_ptr<VarTypeASTnode> Type;
  std::unique_ptr<IdentASTnode> Id;
  std::vector<std::unique_ptr<ParamASTnode>> Params;
public:
  PrototypeAST(std::unique_ptr<VarTypeASTnode> type, std::unique_ptr<IdentASTnode> id, std::vector<std::unique_ptr<ParamASTnode>> params) : Type(std::move(type)), Id(std::move(id)), Params(std::move(params)) {}
  std::string to_string () const {
    std::string ret = "\n"+indentation+"├── "+"FuncPrototype: " + Type->to_string() + Id->to_string().c_str();
    if (Params.size()>0) {
      indentation = indentation + "   ";
      ret = ret + "\n" + indentation + "├── "+"Parameters:";
      for(unsigned i=0; i<Params.size(); i++) {
        ret = ret + Params.at(i)->to_string().c_str();
      }
      indentation.resize( indentation.size()-3 );
    }
    return ret;
  }
  std::string get_type() const {
    return Type->get_var_type();
  }
  std::string get_name() const {
    return Id->get_id_name();
  }
  Function *codegen();
};

class FunctionAST {
  std::unique_ptr<PrototypeAST> Prototype;
  std::unique_ptr<ASTnode> Body;
public:
  FunctionAST(std::unique_ptr<PrototypeAST> prot, std::unique_ptr<ASTnode> body) : Prototype(std::move(prot)), Body(std::move(body)) {}
  std::string to_string() const {
    std::string ret = "\n"+indentation+"├── "+"Function: ";
    indentation = indentation + "   ";
    ret = ret + Prototype->to_string();
    ret = ret + Body->to_string().c_str();
    indentation.resize( indentation.size()-3 );
    return ret;
  }
  Function *codegen();
};

class ProgramASTnode : public ASTnode {
  std::vector<std::unique_ptr<PrototypeAST>> ExtrnList;
  std::vector<std::unique_ptr<ParamASTnode>> Globals;
  std::vector<std::unique_ptr<FunctionAST>> DeclList;
public:
  ProgramASTnode(std::vector<std::unique_ptr<PrototypeAST>> extrn, std::vector<std::unique_ptr<ParamASTnode>> globals, std::vector<std::unique_ptr<FunctionAST>> decl) : ExtrnList(std::move(extrn)), Globals(std::move(globals)), DeclList(std::move(decl)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret;
    if (ExtrnList.size()>0){
      indentation = indentation + "   ";
      ret = ret + "├── "+"ExternFunctions:";
      for(unsigned i=0; i<ExtrnList.size(); i++) {
        ret = ret + ExtrnList.at(i)->to_string().c_str();
      }
      indentation.resize( indentation.size()-3 );
    }
    if (Globals.size()>0) {
      ret = ret + "\n"+"├── "+"GlobalVarsDeclarations: ";
      for(unsigned i=0; i<Globals.size(); i++) {
        ret = ret + Globals.at(i)->to_string().c_str();
      }
    }
    if (DeclList.size()>0){
      ret = ret + "\n"+"├── "+"FunctionDeclarations: ";
      //indentation = indentation + "   ";
      for(unsigned i=0; i<DeclList.size(); i++) {
        ret = ret+DeclList.at(i)->to_string().c_str();
      }
    }
    return ret;
  }
  void add_global(std::unique_ptr<ParamASTnode> global) {
    Globals.push_back(std::move(global));
  }
};




/* add other AST nodes as nessasary */

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//
static std::unique_ptr<ASTnode> Rval7Expr();
static std::unique_ptr<ASTnode> Rval6Expr();
static std::unique_ptr<ASTnode> Rval5Expr();
static std::unique_ptr<ASTnode> Rval4Expr();
static std::unique_ptr<ASTnode> Rval3Expr();
static std::unique_ptr<ASTnode> Rval2Expr();
static std::unique_ptr<ASTnode> Rval1Expr();
static std::unique_ptr<ASTnode> ParseExpr();
static std::unique_ptr<ASTnode> ParseExprStmt();
static std::unique_ptr<ASTnode> ParseArg();
static std::vector<std::unique_ptr<ASTnode>> ParseArgsList();
static std::vector<std::unique_ptr<ASTnode>> ParseArgsListPrime();
static std::unique_ptr<ASTnode> ParseVarType();
static std::unique_ptr<ASTnode> ParseLocalDecl();
static std::unique_ptr<ASTnode> ParseStmt();
static std::vector<std::unique_ptr<ASTnode>> ParseStmtList();
static std::vector<std::unique_ptr<ASTnode>> ParseLocalDecls();
static std::unique_ptr<ASTnode> ParseBlock();
static std::unique_ptr<ASTnode> ParseElseStmt();
static std::unique_ptr<ParamASTnode> ParseParam();
static std::vector<std::unique_ptr<ParamASTnode>> ParseParamList();
static std::vector<std::unique_ptr<ParamASTnode>> ParseParams();
static std::unique_ptr<ASTnode> ParseFuncDecl();
static std::vector<std::unique_ptr<ASTnode>> ParseDeclList();
static std::unique_ptr<ASTnode> ParseDecl();
static std::unique_ptr<ASTnode> ParseExtern();
static std::vector<std::unique_ptr<ASTnode>> ParseExternList();
static std::unique_ptr<ASTnode> LogError(const char *Str);
static std::unique_ptr<PrototypeAST> ParseExtern2();
static std::vector<std::unique_ptr<PrototypeAST>> ParseExternList2();
static std::vector<std::unique_ptr<PrototypeAST>> ParseExternListPrime2();
static std::unique_ptr<PrototypeAST> ParsePrototype();
static std::unique_ptr<FunctionAST> ParseFunction();
static std::vector<std::unique_ptr<FunctionAST>> ParseFunctionsList();
static std::unique_ptr<ParamASTnode> ParseGlobal();
static std::vector<std::unique_ptr<ParamASTnode>> ParseGlobals();

/* Add function calls for each production */

// program ::= extern_list decl_list | decl_list
static std::unique_ptr<ASTnode> parser() {
  // add body
  if (CurTok.type == EXTERN) {
    auto extern_list = ParseExternList2();
    int t = CurTok.type;
    if ((t != VOID_TOK) && (t != INT_TOK) && (t != FLOAT_TOK) && (t != BOOL_TOK)) {
      LogError("ERROR. Missing function type.");
    }
    auto globals_list = ParseGlobals();
    auto funcs_list = ParseFunctionsList();

    auto Result = std::make_unique<ProgramASTnode>(std::move(extern_list),std::move(globals_list), std::move(funcs_list));
    return std::move(Result);

  } else {
    int t = CurTok.type;
    if ((t != VOID_TOK) && (t != INT_TOK) && (t != FLOAT_TOK) && (t != BOOL_TOK)) {
      LogError("ERROR. Missing function type.");
    }
    std::vector<std::unique_ptr<PrototypeAST>> empty_list;
    auto globals_list = ParseGlobals();
    auto funcs_list = ParseFunctionsList();

    auto Result = std::make_unique<ProgramASTnode>(std::move(empty_list), std::move(globals_list), std::move(funcs_list));
    return std::move(Result);
  }

}

static std::vector<std::unique_ptr<FunctionAST>> ParseFunctionsListPrime() {
  std::vector<std::unique_ptr<FunctionAST>> funcs_list_prime;

  int t = CurTok.type;
  while ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK) || (t==VOID_TOK)) {
    auto func = ParseFunction();
    if (func) {
      funcs_list_prime.push_back(std::move(func));
    }
    t = CurTok.type;
  }

  if (t == EOF_TOK) {
    return funcs_list_prime;
  }

  LogError("ERROR. Missing function type.");

  return funcs_list_prime;

}

static std::vector<std::unique_ptr<FunctionAST>> ParseFunctionsList() {
  std::vector<std::unique_ptr<FunctionAST>> funcs_list;

  auto func = ParseFunction();
  if (func) {
    // func->codegen() etc
    auto *FuncIR = func->codegen();
    funcs_list.push_back(std::move(func));
    auto funcs_list_prime = ParseFunctionsListPrime();
    for(unsigned i=0; i<funcs_list_prime.size(); i++){
      funcs_list.push_back(std::move(funcs_list_prime.at(i)));
    }
  }
  return funcs_list;
}

static std::unique_ptr<ParamASTnode> ParseGlobal() {
  TOKEN type = CurTok;
  getNextToken();
  if (CurTok.type == IDENT) {
    TOKEN ident = CurTok;
    getNextToken();
    if (CurTok.type == SC) {
      auto global_type = std::make_unique<VarTypeASTnode>(type);
      auto global_id = std::make_unique<IdentASTnode>(ident);
      auto global = std::make_unique<ParamASTnode>(std::move(global_type), std::move(global_id));
      getNextToken();
      return std::move(global);
    }
    putBackToken(CurTok);
    putBackToken(ident);
  }
  putBackToken(type);
  getNextToken();
  return nullptr;
}

static std::vector<std::unique_ptr<ParamASTnode>> ParseGlobals() {
  std::vector<std::unique_ptr<ParamASTnode>> globals;
  std::unique_ptr<ParamASTnode> global;

  do {
    global = ParseGlobal();
    globals.push_back(std::move(global));

  } while (global);

  return std::move(globals);
}

static std::unique_ptr<FunctionAST> ParseFunction() {
  auto Proto = ParsePrototype();
  auto block = ParseBlock();
  auto Result = std::make_unique<FunctionAST>(std::move(Proto), std::move(block));

  if (Result) {
    //Result->codegen() etc;
  }
  return std::move(Result);

}

static std::unique_ptr<ASTnode> LogError(const char *Str) {
  fprintf(stderr, "\nLogError: Line number: %d, Column number: %d,\n%s\n\n",lineNo, CurTok.columnNo, Str);
  exit(0);
  // return nullptr;
}

static std::vector<std::unique_ptr<PrototypeAST>> ParseExternListPrime2() {
  std::vector<std::unique_ptr<PrototypeAST>> extern_list_prime;

  int t = CurTok.type;
  while (t == EXTERN) {
    auto ext = ParseExtern2();
    if (ext) {
      // ext->codegen() etc
      auto *ExternIR = ext->codegen();
      extern_list_prime.push_back(std::move(ext));
    }
    t = CurTok.type;
  }

  return extern_list_prime;
}

static std::vector<std::unique_ptr<PrototypeAST>> ParseExternList2() {
  std::vector<std::unique_ptr<PrototypeAST>> extern_list;

  auto ext = ParseExtern2();
  if (ext) {
    // ext->codegen() etc
    auto *ExternIR = ext->codegen();
    extern_list.push_back(std::move(ext));
    auto extern_list_prime = ParseExternListPrime2();
    for(unsigned i=0; i<extern_list_prime.size(); i++) {
      extern_list.push_back(std::move(extern_list_prime.at(i)));
    }
  }
  return extern_list;
}

static std::unique_ptr<PrototypeAST> ParseExtern2() {
  if (CurTok.type == EXTERN) {
    getNextToken();
    auto prototype =  ParsePrototype();

    if (CurTok.type != SC) {
      LogError("ERROR. missing semicolon for extern function.");
    }
    getNextToken();
    return std::move(prototype);

  }
  return nullptr;
}

static std::unique_ptr<PrototypeAST> ParsePrototype() {

  int t = CurTok.type;
  if ((t==VOID_TOK) || (t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
    auto type = std::make_unique<VarTypeASTnode>(CurTok);
    getNextToken();

    if (CurTok.type == IDENT) {
      TOKEN ident = CurTok;
      auto ident_node = std::make_unique<IdentASTnode>(CurTok);
      getNextToken();
      if (CurTok.type != LPAR) {
        LogError("ERROR. missing left parantheses for extern function.");
      }

      getNextToken();
      auto params = ParseParams();

      if (CurTok.type != RPAR) {
        LogError("ERROR. missing right parantheses for extern function.");
      }

      auto Result = std::make_unique<PrototypeAST>(std::move(type),std::move(ident_node),std::move(params));
      getNextToken();
      return std::move(Result);

    }else {
      LogError("ERROR. missing identity for extern function.");
    }

  } else {
    LogError("ERROR. Missing type of extern function.");
  }
  return nullptr;

}

static std::vector<std::unique_ptr<ParamASTnode>> ParseParams() {
  std::vector<std::unique_ptr<ParamASTnode>> params;
  int t = CurTok.type;
  if ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
    params = ParseParamList();
    return params;

  } else if (t == VOID_TOK) {
    auto void_type = std::make_unique<VarTypeASTnode>(CurTok);
    auto param = std::make_unique<ParamASTnode>(std::move(void_type), nullptr);
    getNextToken();
    params.push_back(std::move(param));
    return params;

  } else {
    if (t == RPAR) {
      return params;
    }

    LogError("ERROR. Missing type of parameter.");
  }

  return params;
}

static std::vector<std::unique_ptr<ParamASTnode>> ParseParamListPrime() {
  std::vector<std::unique_ptr<ParamASTnode>> param_list_prime;

  int i = CurTok.type;
  while (i == COMMA) {
    getNextToken();
    auto param = ParseParam();
    if (param) {
      param_list_prime.push_back(std::move(param));
    }
    i = CurTok.type;
  }

  if (CurTok.type == RPAR) {
      return param_list_prime;
  }
  LogError("ERROR. Missing comma between parameters.");
  return param_list_prime;

}

static std::vector<std::unique_ptr<ParamASTnode>> ParseParamList() {
  std::vector<std::unique_ptr<ParamASTnode>> param_list;

  auto param = ParseParam();
  if (param) {
    param_list.push_back(std::move(param));
    auto param_list_prime = ParseParamListPrime();
    for(unsigned i=0; i<param_list_prime.size(); i++) {
      param_list.push_back(std::move(param_list_prime.at(i)));
    }
  }
  return param_list;
}

static std::unique_ptr<ParamASTnode> ParseParam() {
  int t = CurTok.type;
  if ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
    auto type = std::make_unique<VarTypeASTnode>(CurTok);

    getNextToken();
    if (CurTok.type == IDENT) {
      auto ident = std::make_unique<IdentASTnode>(CurTok);
      auto param = std::make_unique<ParamASTnode>(std::move(type), std::move(ident));
      getNextToken();
      return std::move(param);

    } else {
      LogError("ERROR. Missing identity of parameter.");
    }
  } else {
    LogError("ERROR. Missing type of parameter.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseReturnStmt() {
  getNextToken();
  int t = CurTok.type;
  if (t == SC) {
    auto semicol = std::make_unique<SemicolASTnode>(CurTok);
    auto Result = std::make_unique<ReturnASTnode>(nullptr, std::move(semicol));
    getNextToken();
    return std::move(Result);
  } else if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto expr = ParseExpr();
    if (CurTok.type==SC) {
      auto semicol = std::make_unique<SemicolASTnode>(CurTok);
      auto Result = std::make_unique<ReturnASTnode>(std::move(expr), std::move(semicol));
      getNextToken();
      return std::move(Result);
    } else {
      LogError("ERROR. Missing semicolon after return expression.");
    }

  } else {
    LogError("ERROR. Missing semicolon or return expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseIfStmt() {
  getNextToken();
  if (CurTok.type == LPAR) {
    getNextToken();
    auto expr = ParseExpr();
    if (CurTok.type == RPAR) {
      getNextToken();
      if (CurTok.type == LBRA) {
        auto block = ParseBlock();
        auto else_stmt = ParseElseStmt();

        auto Result = std::make_unique<IfASTnode>(std::move(expr), std::move(block), std::move(else_stmt));

        return std::move(Result);

      } else {
        LogError("ERROR. Missing left bracket for IF block");
      }

    } else {
      LogError("ERROR. Missing right parantheses for IF expression.");
    }

  } else {
    LogError("ERROR. Missing left parantheses for IF expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseElseStmt() {
  if (CurTok.type == ELSE) {
    getNextToken();
    if (CurTok.type == LBRA) {
      auto block = ParseBlock();
      if (block) {
        auto Result = std::make_unique<ElseASTnode>(std::move(block));
        return std::move(Result);
      }
    } else {
      LogError("ERROR. Missing left bracket in ELSE block");
    }
  } else {
    //else_stmt ::= eps
    return nullptr;
  }
  return nullptr;

}

static std::unique_ptr<ASTnode> ParseWhileStmt() {
  getNextToken();
  if (CurTok.type == LPAR) {
    getNextToken();
    auto expr = ParseExpr();
    if (CurTok.type == RPAR) {
      getNextToken();
      auto stmt = ParseStmt();
      if (expr && stmt) {
        auto Result = std::make_unique<WhileASTnode>(std::move(expr),std::move(stmt));
        return std::move(Result);
      }

    } else {
      LogError("ERROR. Missing right parantheses for WHILE expression.");
    }
  } else {
    LogError("ERROR. Missing left parantheses for WHILE expression.");
  }
  return nullptr;

}

static std::unique_ptr<ASTnode> ParseBlock() {
  if (CurTok.type == LBRA) {
    getNextToken();
    auto local_decls = ParseLocalDecls();
    auto stmt_list = ParseStmtList();

    if (CurTok.type == RBRA) {
      auto Result = std::make_unique<BlockASTnode>(std::move(local_decls), std::move(stmt_list));
      getNextToken();
      return std::move(Result);

    } else {
      LogError("ERROR. Missing right bracket for the block.");
    }

  } else {
    LogError("ERROR. Missing left bracket for the block.");
  }
  return nullptr;
}

static std::vector<std::unique_ptr<ASTnode>> ParseStmtList() {
  std::vector<std::unique_ptr<ASTnode>> stmt_list;

  int t = CurTok.type;
  if (t == RBRA) {
    return stmt_list;
  }
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR) || (t==SC) || (t==IF) || (t==RETURN) || (t==WHILE) || (t==LBRA)) {
    auto stmt = ParseStmt();
    if (stmt) {
      stmt_list.push_back(std::move(stmt));
      t = CurTok.type;
      while((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR) || (t==SC) || (t==IF) || (t==RETURN) || (t==WHILE) || (t==LBRA)) {
        stmt = ParseStmt();
        if (stmt) {
          stmt_list.push_back(std::move(stmt));
        }
        t = CurTok.type;
      }
    }

  } else {
    LogError("ERROR. Statement defined incorrectly in the block.");
  }
  return stmt_list;
}

static std::vector<std::unique_ptr<ASTnode>> ParseLocalDecls() {
  std::vector<std::unique_ptr<ASTnode>> decl_list;
  int t = CurTok.type;
  if ((t==INT_TOK) || (t==BOOL_TOK) || (t==FLOAT_TOK)) {
    auto local_decl = ParseLocalDecl();
    if (local_decl) {
      decl_list.push_back(std::move(local_decl));
      int t = CurTok.type;
      while ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
        local_decl = ParseLocalDecl();
        decl_list.push_back(std::move(local_decl));
        t = CurTok.type;
      }
    }
    return decl_list;
  } else if ((t==RBRA) || (t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR) || (t==SC) || (t==IF) || (t==RETURN) || (t==WHILE) || (t==LBRA)) {
    return decl_list;
  } else {
    LogError("ERROR. Local declaration defined incorrectly.");
  }

  return decl_list;

}

static std::unique_ptr<ASTnode> ParseLocalDecl() {
  int t = CurTok.type;
  if ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {

    auto var_type = ParseVarType();
    //t = CurTok.type;
    if (CurTok.type == IDENT) {

      TOKEN ident = CurTok;
      getNextToken();

      if (CurTok.type == SC) {
        auto ident_node = std::make_unique<IdentASTnode>(ident);
        auto semicol = std::make_unique<SemicolASTnode>(CurTok);

        auto Result = std::make_unique<LocalDeclASTnode>(std::move(var_type),std::move(ident_node),std::move(semicol));
        getNextToken();
        return std::move(Result);


      } else {
        LogError("ERROR. Semicolon is missing for local declaration.");
      }

    } else {
      LogError("ERROR. Identity missing for locally declared variable.");
    }

  } else {
    LogError("ERROR. Missing type of locally declared variable.");
  }
  return nullptr;

}

static std::unique_ptr<ASTnode> ParseVarType() {
  int t = CurTok.type;
  if ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
    auto var_type = std::make_unique<VarTypeASTnode>(CurTok);
    getNextToken();
    return std::move(var_type);
  } else {
    LogError("ERROR. Expected either Int, Bool or Float type.");
  }
  return nullptr;
}

// rval7 ::= INT_LIT
static std::unique_ptr<ASTnode> ParseIntNumberExpr() {
  auto Result = std::make_unique<IntASTnode>(CurTok, IntVal);
  getNextToken();
  return std::move(Result);
}

// rval7 ::= FLOAT_LIT
static std::unique_ptr<ASTnode> ParseFloatNumberExpr() {
  auto Result = std::make_unique<FloatASTnode>(CurTok, FloatVal);
  getNextToken();
  return std::move(Result);
}

// rval7 ::= BOOL_LIT
static std::unique_ptr<ASTnode> ParseBoolNumberExpr() {
  auto Result = std::make_unique<BoolASTnode>(CurTok, BoolVal);
  getNextToken();
  return std::move(Result);
}

static std::unique_ptr<ASTnode> ParseIdentExpr() {
  auto Result = std::make_unique<IdentASTnode>(CurTok);
  getNextToken();
  return std::move(Result);
}

static std::unique_ptr<ASTnode> ParseNegativeExpr() {
  char op;
  if (CurTok.type == MINUS) {
    op = '-';
  } else {
    op = '!';
  }
  TOKEN negTok = CurTok;
  getNextToken();
  auto expr = Rval7Expr();
  if (expr) {
    auto Result = std::make_unique<NegativeASTnode>(negTok, op, std::move(expr));
    return std::move(Result);
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseStmt() {
  int t = CurTok.type;

  if (t==LBRA) { // BLOCK ------------------------------------------------------
    auto block = ParseBlock();
    if (block) {return std::move(block);}
  }
  else if (t==IF) {
    auto if_stmt = ParseIfStmt();
    if(if_stmt) {return std::move(if_stmt);}
  }
  else if (t==WHILE) {
    auto while_stmt = ParseWhileStmt();
    if (while_stmt) {return std::move(while_stmt);}
  }
  else if (t==RETURN) {
    auto return_stmt = ParseReturnStmt();
    if (return_stmt) {return std::move(return_stmt);}

  }
  else if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR) || (t==SC)) {
    auto expr_stmt = ParseExprStmt();
    if (expr_stmt) {
      //getNextToken();
      return std::move(expr_stmt);
    }
  } else {
    LogError("ERROR. Missing statement definition.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseExprStmt() {
  int t = CurTok.type;

  //expand by expr_stmt ::= expr
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto expr_parsed = ParseExpr();

    t = CurTok.type;
    if (t==SC) {
      auto semicol = std::make_unique<SemicolASTnode>(CurTok);
      if (expr_parsed) {
        auto Result = std::make_unique<ExprSemicolASTnode>(std::move(expr_parsed),std::move(semicol));
        getNextToken();
        return std::move(Result);
      }
    } else {
      LogError("ERROR. Semicolon is expected after expression.");
    }
  }
  else if (t==SC) {
    auto semicol = std::make_unique<SemicolASTnode>(CurTok);
    getNextToken();
    return std::move(semicol);

  } else {
    LogError("ERROR. Expression or semicolon is expected.");
  }
  return nullptr;

}

static std::unique_ptr<ASTnode> ParseExpr() {
  int t = CurTok.type;

  if (t==IDENT) {
    TOKEN identTok = CurTok;
    getNextToken();
    if (CurTok.type == ASSIGN) {
      auto ident = std::make_unique<IdentASTnode>(identTok);
      getNextToken();
      auto expr_parsed = ParseExpr();
      if (expr_parsed) {
        auto Result = std::make_unique<AssignASTnode>(std::move(ident), std::move(expr_parsed));
        return std::move(Result);
      }

    } else {
      putBackToken(CurTok);
      putBackToken(identTok);
      getNextToken();
    }
  }

  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto Rval = Rval1Expr();
    if (Rval) {
      return std::move(Rval);
    }

  } else {
    LogError("ERROR. Assignment or expression is expected.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> Rval1Expr() {
  int t = CurTok.type;
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto Rval2 = Rval2Expr();

    t = CurTok.type;
    TOKEN op = CurTok;
    if (t == OR) {
      getNextToken();
      auto Rval1 = Rval1Expr();
      if (Rval1 && Rval2) {
        auto Result = std::make_unique<ExpressionASTnode>(op, std::move(Rval2), std::move(Rval1));
        //getNextToken();
        return std::move(Result);
      }
    } else {
      // FIRST(rval1) = eps
      if (Rval2) {return Rval2;}
    }

  } else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> Rval2Expr() {
  int t = CurTok.type;
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto Rval3 = Rval3Expr();

    t = CurTok.type;
    TOKEN op = CurTok;
    if (t == AND) {
      getNextToken();
      auto Rval2 = Rval2Expr();
      if (Rval2 && Rval3) {
        auto Result = std::make_unique<ExpressionASTnode>(op, std::move(Rval3), std::move(Rval2));
        return std::move(Result);
      }
    } else {
      // FIRST(rval2) = eps
      if (Rval3) {return Rval3;}
    }

  } else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> Rval3Expr() {
  int t = CurTok.type;
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto Rval4 = Rval4Expr();

    t = CurTok.type;
    TOKEN op = CurTok;
    if ((t==EQ) || (t==NE)) {
      getNextToken();
      auto Rval3 = Rval3Expr();
      if (Rval3 && Rval4) {
        auto Result = std::make_unique<ExpressionASTnode>(op, std::move(Rval4), std::move(Rval3));
        return std::move(Result);
      }
    } else {
      // FIRST(rval3) = eps
      if (Rval4) {return Rval4;}
    }

  } else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> Rval4Expr() {

  int t = CurTok.type;
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto Rval5 = Rval5Expr();

    t = CurTok.type;
    TOKEN op = CurTok;
    if ((t==LE) || (t==LT) || (t==GE) || (t==GT)) {
        getNextToken();
        auto Rval4 = Rval4Expr();
        if (Rval4 && Rval5) {
          auto Result = std::make_unique<ExpressionASTnode>(op, std::move(Rval5), std::move(Rval4));
          return std::move(Result);
        }
    } else {
      // FIRST(rval4) = eps
      if (Rval5) {return Rval5;}
    }

  } else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> Rval5Expr() {

  int t = CurTok.type;
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto Rval6 = Rval6Expr();

    t = CurTok.type;
    TOKEN op = CurTok;
    if ((t==PLUS) || (t==MINUS)) {
      //expand by rval5 ::= rval6 rval5'
      getNextToken();
      auto Rval5 = Rval5Expr();
      if (Rval5 && Rval6) {
        auto Result = std::make_unique<ExpressionASTnode>(op, std::move(Rval6), std::move(Rval5));
        return std::move(Result);
      }

    } else {
      // FIRST(rval5) = eps
      if (Rval6) {return Rval6;}
    }

  } else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> Rval6Expr() {
  int t = CurTok.type;

  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto Rval7 = Rval7Expr();

    t = CurTok.type;
    TOKEN op = CurTok;
    if((t==ASTERIX) || (t==DIV) || (t==MOD)) {
      //expand by rval6 ::= rval7 rval6'
      getNextToken();
      auto Rval6 = Rval6Expr();
      if (Rval6 && Rval7) {
        auto Result = std::make_unique<ExpressionASTnode>(op, std::move(Rval7), std::move(Rval6));
        return std::move(Result);
      }

    } else {
      // FIRST(rval6') = eps
      if (Rval7) {return Rval7;}
    }

  } else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> Rval7Expr() {
  if (CurTok.type == INT_LIT) {
    //expand by rval7 ::= INT_LIT
    auto int_lit = ParseIntNumberExpr();
    if(int_lit) {return int_lit;}
  }
  else if (CurTok.type == FLOAT_LIT) {
    //expand by rval7 ::= FLOAT_LIT
    auto float_lit = ParseFloatNumberExpr();
    if (float_lit) {return float_lit;}
  }
  else if (CurTok.type == BOOL_LIT) {
    //expand by rval7 ::= BOOL_LIT
    auto bool_lit = ParseBoolNumberExpr();
    if (bool_lit) {return bool_lit;}
  }
  else if (CurTok.type == MINUS) {
    //expand by rval7 ::= "-"rval7
    auto negative_expr = ParseNegativeExpr();
    if (negative_expr) {return negative_expr;}
  }
  else if (CurTok.type == NOT) {
    //expand by rval7 ::= "!"rval7
    auto negation_expr = ParseNegativeExpr();
    if (negation_expr) {return negation_expr;}
  }
  else if (CurTok.type == IDENT) {
    TOKEN identTok = CurTok;
    getNextToken();
    if (CurTok.type == LPAR) {
      //expand by rval7 ::= IDENT "(" args ")" ---------------------------------
      auto ident = std::make_unique<IdentASTnode>(identTok);
      getNextToken();
      auto arg_list = ParseArgsList();

      auto Result = std::make_unique<FunCallASTnode>(std::move(ident), std::move(arg_list), identTok);
      getNextToken();
      return std::move(Result);

    } else {
      //expand by rval7 ::= IDENT
      putBackToken(CurTok);
      putBackToken(identTok);
      getNextToken();
      auto ident = ParseIdentExpr();
      if (ident) {return ident;}
    }
  }
  else if (CurTok.type == LPAR) {
    //expand by rval7 ::= "(" expr ")"
    getNextToken();
    auto expr_parsed = ParseExpr();
    if (expr_parsed) {
      auto Result = std::make_unique<ExprEnclosedASTnode>(std::move(expr_parsed));
      getNextToken();
      return std::move(Result);
    }
  }
  else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }
  return nullptr;
}

static std::vector<std::unique_ptr<ASTnode>> ParseArgsList() {

  std::vector<std::unique_ptr<ASTnode>> arg_list;

  auto arg = ParseArg();
  if(arg) {
    arg_list.push_back(std::move(arg));
    auto arg_list_prime = ParseArgsListPrime();
    for(unsigned i=0; i<arg_list_prime.size(); i++) {
      arg_list.push_back(std::move(arg_list_prime.at(i)));
    }
  }
  return arg_list;
}

static std::vector<std::unique_ptr<ASTnode>> ParseArgsListPrime() {

  std::vector<std::unique_ptr<ASTnode>> arg_list_prime;

  int i = CurTok.type;
  while (i == COMMA) {
    getNextToken();
    auto arg = ParseArg();
    if (arg) {
      arg_list_prime.push_back(std::move(arg));
    }
    i = CurTok.type;
  }

  if (CurTok.type == RPAR) {
    return arg_list_prime;
  }
  LogError("ERROR. Comma is missing between the arguments.");
  return arg_list_prime;
}

static std::unique_ptr<ASTnode> ParseArg() {
  int t = CurTok.type;
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto arg = ParseExpr();
    if (arg) {
      return std::move(arg);
    }
  } else {
    LogError("ERROR. Expected either variable literal, identity, or '-', '!' and left parantheses for expression.");
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value*> NamedValues;
// static std::map<std::string, AllocaInst*> NamedValues;
// static std::map<std::string, Value*> GlobalNamedValues;

Value *LogErrorV(const char *Str) {
  fprintf(stderr, "\nLogError: \n%s\n\n", Str);
  return nullptr;
}

// static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName) {
//   IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
//   return TmpB.CreateAlloca(Type::)
// }

Value *IntASTnode::codegen() {
  return ConstantInt::get(TheContext, APInt(32, Val, false));
}

Value *FloatASTnode::codegen() {
  return ConstantFP::get(TheContext, APFloat(Val));
}

Value *BoolASTnode::codegen() {
  return ConstantInt::get(TheContext, APInt(1, Val, false));
}

Value *IdentASTnode::codegen() {
  Value *V = NamedValues[Name];
  if (!V) {
    LogErrorV("Unknown variable name");
  }

  return V;
}

Value *ExpressionASTnode::codegen() {
  fprintf(stderr, "CODEGEN ExprAST\n");
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R) {
    return nullptr;
  }

  if (Op.compare("+")==0) {
    return Builder.CreateFAdd(L, R, "addtmp");
  }

  return LogErrorV("invalid binary operator");
}

Value *FunCallASTnode::codegen() {
  Function *CalleeF = TheModule->getFunction(Callee);
  if (!CalleeF) {
    return LogErrorV("Unknown function referenced");
  }

  if (CalleeF->arg_size() != Args.size()) {
    return LogErrorV("Incorrect number of arguments passed");
  }

  std::vector<Value *> ArgsV;
  for(unsigned i = 0; i < Args.size(); i++) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back()) {
      return nullptr;
    }
  }
  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
  std::vector<llvm::Type*> Parameters;
  llvm::Type* param_type;
  for(unsigned i=0; i<Params.size(); i++) {
    if (std::string(Params.at(i)->get_type()).compare("int")==0) {
      param_type = Type::getInt32Ty(TheContext);

    } else if (std::string(Params.at(i)->get_type()).compare("float")==0) {
      param_type = Type::getFloatTy(TheContext);

    } else if (std::string(Params.at(i)->get_type()).compare("bool")==0) {
      param_type = Type::getInt1Ty(TheContext);

    } else if (std::string(Params.at(i)->get_type()).compare("void")==0) {
      param_type = Type::getVoidTy(TheContext);

    }
    Parameters.push_back(param_type);
  }

  FunctionType *FT;

  if (get_type().compare("int")==0) {
    FT = FunctionType::get(Type::getInt32Ty(TheContext), Parameters, false);

  } else if (get_type().compare("float")==0) {
    FT = FunctionType::get(Type::getFloatTy(TheContext), Parameters, false);

  } else if (get_type().compare("bool")==0) {
    FT = FunctionType::get(Type::getInt1Ty(TheContext), Parameters, false);

  } else if (get_type().compare("void")==0) {
    FT = FunctionType::get(Type::getVoidTy(TheContext), Parameters, false);
  }

  std::string Name = get_name();
  Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  unsigned Idx = 0;
  for (auto &Arg : F->args()) {
    Arg.setName(Params.at(Idx)->get_name());
    Idx++;
  }

  return F;
}

Function *FunctionAST::codegen() {
  Function *TheFunction = TheModule->getFunction(Prototype->get_name());

  if (!TheFunction) {
    TheFunction = Prototype->codegen();
  }

  if (!TheFunction) {
    return nullptr;
  }

  if (!TheFunction->empty()) {
    return (Function*)LogErrorV("Function cannot be redefined.");
  }

  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  NamedValues.clear();
  for(auto &Arg : TheFunction->args()) {
    // AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());
    //
    // Builder.CreateStore(&Arg, Alloca);

    NamedValues[Arg.getName()] = &Arg;
  }

  if (Value *RetVal = Body->codegen()) {
    Builder.CreateRet(RetVal);

    verifyFunction(*TheFunction);

    return TheFunction;
  }

  TheFunction->eraseFromParent();
  return nullptr;
}

Value *BlockASTnode::codegen() {
  return nullptr;
}

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ASTnode &ast) {
  os << ast.to_string();
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  // if (argc == 2) {
  //   pFile = fopen(argv[1], "r");
  //   if (pFile == NULL)
  //     perror("Error opening file");
  // } else {
  //   std::cout << "Usage: ./code InputFile\n";
  //   return 1;
  // }
  //
  // // initialize line number and column numbers to zero
  // lineNo = 1;
  // columnNo = 1;
  //
  // // get the first token
  // getNextToken();
  // while (CurTok.type != EOF_TOK) {
  //   fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
  //           CurTok.type);
  //   getNextToken();
  // }
  // fprintf(stderr, "Lexer Finished\n");
  // fclose(pFile);

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);


  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;
  getNextToken();



  // Run the parser now.
  auto ASTree = parser();

  fprintf(stderr, "Parsing Finished\n");


  llvm::outs() << *ASTree << "\n\n";





  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}
