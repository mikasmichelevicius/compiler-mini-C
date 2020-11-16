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
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return std::to_string(Val);
  }
};

class FloatASTnode : public ASTnode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTnode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return std::to_string(Val);
  }
};

class BoolASTnode : public ASTnode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTnode(TOKEN tok, bool val) : Val(val), Tok(tok), Name(boolToString(val)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    //return std::to_string(Val);
    return Name;
  }
};

class SemicolASTnode : public ASTnode {
  std::string Semicol;

public:
  SemicolASTnode(TOKEN tok) : Semicol(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Semicol+"\n";
  }
};

class IdentASTnode : public ASTnode {
  TOKEN Tok;
  std::string Name;
public:
  IdentASTnode(TOKEN tok) : Tok(tok), Name(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Name;
  }
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
    return std::string(1,'(')+std::string(1, Op)+Expr->to_string().c_str()+std::string(1,')');
  }
};

class ExpressionASTnode : public ASTnode {
  std::string Op;
  std::unique_ptr<ASTnode> LHS;
  std::unique_ptr<ASTnode> RHS;
  std::string Name;

public:
  ExpressionASTnode(TOKEN op, std::unique_ptr<ASTnode> lhs, std::unique_ptr<ASTnode> rhs) : Op(op.lexeme.c_str()), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    //return std::string(1,'(')+LHS->to_string().c_str()+Op+RHS->to_string().c_str()+std::string(1,')');
    return LHS->to_string().c_str()+Op+RHS->to_string().c_str();

  }
};

class ExprEnclosedASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;

public:
  ExprEnclosedASTnode(std::unique_ptr<ASTnode> expr) : Expr(std::move(expr)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return std::string(1,'(')+Expr->to_string().c_str()+std::string(1,')');
  }
};

class ExprSemicolASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;
  std::unique_ptr<ASTnode> Semicol;
public:
  ExprSemicolASTnode(std::unique_ptr<ASTnode> expr, std::unique_ptr<ASTnode> semicol) : Expr(std::move(expr)), Semicol(std::move(semicol)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Expr->to_string().c_str()+std::string(Semicol->to_string().c_str());
  }
};

class AssignASTnode : public ASTnode {
  std::string Id;
  std::unique_ptr<ASTnode> Assignment;

public:
  AssignASTnode(TOKEN ident, std::unique_ptr<ASTnode> assignment) : Id(ident.lexeme.c_str()), Assignment(std::move(assignment)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Id+std::string(1,'=')+Assignment->to_string().c_str();
  }
};

class FunCallASTnode : public ASTnode {
  std::string Id;
  std::vector<std::unique_ptr<ASTnode>> Args;
public:
  FunCallASTnode(TOKEN ident, std::vector<std::unique_ptr<ASTnode>> args) : Id(ident.lexeme.c_str()), Args(std::move(args)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret = Id+std::string(1,'(');
    for (unsigned i=0; i<Args.size()-1; i++) {
      ret = ret + std::string(Args.at(i)->to_string().c_str()) + std::string(1,',');
    }
    ret = ret + std::string(Args.at(Args.size()-1)->to_string().c_str()) + std::string(1,')');
    return ret;
  }
};

class VarTypeASTnode : public ASTnode {
  std::string Type;
public:
  VarTypeASTnode(TOKEN tok) : Type(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Type+" ";
  }
};

class VoidASTnode : public ASTnode {
  std::string Type;
public:
  VoidASTnode(TOKEN tok) : Type(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Type;
  }
};

class LocalDeclASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Type;
  std::string Id;
  std::unique_ptr<ASTnode> Semicol;
public:
  LocalDeclASTnode(std::unique_ptr<ASTnode> type, TOKEN ident, std::unique_ptr<ASTnode> semicol) : Type(std::move(type)), Id(ident.lexeme.c_str()), Semicol(std::move(semicol)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Type->to_string().c_str()+Id+std::string(Semicol->to_string().c_str());
  }
};

class BlockASTnode : public ASTnode {
  std::vector<std::unique_ptr<ASTnode>> Decls;
  std::vector<std::unique_ptr<ASTnode>> Stmts;
public:
  BlockASTnode(std::vector<std::unique_ptr<ASTnode>> decls, std::vector<std::unique_ptr<ASTnode>> stmts) : Decls(std::move(decls)), Stmts(std::move(stmts)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret = " {\n";
    for (unsigned i=0; i<Decls.size(); i++) {
      ret = ret + std::string(Decls.at(i)->to_string().c_str());
    }
    for (unsigned i=0; i<Stmts.size(); i++) {
      ret = ret + std::string(Stmts.at(i)->to_string().c_str());
    }
    return ret+"}\n";
  }
};

class WhileASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Expr;
  std::unique_ptr<ASTnode> Stmt;
public:
  WhileASTnode(std::unique_ptr<ASTnode> expr, std::unique_ptr<ASTnode> stmt) : Expr(std::move(expr)), Stmt(std::move(stmt)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret = "while (";
    ret = ret + std::string(Expr->to_string().c_str()) + ")";
    ret = ret + std::string(Stmt->to_string().c_str());
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
    std::string ret = "if (";
    ret = ret + std::string(Expr->to_string().c_str()) + ")";
    ret = ret + std::string(Block->to_string().c_str());
    if (Else) {
      ret = ret + std::string(Else->to_string().c_str());
    }
    return ret;
  }
};

class ElseASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Block;
public:
  ElseASTnode(std::unique_ptr<ASTnode> block) : Block(std::move(block)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret = "else ";
    ret = ret + std::string(Block->to_string().c_str());
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
    std::string ret = "return ";
    if (Expr) {
      ret = ret + std::string(Expr->to_string().c_str());
    }
    ret = ret + std::string(Semicol->to_string().c_str());
    return ret;
  }
};

class ParamASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Type;
  std::string Id;
public:
  ParamASTnode(std::unique_ptr<ASTnode> type, TOKEN tok) : Type(std::move(type)), Id(tok.lexeme.c_str()) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return std::string(Type->to_string().c_str())+Id;
  }
};

class FuncDeclASTnode : public ASTnode {
  std::vector<std::unique_ptr<ASTnode>> Params;
  std::unique_ptr<ASTnode> Block;
public:
  FuncDeclASTnode(std::vector<std::unique_ptr<ASTnode>> params, std::unique_ptr<ASTnode> block) : Params(std::move(params)), Block(std::move(block)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret = "(";
    if (Params.size()>0) {
      for(unsigned i=0; i<Params.size()-1; i++) {
        ret = ret + std::string(Params.at(i)->to_string().c_str())+", ";
      }
      ret = ret + std::string(Params.at(Params.size()-1)->to_string().c_str());
    }
    ret = ret + ")" + std::string(Block->to_string().c_str());
    return ret;
  }
};

class FuncASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Type;
  std::string Id;
  std::unique_ptr<ASTnode> FuncDecl;
public:
  FuncASTnode(std::unique_ptr<ASTnode> type, TOKEN tok, std::unique_ptr<ASTnode> func_decl) : Type(std::move(type)), Id(tok.lexeme.c_str()), FuncDecl(std::move(func_decl)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return std::string(Type->to_string().c_str())+Id+std::string(FuncDecl->to_string().c_str());
  }
};

class VarASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Type;
  std::string Id;
  std::unique_ptr<ASTnode> Semicol;
public:
  VarASTnode(std::unique_ptr<ASTnode> type, TOKEN tok, std::unique_ptr<ASTnode> semicol) : Type(std::move(type)), Id(tok.lexeme.c_str()), Semicol(std::move(semicol)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    return Type->to_string().c_str()+Id+Semicol->to_string().c_str();
  }
};

class ExternASTnode : public ASTnode {
  std::unique_ptr<ASTnode> Type;
  std::string Id;
  std::vector<std::unique_ptr<ASTnode>> Params;
public:
  ExternASTnode(std::unique_ptr<ASTnode> type, TOKEN tok, std::vector<std::unique_ptr<ASTnode>> params) : Type(std::move(type)), Id(tok.lexeme.c_str()), Params(std::move(params)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret = "extern "+std::string(Type->to_string().c_str())+Id+"(";
    if (Params.size()>0) {
      for(unsigned i=0; i<Params.size()-1; i++) {
        ret = ret + std::string(Params.at(i)->to_string().c_str())+", ";
      }
      ret = ret + std::string(Params.at(Params.size()-1)->to_string().c_str());
    }
    ret = ret + ");";
    return ret;
  }
};

class ProgramASTnode : public ASTnode {
  std::vector<std::unique_ptr<ASTnode>> ExtrnList;
  std::vector<std::unique_ptr<ASTnode>> DeclList;
public:
  ProgramASTnode(std::vector<std::unique_ptr<ASTnode>> extrn, std::vector<std::unique_ptr<ASTnode>> decl) : ExtrnList(std::move(extrn)), DeclList(std::move(decl)) {}
  virtual Value *codegen() override {}
  virtual std::string to_string() const override {
    std::string ret;
    if (ExtrnList.size()>0){
      for(unsigned i=0; i<ExtrnList.size(); i++) {
        ret = ret + std::string(ExtrnList.at(i)->to_string().c_str())+"\n";
      }
    }
    if (DeclList.size()>0){
      for(unsigned i=0; i<DeclList.size(); i++) {
        ret = ret + std::string(DeclList.at(i)->to_string().c_str());
      }
    }
    return ret;
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
static std::unique_ptr<ASTnode> ParseParam();
static std::vector<std::unique_ptr<ASTnode>> ParseParamList();
static std::vector<std::unique_ptr<ASTnode>> ParseParams();
static std::unique_ptr<ASTnode> ParseFuncDecl();
static std::vector<std::unique_ptr<ASTnode>> ParseDeclList();
static std::unique_ptr<ASTnode> ParseDecl();
static std::unique_ptr<ASTnode> ParseExtern();
static std::vector<std::unique_ptr<ASTnode>> ParseExternList();

/* Add function calls for each production */

// program ::= extern_list decl_list | decl_list
static void parser() {
  // add body
  if (CurTok.type == EXTERN) {
    auto extern_list = ParseExternList();
    auto decl_list = ParseDeclList();
    auto Result = std::make_unique<ProgramASTnode>(std::move(extern_list), std::move(decl_list));
    fprintf(stderr, "parsed result is: \n%s\n", Result->to_string().c_str());

    // return std::move(Result);

  } else {
    std::vector<std::unique_ptr<ASTnode>> empty_list;
    auto decl_list = ParseDeclList();
    auto Result = std::make_unique<ProgramASTnode>(std::move(empty_list), std::move(decl_list));
    fprintf(stderr, "parsed result is: \n%s\n", Result->to_string().c_str());

    // return std::move(Result);
  }

}

static std::vector<std::unique_ptr<ASTnode>> ParseExternListPrime() {
  std::vector<std::unique_ptr<ASTnode>> extern_list_prime;

  int t = CurTok.type;
  while (t == EXTERN) {
    auto ext = ParseExtern();
    if (ext) {
      extern_list_prime.push_back(std::move(ext));
    }
    t = CurTok.type;
  }

  return extern_list_prime;
}

static std::vector<std::unique_ptr<ASTnode>> ParseExternList() {
  std::vector<std::unique_ptr<ASTnode>> extern_list;

  auto ext = ParseExtern();
  if (ext) {
    extern_list.push_back(std::move(ext));
    auto extern_list_prime = ParseExternListPrime();
    for(unsigned i=0; i<extern_list_prime.size(); i++) {
      extern_list.push_back(std::move(extern_list_prime.at(i)));
    }
  }
  return extern_list;
}

static std::unique_ptr<ASTnode> ParseExtern() {
  if (CurTok.type == EXTERN) {
    getNextToken();
    int t = CurTok.type;
    if ((t==VOID_TOK) || (t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
      auto type = std::make_unique<VarTypeASTnode>(CurTok);
      getNextToken();

      if (CurTok.type == IDENT) {
        TOKEN ident = CurTok;
        getNextToken();
        if (CurTok.type != LPAR) {
          fprintf(stderr, "ERROR missing left parantheses for exter function\n");
          return nullptr;
        }

        getNextToken();
        auto params = ParseParams();

        if (CurTok.type != RPAR) {
          fprintf(stderr, "ERROR missing left parantheses for exter function\n");
          return nullptr;
        }

        getNextToken();
        if (CurTok.type != SC) {
          fprintf(stderr, "ERROR missing semicolon for exter function\n");
          return nullptr;
        }

        auto Result = std::make_unique<ExternASTnode>(std::move(type),ident,std::move(params));
        getNextToken();
        return std::move(Result);

      }else {
        fprintf(stderr, "ERROR missing Ident for extern function\n");
      }

    } else {
      fprintf(stderr, "ERROR missing type of extern function\n");
    }

  } else {
    fprintf(stderr, "KAZKAS ERROR\n");
  }
  return nullptr;
}

static std::vector<std::unique_ptr<ASTnode>> ParseDeclListPrime() {
  std::vector<std::unique_ptr<ASTnode>> decl_list_prime;

  int t = CurTok.type;
  while ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK) || (t==VOID_TOK)) {
    auto decl = ParseDecl();
    if (decl) {
      decl_list_prime.push_back(std::move(decl));
    }
    t = CurTok.type;
  }

  return decl_list_prime;
}

static std::vector<std::unique_ptr<ASTnode>> ParseDeclList() {
  std::vector<std::unique_ptr<ASTnode>> decl_list;

  auto decl = ParseDecl();
  if (decl) {
    decl_list.push_back(std::move(decl));
    auto decl_list_prime = ParseDeclListPrime();
    for(unsigned i=0; i<decl_list_prime.size(); i++){
      decl_list.push_back(std::move(decl_list_prime.at(i)));
    }
  }
  return decl_list;
}

static std::unique_ptr<ASTnode> ParseDecl() {
  int t = CurTok.type;
  if (t == VOID_TOK) {
    auto type = std::make_unique<VarTypeASTnode>(CurTok);
    getNextToken();

    if (CurTok.type == IDENT) {
      TOKEN ident = CurTok;
      getNextToken();
      auto func_decl = ParseFuncDecl();
      auto Result = std::make_unique<FuncASTnode>(std::move(type), ident, std::move(func_decl));
      return std::move(Result);

    }
    else {
      fprintf(stderr, "ERROR missing Ident in function Declaration\n");
    }
  }
  else if ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
    auto type = std::make_unique<VarTypeASTnode>(CurTok);
    getNextToken();

    if (CurTok.type == IDENT) {
      TOKEN ident = CurTok;
      getNextToken();
      if (CurTok.type == SC) {
        auto semicol = std::make_unique<SemicolASTnode>(CurTok);
        auto Result = std::make_unique<VarASTnode>(std::move(type), ident, std::move(semicol));
        getNextToken();
        return std::move(Result);
      }
      else {
        auto func_decl = ParseFuncDecl();
        auto Result = std::make_unique<FuncASTnode>(std::move(type), ident, std::move(func_decl));
        return std::move(Result);

      }


    } else {
      fprintf(stderr, "ERROR missing Ident in function Declaration\n");
    }

  }
  else {
    fprintf(stderr, "ERROR missing function type\n");
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseFuncDecl() {
  if (CurTok.type == LPAR) {
    getNextToken();
    int t = CurTok.type;
    auto params = ParseParams();

    if (CurTok.type == RPAR) {
      getNextToken();
      auto block = ParseBlock();
      auto Result = std::make_unique<FuncDeclASTnode>(std::move(params), std::move(block));
      return std::move(Result);

    } else {
      fprintf(stderr, "ERROR missing right parantheses FuncDecl\n");
    }

  } else {
    fprintf(stderr, "ERROR missing left parantheses FuncDecl\n");
  }
  return nullptr;
}

static std::vector<std::unique_ptr<ASTnode>> ParseParams() {
  std::vector<std::unique_ptr<ASTnode>> params;
  int t = CurTok.type;
  if ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
    params = ParseParamList();
    return params;

  } else if (t == VOID_TOK) {
    auto void_type = std::make_unique<VoidASTnode>(CurTok);
    getNextToken();
    params.push_back(std::move(void_type));
    return params;

  } else {
    return params;
  }
  fprintf(stderr,"RETURNS EMPTY LIST OF PARAMS\n");
  return params;
}

static std::vector<std::unique_ptr<ASTnode>> ParseParamListPrime() {
  std::vector<std::unique_ptr<ASTnode>> param_list_prime;

  int i = CurTok.type;
  while (i == COMMA) {
    getNextToken();
    auto param = ParseParam();
    if (param) {
      param_list_prime.push_back(std::move(param));
    }
    i = CurTok.type;
  }

  return param_list_prime;
}

static std::vector<std::unique_ptr<ASTnode>> ParseParamList() {
  std::vector<std::unique_ptr<ASTnode>> param_list;

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

static std::unique_ptr<ASTnode> ParseParam() {
  int t = CurTok.type;
  if ((t==INT_TOK) || (t==FLOAT_TOK) || (t==BOOL_TOK)) {
    auto type = std::make_unique<VarTypeASTnode>(CurTok);

    getNextToken();
    if (CurTok.type == IDENT) {
      auto param = std::make_unique<ParamASTnode>(std::move(type), CurTok);
      getNextToken();
      return std::move(param);

    } else {
      fprintf(stderr, "ERROR missing IDENT of parameter\n");
    }
  } else {
    fprintf(stderr, "ERROR in ParseParam\n");
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
    }

  } else {
    fprintf(stderr, "ERROR missing semicolon or Return statement\n");
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
        fprintf(stderr, "ERROR left bracket missing in Else blcok\n");
      }

    } else {
      fprintf(stderr, "ERROR missing right parantheses WhileStmt\n");
    }

  } else {
    fprintf(stderr, "ERROR mssing left parantheses WhileStmt\n");
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
      fprintf(stderr, "ERROR left bracket missing in Else blcok\n");
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
      fprintf(stderr, "ERROR missing right parantheses WhileStmt\n");
    }
  } else {
    fprintf(stderr, "ERROR mssing left parantheses WhileStmt\n");
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
      fprintf(stderr, "ERROR missing right bracket in ParseBlock\n");
    }

  } else {
    fprintf(stderr, "ERROR in ParseBlock\n");
  }
  return nullptr;
}

static std::vector<std::unique_ptr<ASTnode>> ParseStmtList() {
  std::vector<std::unique_ptr<ASTnode>> stmt_list;

  int t = CurTok.type;
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

  }
  return stmt_list;
}

static std::vector<std::unique_ptr<ASTnode>> ParseLocalDecls() {
  std::vector<std::unique_ptr<ASTnode>> decl_list;
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
        auto semicol = std::make_unique<SemicolASTnode>(CurTok);

        auto Result = std::make_unique<LocalDeclASTnode>(std::move(var_type),ident,std::move(semicol));
        getNextToken();
        return std::move(Result);


      } else {
        fprintf(stderr, "Semicol is missing after localDecl\n");
      }

    } else {
      fprintf(stderr, "Ident missing in LocalDecl\n");
    }

  } else {
    return nullptr;
    fprintf(stderr, "error in ParseLocalDecl\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
    fprintf(stderr, "error in ParseVarType\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
  } else {
    fprintf(stderr, "error with neg val\n");
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
    fprintf(stderr, "error in ParseStmt\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
      fprintf(stderr, "Semicol is expected in PArseExprStmt\n");
    }
  }
  else if (t==SC) {
    auto semicol = std::make_unique<SemicolASTnode>(CurTok);
    getNextToken();
    return std::move(semicol);

  } else {
    fprintf(stderr, "expected semicolon in ParseExprStmt\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
  }
  return nullptr;

}

static std::unique_ptr<ASTnode> ParseExpr() {
  int t = CurTok.type;

  if (t==IDENT) {
    TOKEN identTok = CurTok;
    getNextToken();
    if (CurTok.type == ASSIGN) {
      getNextToken();
      auto expr_parsed = ParseExpr();
      if (expr_parsed) {
        auto Result = std::make_unique<AssignASTnode>(identTok, std::move(expr_parsed));
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
    fprintf(stderr, "error in ParseExpr\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
    fprintf(stderr, "error in rval1expr\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
    fprintf(stderr, "error in rval2expr\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
    fprintf(stderr, "error in rval3expr\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
    fprintf(stderr, "error in rval4expr\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
    fprintf(stderr, "error in rval5expr\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
    fprintf(stderr, "error in rval6expr\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
      getNextToken();
      auto arg_list = ParseArgsList();

      auto Result = std::make_unique<FunCallASTnode>(identTok, std::move(arg_list));
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
    fprintf(stderr, "error in rval7\n");
    //return LogError(CurTok,"expected either Int, Float or Bool value\n");
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
  return arg_list_prime;
}

static std::unique_ptr<ASTnode> ParseArg() {
  int t = CurTok.type;
  if ((t==INT_LIT) || (t==FLOAT_LIT) || (t==BOOL_LIT) || (t==MINUS) || (t==NOT) || (t==IDENT) || (t==LPAR)) {
    auto arg = ParseExpr();
    if (arg) {
      return std::move(arg);
    }
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

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

  // fprintf(stderr, "Token: %s with type %d and value is %d\n", CurTok.lexeme.c_str(),
  //         CurTok.type, IntVal);



  // Run the parser now.
  parser();
  // auto expr = ParseExternList();
  // for (unsigned i=0; i<expr.size(); i++) {
  //   fprintf(stderr, "===\n");
  //   fprintf(stderr, "%s\n", expr.at(i)->to_string().c_str());
  // }
  // fprintf(stderr, "test node value is \n%s\n", expr->to_string().c_str());
  fprintf(stderr, "%s\n", CurTok.lexeme.c_str());
  fprintf(stderr, "Parsing Finished\n");

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
