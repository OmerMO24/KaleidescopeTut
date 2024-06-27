#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>


enum token {
	tok_eof = -1,

	//commands 
	tok_def = -2,
	tok_extern = -3,

	//primary
	tok_identifier = -4,
	tok_number = -5,
};

static std::string IdentifierStr; //filled in if tok_identifier
static double NumVal; // filled in if tok_number



static int gettok()
{
	static int LastChar = ' ';
	
	//skip any whitespace
	while (isspace(LastChar))
		LastChar = getchar();

	if (isalpha(LastChar)){
		IdentifierStr = LastChar;
		while (isalnum((LastChar = getchar())))
			IdentifierStr += LastChar;

		if (IdentifierStr == "def")
			return tok_def;
		if (IdentifierStr == "extern")
			return tok_extern;
		return tok_identifier;
	}
	
	if (isdigit(LastChar) || LastChar == '.') {
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar) || LastChar == '.');

		NumVal = strtod(NumStr.c_str(), 0);
		return tok_number;
	}

	if (LastChar == '#') {
		// Comment unti end of line 
		do
			LastChar = getchar();
		while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

		if (LastChar != EOF)
			return gettok();
	}

	// CHeck for end of file. Don't eat the EOF.
	if (LastChar == EOF)
		return tok_eof;

	// Otherwise, just return the character as its ascii value
	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;

}




// We are using recursive descen parsing where every function/object represents a non-terminal from our grammar (fuck you PLD)
// ExprAST - Base class for all expression nodes

class ExprAST {
public:
	virtual ~ExprAST() = default; // default constructor for the expression object 
};

// NumberExprAST - Expression class for numeric literals like "1.0"
class NumberExprAST : public ExprAST {
	double Val;

public:
	NumberExprAST(double val) : Val(Val) {}
};


// VariableExprAST - Expression class for referencing a variable, like "a"
class VariableExprAST : public ExprAST {

	std::string Name;

public:
	VariableExprAST(const std::string &Name) : Name(Name) {}
};

// BinaryExprAST - Expression calss for a binary operator e.g. +, *, /, ==
class BinaryExprAST : public ExprAST {

	char Op;
	std::unique_ptr<ExprAST> LHS, RHS;

public:
		BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) :
			Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};


// CallExprAST - Expression class for function calls
class CallExprAST : public ExprAST {

	std::string Callee;
	std::vector<std::unique_ptr<ExprAST>> Args;

public: 
	CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args) : 
		Callee(Callee), Args(std::move(Args)) {}
};

// PrototypeAST - This class represents the "prototype" for a function,
// which captures its name, and its argument names (thus implicitly the number
// of arguments the function takes).
class PrototypeAST {

	std::string Name;
	std::vector<std::string> Args;

public:
	PrototypeAST(const std::string &Name, const std::vector<std::string> Args) 
		: Name(Name), Args(std::move(Args)) {}

	const std::string &getName() const { return Name; }
};


// FunctionAST - This class reprsents a function definition itself
// In Kscp, all values are doubles, so the type of each variable doesn't have to be stored anywhere.
// In a more realistic language, the ExprAST class would probably have a type field
class FunctionAST {
	std::unique_ptr<PrototypeAST> Proto;
	std::unique_ptr<ExprAST> Body;

public:
	FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body) 
		: Proto(std::move(Proto)), Body(std::move(Body)) {}
};

// CurTok/getNextToken - Provide a simple token buffer. CurTok is the current token 
// the parser is looking at. getNextToken reads another token from the Lexer and updates
// CurTok with its results.
static int CurTok;
static int getNextToken() {
	return CurTok = gettok();
}


// LogError* = These are little helper functions for error handling 
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str){
	LogError(Str);
	return nullptr;
}


static std::unique_ptr<ExprAST> ParseExpression();


// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	auto Result = std::make_unique<NumberExprAST>(NumVal);
	getNextToken(); // consume the number
	return std::move(Result);
}


// parenexpr ::= '(' expression ')'
// Expects that current token is a (, but possible that no ) is waiting
// so the parser should emit an error.
static std::unique_ptr<ExprAST> ParseParenExpr() {
	getNextToken(); // eat (.
	auto V = ParseExpression(); // recursively calls ParseExpression() which allows us to handle recursive grammars
	if (!V)
		return nullptr;

	if(CurTok != ')')
		return LogError("expected ')'");
	getNextToken();
	return V;
}



// identifierexpr
//  ::= identifier 
//  ::= identifier '(' epxression* ')'
//  Expected to be called if the current token is a tok_identifier token (variable name, function calls)
static std::unique_ptr<ExprAST> ParseIdentifierExpr () {
	std::string IdName = IdentifierStr;

	getNextToken(); // eat identifier 
	
	if (CurTok != '(') // case of simple variable reference
		return std::make_unique<VariableExprAST>(IdName);

	// function call 
	getNextToken(); // eat (
	std::vector<std::unique_ptr<ExprAST>> Args;
	if (CurTok != ')') {
		while (true) {
			if (auto Arg = ParseExpression())
				Args.push_back(std::move(Arg));
			else
				return nullptr;

			if (CurTok == ')')
				break;

			if (CurTok != ',')
				return LogError("Expected ')' or ','  in argument list");
			getNextToken();
		}
	}

	// Eat the ')'
	getNextToken();

	return std::make_unique<CallExprAST>(IdName, std::move(Args));
}


// We now have all of our basic expression parsing logic 
// We now need to wrap it all in a helper to parse primary epxressions (primarily for identifiers
// and numbers)

// primary 
//	 ::= identifierexpr
//	 ::= numberexpr
//	 ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
	switch (CurTok) {
		default:
			return LogError("unknown token when expecting an expression");
		case tok_identifier:
			return ParseIdentifierExpr();
		case tok_number:
			return ParseNumberExpr();
		case '(':
			return ParseParenExpr();
	}
}




