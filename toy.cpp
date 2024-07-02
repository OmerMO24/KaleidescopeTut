#include "KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>


using namespace llvm;
using namespace llvm::orc;

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
// Includes virtual codegen methods
// The Value class represents an SSA register they're cool check them out.
class ExprAST {
public:
	virtual ~ExprAST() = default; // default constructor for the expression object
	virtual Value *codegen() = 0;
};

// NumberExprAST - Expression class for numeric literals like "1.0"
class NumberExprAST : public ExprAST {
	double Val;

public:
	NumberExprAST(double Val) : Val(Val) {}
	Value *codegen() override;
};


// VariableExprAST - Expression class for referencing a variable, like "a"
class VariableExprAST : public ExprAST {

	std::string Name;

public:
	VariableExprAST(const std::string &Name) : Name(Name) {}
	Value *codegen() override;
};

// BinaryExprAST - Expression calss for a binary operator e.g. +, *, /, ==
class BinaryExprAST : public ExprAST {

	char Op;
	std::unique_ptr<ExprAST> LHS, RHS;

public:
		BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) :
			Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		Value *codegen() override;
};


// CallExprAST - Expression class for function calls
class CallExprAST : public ExprAST {

	std::string Callee;
	std::vector<std::unique_ptr<ExprAST>> Args;

public: 
	CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args) : 
		Callee(Callee), Args(std::move(Args)) {}
	Value *codegen() override;
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
	
	Function *codegen();
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

	Function *codegen();
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


// BinopPrecedence - This holds the precedence for each binary operator defined.
static std::map<char, int> BinopPrecedence;

// GetTokPrecedence - Get the precedence of the pending binary operator token 
static int GetTokPrecedence() {
	if (!isascii(CurTok))
		return -1;
	
	// Make usre it's a declared binop
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0) return -1;
	return TokPrec;
}



// binoprhs
// ::= ('+', primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
	// If this is a binop, find its precedence 
	while (true) {
		int TokPrec = GetTokPrecedence();

		// If this is a binop that binds as least as tightly as the current binop,
		// consume it, otherwise we are done 
		if (TokPrec < ExprPrec)
			return LHS;

		// Okay, we know this is a binop.
		int BinOp = CurTok;
		getNextToken(); // eat binop 
		

		// Parse the primary expression after the binary operator 
		auto RHS = ParsePrimary();
		if (!RHS) 
			return nullptr;
		
		// We now need to check which side the binary op binds less tightly with 
		// If the next operator has higher precedence than the current BinOP, then let it take the RHS as its LHS
		// E.g. If we have parsed a + b so far, we might have "a + b * c", so the parser looks ahead to see if there's another
		// operator after b. If that operator has higher precedence (in this case it does) it takes the curren RHS as its LHS 
		// So we end up with a + (b * c)
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}

		// We merge the LHS and RHS together to form our binary expression
		// This would convert "a+b+" to (a+b) and execute the next loop iteration with the trailing "+" as the current token
		LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
	}
}


// expression 
//	::= primary binorphs
//
static std::unique_ptr<ExprAST> ParseExpression() {
	auto LHS = ParsePrimary();
	if (!LHS) 
		return nullptr;
	
	return ParseBinOpRHS(0, std::move(LHS));
}

// prototype 
//	::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
	if (CurTok != tok_identifier)
		return LogErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr; 
	getNextToken();

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");

	// Read the list of argument names 
	std::vector<std::string> ArgNames;
	while (getNextToken() == tok_identifier)
		ArgNames.push_back(IdentifierStr);
	if(CurTok != ')')
		return LogErrorP("Expected ')' in prototype");

	// success
	getNextToken(); // eat ')'
	
	return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}


// definition ::= 'def' prototype expression
// This is just the function prototype plus an expression to implement the body 
// So functions are essentially composed of a prototype plus a body expression 
static std::unique_ptr<FunctionAST> ParseDefinition() {
	getNextToken(); // eat 'def'
	auto Proto = ParsePrototype();
	if (!Proto) return nullptr;

	if (auto E = ParseExpression())
		return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	return nullptr;
}


// external ::= 'extern' prototype 
// extern is used similarly as in C/C++, declares golobal variable or function that is defined in another file or library 
// No memory allocated by extern
static std::unique_ptr<PrototypeAST> ParseExtern() {
	getNextToken(); // eat extern
	return ParsePrototype();
}

// toplevelexpr ::= expression 
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	if (auto E = ParseExpression()) {
		// Make an anonymous proto
		auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
		return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}


// Codegen: Transforming the AST into LLVM IR (Intermediate Representation)

// LogError used to report errors during code generation (similar to how we reported errors during parsing)
static std::unique_ptr<LLVMContext> TheContext; // Needed to pass into APIs that require it 
static std::unique_ptr<IRBuilder<>> Builder; // helper that makes it easy to generate LLVM instructions 
static std::unique_ptr<Module> TheModule; // Contains functions and global variables; Owns memory for all IR generated
static std::map<std::string, Value *> NamedValues; // Keeps tracks of values defined in current scope and their LLVM representation 
												   // Essentially a symbol table for the code
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;







Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}

// Note that builder only emits code right now, but will be set up later to generate code into something 

// Generating LLVM code for expression nodes

Function *getFunction(std::string Name) {
	// First, see if the function has already been added to the current module 
	if (auto *F = TheModule->getFunction(Name))
		return F;

	// If not, check whether we can codegen the declaration from some existing prototype
	auto FI = FunctionProtos.find(Name);
	if (FI != FunctionProtos.end())
		return FI->second->codegen();

	// If no existing prototype exists, return null
	return nullptr;

}


// In LLVM IR, numeric constants are represented with the ConstantFP class "Constant Floating Point"
// CFP holds the value in an APFloat which itself is capable of holding floating point constants of arbitrary precision
// THe above code just creates and returns a CFP
Value *NumberExprAST::codegen() {
	return ConstantFP::get(*TheContext, APFloat(Val));
}


// For now, we assume that the variable has already been emitted somewhere and its value is available

Value *VariableExprAST::codegen() {
	// Look this variable up in the symbol table  
	Value *V = NamedValues[Name];
	// If the variable does not exist (is not in the symbol table) then V will be null
	if (!V)
		return LogErrorV("Unknown variable Name");
	return V;
}

// Recursively emit code for LHS and RHS, then compute the result of the binary expression.
// fcmp always returns an i1 (one bit integer), kaleidescope only recognizes doubles
// So we combine fcmp with a uitofp (unsigned int to floating pont)
Value *BinaryExprAST::codegen() {
	Value *L = LHS->codegen();
	Value *R = RHS->codegen();
	if (!L || !R)
		return nullptr;

	switch (Op) {
		case '+':
			return Builder->CreateFAdd(L, R, "addtmp"); // IRBuilder knows where to insert the instruction
		case '-':
			return Builder->CreateFSub(L, R, "subtmp");
		case '*':
			return Builder->CreateFMul(L, R, "multmp");
		case '<':
			L = Builder->CreateFCmpULT(L, R, "cmptmp");
			// Convert bool 0/1 to double 0.0 or 1.0
			return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
		default:
			return LogErrorV("invalid binary operator");
	}
}


Value *CallExprAST::codegen() {
	// Look up the name in the gloabl module table 
	Function *CalleeF = getFunction(Callee); // The module is the container that holds the functions
	if (!CalleeF)
		return LogErrorV("Unknown function referenced"); 
	
	// If argument mismatch error 
	if (CalleeF->arg_size() != Args.size())
		return LogErrorV("Incorrect # arguments passed"); // Kaleidescope only has one type, how would we check if the argument types match?
	
	std::vector<Value *> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i) {
		ArgsV.push_back(Args[i]->codegen()); // Recursively codegen each argument to be passed in
		if (!ArgsV.back())
			return nullptr;
	}
	
	return Builder->CreateCall(CalleeF, ArgsV, "calltmp"); // create an LLVM call instruction. LLVM uses the C calling convention 
														   // which allows us to call into stdlib functions like sin and cos with 
														   // no additional overhead
}


// Codegen for functions and prototypes

// returns a function: a prototype represents the external interface for a function
// How LLVM IR represents function declarations
Function *PrototypeAST::codegen() {
	// Make the function type 
	std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(*TheContext));

	FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false); // creates a function type that takes N doubles as arguments
																						  // returns one double as a result & is NOT vararg (takes a
																						  // fixed # of arguments)
	
	Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get()); // Creates the IR function corresponding to the prototype
																						  // External Linkage: callable by functions outside the module
	
	// Set names for all arguemnts 
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);
	// makes IR more readable by keeping argument names consistent. 
	
	return F;
}

// Codegen for function body 
Function *FunctionAST::codegen() {
	
	// Transfer ownership of the prototype to the FunctionProtos map, but keep a reference to it for use below 
	auto &P = *Proto;
	FunctionProtos[Proto->getName()] = std::move(Proto);
	Function *TheFunction = getFunction(P.getName());
	if (!TheFunction)
		return nullptr;

	// Create a new basic block to start insertion into 
	BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction); // This is a 'basic block' which is inserted into TheFunction
	Builder->SetInsertPoint(BB); // Tells the builder to insert new instructions at the end of the basic block
								 // Basic blocks define the control flow graph of a function 

	// Record the function arguments in the NamedValues map 
	NamedValues.clear(); 
	for (auto &Arg : TheFunction->args())
		NamedValues[std::string(Arg.getName())] = &Arg;

	if (Value *RetVal = Body->codegen()) {
		// Finish off the function 
		Builder->CreateRet(RetVal);
		
		// Validate the generated code, checking for consistency;
		verifyFunction(*TheFunction);

		// Optimize the function
		TheFPM->run(*TheFunction, *TheFAM);

		return TheFunction;
	}

	// Error reading body, remove function 
	TheFunction->eraseFromParent();
	return nullptr;

}

// 01/07/24: Adding function pass manager for optimization passes
static void InitializeModulesAndManagers() {
  // Open a new context and module.
	TheContext = std::make_unique<LLVMContext>();
	TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
	TheModule->setDataLayout(TheJIT->getDataLayout());

	// Create a new builder for the module.
	Builder = std::make_unique<IRBuilder<>>(*TheContext);

	// Create new pass and analysis managers.
	TheFPM = std::make_unique<FunctionPassManager>();
	TheLAM = std::make_unique<LoopAnalysisManager>();
	TheFAM = std::make_unique<FunctionAnalysisManager>();
	TheCGAM = std::make_unique<CGSCCAnalysisManager>();
	TheMAM = std::make_unique<ModuleAnalysisManager>();
	ThePIC = std::make_unique<PassInstrumentationCallbacks>();
	TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
                                                     /*DebugLogging*/ true);
	TheSI->registerCallbacks(*ThePIC, TheMAM.get());

	// Add transform passes.
	// Do simple "peephole" optimizations and bit-twiddling optzns.
	TheFPM->addPass(InstCombinePass());
	// Reassociate expressions.
	TheFPM->addPass(ReassociatePass());
	// Eliminate Common SubExpressions.
	TheFPM->addPass(GVNPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	TheFPM->addPass(SimplifyCFGPass());

	// Register analysis passes used in these transform passes.
	PassBuilder PB;
	PB.registerModuleAnalyses(*TheMAM);
	PB.registerFunctionAnalyses(*TheFAM);
	PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      ExitOnErr(TheJIT->addModule(
          ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
      InitializeModulesAndManagers();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
	
	if (auto FnAST = ParseTopLevelExpr()) {
		if (FnAST->codegen()) {
			// Create a ResourceTrackker to track JIT'd memory allocated to our anonymous expression 
			// that way we can free it after executing 
			auto RT = TheJIT->getMainJITDylib().createResourceTracker();

			auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
			ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
			InitializeModulesAndManagers();

			// Search the JIT for the __anon_expr symbol
			auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
			
			// Get the symbol's address and cast it to the right type (takes no args)
			// so we can call it as a native function
			double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
			fprintf(stderr, "Evaluatede to %f\n", FP());

			// Delete the anonymous expression module from the JIT 
			ExitOnErr(RT->remove());

		} 
	} else {
		// Skip next token for error recovery
		getNextToken();
	}
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}


int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

  // Run the main "interpreter loop" now.
  MainLoop();

  return 0;
}
