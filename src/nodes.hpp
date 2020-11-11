#ifndef ECE467_NODE_HPP_INCLUDED
#define ECE467_NODE_HPP_INCLUDED

#include "location.hh"
#include <memory>
#include <vector>

enum Type {VOID, INT, FLOAT, LOGICAL};
typedef enum Type Type;

enum AugmentedAssignOp {PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN};
typedef enum AugmentedAssignOp AugmentedAssignOp;

enum EqOp {EQ, NEQ};
typedef enum EqOp EqOp;

enum CompOp {GT, GE, LT, LE};
typedef enum CompOp CompOp;

enum PlusOp {PLUS, MINUS};
typedef enum PlusOp PlusOp;

enum MulOp {MUL, DIV};
typedef enum MulOp MulOp;



class Node;
class Root;
class Function;
class FunctionList;
class FunctionDeclaration;
class FunctionDefinition;
class ParameterList;
class Block;
class Suite;
class Declaration;
class DeclarationAssign;
class SimpleAssign;
class AugmentedAssign;
class Break;
class Continue;
class ReturnVoid;
class ReturnNotVoid;
class CompoundStatement;
class If;
class For;
class While;
class Expression;
class TernaryExpression;
class OrExpression;
class AndExpression;
class EqExpression;
class PlusExpression;
class MulExpression;
class CastExpression;
class UnaryMinusExpression;
class Int;
class Float;
class Bool;
class NameExpression;
class FunctionCall;

class Node {
public:
	yy::location location;

	virtual ~Node() = 0;
};

class Root: public Node {
public:
	std::unique_ptr<Node> FuncList;

	Root (std::unique_ptr<Node> FunctionList) {
		FuncList = std::move(FunctionList);
	}
};

class Function: public Node {};

class FunctionList: public Node {
public:
	std::vector<std::unique_ptr<Function>> list;

	FunctionList(std::unique_ptr<Function> func){
		list.push_back(std::move(func));
	}
};

class FunctionDeclaration: public Function {
	public:
		Type type;
		std::string name;
		std::unique_ptr<ParameterList> paramList;

		FunctionDeclaration(Type t, std::string n, std::unique_ptr<ParameterList> param_list){
			type = t;
			name = n;
			paramList = std::move(param_list);
		}
};

class FunctionDefinition: public Function{
	public:
		std::unique_ptr<FunctionDeclaration> funcDecl;
		std::unique_ptr<Block> blockNode;

		FunctionDefinition(std::unique_ptr<FunctionDeclaration> function_decl, std::unique_ptr<Block> block){
			funcDecl = std::move(function_decl);
			blockNode = std::move(block);
		}
};

class ParameterList: public Node {
	public:
		std::unique_ptr<std::vector<std::unique_ptr<Declaration>>> paramList;

		ParameterList(){}
};

class Block: public Node {};

class Suite: public Block{
	public:
		std::vector<std::unique_ptr<Node>> suiteList;

		Suite(){}
};

class Declaration: public Node{
	public:
		Type type;
		std::string name;

		Declaration(Type t, std::string n){
			type = t;
			name = n;
		}
};

class DeclarationAssign: public Node {
public:
	std::unique_ptr<Declaration> decl;
	std::unique_ptr<Expression> expr;
	
	DeclarationAssign(std::unique_ptr<Declaration> declaration, std::unique_ptr<Expression> expression){
		decl = std::move(declaration);
		expr = std::move(expression);
	}

};

class SimpleAssign: public Node {
public:
	std::string n;
	std::unique_ptr<Expression> expr;

	SimpleAssign(std::string name, std::unique_ptr<Expression> expression) {
		n = name;
		expr = std::move(expression);
	}
};

class AugmentedAssign: public Node {
public:
	std::string n;
	std::unique_ptr<Expression> expr;
	AugmentedAssignOp op;

	AugmentedAssign(std::string name, std::unique_ptr<Expression> expression, AugmentedAssignOp operation) {
		n = name;
		expr = std::move(expression);
		op = operation;
	}
};

class Break: public Node {};

class Continue: public Node {};

class ReturnVoid: public Node {};

class ReturnNotVoid: public Node {
public:
	std::unique_ptr<Expression> expr;

	ReturnNotVoid(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}
};

class CompoundStatement: public Node {};

class If: public CompoundStatement {
public:
	std::unique_ptr<Expression> expr;
	std::unique_ptr<Block> b;
	
	If(std::unique_ptr<Expression> expression, std::unique_ptr<Block> block) {
		expr = std::move(expression);
		b = std::move(block);
	}
};

class For: public CompoundStatement {
public:
	std::unique_ptr<Node> s1;
	std::unique_ptr<Expression> expr;
	std::unique_ptr<Node> s2;
	std::unique_ptr<Block> b;

	For(std::unique_ptr<Node> statement1, std::unique_ptr<Node> statement2, std::unique_ptr<Expression> expression, std::unique_ptr<Block> block) {
		s1 = std::move(statement1);
		s2 = std::move(statement2);
		expr = std::move(expression);
		b = std::move(block);
	}
};

class While: public CompoundStatement {
public:
	std::unique_ptr<Expression> expr;
	std::unique_ptr<Block> b;
	
	While(std::unique_ptr<Expression> expression, std::unique_ptr<Block> block) {
		expr = std::move(expression);
		b = std::move(block);
	}
};

class Expression : public Node {};

class TernaryExpression : public Expression {
public:
	std::unique_ptr<Expression> oExpression;
	std::unique_ptr<Expression> tExpression1;
	std::unique_ptr<Expression> tExpression2;

	TernaryExpression (std::unique_ptr<Expression> orExpression, std::unique_ptr<Expression> ternaryExpression1, std::unique_ptr<Expression> ternaryExpression2) {
		oExpression = std::move(orExpression);
		tExpression1 = std::move(ternaryExpression1);
		tExpression2 = std::move(ternaryExpression2);
	}
};

class OrExpression : public Expression {
public:
	std::unique_ptr<Expression> oExpression;
	std::unique_ptr<Expression> aExpression;
	
	OrExpression (std::unique_ptr<Expression> orExpression, std::unique_ptr<Expression> andExpression) {
		oExpression = std::move(orExpression);
		aExpression = std::move(andExpression);
	}
};

class AndExpression : public Node {
public:
	std::unique_ptr<Expression> aExpression;
	std::unique_ptr<Expression> eqExpression;

	AndExpression (std::unique_ptr<Expression> andExpression, std::unique_ptr<Expression> equalityExpression) {
		aExpression = std::move(andExpression);
		eqExpression = std::move(equalityExpression);
	}	
};

class EqExpression : public Node {
public:
	std::unique_ptr<Expression> eqExpression;
	std::unique_ptr<Expression> cExpression;
	EqOp op;
	
	EqExpression (std::unique_ptr<Expression> equalityExpression, std::unique_ptr<Expression> comparisonExpression, EqOp equalityOperator) {
		eqExpression = std::move(equalityExpression);
		cExpression = std::move(comparisonExpression);
		op = equalityOperator;
	}
	
};

class CompExpression : public Expression {
public:
	std::unique_ptr<Expression> cExpression;
	std::unique_ptr<Expression> pExpression;
	CompOp op;
	
	CompExpression (std::unique_ptr<Expression> compExpression, std::unique_ptr<Expression> plusExpression, CompOp comparisonOperator) {
		cExpression = std::move(compExpression);
		pExpression = std::move(plusExpression);
		op = comparisonOperator;
	}
	
};

class PlusExpression : public Expression {
public:
	std::unique_ptr<Expression> pExpression;
	std::unique_ptr<Expression> mExpression;
	PlusOp op;
	
	PlusExpression (std::unique_ptr<Expression> plusExpression, std::unique_ptr<Expression> mulExpression, PlusOp plusOperator) {
		pExpression = std::move(plusExpression);
		mExpression = std::move(mulExpression);
		op = plusOperator;
	}
};

class MulExpression : public Expression {
public:
	std::unique_ptr<Expression> mExpression;
	std::unique_ptr<Expression> term;
	MulOp op;

	MulExpression(std::unique_ptr<Expression> multiplicationExpression, std::unique_ptr<Expression> argTerm, MulOp operation) {
		mExpression = std::move(multiplicationExpression);
		term = std::move(argTerm);
		op = operation;
	}
};

class CastExpression : public Expression {
public:
	Type cType;
	std::unique_ptr<Expression> cExpression;

	CastExpression(Type castType, std::unique_ptr<Expression> castExpression) {
		cType = castType;
		cExpression = std::move(castExpression);
	}	
};

class UnaryMinusExpression : public Expression {
public:
	std::unique_ptr<Expression> expr;

	UnaryMinusExpression(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}
};

class Int : public Expression {
public:
	int data;

	Int(int arg) {
		data = arg;
	}
};

class Float : public Expression {
	float data;

	Float(float arg) {
		data = arg;
	}
};

class Bool : public Expression {
public:
	bool data;

	Bool(bool arg) {
		data = arg;
	}
};

class NameExpression : public Expression {
	std::string name;

	NameExpression (std::string arg) {
		this -> name = arg;
	}
};

class FunctionCall : public Expression {
public:
	std::string n;
	std::unique_ptr<std::vector<std::unique_ptr<Expression>>>  args;

	FunctionCall(std::string arg) {
		n = arg;
	}
};




#endif // ECE467_NODE_HPP_INCLUDED
