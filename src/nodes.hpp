#ifndef ECE467_NODE_HPP_INCLUDED
#define ECE467_NODE_HPP_INCLUDED

#include "location.hh"
#include <memory>
#include <vector>
#include <map>
#include <set>
#include <iostream>

enum Type {ERROR, NONE, VOID, INT, FLOAT, LOGICAL};
typedef enum Type Type;

enum AugmentedAssignOp {PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN};
typedef enum AugmentedAssignOp AugmentedAssignOp;

enum BinaryOp {PLUS, MINUS, MUL, DIV, AND, OR, EQ, NEQ, LT, GT, LE, GE};
typedef enum BinaryOp BinaryOp;

class Node;
class Root;
class Function;
class FunctionList;
class FunctionDeclaration;
class FunctionDefinition;
class ParameterList;
class Block;
class Suite;
class ExpressionStatement;
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
class BinaryExpression;
class CastExpression;
class UnaryMinusExpression;
class Int;
class Float;
class Bool;
class NameExpression;
class FunctionCall;
class FunctionState;

class Node {
public:
	yy::location location;

	virtual ~Node() = 0;

	virtual Type checkType(std::map<std::string, Type> & scope) = 0;
	virtual void printTree(){return;}
	virtual bool checkReturn() { return false; }
	virtual bool isReturn() { return false; }
	virtual bool checkFuncDuplicates() { return false; }
	virtual bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {return true; }
	virtual std::unique_ptr<Node> optimizeCP() = 0;

};

class Root: public Node {
public:
	std::unique_ptr<FunctionList> funcList;

	Root (std::unique_ptr<FunctionList> functionList) {
		printf("Making the root node \n");
		funcList = std::move(functionList);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool checkFuncDuplicates() override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<Root> optimizeCP() override;
};

class Function: public Node {
public:
	virtual bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) = 0;
};

class FunctionList: public Node {
public:
	std::vector<std::unique_ptr<Function>> list;

	FunctionList(std::unique_ptr<Function> func){
		printf("Making the function list \n");
		list.push_back(std::move(func));
	}
	
	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool checkFuncDuplicates() override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;

	void printTree() override;
	std::unique_ptr<FunctionList> optimizeCP override;
};

class FunctionDeclaration: public Function {
public:
	Type type;
	std::string name;
	std::unique_ptr<ParameterList> paramList;

	FunctionDeclaration(Type t, std::string n, std::unique_ptr<ParameterList> param_list){
		printf("Making the Function Declaration \n");
		type = t;
		name = n;
		paramList = std::move(param_list);
	}

	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) override;
	std::unique_ptr<FunctionDeclaration> optimizeCP() override;
	void printTree() override;
};

class FunctionDefinition: public Function{
public:
	std::unique_ptr<FunctionDeclaration> funcDecl;
	std::unique_ptr<Block> blockNode;
	Type type;

	FunctionDefinition(std::unique_ptr<FunctionDeclaration> function_decl, std::unique_ptr<Block> block){
		printf("Making the function definition \n");
		funcDecl = std::move(function_decl);
		blockNode = std::move(block);
	}

	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) override;
	std::unique_ptr<FunctionDefinition> optimizeCP() override;
	void printTree() override;
};

class ParameterList: public Node {
public:
		std::unique_ptr<std::vector<std::unique_ptr<Declaration>>> paramList;

		ParameterList() {
			printf("Making the parameter list \n");
			paramList = std::make_unique<std::vector<std::unique_ptr<Declaration>>>();
		}
		Type checkType(std::map<std::string, Type> & scope) override;
		std::unique_ptr<ParameterList> optimizeCP() override;
		void printTree() override;
};

class Block: public Node {};

class Suite: public Block{
	public:
		std::vector<std::unique_ptr<Node>> suiteList;

		Suite(){
			printf("Making the suite \n");
		}
		bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
		Type checkType(std::map<std::string, Type> & scope) override;
		bool checkReturn() override;
		std::unique_ptr<Suite> optimizeCP() override;
		bool isBool(){ return false; };
		void printTree() override;
};

class SingleStatement : public Node {};

class ExpressionStatement: public SingleStatement {
public:
	std::unique_ptr<Expression> expr;
	
	ExpressionStatement(std::unique_ptr<Expression> expression) {
		printf("Inside ExpressionStatement constructor \n");
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;

};

class Declaration: public SingleStatement {
public:
	Type type;
	std::string name;

	Declaration(Type t, std::string n){
		printf("Declaration constructor \n");
		type = t;
		name = n;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Declaration> optimizeCP() override;
};

class DeclarationAssign: public SingleStatement {
public:
	std::unique_ptr<Declaration> decl;
	std::unique_ptr<Expression> expr;
	
	DeclarationAssign(std::unique_ptr<Declaration> declaration, std::unique_ptr<Expression> expression){
		decl = std::move(declaration);
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<DeclarationAssign> optimizeCP() override;
};

class SimpleAssign: public SingleStatement {
public:
	std::string n;
	std::unique_ptr<Expression> expr;

	SimpleAssign(std::string name, std::unique_ptr<Expression> expression) {
		n = name;
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<SimpleAssign> optimizeCP() override;
};

class AugmentedAssign: public SingleStatement {
public:
	std::string n;
	std::unique_ptr<Expression> expr;
	AugmentedAssignOp op;

	AugmentedAssign(std::string name, std::unique_ptr<Expression> expression, AugmentedAssignOp operation) {
		n = name;
		expr = std::move(expression);
		op = operation;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<AugmentedAssign> optimizeCP() override;
};

class Break: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;	
	void printTree() override;
	std::unique_ptr<Break> optimizeCP() override;
};

class Continue: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Continue> optimizeCP() override;

};

class ReturnVoid: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;
	bool isReturn() override;
	void printTree() override;
	std::unique_ptr<ReturnVoid> optimizeCP() override;
};

class ReturnNotVoid: public SingleStatement {
public:
	std::unique_ptr<Expression> expr;

	ReturnNotVoid(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	bool isReturn() override;
	void printTree() override;
	std::unique_ptr<ReturnNotVoid> optimizeCP() override;
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

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<If> optimizeCP() override;
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

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<For> optimizeCP() override;
};

class While: public CompoundStatement {
public:
	std::unique_ptr<Expression> expr;
	std::unique_ptr<Block> b;
	
	While(std::unique_ptr<Expression> expression, std::unique_ptr<Block> block) {
		expr = std::move(expression);
		b = std::move(block);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<While> optimizeCP() override;
};

class Expression : public Node {
public:
	Type type;
	virtual bool isConst() {return false; }
	virtual bool isBool() {return false; }
	virtual bool isFloat() {return false; }
	virtual bool isInt() {return true; }
};

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

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<TernaryExpression> optimizeCP() override;
};

class BinaryExpression : public Expression {
public:
	std::unique_ptr<Expression> expression1;
	std::unique_ptr<Expression> expression2;
	BinaryOp op;

	BinaryExpression (std::unique_ptr<Expression> expr1, std::unique_ptr<Expression> expr2, BinaryOp operation) {
		expression1 = std::move(expr1);
		expression2 = std::move(expr2);
		op = operation;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<Expression> optimizeCP() override;
};

class CastExpression : public Expression {
public:
	std::unique_ptr<Expression> cExpression;

	CastExpression(Type castType, std::unique_ptr<Expression> castExpression) {
		type = castType;
		cExpression = std::move(castExpression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<Expression> optimizeCP() override;
};

class UnaryMinusExpression : public Expression {
public:
	std::unique_ptr<Expression> expr;

	UnaryMinusExpression(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<Expression> optimizeCP() override;
};

class ConstantExpression : public Expression {
	bool isConstant() override {return true};
}

class Int : public ConstantExpression {
public:
	int data;

	Int(int arg) {
		data = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Int> optimizeCP() override;
};

class Float : public ConstantExpression {
public:
	float data;

	Float(float arg) {
		data = arg;
	}
	
	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Float> optimizeCP() override;
};

class Bool : public ConstantExpression {
public:
	bool data;

	Bool(bool arg) {
		data = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Bool> optimizeCP() override;
};

class NameExpression : public Expression {
public:
	std::string name;
	NameExpression (std::string arg) {
		this -> name = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<NameExpression> optimizeCP() override;
};

class FunctionCall : public Expression {
public:
	std::string n;
	std::unique_ptr<std::vector<std::unique_ptr<Expression>>>  args;
	std::vector<Type> arg_types;

	FunctionCall(std::string arg) {
		printf("Function Call constructor \n");
		args = std::make_unique<std::vector<std::unique_ptr<Expression>>>();
		n = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<FunctionCall> optimizeCP() override;
};

#endif // ECE467_NODE_HPP_INCLUDED
