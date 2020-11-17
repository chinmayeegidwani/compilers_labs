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
class Statement;
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
	virtual std::unique_ptr<Node> optimize() {return nullptr; }

};

class Root: public Node {
public:
	std::unique_ptr<FunctionList> funcList;

	Root (std::unique_ptr<FunctionList> functionList) {
		funcList = std::move(functionList);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool checkFuncDuplicates() override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<Node> optimize() override;
};

class Function: public Node {
public:
	virtual bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) = 0;
	virtual std::unique_ptr<Function> optimizeCP() = 0;
};

class FunctionList: public Node {
public:
	std::vector<std::unique_ptr<Function>> list;

	FunctionList(std::unique_ptr<Function> func){
		list.push_back(std::move(func));
	}
	
	FunctionList() {}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool checkFuncDuplicates() override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;

	void printTree() override;
	std::unique_ptr<FunctionList> optimizeCP();
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

	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) override;
	std::unique_ptr<Function> optimizeCP() override;
	void printTree() override;
};

class FunctionDefinition: public Function{
public:
	std::unique_ptr<FunctionDeclaration> funcDecl;
	std::unique_ptr<Block> blockNode;
	Type type;

	FunctionDefinition(std::unique_ptr<FunctionDeclaration> function_decl, std::unique_ptr<Block> block){
		funcDecl = std::move(function_decl);
		blockNode = std::move(block);
		type = funcDecl -> type;
	}

	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) override;
	std::unique_ptr<Function> optimizeCP() override;
	void printTree() override;
};

class ParameterList: public Node {
public:
		std::unique_ptr<std::vector<std::unique_ptr<Declaration>>> paramList;

		ParameterList() {
			paramList = std::make_unique<std::vector<std::unique_ptr<Declaration>>>();
		}
		Type checkType(std::map<std::string, Type> & scope) override;
		std::unique_ptr<ParameterList> optimizeCP();
		void printTree() override;
};

class Block: public Node {
public:
	virtual std::unique_ptr<Block> optimizeCP() = 0;
};

class Suite: public Block{
	public:
		std::vector<std::unique_ptr<Statement>> suiteList;

		Suite(){
		}
		bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
		Type checkType(std::map<std::string, Type> & scope) override;
		bool checkReturn() override;
		std::unique_ptr<Block> optimizeCP() override;
		bool isBool(){ return false; };
		void printTree() override;
};

class Statement : public Node {
public:
	virtual std::unique_ptr<Statement> optimizeCP() = 0;
};

class SingleStatement : public Statement {};

class ExpressionStatement: public SingleStatement {
public:
	std::unique_ptr<Expression> expr;
	
	ExpressionStatement(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<Statement> optimizeCP() override;
};

class Declaration: public SingleStatement {
public:
	Type type;
	std::string name;

	Declaration(Type t, std::string n){
		type = t;
		name = n;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Statement> optimizeCP() override;
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
	std::unique_ptr<Statement> optimizeCP() override;
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
	std::unique_ptr<Statement> optimizeCP() override;
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
	std::unique_ptr<Statement> optimizeCP() override;
};

class Break: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;	
	void printTree() override;
	std::unique_ptr<Statement> optimizeCP() override;
};

class Continue: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Statement> optimizeCP() override;

};

class ReturnVoid: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;
	bool isReturn() override;
	void printTree() override;
	std::unique_ptr<Statement> optimizeCP() override;
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
	std::unique_ptr<Statement> optimizeCP() override;
};

class CompoundStatement: public Statement {};

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
	std::unique_ptr<Statement> optimizeCP() override;
};

class For: public CompoundStatement {
public:
	std::unique_ptr<Statement> s1;
	std::unique_ptr<Expression> expr;
	std::unique_ptr<Statement> s2;
	std::unique_ptr<Block> b;

	For(std::unique_ptr<Statement> statement1, std::unique_ptr<Statement> statement2, std::unique_ptr<Expression> expression, std::unique_ptr<Block> block) {
		s1 = std::move(statement1);
		s2 = std::move(statement2);
		expr = std::move(expression);
		b = std::move(block);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) override;
	void printTree() override;
	std::unique_ptr<Statement> optimizeCP() override;
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
	std::unique_ptr<Statement> optimizeCP() override;
};

class Expression : public Node {
public:
	Type type;
	virtual std::unique_ptr<Expression> optimizeCP() = 0;
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
	std::unique_ptr<Expression> optimizeCP() override;
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

class Int : public Expression {
public:
	int data;

	Int(int arg) {
		data = arg;
		type = INT;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Expression> optimizeCP() override;
};

class Float : public Expression {
public:
	float data;

	Float(float arg) {
		data = arg;
		type = FLOAT;
	}
	
	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Expression> optimizeCP() override;
};

class Bool : public Expression {
public:
	bool data;

	Bool(bool arg) {
		data = arg;
		type = LOGICAL;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Expression> optimizeCP() override;
};

class NameExpression : public Expression {
public:
	std::string name;
	NameExpression (std::string arg) {
		this -> name = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	void printTree() override;
	std::unique_ptr<Expression> optimizeCP() override;
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
	std::unique_ptr<Expression> optimizeCP() override;
};

#endif // ECE467_NODE_HPP_INCLUDED
