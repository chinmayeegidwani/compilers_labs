#ifndef ECE467_NODE_HPP_INCLUDED
#define ECE467_NODE_HPP_INCLUDED

#include "location.hh"
#include <memory>
#include <vector>

enum Type {VOID, INT, FLOAT, LOGICAL};
typedef enum Type Type;

enum AugmentedAssignOp {PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN};
typedef enum AugmentedAssignOp AugmentedAssignOp;

enum BinaryOp {PLUS, MINUS, MUL, DIV, AND, OR, EQ, NEQ, LT, GT, LE, GE}
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

class Node {
public:
	yy::location location;

	virtual ~Node() = 0;

	virtual bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) = 0;
};

class Root: public Node {
public:
	std::unique_ptr<FunctionList> funcList;

	Root (std::unique_ptr<FunctionList> functionList) {
		funcList = std::move(functionList);
	}

	void getReturnTypes (std::map<std::string, Type> & FunctionReturnType) {
		return (*funcList).getReturnTypes(FunctionReturnType)
	}

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		return (*funcList).checkType(parentScope, functionReturnType);
	}
};

class Function: public Node {
	virtual Type getType() = 0;
	virtual std::string getName() = 0;
};

class FunctionList: public Node {
public:
	std::vector<std::unique_ptr<Function>> list;

	FunctionList(std::unique_ptr<Function> func){
		list.push_back(std::move(func));
	}

	void getReturnTypes(std::map<std::string, Type> & FunctionReturnType) {
		for(int i = 0; i < list.size(); i++) {
			FunctionReturnType[(*(list[i])).getName()] = (*(list[i])).getType();
		}
	}

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		bool res = true;
		for(int i = 0; i < list.size(); i++) {
			std::map<std::string, Type> scope = parentScope;
			res = *(list[i]).checkType(scope, functionReturnType);
			if(!res) {
				return false;
			}
		}

		return true;
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

	Type getType() override {
		return type;
	}

	std::string getName() override {
		return name;
	}

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		return paramList -> checkType(parentScope, functionReturnType);
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

	Type getType() overrride {
		return (*funcDecl).getType();
	}

	std::string getName override {
		return (*funcDecl).getName();
	}

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		bool res = funcDecl -> checkType(parentScope, functionReturnType);

		if(!res) {
			return false;
		}

		res = blockNode -> checkType(parentScope, functionReturnType);
		return res;
	}
	

};

class ParameterList: public Node {
	public:
		std::unique_ptr<std::vector<std::unique_ptr<Declaration>>> paramList;

		ParameterList(){}

		bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override{
			bool res = true;
			for(int i = 0; i < (*paramList).size(); i++) {
				res = (*paramList)[i]->checkType(parentScope, functionReturnType);
				if(!res) {
					return false;
				}
			}
		}
};

class Block: public Node {};

class Suite: public Block{
	public:
		std::vector<std::unique_ptr<Node>> suiteList;

		Suite(){}

		bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
			int i = 0;
			bool res = true;
	
			for(i = 0; i < suiteList.size(); i++) {
				res = suiteList[i] -> checkType(parentScope, functionReturnType);
				if(!res) {
					return false;
				}
			}

			return true;
		}
};

class Declaration: public Node{
	public:
		Type type;
		std::string name;

		Declaration(Type t, std::string n){
			type = t;
			name = n;
		}

		bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
			if(parentScope.contains(n)) {
				printf("[output] duplicate_decl %i %i \n", this->location.begin.line, this->location.begin.column);
			}
			parentScope[n] = t;
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

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		if(parentScope.contains(n)) {
			printf("[output] duplicate_decl %i %i \n", this->location.begin.line, this->location.begin.column);
			return false;
		}
		parentScope[n] = t;
		bool res = expr -> checkType(parentScope, functionReturnType);
		return res;	
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

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
	
		if(!parentScope.contains(n)) {
			printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
			return false;
		}

		return expr -> checkType(parentScope, functionReturnType);
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

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
	
		if(!parentScope.contains(n)) {
			printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
			return false;
		}

		return expr -> checkType(parentScope, functionReturnType);
	}
};

class Break: public Node {

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { return true; }	
};

class Continue: public Node {

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { return true; }

};

class ReturnVoid: public Node {

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { return true; }

};

class ReturnNotVoid: public Node {
public:
	std::unique_ptr<Expression> expr;

	ReturnNotVoid(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { 
		return expr -> checkType(parentScope, functionReturnType); 
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

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { 
		scope = parentScope;
		bool res = expr -> checkType(scope, );
		res = res && b -> checkType(scope);
		return res;
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

	bool checkType(std::map<std::string, Type> & parentScope) override { 
		bool res = true;
		scope = parentScope;
		
		if(s1) {
			res = res && s1->checkType(scope);
		}
		
		if(expr) {
			res = res && expr->checkType(scope);
		}
		
		if(s2) {
			res = res && s2->checkType(scope);
		}
		
		res = res && b->checkType(scope);

		return res;
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

	bool checkType(std::map<std::string, Type> & parentScope) override {
		bool res = true;
		scope = parentScope;
		res = res && expr -> checkType(scope);
		res = res && b -> checkType(scope);
		return res;
	}
};

class Expression : public Node {
	Type type;
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

	bool checkType(std::map<std::string, Type> & parentScope) override {
		bool res = true;
		res = res && oExpression -> checkType(parentScope);
		res = res && tExpression1 -> checkType(parentScope);
		res = res && tExpression2 -> checkType(parentScope);
		return res;
	}
};

class BinaryExpression : public Expression {
public:
	std::unique_ptr<Expression> Expression1;
	std::unique_ptr<Expression> Expression2;
	BinaryOp = op;

	BinaryExpression (std::unique_ptr<Expression> orExpression, std::unique_ptr<Expression> andExpression, BinaryOp operation) {
		oExpression = std::move(orExpression);
		aExpression = std::move(andExpression);
		op = operation;
	}

	bool checkType(std::map<std::string, Type> & parentScope) override {
		bool res = true;
		res = res && Expression1 -> checkType(parentScope);
		res = res && Expression2 -> checkType(parentScope);
		return res;
	}
}

class CastExpression : public Expression {
public:
	Type cType;
	std::unique_ptr<Expression> cExpression;

	CastExpression(Type castType, std::unique_ptr<Expression> castExpression) {
		cType = castType;
		cExpression = std::move(castExpression);
	}

	bool checkType(std::map<std::string, Type> & parentScope) override {
		bool res = true;
		res = res && cExpression -> checkType(parentScope);
		return res;
	}	
};

class UnaryMinusExpression : public Expression {
public:
	std::unique_ptr<Expression> expr;

	UnaryMinusExpression(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	bool checkType(std::map<std::string, Type> & parentScope) override {
		bool res = true;
		res = res && cExpression -> checkType(parentScope);
	}
};

class Int : public Expression {
public:
	int data;

	Int(int arg) {
		data = arg;
	}

	bool checkType(std::map<std::string, Type> & parentScope) override {
		return true;
	}
};

class Float : public Expression {
public:
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

	bool checkType(std::map<std::string, Type> & parentScope) override {
		return true;
	}
};

class NameExpression : public Expression {
public:
	std::string name;
	NameExpression (std::string arg) {
		this -> name = arg;
	}

	bool checkType(std::map<std::string, Type> & parentScope) override {
		return true;
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
