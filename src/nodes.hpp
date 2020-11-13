#ifndef ECE467_NODE_HPP_INCLUDED
#define ECE467_NODE_HPP_INCLUDED

#include "location.hh"
#include <memory>
#include <vector>

enum Type {ERROR, NONE, VOID, INT, FLOAT, LOGICAL};
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

	virtual Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) = 0; //checks for type_decl, type_mismatch, type_bool, type_return and duplicate_decl for variables (not functions), returns the type of the node it is called on

//	virtual Type checkReturnType(std::map<std::string, Type> & functionReturnType) { return NONE };
};

class Root: public Node {
public:
	std::unique_ptr<FunctionList> funcList;

	Root (std::unique_ptr<FunctionList> functionList) {
		funcList = std::move(functionList);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		Type res = NONE;
		for(int i = 0; i < list.size(); i++) {
			std::map<std::string, Type> scope = parentScope;
			res = list[i] -> checkType(scope, functionReturnType);
			if(res == ERROR) {
				return ERROR;
			}
		}

		return res;
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		functionReturnType[name] = type;
		Type res = paramList -> checkType(parentScope, functionReturnType);
		return type;
	}
};

class FunctionDefinition: public Function{
public:
	std::unique_ptr<FunctionDeclaration> funcDecl;
	std::unique_ptr<Block> blockNode;
	Type type;

	FunctionDefinition(std::unique_ptr<FunctionDeclaration> function_decl, std::unique_ptr<Block> block){
		funcDecl = std::move(function_decl);
		blockNode = std::move(block);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		type = funcDecl -> checkType(parentScope, functionReturnType);
		Type return_type = blockNode -> checkReturnType(functionReturnType);

		if(return_type == NONE && type == VOID) {
			return type;
		}

		if(type != return_type) {
			printf("[output] type_return: %i %i", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}

		return type;
	}
	

};

class ParameterList: public Node {
	public:
		std::unique_ptr<std::vector<std::unique_ptr<Declaration>>> paramList;

		ParameterList(){}

		Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override{
			Type res = NONE;
			for(int i = 0; i < (*paramList).size(); i++) {
				res = (*paramList)[i]->checkType(parentScope, functionReturnType);
			}
			return res;
		}
};

class Block: public Node {};

class Suite: public Block{
	public:
		std::vector<std::unique_ptr<Node>> suiteList;

		Suite(){}

		Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
			Type res = NONE;
			Type statement_res = NONE;
			for(int i = 0; i < suiteList.size(); i++) {
				statement_res = suiteList[i] -> checkType(parentScope, functionReturnType);
				if (statement_res == ERROR) {
					return ERROR;
				}

				if(res != NONE && statement_res != NONE && statement_res != res) {
					printf("[output] type_return: %i %i \n", this->location.begin.line, this->location.begin.column);
					return ERROR;
				}
				
				if(statement_res != NONE) {
					res = statement_res;
				}
			}

			return res;
		}
};

class SingleStatement : public Node {};

class ExpressionStatement: public SingleStatement {
public:
	std::unique_ptr<Expression> expr;
	
	ExpressionStatement(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		Type res = expr -> checkType(parentScope, functionReturnType);
		if(res == ERROR) {
			return ERROR;
		}
		return NONE;
	}

}
class Declaration: public SingleStatement {
	public:
		Type type;
		std::string name;

		Declaration(Type t, std::string n){
			type = t;
			name = n;
		}

		Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
			if(parentScope.contains(n)) {
				printf("[output] duplicate_decl %i %i \n", this->location.begin.line, this->location.begin.column);
				return ERROR;
			}
			parentScope[n] = t;
			return NONE;
		}
};

class DeclarationAssign: public SingleStatement {
public:
	std::unique_ptr<Declaration> decl;
	std::unique_ptr<Expression> expr;
	
	DeclarationAssign(std::unique_ptr<Declaration> declaration, std::unique_ptr<Expression> expression){
		decl = std::move(declaration);
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		if(parentScope.contains(n)) {
			printf("[output] duplicate_decl %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}
		parentScope[n] = decl -> checkType(parentScope, functionReturnType);
		Type res = expr -> checkType(parentScope, functionReturnType);
		if(parentScope[n] != res) {
			printf("[output] type_mismatch %i %i \n", this->location.begin.line, this->location.begin.column);
		}
		return NONE;	
	}
};

class SimpleAssign: public SingleStatment {
public:
	std::string n;
	std::unique_ptr<Expression> expr;

	SimpleAssign(std::string name, std::unique_ptr<Expression> expression) {
		n = name;
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
	
		if(!parentScope.contains(n)) {
			printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}
		Type res = expr -> checkType(parentScope, functionReturnType);
		if(res == ERROR) {
			return ERROR;
		}
		if(res != parentScope[n]) {
			printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->location.begin.column);
		}
		return NONE;
	}
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
	
		if(!parentScope.contains(n)) {
			printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}

		Type res = expr -> checkType(parentScope, functionReturnType);
		if (res == ERROR) {
			return ERROR;
		}

		if(res != parentScope[n]) {
			printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->locatin.begin.column);
			return ERROR;
		}

		return NONE;
	}
};

class Break: public SingleStatement {

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { return NONE; }	
};

class Continue: public SingleStatment {

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { return NONE; }

};

class ReturnVoid: public SingleStatement {

	bool checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { return VOID; }

};

class ReturnNotVoid: public SingleStatement {
public:
	std::unique_ptr<Expression> expr;

	ReturnNotVoid(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { 
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { 
		scope = parentScope;
		Type res = expr -> checkType(scope, functionReturnType);
		if(res == ERROR) {
			return ERROR;
		}
		if(res != LOGICAL) {
			printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
		}
		res = b -> checkType(scope, functionReturnType);
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override { 
		Type res1 = NONE;
		Type res2 = LOGICAL;
		Type res3 = NONE
		scope = parentScope;
		
		if(s1) {
			res1 = s1->checkType(scope, functionReturnType);
		}
		
		if(expr) {
			res2 = expr->checkType(scope, functionReturnType);
		}

		if(res2 != LOGICAL) {
			printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}

		if(s2) {
			res3 = s2->checkType(scope, functionReturnType);
		}
		
		if(res1 != NONE && res3 != NONE && res1 != res3) {
			printf("[output] type_return: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}

		if(res3 != NONE) {
			res1 = res3;
		}
		res3 = b -> checkType(scope, functionReturnType);

		if(res1 != NONE && res3 != NONE && res1 != res3) {
			printf("[output] type_return: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}

		if(res1 != NONE) {
			res3 = res1;
		}

		return res3;
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		scope = parentScope;
		Type res = expr -> checkType(scope);
		if(res == ERROR) {
			return ERROR;
		}
		if(res != LOGICAL) {
			printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}

		res = b -> checkType(scope);
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		Type res = oExpression -> checkType(parentScope);
		if(res != LOGICAL) {
			printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}
		Type res1 = tExpression1 -> checkType(parentScope);
		Type res2 = tExpression2 -> checkType(parentScope);

		if(res1 != res2) {
			printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}

		type = res1;
		return type;
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

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		Type res1 = Expression1 -> checkType(parentScope);
		Type res2 = Expression2 -> checkType(parentScope);
		if(res1 != res2) {
			printf("[output] type_mismatch: %i %i", this->location.begin.line, this->location.begin.column);
			return ERROR;
		}
		switch(op) {
			case PLUS: type = res1;
			case MINUS: type = res1;
			case MUL: type = res1;
			case DIV: type = res1;
			case AND: type = LOGICAL;
			case OR: type = LOGICAL;
			case EQ: type = LOGICAL;
			case NEQ: type = LOGICAL;
			case LE: type = LOGICAL;
			case LT: type = LOGICAL;
			case GE: type = LOGICAL;
			case GT: type = LOGICAL; 
		}
		return type;
	}
}

class CastExpression : public Expression {
public:
	std::unique_ptr<Expression> cExpression;

	CastExpression(Type castType, std::unique_ptr<Expression> castExpression) {
		Type = castType;
		cExpression = std::move(castExpression);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		res = cExpression -> checkType(parentScope);
		if(res == ERROR) {
			return ERROR;
		}
		return type;
	}	
};

class UnaryMinusExpression : public Expression {
public:
	std::unique_ptr<Expression> expr;

	UnaryMinusExpression(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & parentScope, std::map<std::string, Type> & functionReturnType) override {
		Type res = expr -> checkType(parentScope);
		
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
