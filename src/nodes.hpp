#ifndef ECE467_NODE_HPP_INCLUDED
#define ECE467_NODE_HPP_INCLUDED

#include "location.hh"
#include <memory>

class Node {
public:
	yy::location location;

	virtual ~Node() = 0;
};

class Root: public Node{
	std::unique_ptr<Node> FuncList;

	Root (std::unique_ptr<Node> FunctionList){
		FuncList = FunctionList;
	}
}

class Function: publicNode{

};

class FunctionList: public Node{
	std::vector<std::unique_ptr<Function>> list;

	FunctionList(std::unique_ptr<Function> func){
		list.push_back(std::move(func));
	}
};

class FunctionDeclaration: public Function{
	public:
		std::unique_ptr<Type> TypeNode;
		std::unique_ptr<Name> NameNode;
		std::unique_ptr<ParameterList> ParamList;

		FunctionDefinition(std::unique_ptr<Type> type, std::unique_ptr<Name> name, std::unique_ptr<ParameterList> param_list){
			TypeNode = std::move(type);
			NameNode = std::move(name);
			ParamList = std::move(param_list);
		}


};

class FunctionDefinition: public Function{
	public:
		std::unique_ptr<FunctionDeclaration> FuncDecl;
		std::unique_ptr<Block> BlockNode;

		FunctionDefinition(std::unique_ptr<FunctionDeclaration> function_decl, std::unique_ptr<Block> block){
			FuncDecl = std::move(function_decl);
			BlockNode = std::move(block);
		}
};

class Type: public Node{
	public:
		std::unique_ptr<Node> TypeNode;

		Type(std::unique_ptr<Node>type){
			TypeNode = std::move(type);
		}
}

class Name: public Node{
	public:
		std::unique_ptr<Node> NameNode;

		Type(std::unique_ptr<Node>name){
			NameNode = std::move(type);
		}
}

class ParameterList: public Node{
	public:
		std::vector<std::unique_ptr<Declaration>> ParamList;

		ParameterList(std::unique_ptr<Declaration> declaration){
			ParamList.push_back(std::move(declaration));
			//ParamList.push_back(std::move(declaration_extra));
		}
}

class Block: public Node{

}

class Suite: public Block{
	public:
		std::vector<std::unique_ptr<Statement>> SuiteList;

		Suite(std::unique_ptr<Statement> statement){
			SuiteList.push_back(std::move(statement));
		}
}

class Declaration: public Node{
	public:
		std::unique_ptr<Type> TypeNode;
		std::unique_ptr<Name> NameNode;

		Declaration(std::unique_ptr<Type> type; std::unique_ptr<Name> name;){
			TypeNode = std::move(type);
			NameNode = std::move(name);
		}
}

class Statement: public Node{
	//empty class
}


class singleStatement: public Statement{

}

class compoundStatement: public Statement{


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
	
	OrExpression (std::unique_ptr<Node> orExpression, std::unique_ptr<Expression> andExpression) {
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

enum EqOp {EQ, NEQ};

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

enum CompOp {GT, GE, LT, LE};

class CompExpression : public Expression {
public:
	std::unique_ptr<Expression> cExpression;
	std::unique_ptr<Expression> pExpression;
	ComparisonOp op;
	
	CompExpression (std::unique_ptr<Expression> compExpression, std::unique_ptr<Expression> plusExpression, CompOp comparisonOperator) {
		cExpression = std::move(compExpression);
		pExpression = std::move(plusExpression);
		op = comparisonOperator;
	}
	
};

enum PlusOp {PLUS, MINUS};

class PlusExpression : public Expression {
public:
	std::unique_ptr<Expression> pExpression;
	std::unique_ptr<Expression> mExpression;
	PlusOp op;
	
	PlusExpression (std::unique_ptr<Expression> plusExpression, std::unique_ptr<Expression> mulExpression, plusOp plusOperator) {
		pExpression = std::move(plusExpression);
		mExpression = std::move(mulExpression);
		op = plusOperator;
	}
};

enum MulOp {MUL, DIV};

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
}

class Float : public Expression {
	float data;

	Float(float arg) {
		data = arg;
	}
}

class Bool : public Expression {
public:
	bool data;

	Bool(bool arg) {
		data = arg;
	}
}

class NameExpression : public Expression {
	std::string name;

	NameExpression (std::unique_ptr<Node> arg) {
		this -> name = arg -> name;
	}
}

class FunctionCall : public Expression {
public:
	string n;
	std::vector<std::unique_ptr<Expression>>  args;

	FunctionCall(std::unique_ptr<Node> arg) {
		n = arg -> name;
	}
}




#endif // ECE467_NODE_HPP_INCLUDED
