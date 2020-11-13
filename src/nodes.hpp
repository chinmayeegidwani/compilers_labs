#ifndef ECE467_NODE_HPP_INCLUDED
#define ECE467_NODE_HPP_INCLUDED

#include "location.hh"
#include <memory>
#include <vector>
#include <map>
#include <set>

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

	virtual bool checkReturn() { return false; }
	virtual bool isReturn() { return false; }
	virtual bool checkFuncDuplicates() { return false; };
	virtual Node optimize() = 0;

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
	std::unique_ptr<Root> optimize(){
			// make new node
			// make new root node - opt root
			// optRoot -> funcList = optimize(this->funcList)
			std::unique_ptr<FunctionList> optFuncList = funcList.optimize();
			optRoot = make_node<Root>(this->location, optFuncList);
			return optRoot;
	}
};

class Function: public Node {
public:
	virtual bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) = 0;
};

class FunctionList: public Node {
public:
	std::vector<std::unique_ptr<Function>> list;

	FunctionList(std::unique_ptr<Function> func){
		list.push_back(std::move(func));
	}
	
	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool checkFuncDuplicates() override;
	std::unique_ptr<FunctionList> optimize(){
	std::vector<std::unique_ptr<Function>> optList;

	for(int i = 0; i < list.size; i++){
		optList[i] = list[i] -> optimize();
	}
	optFuncList = make_node<FunctionList> (this->location, optList);
	return optFuncList;
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

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) override;
	std::unique_ptr<FunctionDeclaration> optimize(){
		Type optType;
		std::string optName;
		std::unique_ptr<ParameterList> optParamList;

		optType = type -> optimize();
		optName = name -> optimize();
		optParamList = paramList -> optimize();

		optFuncDecl = make_node<FunctionDeclaration>(this->location, optType, optName, optParamList);
		return optFuncDecl;
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

	Type checkType(std::map<std::string, Type> & scope) override;
	bool checkReturn() override;
	bool setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) override;
	std::unique_ptr<FunctionDefinition> optimize(){
		std::unique_ptr<FunctionDeclaration> optFuncDecl;
		std::unique_ptr<Block> optBlockNode;
		Type optType;

		optFuncDecl = funcDecl -> optimize();
		optBlockNode = blockNode;

		funcDefn = make_node<FunctionDefinition>(this-> location, optFuncDecl, optBlockNode);
		return funcDefn;
	}

};

class ParameterList: public Node {
	public:
		std::unique_ptr<std::vector<std::unique_ptr<Declaration>>> paramList;

		ParameterList(){}
		Type checkType(std::map<std::string, Type> & scope) override;
		std::unique_ptr<ParameterList> optimize(){
			std::unique_ptr<std::vector<std::unique_ptr<Declaration>>> optParamList;
			for(int i = 0; i < (*paramList).size; i++){
				optParamList[i] = paramList[i] -> optimize();
			}
			parameterList = make_node<ParameterList>(this->location);
			return parameterList;
		}
};

class Block: public Node {};

class Suite: public Block{
	public:
		std::vector<std::unique_ptr<Node>> suiteList;

		Suite(){}

		Type checkType(std::map<std::string, Type> & scope) override;
		bool checkReturn() override;
		std::unique_ptr<Suite> optimize(){
			std::vector<std::unique_ptr<Node>> optSuiteList;

			for(int i = 0; i < suiteList.size(); i++){
				if(!suiteList[i]->optimize()){
					// null ptr returned, so suite eliminated further down
					// skip this suite
					continue;
				}
				optSuiteList.push_back(suiteList[i] -> optimize());
			}
			optSuite = make_node<Suite>(this->location);
			return optSuite;
		}

		bool isBool(){return false;
};

class SingleStatement : public Node {};

class ExpressionStatement: public SingleStatement {
public:
	std::unique_ptr<Expression> expr;
	
	ExpressionStatement(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;

};

	std::unique_ptr<ExpressionStatement> optimize(){
		std::unique_ptr<Expression> optExpr;
		optExpr = expr -> optimize();
		optExpressionStatement = make_node<ExpressionStatement>(this->location, optExpr);
		return optExpressionStatement;
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

		Type checkType(std::map<std::string, Type> & scope) override;
		std::unique_ptr<Declaration> optimize(){
			Type optType;
			std::string optName;

			optType = type->optimize();
			optName = name;

			optDeclaration = make_node<Declaration>(this->location, optType, optName);
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

	Type checkType(std::map<std::string, Type> & scope) override;
	std::unique_ptr<DeclarationAssign> optimize(){
		std::unique_ptr<Declaration> optDecl;
		std::unique_ptr<Expression> optExpr;

		optDecl = decl->optimize();
		optExpr = expr->optimize();

		optDeclAssign = make_node<DeclarationAssign>(this->location, optDecl, optExpr);
		return optDeclAssign;
	}
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
	std::unique_ptr<SimpleAssign> optimize(){
		std::string optN;
		std::unique_ptr<Expression> optExpr;

		optN = n;
		optExpr = expr->optimize();
		optSimpleAss = make_node<SimpleAssign>(this->location, optN, optExpr);
		return optSimpleAss;
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

	Type checkType(std::map<std::string, Type> & scope) override;
	std::unique_ptr<AugmentedAssign> optimize(){
		std::string optN;
		std::unique_ptr<Expression> optExpr;
		AugmentedAssignOp opOp;

		optN = n;
		optExpr = expr->optimize();
		opOp = op;

		optAugmentedAss = make_node<AugmentedAssign>(this->location, optN, optExpr, opOp);
		return optAugmentedAss;
	}
};

class Break: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;	
};

class Continue: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;

};

class ReturnVoid: public SingleStatement {

	Type checkType(std::map<std::string, Type> & scope) override;
	bool isReturn() override;

};

class ReturnNotVoid: public SingleStatement {
public:
	std::unique_ptr<Expression> expr;

	ReturnNotVoid(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
	bool isReturn() override;
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
	std::unique_ptr<Block> optimize(){
		if(expr->isBool()){
			if(expr->whichBool){
				// if expression == true, return block
				std::unique_ptr<Block> optb;
				optb = b;
				return optb;
			} else{
				// if expression == false, return null
				return nullptr;
			}
		} else {
			// if expression is not a boolean
			std::unique_ptr<Expression> optExpr;
			std::unique_ptr<Block> optb;

			optb = b;
			optExpr = expr->optimize();
			optIf = make_node<If>(this->location, optExpr, optb);
			return optIf;
		}
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

	Type checkType(std::map<std::string, Type> & scope) override;
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
};

class Expression : public Node {
public:
	Type type;
	bool isConst() {return false;}
	bool isBool(){return false;}
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
};

class CastExpression : public Expression {
public:
	std::unique_ptr<Expression> cExpression;

	CastExpression(Type castType, std::unique_ptr<Expression> castExpression) {
		type = castType;
		cExpression = std::move(castExpression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
};

class UnaryMinusExpression : public Expression {
public:
	std::unique_ptr<Expression> expr;

	UnaryMinusExpression(std::unique_ptr<Expression> expression) {
		expr = std::move(expression);
	}

	Type checkType(std::map<std::string, Type> & scope) override;
};

class Int : public Expression {
public:
	int data;

	Int(int arg) {
		data = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
};

class Float : public Expression {
public:
	float data;

	Float(float arg) {
		data = arg;
	}
	
	Type checkType(std::map<std::string, Type> & scope) override;
};

class Bool : public Expression {
public:
	bool data;

	Bool(bool arg) {
		data = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
};

class NameExpression : public Expression {
public:
	std::string name;
	NameExpression (std::string arg) {
		this -> name = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
};

class FunctionCall : public Expression {
public:
	std::string n;
	std::unique_ptr<std::vector<std::unique_ptr<Expression>>>  args;

	FunctionCall(std::string arg) {
		n = arg;
	}

	Type checkType(std::map<std::string, Type> & scope) override;
};




#endif // ECE467_NODE_HPP_INCLUDED
