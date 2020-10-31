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

/*
class DeclarationExtra: public ParameterList{
	public:
		DeclarationExtra(std::unique_ptr<Declaration> declaration){
			ParamList.push_back(std::move(declaration));
		}
} */


class singleStatement: public Statement{

}

class compoundStatement: public Statement{


};



// class Expression: public Node{ ... }

#endif // ECE467_NODE_HPP_INCLUDED
