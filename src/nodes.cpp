#include "nodes.hpp"

Node::~Node() = default;

Type Root::checkType(std::map<std::string, Type> & scope) {
	return (*funcList).checkType(scope);
}

Type FunctionList::checkType(std::map<std::string, Type> & scope) {
	Type res = NONE;
	for(long unsigned int i = 0; i < list.size(); i++) {
		res = list[i] -> checkType(scope);
		if(res == ERROR) {
			return ERROR;
		}
	}
	return res;
}

Type FunctionDeclaration::checkType(std::map<std::string, Type> & scope) {
	scope[name] = type;
	std::map<std::string, Type> newScope = scope;
	Type res = paramList -> checkType(newScope);
	if(res == ERROR) {
		return ERROR;
	}
	return type;
}

Type FunctionDefinition::checkType(std::map<std::string, Type> & scope) {
	type = funcDecl -> checkType(scope);
	std::map<std::string, Type> newScope = scope;
	Type temp = funcDecl -> paramList -> checkType(newScope);
	if(temp == ERROR) {
		return ERROR;
	}

	Type return_type = blockNode -> checkType(newScope);
	if(return_type == ERROR) {
		return ERROR;
	}

	if(return_type == NONE && type == VOID) {
		return type;
	}

	if(type != return_type) {
		printf("[output] type_return: %i %i", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	return type;
}

Type ParameterList::checkType(std::map<std::string, Type> & scope) {
	Type res = NONE;
	for(unsigned long int i = 0; i < (*paramList).size(); i++) {
		res = (*paramList)[i]->checkType(scope);
		if(res == ERROR) {
			return ERROR;
		}
	}
	return NONE;
}

Type Suite::checkType(std::map<std::string, Type> & scope) {
	Type res = NONE;
	Type statement_res = NONE;
	for(unsigned long int i = 0; i < suiteList.size(); i++) {
		statement_res = suiteList[i] -> checkType(scope);
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

Type ExpressionStatement::checkType(std::map<std::string, Type> & scope) {
	Type res = expr -> checkType(scope);
	if(res == ERROR) {
		return ERROR;
	}
	return NONE;
}

Type Declaration::checkType(std::map<std::string, Type> & scope) {
	if(scope.find(name) != scope.end()) {
		printf("[output] duplicate_decl %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	scope[name] = type;
	return NONE;
}

Type DeclarationAssign::checkType(std::map<std::string, Type> & scope) {
	Type temp = decl -> checkType(scope);
	if(temp == ERROR) {
		return ERROR;
	}
	Type res = expr -> checkType(scope);
	if(decl -> type != res) {
		printf("[output] type_mismatch %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	return NONE;	
}

Type SimpleAssign::checkType(std::map<std::string, Type> & scope) {

	if(scope.find(n) == scope.end()) {
		printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	Type res = expr -> checkType(scope);
	if(res == ERROR) {
		return ERROR;
	}
	if(res != scope[n]) {
		printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	return NONE;
}

Type AugmentedAssign::checkType(std::map<std::string, Type> & scope) {

	if(scope.find(n) == scope.end()) {
		printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	Type res = expr -> checkType(scope);
	if (res == ERROR) {
		return ERROR;
	}

	if(res != scope[n]) {
		printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	return NONE;
}

Type Break::checkType(std::map<std::string, Type> & scope) {return NONE; }

Type Continue::checkType(std::map<std::string, Type> & scope) { return NONE; }

Type ReturnVoid::checkType(std::map<std::string, Type> & scope) { return VOID; }

Type ReturnNotVoid::checkType(std::map<std::string, Type> & scope) { 
	return expr -> checkType(scope); 
}

Type If::checkType(std::map<std::string, Type> & scope) { 
	std::map<std::string, Type> newScope = scope;
	Type res = expr -> checkType(newScope);
	if(res == ERROR) {
		return ERROR;
	}
	if(res != LOGICAL) {
		printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	res = b -> checkType(newScope);
	return res;
}

Type For::checkType(std::map<std::string, Type> & scope) { 
	Type res1 = NONE;
	Type res2 = LOGICAL;
	Type res3 = NONE;
	std::map<std::string, Type> newScope = scope;
	
	if(s1) {
		res1 = s1->checkType(newScope);
		if(res1 == ERROR) {
			return ERROR;
		}
	}
	
	if(expr) {
		res2 = expr->checkType(newScope);
		if(res2 == ERROR) {
			return ERROR;
		}
	}

	if(res2 != LOGICAL) {
		printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	if(s2) {
		res3 = s2->checkType(newScope);
		if(res3 == ERROR) {
			return ERROR;
		}
	}
	
	if(res1 != NONE && res3 != NONE && res1 != res3) {
		printf("[output] type_return: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	if(res3 != NONE) {
		res1 = res3;
	}
	res3 = b -> checkType(newScope);
	if(res3 == ERROR) {
		return ERROR;
	}
	if(res1 != NONE && res3 != NONE && res1 != res3) {
		printf("[output] type_return: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	if(res1 != NONE) {
		res3 = res1;
	}

	return res3;
}

Type While::checkType(std::map<std::string, Type> & scope) {
	std::map<std::string, Type> newScope = scope;
	Type res = expr -> checkType(newScope);
	if(res == ERROR) {
		return ERROR;
	}
	if(res != LOGICAL) {
		printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	res = b -> checkType(newScope);
	return res;
}

Type TernaryExpression::checkType(std::map<std::string, Type> & scope) {
	Type res = oExpression -> checkType(scope);
	if(res != LOGICAL) {
		printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	Type res1 = tExpression1 -> checkType(scope);
	Type res2 = tExpression2 -> checkType(scope);

	if(res1 != res2) {
		printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	type = res1;
	return type;
}

Type BinaryExpression::checkType(std::map<std::string, Type> & scope) {
	Type res1 = expression1 -> checkType(scope);
	Type res2 = expression2 -> checkType(scope);
	if(res1 != res2) {
		printf("[output] type_mismatch: %i %i", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	switch(op) {
		case PLUS: type = res1; break;
		case MINUS: type = res1; break;
		case MUL: type = res1; break;
		case DIV: type = res1; break;
		case AND: type = LOGICAL; break;
		case OR: type = LOGICAL; break;
		case EQ: type = LOGICAL; break;
		case NEQ: type = LOGICAL; break;
		case LE: type = LOGICAL; break;
		case LT: type = LOGICAL; break;
		case GE: type = LOGICAL; break;
		case GT: type = LOGICAL; break;
		default: printf("Unrecognized binary op \n"); break;
	}
	return type;
}


Type CastExpression::checkType(std::map<std::string, Type> & scope) {
	Type res = cExpression -> checkType(scope);
	if(res == ERROR) {
		return ERROR;
	}
	return type;
}

Type UnaryMinusExpression::checkType(std::map<std::string, Type> & scope) {
	Type type = expr -> checkType(scope);
	return type;
	
}

Type Int::checkType(std::map<std::string, Type> & scope) {
	type = INT;
	return type;
}

Type Float::checkType(std::map<std::string, Type> & scope) {
	type = FLOAT;
	return type;
}

Type Bool::checkType(std::map<std::string, Type> & scope) {
	type = LOGICAL;
	return type;
}

Type NameExpression::checkType(std::map<std::string, Type> & scope) {
	if(scope.find(name) == scope.end()) {
		printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		type = ERROR;
		return type;
	}
	type = scope[name];
	return type;
}

Type FunctionCall::checkType(std::map<std::string, Type> & scope) {
	if(scope.find(n) == scope.end()) {
		printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		type = ERROR;
		return type;
	}
	type = scope[n];
	return type;
}



