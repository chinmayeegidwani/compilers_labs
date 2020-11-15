#include "nodes.hpp"

Node::~Node() = default;

Type Root::checkType(std::map<std::string, Type> & scope) {
	Type result = (*funcList).checkType(scope);
	if(scope.find("main") == scope.end()) {
		printf("[output] main_function %i %i", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	if((scope.find("main") -> second) != INT) {
		printf("[output] main_function %i %i", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	return result;
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
	if(paramList == nullptr) {
		return NONE;
	}
	for(unsigned long int i = 0; i < paramList -> size(); i++) {
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
		std::cout << "Printing the variables in the scope" << std::endl;
		for(auto j = scope.begin(); j != scope.end(); j++) {
			std::cout << j -> first << std::endl;
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

	std::cout << "Printing the variables in the scope, inside Declaration, for name" << name << std::endl;
	for(auto j = scope.begin(); j != scope.end(); j++) {
		std::cout << j -> first << std::endl;
	}

	scope[name] = type;

	std::cout << "Printing the variables in the scope, inside Declaration" << std::endl;
	for(auto j = scope.begin(); j != scope.end(); j++) {
		std::cout << j -> first << std::endl;
	}

	return NONE;
}

Type DeclarationAssign::checkType(std::map<std::string, Type> & scope) {
	std::cout << "Call to checkType DeclarationAssign" << std::endl;
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
		printf("Culprit 1 \n");
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
		printf("Culprit 2 \n");
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
	std::cout << "Checking the type of the ternary predicate " << res << std::endl;
	if(res != LOGICAL) {
		printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	Type res1 = tExpression1 -> checkType(scope);
	std::cout << "Type of first expression " << res1 << std::endl;
	Type res2 = tExpression2 -> checkType(scope);
	std::cout << "Type of second expression " << res2 << std::endl;
	if(res1 != res2) {
		printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	type = res1;
	std::cout << "Setting the type of the ternary expression " << type << std::endl;
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
	type = expr -> checkType(scope);
	return type;
	
}

Type Int::checkType(std::map<std::string, Type> & scope) {
	return type;
}

Type Float::checkType(std::map<std::string, Type> & scope) {
	return type;
}

Type Bool::checkType(std::map<std::string, Type> & scope) {
	return type;
}

Type NameExpression::checkType(std::map<std::string, Type> & scope) {
	if(scope.find(name) == scope.end()) {
		printf("Culprit 3 \n");
		printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		type = ERROR;
		return type;
	}
	type = scope[name];
	return type;
}

Type FunctionCall::checkType(std::map<std::string, Type> & scope) {
	if(scope.find(n) == scope.end()) {
		printf("Culprit 4 \n");
		printf("[output] type_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		type = ERROR;
		return type;
	}
	type = scope[n];
	Type temp;
	for(unsigned long int i = 0; i < (*args).size(); i++) {
		temp = (*args)[i] -> checkType(scope);
		if(temp == ERROR) {
			return ERROR;
		}
		arg_types.push_back(temp);
	}

	return type;
}

bool ReturnVoid::isReturn() {return true; }
bool ReturnNotVoid::isReturn() {return true; }

bool Root::checkReturn() {
	return funcList -> checkReturn();
}

bool FunctionList::checkReturn() {
	bool result = true;
	for(unsigned long int i = 0; i < list.size(); i++) {
		result = result && list[i] -> checkReturn();
	}
	return result;
}

bool FunctionDeclaration::checkReturn() {
	return true;
}

bool FunctionDefinition::checkReturn() {
	bool result = blockNode -> checkReturn();
	if(!result && type == VOID) {
		return true;
	}
	if(!result) {
		printf("[output] return_statement: %i %i \n", this->location.begin.line, this->location.begin.column);
	}
	return result;
}

bool Suite::checkReturn() {
	bool result = false;
	for(unsigned long int i = 0; i  < suiteList.size(); i++) {
		result = result || suiteList[i] -> isReturn();
	}
	return result;
}

bool Root::checkFuncDuplicates() {
	return funcList->checkFuncDuplicates();
}

bool FunctionList::checkFuncDuplicates() {
	std::set<std::string> declared;
	std::set<std::string> defined;
	bool res = true;
	for(unsigned long int i = 0; i < list.size(); i++) {
		res = res && (list[i]->setDefDecl(declared, defined));
	}
	return res;
}

bool FunctionDeclaration::setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) {
	if(declared.find(name) != declared.end()) {
		printf("[output] duplicate_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		return false;
	}

	declared.insert(name);
	return true;
}

bool FunctionDefinition::setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) {
	if(defined.find(funcDecl -> name) != defined.end()) {
		printf("[output] duplicated_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		return false;
	}

	defined.insert(funcDecl -> name);
	declared.insert(funcDecl -> name);
	return true;
}

bool Root::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	return funcList -> checkTypeArg(funcSig);
}

bool FunctionList::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool result = true;
	for(unsigned long int i = 0; i < list.size(); i++) {
		result = result && list[i] -> checkTypeArg(funcSig);	
	}
	return result;
}

bool FunctionDeclaration::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	if(funcSig.find(name) != funcSig.end()) {
		return true;
	}

	std::vector<Type> args;
	funcSig.insert({name, args});
	for(unsigned long int i = 0; i < (*(this -> paramList -> paramList)).size(); i++) {
		funcSig[name].push_back((*(this->paramList->paramList))[i] -> type);
	}
	return true;
}

bool FunctionDefinition::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool res = funcDecl -> checkTypeArg(funcSig);
	if(!res) {
		return res;	
	}
	
	res = blockNode -> checkTypeArg(funcSig);
	return res;
}

bool Suite::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool res = true;
	for(unsigned long int i = 0; i < suiteList.size(); i++) {
		res = suiteList[i] -> checkTypeArg(funcSig);
		if(!res) {
			return res;
		}
	}
	return res;
}

bool ExpressionStatement::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) { 
	return expr -> checkTypeArg(funcSig);
}

bool DeclarationAssign::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	return expr -> checkTypeArg(funcSig);
}

bool SimpleAssign::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	return expr -> checkTypeArg(funcSig);
}

bool AugmentedAssign::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	return expr -> checkTypeArg(funcSig);
}

bool ReturnNotVoid::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	return expr -> checkTypeArg(funcSig);
}

bool If::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool res = expr -> checkTypeArg(funcSig);
	res = res && b -> checkTypeArg(funcSig);
	return res;
}

bool For::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool res = true;	
	if(s1) {
		res = res && (s1 -> checkTypeArg(funcSig));
	}

	if(expr) {
		res = res && (expr -> checkTypeArg(funcSig));	
	}

	if(s2) {
		res = res && (expr -> checkTypeArg(funcSig));	
	}
	
	res = res && (b -> checkTypeArg(funcSig));
	return res;
}

bool While::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool res = true;
	res = res && (expr -> checkTypeArg(funcSig));
	res = res && (b -> checkTypeArg(funcSig));
	return res;
}

bool TernaryExpression::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool res = true;
	res = res && oExpression -> checkTypeArg(funcSig);
	res = res && tExpression1 -> checkTypeArg(funcSig);
	res = res && tExpression2 -> checkTypeArg(funcSig);
	return res;
}

bool BinaryExpression::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	bool res = true;
	res = res && expression1 -> checkTypeArg(funcSig);
	res = res && expression2 -> checkTypeArg(funcSig);
	return res;
}

bool CastExpression::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	return cExpression -> checkTypeArg(funcSig);
}

bool UnaryMinusExpression::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	return expr -> checkTypeArg(funcSig);
}

bool FunctionCall::checkTypeArg(std::map<std::string, std::vector<Type>> & funcSig) {
	if(funcSig[n].size() != arg_types.size()) {
		printf("[Output] type_arg: %i %i \n", this->location.begin.line, this->location.begin.column);
		return false;	
	}

	for(unsigned long int i = 0; i < arg_types.size(); i++) {
		if(arg_types[i] != (funcSig[n])[i]) {
			return false;
		}
	}
	return true;
}

void Root::printTree(){
	printf("root (%i, %i){\n", this->location.begin.line, this->location.begin.column);
	funcList->printTree(); 
	printf("}\n");
	return;
}

void FunctionList::printTree(){
	for(unsigned long int i=0; i < list.size(); i++){
		list[i]->printTree();
	}
	return;
}

void FunctionDeclaration::printTree(){
	printf("\n	func decl (%i, %i) {", this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("\n		return type: %s", types[type]);
	printf("\n		name: %s", name.c_str());
	paramList->printTree();
	printf("	} \n");
	return;
}

void FunctionDefinition::printTree(){
	funcDecl->printTree();
	printf("\n	func defn (%i, %i) {", this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	blockNode->printTree();
	printf("\n		return type: %s", types[type]);
	printf("\n	} ");
	return;
}

void ParameterList::printTree(){
	printf("\n		params(%i, %i) {", this->location.begin.line, this->location.begin.column);
	for(unsigned long int i = 0; i < (*paramList).size(); i++){
		(*paramList)[i]->printTree();
	}
	printf("\n		}");
	return;
}

void Suite::printTree(){
	printf("\n	suite (%i, %i) {", this->location.begin.line, this->location.begin.column);
	for(unsigned long int i=0; i<suiteList.size(); i++){
		suiteList[i]->printTree();
	}
	printf("\n	} ");
	return;
}

void ExpressionStatement::printTree(){
	printf("\n		expr stmt (%i, %i) { ", this->location.begin.line, this->location.begin.column);
	expr->printTree();
	printf("\n		}");
	return;
}

void Declaration::printTree(){
	printf("\n		decl (%i, %i) { ", this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("%s %s", types[type], name.c_str());
	printf(" }");
	return;
}

void DeclarationAssign::printTree(){
	printf("\n		decl assign (%i, %i) { ", this->location.begin.line, this->location.begin.column);
	decl->printTree();
	expr->printTree();
	printf("\n		}");
	return;
}

void SimpleAssign::printTree(){
	printf("\n		simp assign(%i, %i) {", this->location.begin.line, this->location.begin.column);
	printf(n.c_str());
	expr->printTree();
	printf("\n		}");
	return;
}

void AugmentedAssign::printTree(){
	printf("\n		aug assign(%i, %i) { ", this->location.begin.line, this->location.begin.column);
	printf("var name: %s ", n.c_str());
	const char* augops[4] = {"plus_assign", "minus_assign", "star_assign", "slash_assign"};
	printf("op: %s ", augops[op]);
	expr->printTree();
	printf("		} \n");
	return;
}

void Break::printTree(){
	printf("			break(%i, %i)\n", this->location.begin.line, this->location.begin.column);
	return;
}

void Continue::printTree(){
	printf("			continue(%i, %i)\n",this->location.begin.line, this->location.begin.column);
	return;
}

void If::printTree(){
	printf("		if(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	b -> printTree();
	printf("		}\n");
	return;
}

void For::printTree(){
	printf("		for(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	if(s1) {
		s1->printTree();
	}
	if(expr) {
		expr->printTree();
	}
	if(s2) {
		s2->printTree();
	}
	b -> printTree();
	printf("		}\n");
	return;
}

void While::printTree(){
	printf("		while(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	b -> printTree();
	printf("		}\n");
	return;
}

void TernaryExpression::printTree(){
	printf("		ternary expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("type: %s \n", types[type]);
	oExpression->printTree();
	tExpression1->printTree();
	tExpression2->printTree();
	printf("		}\n");
	return;
}

void BinaryExpression::printTree(){
	printf("		binary expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("type: %s \n", types[type]);
	expression1->printTree();
	expression2->printTree();
	const char* binops[12] = {"plus", "minus", "mul", "div", "and", "or", "eq", "neq", "lt", "gt", "le", "ge"};
	printf("op: %s\n", binops[op]);
	printf("		}\n");
	return;
}

void CastExpression::printTree(){
	printf("		cast expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("type: %s \n", types[type]);
	cExpression->printTree();
	printf("		}\n");
	return;
}

void UnaryMinusExpression::printTree(){
	printf("		unary minus expr(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("type: %s \n", types[type]);
	expr->printTree();
	printf("		}\n");
	return;
}

void Int::printTree(){
	printf("\n			int(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	printf("%i } \n", data);
	return;
}

void Float::printTree(){
	printf("\n			float(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	printf("%f } \n", data);
	return;
}

void Bool::printTree(){
	printf("\n			bool(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	if(data){
		printf("true");
	} else{
		printf("false");
	}
	printf(" }\n");
	return;
}

void NameExpression::printTree(){
	printf("			name expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf(" %s %s } \n", types[type], name.c_str());
	return;
}

void FunctionCall::printTree(){
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("			func call(%i, %i){ \n",this->location.begin.line, this->location.begin.column);
	printf("			func: %s return type: %s \n", n.c_str(), types[type]);
	//args->printTree();

	for(unsigned long int i=0; i<arg_types.size(); i++){
		printf("			type %lu: %s\n", i, types[arg_types[i]]);
		(*args)[i]->printTree();
	}

	printf("			}\n");
	return;
}

void ReturnVoid::printTree(){
	printf("	return void(%i, %i) \n ",this->location.begin.line, this->location.begin.column);
	return;
}

void ReturnNotVoid::printTree(){
	printf("	return expr(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	printf("	}\n");
	return;
}

std::unique_ptr<Node> Root::optimize() {
		std::unique_ptr<FunctionList> optFuncList = funcList -> optimizeCP();
		std::unique_ptr<Root> optRoot = std::make_unique<Root>(std::move(optFuncList));
		optRoot -> location = this -> location;
		return optRoot;
}

std::unique_ptr<FunctionList> FunctionList::optimizeCP(){
	std::vector<std::unique_ptr<Function>> optList;

	for(long unsigned int i = 0; i < list.size(); i++){
		optList.push_back(list[i] -> optimizeCP());
	}
	std::unique_ptr<FunctionList> optFuncList = std::make_unique<FunctionList> ();
	optFuncList -> list = std::move(optList);

	return optFuncList;
}

std::unique_ptr<Function> FunctionDeclaration::optimizeCP(){
	std::string optName;
	std::unique_ptr<ParameterList> optParamList;

	std::unique_ptr<FunctionDeclaration> optFuncDecl = std::make_unique<FunctionDeclaration>(type, name, std::move(paramList));
	optFuncDecl -> location = this -> location;
	return optFuncDecl;
}

std::unique_ptr<Function> FunctionDefinition::optimizeCP() {
	std::unique_ptr<Block> optBlock = blockNode -> optimizeCP();
	
	std::unique_ptr<FunctionDefinition> optFuncDefn = std::make_unique<FunctionDefinition>(std::move(funcDecl), std::move(optBlock));
	optFuncDefn -> location = this -> location;
	return optFuncDefn;
}

std::unique_ptr<Block> Suite::optimizeCP(){
	std::unique_ptr<Suite> optSuite = std::make_unique<Suite>();

	for(long unsigned int i = 0; i < suiteList.size(); i++){
		optSuite -> suiteList.push_back(this -> suiteList[i] -> optimizeCP());
	}
	optSuite -> location = this -> location;
	return optSuite;
}

std::unique_ptr<Statement> ExpressionStatement::optimizeCP(){
	std::unique_ptr<Expression> exprOpt;
	exprOpt = expr->optimizeCP();
	std::unique_ptr<ExpressionStatement> exprStatementOpt = std::make_unique<ExpressionStatement>(std::move(exprOpt));
	exprStatementOpt -> location = this -> location;
	return exprStatementOpt;
}

std::unique_ptr<Statement> Declaration::optimizeCP() {
	std::unique_ptr<Declaration> res = std::make_unique<Declaration>(type, name);
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> DeclarationAssign::optimizeCP(){
	std::unique_ptr<Expression> optExpr = expr->optimizeCP();

	std::unique_ptr<DeclarationAssign> res = std::make_unique<DeclarationAssign>(std::move(decl), std::move(optExpr));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> SimpleAssign::optimizeCP() {
	std::unique_ptr<Expression> optExpression = expr -> optimizeCP();

	std::unique_ptr<SimpleAssign> res = std::make_unique<SimpleAssign>(n, std::move(optExpression));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> AugmentedAssign::optimizeCP() {
	std::unique_ptr<Expression> optExpression = expr -> optimizeCP();
	std::unique_ptr<AugmentedAssign> res = std::make_unique<AugmentedAssign>(n, std::move(optExpression), op);
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> Break::optimizeCP() {
	std::unique_ptr<Break> res = std::make_unique<Break>();
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> Continue::optimizeCP() {
	std::unique_ptr<Continue> res = std::make_unique<Continue>();
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> ReturnVoid::optimizeCP() {
	std::unique_ptr<ReturnVoid> res = std::make_unique<ReturnVoid>();
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> ReturnNotVoid::optimizeCP() {
	std::unique_ptr<Expression> optExpression = expr -> optimizeCP();
	std::unique_ptr<ReturnNotVoid> res = std::make_unique<ReturnNotVoid>(std::move(optExpression));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> If::optimizeCP() {
	std::unique_ptr<Expression> optExpression = expr -> optimizeCP();
	std::unique_ptr<Block> optBlock = b -> optimizeCP();
	std::unique_ptr<If> res = std::make_unique<If>(std::move(optExpression), std::move(optBlock));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> For::optimizeCP() {
	std::unique_ptr<Statement> s1Opt = nullptr;
	std::unique_ptr<Expression> exprOpt = nullptr;
	std::unique_ptr<Statement> s2Opt = nullptr;

	if(s1) {
		s1Opt = s1 -> optimizeCP();
	}

	if(expr) {
		exprOpt = expr -> optimizeCP();
	}

	if(s2) {
		s2Opt = s2 -> optimizeCP();
	}

	std::unique_ptr<Block> bOpt = b -> optimizeCP();
	std::unique_ptr<For> res = std::make_unique<For>(std::move(s1Opt), std::move(s2Opt), std::move(exprOpt), std::move(bOpt));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Statement> While::optimizeCP() {
	std::unique_ptr<Expression> optExpr = expr -> optimizeCP();
	std::unique_ptr<Block> optBlock = b -> optimizeCP();
	std::unique_ptr<While> res = std::make_unique<While>(std::move(optExpr), std::move(optBlock));
	return res;
}

std::unique_ptr<Expression> TernaryExpression::optimizeCP() {
	std::unique_ptr<Expression> oExpressionOpt = oExpression -> optimizeCP();
	std::unique_ptr<Expression> tExpression1Opt = tExpression1 -> optimizeCP();
	std::unique_ptr<Expression> tExpression2OPt = tExpression2 -> optimizeCP();
	std::unique_ptr<TernaryExpression> res = std::make_unique<TernaryExpression>(std::move(oExpressionOpt), std::move(tExpression1Opt), std::move(tExpression2OPt));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> BinaryExpression::optimizeCP() {
	std::unique_ptr expr1Opt = expression1 -> optimizeCP();
	std::unique_ptr expr2Opt = expression2 -> optimizeCP();

	Int* intOpt1 = dynamic_cast<Int *>(expr1Opt.get());
	Int* intOpt2 = dynamic_cast<Int *>(expr1Opt.get());

	if(intOpt1 != nullptr && intOpt2 != nullptr) {
		std::cout << "Reducing two integers " << std::endl;
		switch(op) {
			case PLUS: { std::unique_ptr<Int> res = std::make_unique<Int>(intOpt1 -> data + intOpt2 -> data); res -> location = this -> location; return res; break; }
			case MINUS: { std::unique_ptr<Int> res = std::make_unique<Int>(intOpt1 -> data - intOpt2 -> data); res -> location = this -> location; return res; break; }
			case MUL: { std::unique_ptr<Int> res = std::make_unique<Int>(intOpt1 -> data * intOpt2 -> data); res -> location = this -> location; return res; break; }
			case DIV: { std::unique_ptr<Int> res = std::make_unique<Int>(intOpt1 -> data / intOpt2 -> data); res -> location = this -> location; return res; break; }
			case AND: { std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data && intOpt2 -> data); res -> location = this -> location; return res; break; }
			case OR: { std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data || intOpt2 -> data); res -> location = this -> location; return res; break; }
			case EQ:  { std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data == intOpt2 -> data); res -> location = this -> location; return res; break; }
			case NEQ: { std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data != intOpt2 -> data); res -> location = this -> location; return res; break; }
			case LT: { std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data < intOpt2 -> data); res -> location = this -> location; return res; break; }
			case LE: { std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data <= intOpt2 -> data); res -> location = this -> location; return res; break; }
			case GE: { std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data >= intOpt2 -> data); res -> location = this -> location; return res; break; }
			case GT: {std::unique_ptr<Bool> res = std::make_unique<Bool>(intOpt1 -> data > intOpt2 -> data); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized binary operator: error" << std::endl;
		}
	}

	Float* floatOpt1 = dynamic_cast<Float *>(expr1Opt.get());
	Float* floatOpt2 = dynamic_cast<Float *>(expr2Opt.get());

	if(floatOpt1 && floatOpt2) {
		switch(op) {
			case PLUS: { std::unique_ptr<Float> res = std::make_unique<Float>(floatOpt1 -> data + floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case MINUS: { std::unique_ptr<Float> res = std::make_unique<Float>(floatOpt1 -> data - floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case MUL: { std::unique_ptr<Float> res = std::make_unique<Float>(floatOpt1 -> data * floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case DIV: { std::unique_ptr<Float> res = std::make_unique<Float>(floatOpt1 -> data / floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case AND: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data && floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case OR: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data || floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case EQ: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data == floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case NEQ: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data != floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case LT: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data < floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case LE: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data <= floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case GE: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data >= floatOpt2 -> data); res -> location = this -> location; return res; break; }
			case GT: { std::unique_ptr<Bool> res = std::make_unique<Bool>(floatOpt1 -> data > floatOpt2 -> data); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized binary operator: error" << std::endl;
		}
	}

	Bool* boolOpt1 = dynamic_cast<Bool *>(expr1Opt.get());
	Bool* boolOpt2 = dynamic_cast<Bool *>(expr2Opt.get());

	if(boolOpt1 && boolOpt2) {
		switch(op) {
			case PLUS: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data + boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case MINUS: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data - boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case MUL: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data * boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case DIV: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data / boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case AND: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data && boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case OR: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data || boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case EQ: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data == boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case NEQ: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data != boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case LT: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data < boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case LE: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data <= boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case GE: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data >= boolOpt2 -> data); res -> location = this -> location; return res; break; }
			case GT: { std::unique_ptr<Bool> res = std::make_unique<Bool>(boolOpt1 -> data > boolOpt2 -> data); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized binary operator: error" << std::endl;
		}
	}
	std::unique_ptr<BinaryExpression> res = std::make_unique<BinaryExpression>(std::move(expr1Opt), std::move(expr2Opt), op);
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> CastExpression::optimizeCP() {
	std::unique_ptr<Expression> optExpr = cExpression -> optimizeCP();
	
	Int * intOpt = dynamic_cast<Int *>(optExpr.get());

	if(intOpt) {
		switch (type) {
			case INT: { std::unique_ptr<Int> res = std::make_unique<Int>((int) intOpt -> data); res -> location = this -> location; return res; break; }
			case FLOAT: { std::unique_ptr<Float> res = std::make_unique<Float>((float) intOpt -> data); res -> location = this -> location; return res; break; }
			case LOGICAL: { std::unique_ptr<Bool> res = std::make_unique<Bool>((bool) intOpt -> data); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized type: error" << std::endl; break;
		}
	}

	Float * floatOpt = dynamic_cast<Float *>(optExpr.get());

	if(floatOpt) {
		switch (type) {
			case INT: { std::unique_ptr<Int> res = std::make_unique<Int>((int) floatOpt -> data); res -> location = this -> location; return res; break; }
			case FLOAT: { std::unique_ptr<Float> res = std::make_unique<Float>((float) floatOpt -> data); res -> location = this -> location; return res; break; }
			case LOGICAL: { std::unique_ptr<Bool> res = std::make_unique<Bool>((bool) floatOpt -> data); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized type: error" << std::endl; break;
		}
	}

	Bool * boolOpt = dynamic_cast<Bool *>(optExpr.get());

	if(boolOpt) {
		switch (type) {
			case INT: { std::unique_ptr<Int> res = std::make_unique<Int>((int) boolOpt -> data); res -> location = this -> location; return res; break; }
			case FLOAT: { std::unique_ptr<Float> res = std::make_unique<Float>((float) boolOpt -> data); res -> location = this -> location; return res; break; }
			case LOGICAL: { std::unique_ptr<Bool> res = std::make_unique<Bool>((bool) boolOpt -> data); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized type: error" << std::endl; break;
		}
	}

	std::unique_ptr res = std::make_unique<CastExpression>(type, std::move(optExpr));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> UnaryMinusExpression::optimizeCP() {
	std::unique_ptr<Expression> optExpr = expr -> optimizeCP();

	// returns a nullptr if the cast is not possible
	Int * intOpt = dynamic_cast<Int *>(optExpr.get());

	if(intOpt) {
		switch (type) {
			case INT : { std::unique_ptr<Int> res = std::make_unique<Int>(- (intOpt -> data)); res -> location = this -> location; return res; break; }
			case FLOAT: { std::unique_ptr<Float> res = std::make_unique<Float>(- (intOpt -> data)); res -> location = this -> location; return res; break; }
			case LOGICAL: { std::unique_ptr<Bool> res = std::make_unique<Bool>(- (intOpt -> data)); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized type: error" << std::endl; break;
		}
	}

	Float * floatOpt = dynamic_cast<Float *>(optExpr.get());

	if(floatOpt) {
		switch (type) {
			case INT: { std::unique_ptr<Int> res = std::make_unique<Int>(- (floatOpt -> data)); res -> location = this -> location; return res; break; }
			case FLOAT: { std::unique_ptr<Float> res = std::make_unique<Float>(- (floatOpt -> data)); res -> location = this -> location; return res; break; }
			case LOGICAL: { std::unique_ptr<Bool> res = std::make_unique<Bool>(- (floatOpt -> data)); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized type: error" << std::endl; break;
		}
	}

	Bool * boolOpt = dynamic_cast<Bool *>(optExpr.get());

	if(boolOpt) {
		switch (type) {
			case INT: { std::unique_ptr<Int> res = std::make_unique<Int>(- (boolOpt -> data)); res -> location = this -> location; return res; break; }
			case FLOAT:  { std::unique_ptr<Float> res = std::make_unique<Float>(- (boolOpt -> data)); res -> location = this -> location; return res; break; }
			case LOGICAL:  { std::unique_ptr<Bool> res = std::make_unique<Bool>(- (boolOpt -> data)); res -> location = this -> location; return res; break; }
			default: std::cout << "Unrecognized type: error" << std::endl; break;
		}
	}
	std::unique_ptr<CastExpression> res = std::make_unique<CastExpression>(type, std::move(optExpr));
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> Int::optimizeCP() {
	std::unique_ptr<Int> res = std::make_unique<Int>(data);
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> Float::optimizeCP() {
	std::unique_ptr<Float> res = std::make_unique<Float>(data);
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> Bool::optimizeCP() {
	std::unique_ptr<Bool> res = std::make_unique<Bool>(data);
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> NameExpression::optimizeCP(){
	std::cout << "Optimizing name expression" << std::endl;
	std::unique_ptr<NameExpression> res = std::make_unique<NameExpression>(name);
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> FunctionCall::optimizeCP(){
	std::unique_ptr<FunctionCall> res = std::make_unique<FunctionCall>(n);
	for(unsigned long int i=0; i < args -> size(); i++){
		res -> args -> push_back((*(this -> args))[i] -> optimizeCP());
	}
	res -> location = this -> location;
	return res;
}