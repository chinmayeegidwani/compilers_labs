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
	printf("Reached the call to Root \n");
	return funcList->checkFuncDuplicates();
}

bool FunctionList::checkFuncDuplicates() {
	printf("Reached the call to Function List \n");
	std::set<std::string> declared;
	std::set<std::string> defined;
	bool res = true;
	for(unsigned long int i = 0; i < list.size(); i++) {
		res = res && list[i]->setDefDecl(declared, defined);
	}
	return res;
}

bool FunctionDeclaration::setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) {
	printf("Reached the call to Function Declaration \n");
	if(declared.find(name) != declared.end()) {
		printf("[output] duplicate_decl: %i %i \n", this->location.begin.line, this->location.begin.column);
		return false;
	}

	declared.insert(name);
	return true;
}

bool FunctionDefinition::setDefDecl(std::set<std::string> & declared, std::set<std::string> & defined) {
	printf("Reached the call to Function Definition \n");
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
	printf("	func decl (%i, %i) {\n", this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("		return type: %s\n", types[type]);
	printf("		name: %s\n", name.c_str());
	paramList->printTree();
	printf("	} \n");
	return;
}

void FunctionDefinition::printTree(){
	printf("	func decl (%i, %i) {\n", this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	funcDecl->printTree();
	blockNode->printTree();
	printf("		return type: %s\n", types[type]);
	printf("	} \n");
	return;
}

void ParameterList::printTree(){
	printf("		params(%i, %i)\n", this->location.begin.line, this->location.begin.column);
	for(unsigned long int i = 0; i < (*paramList).size(); i++){
		(*paramList)[i]->printTree();
	}
	printf("		} \n");
	return;
}

void Suite::printTree(){
	printf("	suite (%i, %i)\n", this->location.begin.line, this->location.begin.column);
	for(unsigned long int i=0; i<suiteList.size(); i++){
		suiteList[i]->printTree();
	}
	printf("	} \n");
	return;
}

void ExpressionStatement::printTree(){
	printf("		expr stmt (%i, %i)\n", this->location.begin.line, this->location.begin.column);
	expr->printTree();
	printf("		} \n");
	return;
}

void Declaration::printTree(){
	printf("		decl (%i, %i)", this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("%s %s", types[type], name.c_str());
	printf("		} \n");
	return;
}

void DeclarationAssign::printTree(){
	printf("		decl assign (%i, %i)", this->location.begin.line, this->location.begin.column);
	decl->printTree();
	expr->printTree();
	printf("		} \n");
	return;
}

void SimpleAssign::printTree(){
	printf("		simp assign(%i, %i)", this->location.begin.line, this->location.begin.column);
	printf(n.c_str());
	expr->printTree();
	printf("		} \n");
	return;
}

void AugmentedAssign::printTree(){
	printf("		aug assign(%i, %i)", this->location.begin.line, this->location.begin.column);
	printf("var name: %s\n", n.c_str());
	const char* augops[4] = {"plus_assign", "minus_assign", "star_assign", "slash_assign"};
	printf("op: %s\n", augops[op]);
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
	printf("		}\n");
	return;
}

void For::printTree(){
	printf("		for(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	s1->printTree();
	expr->printTree();
	s2->printTree();
	printf("		}\n");
	return;
}

void While::printTree(){
	printf("		while(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	printf("		}\n");
	return;
}

void TernaryExpression::printTree(){
	printf("		ternary expr(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	oExpression->printTree();
	tExpression1->printTree();
	tExpression2->printTree();
	printf("		}\n");
	return;
}

void BinaryExpression::printTree(){
	printf("		binary expr(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	expression1->printTree();
	expression2->printTree();
	const char* binops[12] = {"plus", "minus", "mul", "div", "and", "or", "eq", "neq", "lt", "gt", "le", "ge"};
	printf("op: %s\n", binops[op]);
	printf("		}\n");
	return;
}

void CastExpression::printTree(){
	printf("		cast expr(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	cExpression->printTree();
	printf("		}\n");
	return;
}

void UnaryMinusExpression::printTree(){
	printf("		unary minus expr(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	printf("		}\n");
	return;
}

void Int::printTree(){
	printf("			int(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	printf("%i\n", data);
	printf("			}\n");
	return;
}

void Float::printTree(){
	printf("			float(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	printf("%f\n", data);
	printf("			}\n");
	return;
}

void Bool::printTree(){
	printf("			float(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	if(data){
		printf("true");
	} else{
		printf("false");
	}
	printf("			}\n");
	return;
}

void NameExpression::printTree(){
	printf("			name expr(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	printf("%s", name.c_str());
	printf("			}\n");
	return;
}

void FunctionCall::printTree(){
	printf("			func call(%i, %i){\n",this->location.begin.line, this->location.begin.column);
	printf("func: %s", n.c_str());
	args->printTree();
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};

	for(unsigned long int i=0; i<arg_types.size(); i++){
		printf("type %i: %s\n", i, types[arg_types[i]]);
	}
	printf("			}\n");
	return;
}



/*
std::unique_ptr<Root> Root::optimize() {
		std::unique_ptr<FunctionList> optFuncList = funcList.optimize();
		optRoot = make_node<Root>(this->location, optFuncList);
		return optRoot;
}

std::unique_ptr<FunctionList> FunctionList::optimize(){
	std::vector<std::unique_ptr<Function>> optList;

	for(int i = 0; i < list.size; i++){
		optList[i] = list[i] -> optimize();
	}
	optFuncList = make_node<FunctionList> (this->location, optList);
	return optFuncList;
}

std::unique_ptr<FunctionDeclaration> FunctionDeclaration::optimize(){
	Type optType;
	std::string optName;
	std::unique_ptr<ParameterList> optParamList;

	optType = type -> optimize();
	optName = name -> optimize();
	optParamList = paramList -> optimize();

	optFuncDecl = make_node<FunctionDeclaration>(this->location, optType, optName, optParamList);
	return optFuncDecl;
}

std::unique_ptr<Suite> Suite::optimize(){
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

std::unique_ptr<DeclarationAssign> DeclarationAssign::optimize(){
	std::unique_ptr<Declaration> optDecl;
	std::unique_ptr<Expression> optExpr;

	optDecl = decl->optimize();
	optExpr = expr->optimize();

	optDeclAssign = make_node<DeclarationAssign>(this->location, optDecl, optExpr);
	return optDeclAssign;
}
*/

