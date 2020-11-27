#include "nodes.hpp"

Node::~Node() = default;

Type Root::checkType(std::map<std::string, Type> & scope) {
	Type result = (*funcList).checkType(scope);

	if(result == ERROR) {
		return ERROR;
	}

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
	for(unsigned long int i = 0; i < paramList.size(); i++) {
		paramList[i]->checkType(newScope);
	}

	return type;
}

Type FunctionDefinition::checkType(std::map<std::string, Type> & scope) {
	scope[funcDecl -> name] = funcDecl -> type;
	std::map<std::string, Type> newScope = scope;
	for(unsigned long int i = 0; i < funcDecl->paramList.size(); i++) {
		funcDecl -> paramList[i]->checkType(newScope);
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

Type Declaration::checkType(std::map<std::string, Type> & scope) {
	if(scope.find(name) != scope.end()) {
		printf("[output] duplicate_decl %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	scope[name] = type;

	return NONE;
}

Type ExpressionStatement::checkType(std::map<std::string, Type> & scope) {
	Type res = expr -> checkType(scope);

	if(res == ERROR) {
		return ERROR;
	}

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
	if(res == ERROR) {
		return ERROR;
	}
	if(res != LOGICAL) {
		printf("[output] type_bool: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}
	Type res1 = tExpression1 -> checkType(scope);
	if(res1 == ERROR) {
		return ERROR;
	}
	Type res2 = tExpression2 -> checkType(scope);
	if(res2 == ERROR) {
		return ERROR;
	}
	if(res1 != res2) {
		printf("[output] type_mismatch: %i %i \n", this->location.begin.line, this->location.begin.column);
		return ERROR;
	}

	type = res1;
	return type;
}

Type BinaryExpression::checkType(std::map<std::string, Type> & scope) {
	Type res1 = expression1 -> checkType(scope);
	if(res1 == ERROR) {
		return ERROR;
	}
	Type res2 = expression2 -> checkType(scope);
	if(res2 == ERROR) {
		return ERROR;
	}
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
	for(unsigned long int i = 0; i < args.size(); i++) {
		temp = args[i] -> checkType(scope);
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
		if(!result) {
			return result;
		}
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
	for(unsigned long int i = 0; i < paramList.size(); i++) {
		funcSig[name].push_back(paramList[i] -> type);
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
		printf("[output] type_arg: %i %i \n", this->location.begin.line, this->location.begin.column);
		return false;	
	}

	for(unsigned long int i = 0; i < arg_types.size(); i++) {
		if(arg_types[i] != (funcSig[n])[i]) {
			printf("[Output] type_arg: %i %i \n", this->location.begin.line, this->location.begin.column);
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
	printf("\n		params(%i, %i) {", this->location.begin.line, this->location.begin.column);
	for(unsigned long int i = 0; i < paramList.size(); i++){
		paramList[i]->printTree();
	}
	printf("\n		}");
	return;
}

void FunctionDefinition::printTree(){
	funcDecl->printTree();
	printf("\n	func defn (%i, %i) {", this->location.begin.line, this->location.begin.column);
	blockNode->printTree();
	printf("\n	} ");
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
	printf("\n			break(%i, %i)", this->location.begin.line, this->location.begin.column);
	return;
}

void Continue::printTree(){
	printf("\n			continue(%i, %i)",this->location.begin.line, this->location.begin.column);
	return;
}

void If::printTree(){
	printf("\n		if(%i, %i){",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	b -> printTree();
	printf("\n		}");
	return;
}

void For::printTree(){
	printf("\n		for(%i, %i){",this->location.begin.line, this->location.begin.column);
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
	printf("\n		}");
	return;
}

void While::printTree(){
	printf("\n		while(%i, %i){",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	b -> printTree();
	printf("\n		}");
	return;
}

void TernaryExpression::printTree(){
	printf("\n		ternary expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	oExpression->printTree();
	tExpression1->printTree();
	tExpression2->printTree();
	printf("\n		}");
	return;
}

void BinaryExpression::printTree(){
	printf("\n		binary expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	const char* binops[12] = {"plus", "minus", "mul", "div", "and", "or", "eq", "neq", "lt", "gt", "le", "ge"};
	printf("\n		op: %s ", binops[op]);
	expression1->printTree();
	expression2->printTree();
	printf("\n		}");
	return;
}

void CastExpression::printTree(){
	printf("\n		cast expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	const char* types[6] = {"error", "none", "void", "int", "float", "logical"};
	printf("\n		cast type: %s \n", types[type]);
	cExpression->printTree();
	printf("\n		}");
	return;
}

void UnaryMinusExpression::printTree(){
	printf("\n		unary minus expr(%i, %i){",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	printf("\n		}\n");
	return;
}

void Int::printTree(){
	printf("\n			int(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	printf("%i } ", data);
	return;
}

void Float::printTree(){
	printf("\n			float(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	printf("%f } ", data);
	return;
}

void Bool::printTree(){
	printf("\n			bool(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	if(data){
		printf("true");
	} else{
		printf("false");
	}
	printf(" }");
	return;
}

void NameExpression::printTree(){
	printf("\n			name expr(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	printf(" %s }", name.c_str());
	return;
}

void FunctionCall::printTree(){
	printf("\n			func call(%i, %i){ ",this->location.begin.line, this->location.begin.column);
	printf("\n			func: %s ", n.c_str());

	for(unsigned long int i=0; i < args.size(); i++){
		printf("\n		args: ");
		args[i]->printTree();
	}

	printf("\n			}");
	return;
}

void ReturnVoid::printTree(){
	printf("\n	return void(%i, %i)",this->location.begin.line, this->location.begin.column);
	return;
}

void ReturnNotVoid::printTree(){
	printf("\n	return expr(%i, %i){",this->location.begin.line, this->location.begin.column);
	expr->printTree();
	printf("\n	}");
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

	std::unique_ptr<FunctionDeclaration> optFuncDecl = std::make_unique<FunctionDeclaration>(type, name, std::move(paramList));
	optFuncDecl -> location = this -> location;
	return optFuncDecl;
}

std::unique_ptr<Function> FunctionDefinition::optimizeCP() {
	std::unique_ptr<Block> optBlock = blockNode -> optimizeCP();
	
	std::unique_ptr<FunctionDefinition> optFuncDefn = std::make_unique<FunctionDefinition>(std::move(funcDecl), std::move(optBlock));
	optFuncDefn -> type = type;
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
	res -> type = type;
	res -> location = this -> location;
	return res;
}

std::unique_ptr<Expression> BinaryExpression::optimizeCP() {
	std::unique_ptr expr1Opt = expression1 -> optimizeCP();
	std::unique_ptr expr2Opt = expression2 -> optimizeCP();

	Int* intOpt1 = dynamic_cast<Int *>(expr1Opt.get());
	Int* intOpt2 = dynamic_cast<Int *>(expr2Opt.get());

	if(intOpt1 != nullptr && intOpt2 != nullptr) {
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
	res -> type = type;
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
	res -> type = type;
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
	res -> type = type;
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
	std::unique_ptr<NameExpression> res = std::make_unique<NameExpression>(name);
	res -> location = this -> location;
	res -> type = type;
	return res;
}

std::unique_ptr<Expression> FunctionCall::optimizeCP(){
	std::unique_ptr<FunctionCall> res = std::make_unique<FunctionCall>(n);
	for(unsigned long int i=0; i < args.size(); i++){
		res -> args.push_back((this -> args)[i] -> optimizeCP());
	}
	res -> location = this -> location;
	res -> type = type;
	res -> arg_types = arg_types;
	return res;
}

void Root::codegenP(CompilationUnit * unit) {
	funcList -> codegenP(unit);
	return;
}

void FunctionList::codegenP(CompilationUnit * unit) {

	for(unsigned long int i = 0; i < list.size(); i++) {
		list[i] -> codegen(unit);
	}

	return;
}

llvm::Type * typeHelper(CompilationUnit * unit, Type type) {

	switch(type) {
		case INT: { return llvm::Type::getInt32Ty(*(unit -> context)); break; }
		case FLOAT: { return llvm::Type::getFloatTy(*(unit -> context)); break; }
		case LOGICAL: { return llvm::Type::getInt1Ty(*(unit -> context)); break; }
		case VOID: { return llvm::Type::getVoidTy(*(unit -> context)); break; }
		default: std::cout << "Unknown type provided to typeHelper" << std::endl;
	}

	return nullptr;
}

llvm::Function * FunctionDeclaration::codegen(CompilationUnit * unit) {
	//check to see if the function has already been declared
	if(llvm::Function * F = unit -> module -> getFunction(name)) {
		return F;
	}

	std::vector<llvm::Type *> params;
	for(unsigned long int i = 0; i < paramList.size(); i++) {
		params.push_back(typeHelper(unit, paramList[i] -> type));
	}

	llvm::FunctionType * signature = llvm::FunctionType::get(typeHelper(unit, type), params, false);
	llvm::Function * F = llvm::Function::Create(signature, llvm::Function::ExternalLinkage, name, *(unit -> module));

	unsigned long int i = 0;
	for(llvm::Argument& a : F -> args()) {
		a.setName(paramList[i] -> name);
		i++;
	}

	return F;
}

llvm::Function * FunctionDefinition::codegen(CompilationUnit * unit) {

	llvm::Function * F = funcDecl -> codegen(unit);
	llvm::BasicBlock * BB = llvm::BasicBlock::Create(*(unit -> context), "entry", F);
	unit -> builder.SetInsertPoint(BB);

	unit -> namedValues.clear();
	for(auto& arg : F -> args()) {
		llvm::AllocaInst * alloca = unit -> builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
		unit -> builder.CreateStore(&arg, alloca);
		unit -> namedValues[std::string(arg.getName())] = alloca;
	}

	blockNode -> codegen(unit);

	llvm::verifyFunction(*F);
	return F;
}

bool Suite::codegen(CompilationUnit * unit) {
	bool retVal = false;
	for(unsigned long int i = 0; i < suiteList.size(); i++) {
		retVal = suiteList[i] -> codegen(unit);
		if(retVal) {
			return retVal;
		}
	}

	return false;
}

bool ExpressionStatement::codegen(CompilationUnit * unit) {
	expr -> codegen(unit);
	return false;
}
bool Declaration::codegen(CompilationUnit * unit) {
	llvm::AllocaInst * alloca = unit -> builder.CreateAlloca(typeHelper(unit, type), nullptr, name);
	unit -> namedValues[name] = alloca;
	return false;
}

bool DeclarationAssign::codegen(CompilationUnit * unit) {
	llvm::Value * toAssign = expr -> codegen(unit);
	decl -> codegen(unit);
	unit -> builder.CreateStore(toAssign, unit -> namedValues[decl -> name]);

	return false;
}

bool SimpleAssign::codegen(CompilationUnit * unit) {
	llvm::Value * toAssign = expr -> codegen(unit);
	unit -> builder.CreateStore(toAssign, unit -> namedValues[n]);

	return false;
}

bool AugmentedAssign::codegen(CompilationUnit * unit) {
	llvm::Value * toAssign = expr -> codegen(unit);
	llvm::LoadInst * currVal = unit -> builder.CreateLoad(unit -> namedValues[n] -> getType(), unit -> namedValues[n], n);
	llvm::Value * result;
	switch(op) {
		case PLUS_ASSIGN: {
			result = unit -> builder.CreateAdd(currVal, toAssign, n);
			unit -> builder.CreateStore(result, unit -> namedValues[n]);
			break;
		}
		case MINUS_ASSIGN: {
			result = unit -> builder.CreateSub(currVal, toAssign, n);
			unit -> builder.CreateStore(result, unit -> namedValues[n]);
			break;
		}
		case STAR_ASSIGN: {
			result = unit -> builder.CreateMul(currVal, toAssign, n);
			unit -> builder.CreateStore(result, unit -> namedValues[n]);
			break;
		}
		case SLASH_ASSIGN: {
			result = unit -> builder.CreateSDiv(currVal, toAssign, n);
			unit -> builder.CreateStore(result, unit -> namedValues[n]);
			break;
		}
		default: {
			std::cout << "Unknown augmented assign operator" << std::endl;
			break;
		}
	}

	return false;
}

bool Continue::codegen(CompilationUnit * unit) {
	unit -> builder.CreateBr(unit -> headers.front());
	return true;
}

bool Break::codegen(CompilationUnit * unit) {
	unit -> builder.CreateBr(unit -> afters.front());
	return true;
}

bool ReturnVoid::codegen(CompilationUnit * unit) {
	unit -> builder.CreateRetVoid();
	return true;
}

bool ReturnNotVoid::codegen(CompilationUnit * unit) {
	llvm::Value * retVal = expr -> codegen(unit);
	unit -> builder.CreateRet(retVal);
	return true;
}

bool If::codegen(CompilationUnit * unit) {
	llvm::Value * cond = expr -> codegen(unit);
	llvm::Function * F = unit -> builder.GetInsertBlock() -> getParent();

	llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*(unit -> context), "then", F);
	llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*(unit -> context), "merge");

	unit -> builder.CreateCondBr(cond, thenBB, mergeBB);
	unit -> builder.SetInsertPoint(thenBB);

	bool isTerminated = b -> codegen(unit);

	if(!isTerminated) {
		unit -> builder.CreateBr(mergeBB); 
	}

	F -> getBasicBlockList().push_back(mergeBB);
	unit -> builder.SetInsertPoint(mergeBB);

	return false;
}

bool For::codegen(CompilationUnit * unit) {
	llvm::Function* F = unit -> builder.GetInsertBlock() -> getParent();

	if(s1) {
		s1 -> codegen(unit);
	}


	llvm::BasicBlock* loopCondition =  llvm::BasicBlock::Create(*(unit -> context), "loop condition", F);
	llvm::BasicBlock* loopInduction = llvm::BasicBlock::Create(*(unit->context), "loop induction");
	llvm::BasicBlock* loopBody = llvm::BasicBlock::Create(*(unit -> context), "loop body");
	llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*(unit -> context), "loop body");

	//for the break and continue statements
	unit -> headers.push_front(loopInduction);
	unit -> afters.push_front(mergeBB);

	unit -> builder.CreateBr(loopCondition);
	unit -> builder.SetInsertPoint(loopCondition);

	if(expr) {
		llvm::Value * cond = expr -> codegen(unit);
		unit -> builder.CreateCondBr(cond, loopBody, mergeBB);
	}

	else {
		unit -> builder.CreateBr(loopBody);
	}

	F -> getBasicBlockList().push_back(loopBody);
	unit -> builder.SetInsertPoint(loopBody);

	bool isTerminated = b -> codegen(unit);

	if(!isTerminated) {
		unit -> builder.CreateBr(loopInduction);
	}

	F -> getBasicBlockList().push_back(loopInduction);
	unit -> builder.SetInsertPoint(loopInduction);

	if(s2) {
		s2 -> codegen(unit);
	}

	unit -> builder.CreateBr(loopCondition);

	F -> getBasicBlockList().push_back(mergeBB);
	unit -> builder.SetInsertPoint(mergeBB);

	unit -> headers.pop_front();
	unit -> afters.pop_front();
	
	return false;
}

bool While::codegen(CompilationUnit * unit) {
	llvm::Function* F = unit -> builder.GetInsertBlock() -> getParent();

	llvm::BasicBlock* loopCondition = llvm::BasicBlock::Create(*(unit -> context), "loop condition", F);
	llvm::BasicBlock* loopBody = llvm::BasicBlock::Create(*(unit -> context), "loop body");
	llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*(unit -> context), "merge block");

	//for the break and continue statements
	unit -> headers.push_front(loopCondition);
	unit -> afters.push_front(mergeBB);

	unit -> builder.CreateBr(loopCondition);
	unit -> builder.SetInsertPoint(loopCondition);

	llvm::Value * cond = expr -> codegen(unit);
	unit -> builder.CreateCondBr(cond, loopBody, mergeBB);

	F -> getBasicBlockList().push_back(loopBody);
	unit -> builder.SetInsertPoint(loopBody);
	bool isTerminated = b -> codegen(unit);
	if(!isTerminated) {
		unit -> builder.CreateBr(loopCondition);
	}

	F -> getBasicBlockList().push_back(mergeBB);
	unit -> builder.SetInsertPoint(mergeBB);
	unit -> headers.pop_front();
	unit -> afters.pop_front();

	return false;

}

llvm::Value * TernaryExpression::codegen(CompilationUnit * unit) {
	llvm::Value * predicate = oExpression -> codegen(unit);
	llvm::Function * F = unit -> builder.GetInsertBlock() -> getParent();

	llvm::BasicBlock * true_expression = llvm::BasicBlock::Create(*(unit -> context), "true expression", F);
	llvm::BasicBlock * false_expression = llvm::BasicBlock::Create(*(unit -> context), "false expression");
	llvm::BasicBlock * mergeBB = llvm::BasicBlock::Create(*(unit -> context), "merge block");

	unit -> builder.CreateCondBr(predicate, true_expression, false_expression);

	unit -> builder.SetInsertPoint(true_expression);
	llvm::Value * true_value = tExpression1 -> codegen(unit);

	unit -> builder.CreateBr(mergeBB);

	//in case our expression is another ternary expression, and we are no longe on the same block
	true_expression = unit -> builder.GetInsertBlock();

	F -> getBasicBlockList().push_back(false_expression);
	unit -> builder.SetInsertPoint(false_expression);
	llvm::Value * false_value = tExpression2 -> codegen(unit);
	unit -> builder.CreateBr(mergeBB);
	false_expression = unit -> builder.GetInsertBlock();

	F -> getBasicBlockList().push_back(mergeBB);
	unit -> builder.SetInsertPoint(mergeBB);
	llvm::PHINode* PN = unit -> builder.CreatePHI(true_value -> getType(), 2, "ternary temporary");
	
	PN -> addIncoming(true_value, true_expression);
	PN -> addIncoming(false_value, false_expression);

	return PN;
}

llvm::Value * BinaryExpression::codegen(CompilationUnit * unit) {
	llvm::Value * res;
	llvm::Value * lhs = expression1 -> codegen(unit);
	llvm::Value * rhs = expression2 -> codegen(unit);

	if(lhs -> getType() == typeHelper(unit, INT) || lhs -> getType() == typeHelper(unit, LOGICAL)) {
		switch(op) {
			case PLUS:
			{
				res = unit -> builder.CreateAdd(lhs, rhs, "Add temp ints");
				break;
			}
			case MINUS:
			{
				res = unit -> builder.CreateSub(lhs, rhs, "Subtract temp ints");
				break;
			}
			case MUL:
			{
				res = unit -> builder.CreateMul(lhs, rhs, "Multiply temp ints");
				break;
			}
			case DIV:
			{
				res = unit -> builder.CreateSDiv(lhs, rhs, "Signed division temp ints");
				break;
			}
			case LT:
			{
				res = unit -> builder.CreateICmpSLT(lhs, rhs, "Signed comparison LT ints");
				break;
			}
			case LE:
			{
				res = unit -> builder.CreateICmpSLE(lhs, rhs, "Signed comparison LE ints");
				break;
			}
			case GT:
			{
				res = unit -> builder.CreateICmpSGT(lhs, rhs, "Signed comparison GT ints");
				break;
			}
			case GE:
			{
				res = unit -> builder.CreateICmpSGE(lhs, rhs, "Signed comparision GE ints");
				break;
			}
			case NEQ:
			{
				res = unit -> builder.CreateICmpNE(lhs, rhs, "Not equal ints");
				break;
			}
			case EQ:
			{
				res = unit -> builder.CreateICmpEQ(lhs, rhs, "Equal ints");
				break;
			}
			case OR:
			{
				res = unit -> builder.CreateOr(lhs, rhs, "Or ints");
				break;
			}
			case AND:
			{
				res = unit -> builder.CreateAnd(lhs, rhs, "And ints");
				break;
			}
			default: std::cout << "Input binary operator not recognized" << std::endl; break;
		}
	}

	else if(lhs -> getType() == typeHelper(unit, FLOAT)) {
		switch(op) {
			case PLUS:
			{
				res = unit -> builder.CreateFAdd(lhs, rhs, "Add temp float");
				break;
			}
			case MINUS:
			{
				res = unit -> builder.CreateFSub(lhs, rhs, "Subtract temp float");
				break;
			}
			case MUL:
			{
				res = unit -> builder.CreateMul(lhs, rhs, "Multiply temp float");
				break;
			}
			case DIV:
			{
				res = unit -> builder.CreateFDiv(lhs, rhs, "Division temp float");
				break;
			}
			case LT:
			{
				res = unit -> builder.CreateFCmpOLT(lhs, rhs, "Comparison LT float");
				break;
			}
			case LE:
			{
				res = unit -> builder.CreateFCmpOLE(lhs, rhs, "Comparison LE float");
				break;
			}
			case GT:
			{
				res = unit -> builder.CreateFCmpOGT(lhs, rhs, "Comparison GT float");
				break;
			}
			case GE:
			{
				res = unit -> builder.CreateFCmpOGE(lhs, rhs, "Comparision GE float");
				break;
			}
			case NEQ:
			{
				res = unit -> builder.CreateFCmpONE(lhs, rhs, "NE float");
				break;
			}
			case EQ:
			{
				res = unit -> builder.CreateFCmpOEQ(lhs, rhs, "EQ float");
				break;
			}

			default: std::cout << "Input binary operator not recognized" << std::endl; break;
		}
	}

	return res;
}

llvm::Value * CastExpression:: codegen(CompilationUnit* unit) {
	llvm::Value * res = cExpression -> codegen(unit);
	if(res -> getType() == typeHelper(unit, INT) || res -> getType() == typeHelper(unit, LOGICAL)) {
		switch(type) {
			case INT:
			{
				break;
			}
			case LOGICAL:
			{
				break;
			}
			case FLOAT:
			{
				return unit -> builder.CreateSIToFP(res, typeHelper(unit, FLOAT), "Int or bool to float");
			}
			default: std::cout << "Unrecognized type" << std::endl; break;	
		}
		
	}

	else if(res -> getType() == typeHelper(unit, FLOAT)) {
		switch(type) {
			case INT:
			{
				return unit -> builder.CreateFPToSI(res, typeHelper(unit, INT), "Float to int");
			}
			case LOGICAL:
			{
				return unit -> builder.CreateFPToSI(res, typeHelper(unit, LOGICAL), "Float to bool");
			}
			case FLOAT:
			{
				break;
			}
			default: std::cout << "Unrecognized type" << std::endl; break;	
		}
	}

	return res;
}

llvm::Value * NameExpression::codegen(CompilationUnit* unit) {
	llvm::Value * V = unit -> namedValues[name];
	return unit -> builder.CreateLoad(V, name.c_str());
}

llvm::Value * UnaryMinusExpression::codegen(CompilationUnit* unit) {
	llvm::Value * R = expr -> codegen(unit);
	if(expr -> type == INT || expr -> type == LOGICAL) {
		return unit -> builder.CreateNeg(R, "unary minus int/bool");
	}
	return unit -> builder.CreateFNeg(R, "floating point unary minus");
}

llvm::Value * Float::codegen(CompilationUnit* unit) {
	return llvm::ConstantFP::get(llvm::Type::getFloatTy(*(unit -> context)), data);
}

llvm::Value * Int::codegen(CompilationUnit* unit) {
	return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*(unit -> context)), data, true);
}

llvm::Value * Bool::codegen(CompilationUnit* unit) {
	return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*(unit -> context)), (int) data, true);
}

llvm::Value * FunctionCall::codegen(CompilationUnit* unit) {
	llvm::Function * callee = unit -> module -> getFunction(n);
	if(!callee) {
		std::cout << "Cannot find called function in this module" << std::endl;
	}

	std::vector<llvm::Value *> llvm_args;
	for(unsigned long int i = 0; i < args.size(); i++) {
		llvm_args.push_back(args[i] -> codegen(unit));
	}

	if(callee -> getReturnType() == typeHelper(unit, VOID)) {
		return unit -> builder.CreateCall(callee, llvm_args);
	}

	return unit -> builder.CreateCall(callee, llvm_args, "func call");

}