%code requires {

// this will be added to your parser.hpp file

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

#include "nodes.hpp"
#include <memory>

}

%code {

// this will be added to your parser.cpp file

#include "lexer.hpp"

static yy::parser::symbol_type yylex(yyscan_t);

template <typename T, typename... Args> static std::unique_ptr<T> make_node(yy::parser::location_type const&, Args&&...);

}

/* see https://www.gnu.org/software/bison/manual/html_node/Declarations.html */

%require "3.6"
%language "c++"
%locations
%param { yyscan_t lexer }
%parse-param { std::unique_ptr<Node>& root }
%verbose
%define api.value.type variant
%define api.token.constructor
%define api.value.automove
%define parse.trace
%define parse.assert

%token <std::string> TOK_ID
%token <int> TOK_INT
%token <double> TOK_FLOAT
%token TOK_TRUE 
%token TOK_FALSE
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_EQ
%token TOK_NE
%token TOK_LT
%token TOK_GT
%token TOK_LE
%token TOK_GE
%token TOK_PLUS
%token TOK_MINUS
%token TOK_STAR
%token TOK_SLASH
%token TOK_LOG_AND
%token TOK_LOG_OR
%token TOK_PLUS_ASSIGN
%token TOK_MINUS_ASSIGN
%token TOK_STAR_ASSIGN
%token TOK_SLASH_ASSIGN
%token TOK_TYPE_INT
%token TOK_TYPE_FLOAT
%token TOK_TYPE_BOOL
%token TOK_TYPE_VOID
%token TOK_IF
%token TOK_WHILE
%token TOK_FOR
%token TOK_BREAK
%token TOK_CONTINUE
%token TOK_RETURN
%token TOK_COMMA
%token TOK_SEMIC
%token TOK_COLON
%token TOK_QM
%token TOK_ASSIGN

%type <std::unique_ptr<Node>> root
%type <std::unique_ptr<Function>> function
%type <std::unique_ptr<FunctionDeclaration>> function_decl
%type <std::unique_ptr<FunctionDefinition>> function_defn
%type <std::unique_ptr<FunctionList>> function_list
%type <Type> type
%type <std::string> name
%type <std::unique_ptr<ParameterList>> parameter_list
%type <std::unique_ptr<std::vector<std::unique_ptr<Declaration>>>> declaration_extra
%type <std::unique_ptr<Block>> block
%type <std::unique_ptr<Declaration>> declaration
%type <std::unique_ptr<Suite>> suite
%type <std::unique_ptr<Node>> statement
%type <std::unique_ptr<SingleStatement>> single_statement
%type <AugmentedAssignOp> augmented_assign
%type <std::unique_ptr<CompoundStatement>> compound_statement
%type <std::unique_ptr<Expression>> expression
%type <std::unique_ptr<Expression>> ternary_expression
%type <std::unique_ptr<Expression>> or_expression
%type <std::unique_ptr<Expression>> and_expression
%type <BinaryOp> eq_op
%type <std::unique_ptr<Expression>> eq_expression
%type <BinaryOp> comp_op
%type <std::unique_ptr<Expression>> comp_expression
%type <BinaryOp> plus_minus_op
%type <std::unique_ptr<Expression>> plus_expression
%type <BinaryOp> mul_div_op
%type <std::unique_ptr<Expression>> mul_expression
%type <std::unique_ptr<Expression>> term
%type <std::unique_ptr<Expression>> factor
%type <std::unique_ptr<FunctionCall>> function_call
%type <std::unique_ptr<std::vector<std::unique_ptr<Expression>>>> comma_expression
%%

root
	: function_list { $$ = make_node<Root>(@1, $1); }
	;

function_list
	: function { $$ = make_node<FunctionList>(@$, $1);}
	| function_list function {$$= $1; $$->list.push_back($2);}
	;

function
	: function_decl TOK_SEMIC {$$ = $1; }
	| function_defn { $$ = $1; }
	;

function_decl
	: type name TOK_LPAREN parameter_list TOK_RPAREN { $$ = make_node<FunctionDeclaration>(@$, $1, $2, $4);}
	;

function_defn
	: function_decl block {$$ = make_node<FunctionDefinition>(@$, $1, $2);}
	;

type
	: TOK_TYPE_INT {$$ = INT; }
	| TOK_TYPE_FLOAT {$$ = FLOAT; }
	| TOK_TYPE_BOOL {$$ = LOGICAL; }
	| TOK_TYPE_VOID {$$ = VOID; }
	;

name 
	: TOK_ID {$$ = $1; }
	;

parameter_list
	: %empty {$$ = make_node<ParameterList>(@$); }
	| declaration declaration_extra {$$ = make_node<ParameterList>(@$); $$ -> paramList = $2; (*($$ -> paramList)).push_back($1); }
	;

declaration_extra
	: %empty {$$ = std::make_unique<std::vector<std::unique_ptr<Declaration>>>(); }
	| TOK_COMMA declaration declaration_extra {$$ = $3; (*$$).push_back($2); }
	;

block
	: TOK_LBRACE suite TOK_RBRACE {$$ = $2; }
	;

suite
	: %empty {$$ = make_node<Suite>(@$); }
	| statement suite {$$ = $2; $$ -> suiteList.push_back($1); }
	;

declaration
	: type name {$$ = make_node<Declaration>(@$, $1, $2); }
	;

statement
	: single_statement TOK_SEMIC { $$ = $1; }
	| compound_statement { $$ = $1; }
	;

single_statement
	: declaration TOK_ASSIGN expression { $$ = make_node<DeclarationAssign>(@$, $1, $3); }
	| name TOK_ASSIGN expression { $$ = make_node<SimpleAssign>(@$, $1, $3); }
	| name augmented_assign expression { $$ = make_node<AugmentedAssign>(@$, $1, $3, $2); }
	| TOK_BREAK { $$ = make_node<Break>(@$); }
	| TOK_CONTINUE { $$ = make_node<Continue>(@$); }
	| TOK_RETURN { $$ = make_node<ReturnVoid>(@$); }
	| TOK_RETURN expression { $$ = make_node<ReturnNotVoid>(@$, $2); }
	| expression { $$ = make_node<ExpressionStatement>(@$, $1); }
	;

augmented_assign
	: TOK_PLUS_ASSIGN { $$ = PLUS_ASSIGN; }
	| TOK_MINUS_ASSIGN { $$ = MINUS_ASSIGN;  }
	| TOK_STAR_ASSIGN { $$= STAR_ASSIGN;  }
	| TOK_SLASH_ASSIGN { $$ = SLASH_ASSIGN; }
	; 

compound_statement
	: TOK_IF TOK_LPAREN expression TOK_RPAREN block {$$ = make_node<If>(@$, $3, $5); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC TOK_SEMIC TOK_RPAREN block {$$ = make_node<For>(@$, nullptr, nullptr, nullptr, $6); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC TOK_SEMIC TOK_RPAREN block {$$ = make_node<For>(@$, $3, nullptr, nullptr, $7); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC expression TOK_SEMIC TOK_RPAREN block {$$ = make_node<For>(@$, nullptr, nullptr, $4, $7); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC TOK_SEMIC single_statement TOK_RPAREN block {$$ = make_node<For>(@$, nullptr, $5, nullptr, $7); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC expression TOK_SEMIC TOK_RPAREN block {$$ = make_node<For>(@$, $3, nullptr, $5, $8); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC TOK_SEMIC single_statement TOK_RPAREN block {$$ = make_node<For>(@$, $3, $6, nullptr, $8); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC expression TOK_SEMIC single_statement TOK_RPAREN block {$$ = make_node<For>(@$, nullptr, $6, $4, $8); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC expression TOK_SEMIC single_statement TOK_RPAREN block {$$ = make_node<For>(@$, $3, $7, $5, $9); }
	| TOK_WHILE TOK_LPAREN expression TOK_RPAREN block {$$ = make_node<While>(@$, $3, $5); }
	; 

expression
	: ternary_expression { $$ = $1; }
	;

ternary_expression
	: or_expression TOK_QM ternary_expression TOK_COLON ternary_expression { $$ = make_node<TernaryExpression>(@$, $1, $3, $5); }
	| or_expression { $$ = $1; }
	;

or_expression
	: or_expression TOK_LOG_OR and_expression { BinaryOp op = OR; $$ = make_node<BinaryExpression>(@$, $1, $3, op); }
	| and_expression { $$ = $1; }
	;

and_expression
	: and_expression TOK_LOG_AND eq_expression { BinaryOp op = AND; $$ = make_node<BinaryExpression>(@$, $1, $3, op); }
	| eq_expression { $$ = $1; }
	;

eq_expression
	: eq_expression eq_op comp_expression { $$ = make_node<BinaryExpression>(@$, $1, $3, $2); }
	| comp_expression { $$ = $1; }
	;

eq_op
	: TOK_NE { $$ = NEQ; }
	| TOK_EQ { $$ = EQ; }
	;

comp_expression
	: comp_expression comp_op plus_expression { $$ = make_node<BinaryExpression>(@$, $1, $3, $2); }
	| plus_expression { $$ = $1; }
	;

comp_op
	: TOK_GE { $$ = GE; }
	| TOK_LE { $$ = LE; }
	| TOK_LT { $$ = LT; }
	| TOK_GT { $$ = GT; }
	;

plus_expression
	: plus_expression plus_minus_op mul_expression { $$ = make_node<BinaryExpression>(@$, $1, $3, $2); }
	| mul_expression { $$ = $1; }
	;

plus_minus_op
	: TOK_PLUS { $$ = PLUS; }
	| TOK_MINUS { $$ = MINUS; }
	;

mul_expression
	: mul_expression mul_div_op term { $$ = make_node<BinaryExpression>(@$, $1, $3, $2); }
	| term { $$ = $1; }
	;

mul_div_op
	: TOK_STAR { $$ = MUL; }
	| TOK_SLASH { $$ = DIV; }
	;

term
	: TOK_LPAREN type TOK_RPAREN term { $$ = make_node<CastExpression>(@$, $2, $4); }
	| TOK_MINUS term { $$ = make_node<UnaryMinusExpression>(@$, $2); }
	| factor { $$ = $1; }
	;

factor
	: TOK_INT { $$ = make_node<Int>(@$, $1); }
	| TOK_FLOAT { $$ = make_node<Float>(@$, $1); }
	| TOK_TRUE { $$ = make_node<Bool>(@$, true); }
	| TOK_FALSE { $$ = make_node<Bool>(@$, true); }
	| TOK_LPAREN expression TOK_RPAREN { $$ = $2; }
	| name {$$ = make_node<NameExpression>(@$, $1); }
	| function_call { $$ = $1; }
	;

function_call
	: name TOK_LPAREN TOK_RPAREN { $$ = make_node<FunctionCall>(@$, $1); }
	| name TOK_LPAREN expression comma_expression TOK_RPAREN { $$ = make_node<FunctionCall>(@$, $1); $$ -> args = $4; (*($$ -> args)).push_back($3); }
	;

comma_expression
	: %empty {std::make_unique<std::vector<std::unique_ptr<Expression>>>(); }
	| TOK_COMMA expression comma_expression { $$ = $3; (*$$).push_back($2); }
	;

%%

yy::parser::symbol_type yylex(yyscan_t lexer) {
	yy::parser::symbol_type s;
	int x = yylex(&s, nullptr, lexer);
	assert(x == 1);
	return s;
}

void yy::parser::error(location_type const& loc, std::string const& msg) {
	std::cout << "[error] parser error at " << loc << ": " << msg << ".\n";
}

template <typename T, typename... Args> static std::unique_ptr<T> make_node(yy::parser::location_type const& loc, Args&&... args) {
	std::unique_ptr<T> n = std::make_unique<T>(std::forward<Args>(args)...);
	n->location = loc;
	return n;
}
