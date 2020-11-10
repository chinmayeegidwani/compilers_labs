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
%token <std::string> TOK_TYPE
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
%type <std::unique_ptr<Expression>> expression
%type <std::unique_ptr<Expression>> ternary_expression
%type <std::unique_ptr<Expression>> or_expression
%type <std::unique_ptr<Expression>> and_expression
%type <EqOp> eq_op
%type <std::unique_ptr<Expression>> eq_expression
%type <CompOp> comp_op
%type <std::unique_ptr<Expression>> comp_expression
%type <PlusOp> plus_minus_op
%type <std::unqiue_ptr<Expression>> plus_expression
%type <MulOp> mul_div_op
%type <std::unique_ptr<Expression>> mul_expression
%type <std::unique_ptr<Expression>> term
%type <std::unique_ptr<Expression>> factor
%type <std::unique_ptr<Expression>> function_call

%start root

%%

root
	: function_list { printf("root := function_list \n"); }
	;

function_list
	: function { printf("function_list := function  \n"); }
	| function_list function { printf("function_list := function_list function \n"); }
	;

function
	: function_decl TOK_SEMIC { printf("function := function_decl TOK_SEMIC \n"); }
	| function_defn { printf("function := function_defn \n"); }
	;

function_decl
	: type name TOK_LPAREN parameter_list TOK_RPAREN { printf("function_decl := type name lparen parameter_list rparen \n"); }
	;

function_defn
	: function_decl block { printf("function_defn := function_decl block \n"); }
	;

type
	: TOK_TYPE { printf("type := TOK_TYPE  \n"); }
	;

name 
	: TOK_ID { printf("name := identifier \n"); }
	;

parameter_list
	: %empty { printf("parameter_list := %%empty \n"); }
	| declaration declaration_extra { printf("parameter_list := declaration declaration_extra \n"); }
	;

declaration_extra
	: %empty { printf("declaration_extra := %%empty \n"); }
	| TOK_COMMA declaration declaration_extra { printf("declaration_extra := TOK_COMMA declaration declaration_extra \n"); }
	;

block
	: TOK_LBRACE suite TOK_RBRACE { printf("block := TOK_LBRACE suite TOK_RBRACE \n"); }
	;

suite
	: %empty { printf("suite := %%empty \n"); }
	| statement suite { printf("suite := statement suite \n"); }
	;

declaration
	: type name { printf("declaration := type name \n"); }
	;

statement
	: single_statement TOK_SEMIC { printf("statement := single_statement TOK_SEMIC \n"); }
	| compound_statement { printf("statement := compound_statement \n"); }
	;

single_statement
	: declaration TOK_ASSIGN expression { printf("single_statement := declaration assign expression \n"); }
	| name TOK_ASSIGN expression { printf("single_statement := name assign expression \n"); }
	| name augmented_assign expression { printf("single_statemen := name augmented_assign expression \n"); }
	| TOK_BREAK { printf("single_statement := TOK_BREAK \n"); }
	| TOK_CONTINUE { printf("single_statement := TOK_CONTINUE \n"); }
	| TOK_RETURN { printf("single_statement := TOK_RETURN \n"); }
	| TOK_RETURN expression { printf("single_statement := TOK_RETURN expression \n"); }
	| expression { printf("single_statement := expression \n"); }
	;

augmented_assign
	: TOK_PLUS_ASSIGN { printf("augmented_assign := TOK_PLUS_ASSIGN \n"); }
	| TOK_MINUS_ASSIGN { printf("augmented_assign := TOK_MINUS_ASSIGN \n"); }
	| TOK_STAR_ASSIGN { printf("augmented_assign := TOK_STAR_ASSIGN \n"); }
	| TOK_SLASH_ASSIGN { printf("augmented_assign := TOK_SLASH_ASSIGN \n"); }
	;

compound_statement
	: TOK_IF TOK_LPAREN expression TOK_RPAREN block {printf("compound_statement := TOK_IF TOK_LPAREN expression TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC TOK_SEMIC TOK_RPAREN block {printf("compound_statement := TOK_FOR TOK_LPAREN single_statement? \
							TOK_SEMIC expression? TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC TOK_SEMIC TOK_RPAREN block {printf("compound_statement := TOK_FOR TOK_LPAREN single_statement? \
							TOK_SEMIC expression? TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC expression TOK_SEMIC TOK_RPAREN block {printf("compound_statement := TOK_FOR TOK_LPAREN single_statement? \
							TOK_SEMIC expression? TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC TOK_SEMIC single_statement TOK_RPAREN block {printf("compound_statement := TOK_FOR TOK_LPAREN single_statement? \
							TOK_SEMIC expression? TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC expression TOK_SEMIC TOK_RPAREN block {printf("compound_statement := TOK_FOR \
												TOK_LPAREN single_statement? TOK_SEMIC expression? \
												TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC TOK_SEMIC single_statement TOK_RPAREN block {printf("compound_statement := TOK_FOR \
												TOK_LPAREN single_statement? TOK_SEMIC expression? \
												TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN TOK_SEMIC expression TOK_SEMIC single_statement TOK_RPAREN block {printf("compound_statement := TOK_FOR \
												TOK_LPAREN single_statement? TOK_SEMIC expression? \
												TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC expression TOK_SEMIC single_statement TOK_RPAREN block {printf("compound_statement := TOK_FOR \
												TOK_LPAREN single_statement? TOK_SEMIC expression? \
												TOK_SEMIC single_statement? TOK_RPAREN block \n"); }
	| TOK_WHILE TOK_LPAREN expression TOK_RPAREN block {printf("compound_statement := TOK_WHILE TOK_LPAREN expression TOK_RPAREN block \n"); }
	; 

expression
	: ternary_expression { $$ = $1; }
	;

ternary_expression
	: or_expression TOK_QM ternary_expression TOK_COLON ternary_expression { $$ = make_node<TernaryExpression>(@$, $1, $2, $3); }
	| or_expression { $$ = $1; }
	;

or_expression
	: or_expression TOK_LOG_OR and_expression { $$ = make_node<OrExpression>(@$, $1, $2); }
	| and_expression { $$ = $1; }
	;

and_expression
	: and_expression TOK_LOG_AND eq_expression { $$ = make_node<AndExpression>(@$, $1, $2); }
	| eq_expression { $$ = $1; }
	;

eq_expression
	: eq_expression eq_op comp_expression { $$ = make_node<EqExpression>(@$, $1, $3, $2); }
	| comp_expression { $$ = $1; }
	;

eq_op
	: TOK_NE { $$ = NE; }
	| TOK_EQ { $$ = EQ; }
	;

comp_expression
	: comp_expression comp_op plus_expression { $$ = make_node<CompExpression>(@$, $1, $3, $2); }
	| plus_expression { $$ = $1; }
	;

comp_op
	: TOK_GE { $$ = GE; }
	| TOK_LE { $$ = LE; }
	| TOK_LT { $$ = LT; }
	| TOK_GT { $$ = GT; }
	;

plus_expression
	: plus_expression plus_minus_op mul_expression { $$ = make_node<PlusExpression>(@$, $1, $3, $2); }
	| mul_expression { $$ = $1; }
	;

plus_minus_op
	: TOK_PLUS { $$ = PLUS; }
	| TOK_MINUS { $$ = MINUS; }
	;

mul_expression
	: mul_expression mul_div_op term { $$ = make_node<MulExpression>(@$, $1, $3, $2); }
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
	| name TOK_LPAREN expression comma_expression TOK_RPAREN { $$ = make_node<FunctionCall>(@$, $1); $$ -> args.push_back($3); }
	;

comma_expression
	: %empty
	| TOK_COMMA expression comma_expression { $$ -> args.push_back($2); }
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
