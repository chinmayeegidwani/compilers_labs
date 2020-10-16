%code requires {

// this will be added to your parser.hpp file

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

#include <memory>

class Node;

}

%code {

// this will be added to your parser.cpp file

#include "lexer.hpp"
#include "nodes.hpp"

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

%type <Node*> root

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
	: TOK_ID { printf("type := identifier  \n"); }
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
	| name plus_minus_op TOK_ASSIGN expression { printf("single statement := name plus_minus_op assign expression \n"); }
	| name mul_div_op TOK_ASSIGN expression { printf("single statement := name mul_div assign expression \n"); }
	| name TOK_LOG_AND TOK_ASSIGN expression { printf("single statement := name TOK_LOG_AND TOK_ASSIGN expression \n"); }
	| name TOK_LOG_OR TOK_ASSIGN expression { printf("single statement := name TOK_LOG_OR TOK_ASSIGN expression \n"); }
	| TOK_BREAK { printf("single_statement := TOK_BREAK \n"); }
	| TOK_CONTINUE { printf("single_statement := TOK_CONTINUE \n"); }
	| TOK_RETURN { printf("single_statement := TOK_RETURN \n"); }
	| TOK_RETURN expression { printf("single_statement := TOK_RETURN expression \n"); }
	| expression { printf("single_statement := expression \n"); }
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
	: ternary_expression { printf("expression := ternary_expression \n"); }
	;

ternary_expression
	: or_expression TOK_QM ternary_expression TOK_COLON ternary_expression { printf("ternary_expression := or_expression TOK_QM \
										ternary_expression TOK_COLON ternary_expression \n"); }
	| or_expression { printf("ternary_expression := or_expression \n"); }
	;

or_expression
	: or_expression TOK_LOG_OR and_expression { printf("or_expression := or_expression TOK_LOG_OR and_expression \n"); }
	| and_expression { printf("or_expression := and_expression \n"); }
	;

and_expression
	: and_expression TOK_LOG_AND eq_expression { printf("and_expression := and_expression TOK_LOG_AND eq_expression \n"); }
	| eq_expression { printf("and_expression := eq_expression \n"); }
	;

eq_expression
	: eq_expression eq_op comp_expression { printf("eq_expression := eq_expression eq_op comp_expression \n"); }
	| comp_expression { printf("eq_expression := comp_expression \n"); }
	;
eq_op
	: TOK_NE { printf("eq_op := TOK_NE \n"); }
	| TOK_EQ { printf("eq_op := TOK_EQ \n"); }
	;

comp_expression
	: comp_expression comp_op plus_expression { printf("comp_expression := comp_expression comp_op plus_expression \n"); }
	| plus_expression { printf("comp_expression := plus_expression \n"); }
	;

comp_op
	: TOK_GE { printf("comp_op := TOK_GE \n"); }
	| TOK_LE { printf("comp_op := TOK_LE \n"); }
	| TOK_LT { printf("comp_op := TOK_LT \n"); }
	| TOK_GT { printf("comp_op := TOK_GT \n"); }
	;

plus_expression
	: plus_expression plus_minus_op mul_expression { printf("plus_expression := plus_expression plus_minus_op mul_expression \n"); }
	| mul_expression { printf("plus_expression := mul_expression  \n"); }
	;

plus_minus_op
	: TOK_PLUS { printf("plus_minus_op := TOK_PLUS \n"); }
	| TOK_MINUS { printf("plus_minus_op := TOK_MINUS \n"); }
	;

mul_expression
	: mul_expression mul_div_op term { printf("mul_expression := mul_expression mul_div_op term \n"); }
	| term { printf("mul_expression := term \n"); }
	;

mul_div_op
	: TOK_STAR { printf("mul_div_op := TOK_STAR \n"); }
	| TOK_SLASH { printf("mul_div_op := TOK_SLASH \n"); }
	;

term
	: TOK_LPAREN type TOK_RPAREN term { printf("term := TOK_LPAREN type TOK_RPAREN cast_expression \n"); }
	| TOK_MINUS term { printf("term := TOK_MINUS factor \n"); }
	| factor { printf("term := factor \n"); }
	;

factor
	: TOK_INT { printf("factor := TOK_INT \n"); }
	| TOK_FLOAT { printf("factor := TOK_FLOAT  \n"); }
	| TOK_TRUE { printf("factor := TOK_TRUE  \n"); }
	| TOK_FALSE { printf("factor := TOK_FALSE \n"); }
	| TOK_LPAREN expression TOK_RPAREN { printf("factor := TOK_LPAREN expression TOK_RPAREN \n"); }
	| function_call { printf("factor := function_call \n"); }
	;

function_call
	: name TOK_LPAREN TOK_RPAREN { printf("function_call := name lparen (expression (comma expression)*)? rparen \n"); }
	| name TOK_LPAREN expression comma_expression TOK_RPAREN { printf("function_call := name lparen (expression (comma expression)*)? rparen \n"); }
	;

comma_expression
	: %empty { printf("comma_expression := %%empty \n"); }
	| TOK_COMMA expression comma_expression { printf("comma_expression := TOK_COMMA expression comma_expression \n"); }
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
