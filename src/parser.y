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
%left TOK_LOG_OR /* lower precedence comes first */
%left TOK_LOG_AND
%left TOK_MINUS TOK_PLUS
%left TOK_SLASH TOK_STAR
%type <Node*> root

%start root

%%

root
	: function_list { printf("root := function_list \n"); }
	;

function_list
	: function
	| function_list function { printf("function_list := function | function_list function \n"); }
	;

function
	: function_decl TOK_SEMIC
	| function_defn { printf("function := function_decl TOK_SEMIC | function_defn \n"); }
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
	: %empty
	| declaration declaration_extra { printf("parameter_list := %%empty | declaration declaration_extra \n"); }
	;

declaration_extra
	: %empty
	| TOK_COMMA declaration declaration_extra { printf("declaration_extra := %%empty | TOK_COMMA declaration declaration_extra \n"); }
	;

block
	: TOK_LBRACE suite TOK_RBRACE { printf("block := TOK_LBRACE suite TOK_RBRACE \n"); }
	;

suite
	: %empty
	| statement suite { printf("suite := %%empty | statement suite \n"); }
	;

declaration
	: type name { printf("declaration := type name \n"); }
	;

statement
	: single_statement TOK_SEMIC
	| compound_statement { printf("statement := single_statement TOK_SEMIC| compound_statement \n"); }
	;

single_statement
	: declaration TOK_ASSIGN expression
	| name TOK_ASSIGN expression
	| name binary_op TOK_ASSIGN expression
	| TOK_BREAK
	| TOK_CONTINUE
	| TOK_RETURN
	| TOK_RETURN expression
	| expression { printf("single_statement := declaration assign expression | \
				name assign expression | name binary_op assign expression \
				| TOK_BREAK | TOK_CONTINUE | TOK_RETURN | TOK_RETURN expression \n"); }
	;

expression
	: TOK_TRUE
	| TOK_FALSE
	| TOK_INT
	| TOK_FLOAT
	| binary_expression
	| unary_expression
	| relational_expression
	| ternary_expression
	| cast_expression
	| function_call 
	| TOK_LPAREN expression TOK_RPAREN { printf("expression := TOK_TRUE | TOK_FALSE | TOK_INT | TOK_FLOAT | binary_expression | \
				unary_expression | relational_expression | ternary_expression | cast_expression | function_call | \
				TOK_LPAREN expression TOK_RPAREN \n"); }
	;

compound_statement
	: TOK_IF TOK_LPAREN expression TOK_RPAREN block
	| TOK_FOR TOK_LPAREN TOK_SEMIC TOK_SEMIC TOK_RPAREN block
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC TOK_SEMIC TOK_RPAREN block
	| TOK_FOR TOK_LPAREN TOK_SEMIC expression TOK_SEMIC TOK_RPAREN block
	| TOK_FOR TOK_LPAREN TOK_SEMIC TOK_SEMIC single_statement TOK_RPAREN block
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC expression TOK_SEMIC TOK_RPAREN block
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC TOK_SEMIC single_statement TOK_RPAREN block
	| TOK_FOR TOK_LPAREN TOK_SEMIC expression TOK_SEMIC single_statement TOK_RPAREN block
	| TOK_FOR TOK_LPAREN single_statement TOK_SEMIC expression TOK_SEMIC single_statement TOK_RPAREN block
	| TOK_WHILE TOK_LPAREN expression TOK_RPAREN block {printf("compound_statement := TOK_IF TOK_LPAREN expression TOK_RPAREN block | \
											TOK_FOR TOK_LPAREN single_statement? TOK_SEMIC expression? block \
											TOK_SEMIC single_statement? TOK_RPAREN block \
											| TOK_WHILE TOK_LPAREN expression TOK_RPAREN block \n"); }
	; 

binary_expression
	: expression binary_op expression {printf("binary_expression := expression binary_op expression \n"); }
	;

unary_expression
	: unary_op expression {printf("unary_expression := unary_op expression \n"); }
	;

relational_expression
	: expression relational_op expression {printf("relational_expression := expression relational_op expression \n"); }
	;

binary_op
	: TOK_PLUS
	| TOK_MINUS
	| TOK_STAR
	| TOK_SLASH
	| TOK_LOG_AND
	| TOK_LOG_OR {printf("binary_op := TOK_PLUS | TOK_MINUS | TOK_STAR | TOK_SLASH | TOK_LOG_AND | TOK_LOG_OR \n"); }
	;

unary_op
	: TOK_MINUS { printf("unary_op := minus \n"); }
	;

relational_op
	: TOK_EQ
	| TOK_NE
	| TOK_LT
	| TOK_GT
	| TOK_LE
	| TOK_GE { printf("relational_op := eq | ne | lt | gt | le | ge \n"); }
	;

ternary_expression
	: expression TOK_QM expression TOK_COLON expression { printf("ternary_expression := expression question_mark expression colon expression \n"); }
	;

cast_expression
	: TOK_LPAREN type TOK_RPAREN expression { printf("cast_expression := lparen type rparen expression \n"); }
	;

function_call
	: name TOK_LPAREN TOK_RPAREN
	| name TOK_LPAREN expression comma_expression TOK_RPAREN { printf("function_call := name lparen (expression (comma expression)*)? rparen \n"); }
	;

comma_expression
	: %empty
	| TOK_COMMA expression comma_expression { printf("comma_expression := %%empty | TOK_COMMA expression comma_expression \n"); }
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
