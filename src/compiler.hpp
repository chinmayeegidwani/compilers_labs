#ifndef ECE467_COMPILER_HPP_INCLUDED
#define ECE467_COMPILER_HPP_INCLUDED

#include "llvm/Support/Error.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include <string>
#include <memory>
#include <deque>
#include <map>
#include <optional>
#include <functional>
#include <system_error>

class Node;
class CompilationUnit;

int lex(char const*);
int parse(char const*, std::unique_ptr<Node>&);
bool verify_ast(Node*);
std::unique_ptr<Node> optimize(std::unique_ptr<Node>);
void print_ast(Node*);
std::unique_ptr<CompilationUnit> compile(Node*);

class CompilationUnit {
public:
	static void initialize();

	CompilationUnit();
	bool process(Node*);
	std::optional<llvm::Error> build();
	std::error_code dump(std::string);
	int run(int, char**);

	using MainFunction = std::function<int(int, char**)>;
	std::map<std::string, llvm::AllocaInst *> namedValues;
	std::deque<llvm::BasicBlock *> headers;
	std::deque<llvm::BasicBlock *> afters;
	std::unique_ptr<llvm::LLVMContext> context;
	llvm::IRBuilder<> builder;
	std::unique_ptr<llvm::Module> module;
	MainFunction main;
	friend class Node;
};

#endif // ECE467_COMPILER_HPP_INCLUDED
