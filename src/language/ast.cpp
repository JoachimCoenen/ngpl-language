#include "ast.h"

#include "cat_exception.h"

namespace ngpl {


int BinaryOperatorCall::getPrecedence(const cat::String& name) {
	if (false) {
	} else if (cat::isAnyOf(name, "*", "/", "%")) {
	return 9;
	} else if (cat::isAnyOf(name, "+", "-")) {
	return 8;
	} else if (cat::isAnyOf(name, "<<", ">>")) {
	return 7;
	} else if (cat::isAnyOf(name, "<", ">", "<=", ">=", "is", "as")) {
	return 6;
	} else if (cat::isAnyOf(name, "==", "!=")) {
	return 5;
	} else if (name == "&") {
	return 4;
	} else if (name == "^") {
	return 3;
	} else if (name == "|") {
	return 2;
	} else if (cat::isAnyOf(name, "&&", "and")) {
	return 1;
	} else if (cat::isAnyOf(name, "||", "or")) {
	return 0;
	} else {
	throw cat::Exception(cat::SW() << "Unkown precedence for operator '" << name << "'." );
	}
}

}
