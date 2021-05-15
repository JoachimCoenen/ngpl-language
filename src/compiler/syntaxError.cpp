#include "syntaxError.h"

#include "toStringUtils.h"

using cat::SW;

namespace ngpl {

SyntaxError::SyntaxError(const cat::String& message, const Position& pos)
	: CompileError(pos),
	  _rawMessage({}, cat::String(message))
{}


}
