#include "syntaxError.h"

using cat::SW;

namespace ngpl {

SyntaxError::SyntaxError(const cat::String& message, const Position& pos)
	: CompileError(pos),
	  _rawMessage({}, cat::String(message))
{}

SemanticsError::SemanticsError(const cat::String& message, const Position& pos)
	: CompileError(pos),
	  _rawMessage({}, cat::String(message))
{}

}
