#include "syntaxError.h"

#include "toStringUtils.h"

using cat::SW;

namespace ngpl {

SyntaxError::SyntaxError(const std::string& message, const Position& pos)
	: Exception(SW() << message << " at line " << pos.line()+1 << ", col " << pos.column()+1 << "."),
	  _pos(pos),
	  _rawMessage(message)
{}


}
