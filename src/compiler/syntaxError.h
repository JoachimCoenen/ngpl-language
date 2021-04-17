#ifndef SYNTAXERROR_H
#define SYNTAXERROR_H

#include "../language/position.h"

#include "cat_exception.h"


namespace ngpl {

class SyntaxError : public cat::Exception
{
	// TODO: maybe add token argument to SyntaxError?
public:
	SyntaxError(const cat::String& message, const Position& pos);

	const Position& pos() const { return _pos; }
	const cat::String& rawMessage() const { return _rawMessage; }

protected:
	Position _pos;
	cat::String _rawMessage;
};

}

#endif // SYNTAXERROR_H
