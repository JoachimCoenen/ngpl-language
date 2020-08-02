#ifndef SYNTAXERROR_H
#define SYNTAXERROR_H

#include "../language/position.h"

#include "cat_utils.h"


namespace ngpl {

class SyntaxError : public cat::Exception
{
	// TODO: maybe add token argument to SyntaxError?
public:
	SyntaxError(const std::string& message, const Position& pos);

	const Position& pos() const { return _pos; }
	const std::string& rawMessage() const { return _rawMessage; }

protected:
	Position _pos;
	std::string _rawMessage;
};

}

#endif // SYNTAXERROR_H
