#ifndef SYNTAXERROR_H
#define SYNTAXERROR_H

#include "../language/position.h"

#include "compileError.h"


namespace ngpl {

class SyntaxError : public CompileError
{
	// TODO: maybe add token argument to SyntaxError?
public:
	SyntaxError(const cat::String& message, const Position& pos);

	const cat::String& rawMessage() const override { return *_rawMessage; }

	virtual CompileError* makeCopy() const override {
		return new SyntaxError(*this);
	}

protected:
	cat::SharedPtr<cat::String> _rawMessage;
};

}

#endif // SYNTAXERROR_H
