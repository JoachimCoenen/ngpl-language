#ifndef MEMBER_H
#define MEMBER_H

#include "util/types.h"

#include "catPointers.h"

namespace ngpl {


PTRS_FOR_CLASS(Member)
class Member
{
public:
	Member(
		const cat::String& name,
		const cat::String& qualifier
	)
		: _name(name),
		  _qualifier(qualifier)
	{}

	virtual ~Member() {};


	const cat::String& name() const { return _name; }
	const cat::String& qualifier() const { return _qualifier; }

	virtual cat::String asCodeString() const;
	virtual cat::String asQualifiedCodeString() const ;
protected:
	cat::String _name;
	cat::String _qualifier;

};

}

#endif // MEMBER_H
