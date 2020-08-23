#ifndef MEMBER_H
#define MEMBER_H

#include "util/types.h"

namespace ngpl {


PTRS_FOR_CLASS(Member)
class Member
{
public:
	Member(
		const std::string& name,
		const std::string& qualifier
	)
		: _name(name),
		  _qualifier(qualifier)
	{}

	virtual ~Member() {};


	const std::string& name() const { return _name; }
	const std::string& qualifier() const { return _qualifier; }

	virtual std::string asCodeString() const;
	virtual std::string asQualifiedCodeString() const ;
protected:
	std::string _name;
	std::string _qualifier;

};

}

#endif // MEMBER_H
