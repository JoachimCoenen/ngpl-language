#include "type.h"

#include "scope.h"

#include "toStringUtils.h"
#include "ranges.h"

namespace ngpl {

Type::Type() : Member("", ""), _scope(nullptr)
{}


Type::Type(const cat::String& name, const cat::String& qualifier, uint64_t fixedSize, TypeKind typeKind, bool isFinished)
	: Member(name, qualifier), _scope(nullptr), _fixedSize(fixedSize), _typeKind(typeKind), _isFinished(isFinished)
{}

bool Type::isAssignableTo(const Type& destination) const
{
	if (this == &destination) {
		return true;
	}
	if (this->qualifier().empty() and this->name() == "Any") {
		return true;
	}
	if (destination.qualifier().empty() and destination.name() == "Any") {
		return true;
	}

	return false;
}

void Type::finish() {
	_fixedSize = Address(scope()->getFrameSize());
	_isFinished = true;
}


//Type::Type(cat::String&& name, uint64_t fixedSize, bool isBasic, bool isFinished)
//	: _name(std::move(name)), _fixedSize(fixedSize), _isBasic(isBasic), _isFinished(isFinished)
//{}

cat::WriterObjectABC& Type::print(cat::WriterObjectABC& s) const
{
	s+= cat::nlIndent;
	s += "type ";
	s += asCodeString();

	s.incIndent();
	{
		s += cat::nlIndent;
		cat::SW s2;
		s2.setIndent(s.getIndent());
		scope()->print(s2);
		if (s2.str().size() > 0) {
			s += s2.str();
			s += cat::nl;
		}
		body().print(s);
	}
	s.decIndent();

   return s;
}


cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const Type& v)
{
	s += "Type";
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, name),
	MEMBER_PAIR_GET(v, fixedSize),
	MEMBER_PAIR_GET(v, isBasic)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const TypeKind& v) {
	switch (v) {
		FORMAT_ENUM_VAL_CASE(TypeKind, BASIC);
		FORMAT_ENUM_VAL_CASE(TypeKind, TUPLE_LIKE);
		FORMAT_ENUM_VAL_CASE(TypeKind, CLASS_LIKE);
		FORMAT_ENUM_VAL_CASE(TypeKind, COMPLEX_ENUM);
	}
	throw cat::Exception("TypeKind " + std::to_string(int(v)) + " not handeled!");
}


}
