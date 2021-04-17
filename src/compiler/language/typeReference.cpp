#include "typeReference.h"

#include "ranges.h"


namespace ngpl {

TypeReference::TypeReference(TypeCWeakPtr baseType, std::vector<TypeReference>&& arguments, bool isReference)
	: _baseType(std::move(baseType)),
	  _arguments(std::move(arguments)),
	  _isReference(isReference)
{}

uint64_t TypeReference::fixedSize() const
{
	if (isReference()) {
		return 1;
	} else {
		return baseType()->fixedSize();
	}
}

cat::String TypeReference::asCodeString() const
{
	cat::SW result;
	result << _baseType->asCodeString();
	if (isGeneric()) {
		result << "<" << cat::range(arguments()).map(LAMBDA(t){ return t.asCodeString(); }).join(", ") << ">";
	}
	if (isReference()) {
		result << "&";
	}
	return result;
}

cat::String TypeReference::asQualifiedCodeString() const
{
	cat::SW result;
	result << _baseType->asQualifiedCodeString();
	if (isGeneric()) {
		result << "<" << cat::range(arguments()).map(LAMBDA(t){ return t.asQualifiedCodeString(); }).join(", ") << ">";
	}
	if (isReference()) {
		result << "&";
	}
	return result;
}

bool operator ==(const TypeReference& lhs, const TypeReference& rhs) {
	return lhs.baseType() == rhs.baseType() and
			lhs.arguments() == rhs.arguments() and
			lhs.isReference() == rhs.isReference();
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const TypeReference& v)
{
	s += "TypeReference";
	auto tuple = std::make_tuple(
				MEMBER_PAIR_GET_FMT(v, baseType, [](cat::WriterObjectABC& s, const TypeCWeakPtr& v) {
					s << "<\"" << v->asQualifiedCodeString() << "\">";
				}),
				MEMBER_PAIR_GET(v, arguments),
				MEMBER_PAIR_GET(v, isReference)
				);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}


} // namespace ngpl
