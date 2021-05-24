#include "typeReference.h"

#include "ranges.h"


namespace ngpl {

TypeReference::TypeReference(TypeCWeakPtr baseType, std::vector<TypeReference>&& arguments, int pointerDepth, bool isReference)
	: _baseType(std::move(baseType)),
	  _arguments(std::move(arguments)),
	  _pointerDepth(pointerDepth),
	  _isReference(isReference)
{}

bool TypeReference::isAssignableTo(const TypeReference& destination) const
{
	if (*this == destination) {
		return true;
	}

//	if (this->isReference() and not destination.isReference()) {
//		return false;
//	}

	if (destination.baseType()->qualifier() == "" and destination.baseType()->name() == "Any") {
		int realPtrDepth = this->pointerDepth();
		if (this->baseType()->isClass()) {
			realPtrDepth += 1;
		}
		if (realPtrDepth >= destination.pointerDepth()) {
			return true;
		}
	}

	if (this->baseType()->qualifier() == "" and this->baseType()->name() == "Any") {
		int realPtrDepth = destination.pointerDepth();
		if (destination.baseType()->isClass()) {
			realPtrDepth += 1;
		}
		if (this->pointerDepth() <= realPtrDepth) {
			return true;
		} else {
			return false;
		}
	} else if (this->pointerDepth() != destination.pointerDepth()) {
		return false;
	}

	if (not this->baseType()->isAssignableTo(*destination.baseType())) {
		return false;
	}

	if (this->arguments() != destination.arguments()) {
		return false;
	}
	return true;
}

uint64_t TypeReference::fixedSize() const
{
	if (isRepresentedByReference()) {
		return 1;
	} else {
		return baseType()->fixedSize();
	}
}

cat::String TypeReference::asCodeString() const
{
	cat::SW result;
	if (pointerDepth()) {
		result << cat::String(pointerDepth(), '&');
	}
	result << _baseType->asCodeString();
	if (isGeneric()) {
		result << "<" << cat::range(arguments()).map(LAMBDA(t){ return t.asCodeString(); }).join(", ") << ">";
	}
	if (isReference()) {
		result << "%";
	}
	return result;
}

cat::String TypeReference::asQualifiedCodeString() const
{
	cat::SW result;
	if (pointerDepth()) {
		result << cat::String(pointerDepth(), '&');
	}
	result << _baseType->asQualifiedCodeString();
	if (isGeneric()) {
		result << "<" << cat::range(arguments()).map(LAMBDA(t){ return t.asQualifiedCodeString(); }).join(", ") << ">";
	}
	if (isReference()) {
		result << "%";
	}
	return result;
}

bool operator ==(const TypeReference& lhs, const TypeReference& rhs) {
	const bool baseTypeEquals = lhs.baseType() == rhs.baseType();
	const bool argumentsEqual = lhs.arguments() == rhs.arguments();
	const bool pointerDepthEquals = lhs.pointerDepth() == rhs.pointerDepth();
	const bool isReferenceEquals = lhs.isReference() == rhs.isReference();
	return baseTypeEquals and
			argumentsEqual and
			pointerDepthEquals and
			isReferenceEquals;
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const TypeReference& v)
{
	s += "TypeReference";
	auto tuple = std::make_tuple(
				MEMBER_PAIR_GET_FMT(v, baseType, [](cat::WriterObjectABC& s, const TypeCWeakPtr& v) {
					s << "<\"" << v->asQualifiedCodeString() << "\">";
				}),
				MEMBER_PAIR_GET(v, arguments),
				MEMBER_PAIR_GET(v, pointerDepth),
				MEMBER_PAIR_GET(v, isReference)
				);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}


} // namespace ngpl
