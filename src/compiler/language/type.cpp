#include "type.h"

#include "scope.h"

#include "toStringUtils.h"
#include "ranges.h"

namespace ngpl {

Type::Type() : Member("", "")
{}


Type::Type(const std::string& name, const std::string& qualifier, uint64_t fixedSize, bool isBasic, bool isFinished)
	: Member(name, qualifier), _fixedSize(fixedSize), _isBasic(isBasic), _isFinished(isFinished)
{}


//Type::Type(std::string&& name, uint64_t fixedSize, bool isBasic, bool isFinished)
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


FunctionSignature::FunctionSignature(std::vector<TypeCWeakPtr>&& argumentTypes
)
	: //_name(std::move(name)),
	  //_returnType(std::move(returnType)),
	  _argumentTypes(std::move(argumentTypes))
{}

std::string FunctionSignature::asCodeString() const
{
	return cat::range(argumentTypes()).map(LAMBDA(t){ return t->asCodeString(); }).join(", ");
}

std::string FunctionSignature::asQualifiedCodeString() const
{
	return cat::range(argumentTypes()).map(LAMBDA(t){ return t->asQualifiedCodeString(); }).join(", ");
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

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const FunctionSignature& v)
{
	s += "FunctionSignature";
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, argumentTypes)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}


}
