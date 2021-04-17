#include "function.h"

#include "ranges.h"

namespace ngpl {



FunctionSignature::FunctionSignature(
		cat::DynArray<TypeReference>&& parameterTypes,
		TypeReference&& returnType)
	: //_name(std::move(name)),
	  _parameterTypes(std::move(parameterTypes)),
	  _returnType(std::move(returnType))
{}

bool FunctionSignature::isCallableWith(const CallArgTypes& argTypes) const
{
	return _parameterTypes == argTypes.types;
}

cat::String FunctionSignature::asCodeString() const
{
	cat::String args = cat::range(parameterTypes()).map_c(LAMBDA(t){ return t.asCodeString(); }).join(", ");
	return cat::SW() << "(" << args << ") -> " << _returnType.asCodeString();
}

cat::String FunctionSignature::asQualifiedCodeString() const
{
	cat::String args = cat::range(parameterTypes()).map_c(LAMBDA(t){ return t.asQualifiedCodeString(); }).join(", ");
	return cat::SW() << "(" << args << ") -> " << _returnType.asQualifiedCodeString();
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const FunctionSignature& v)
{
	s += "FunctionSignature";
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, parameterTypes)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}


int32_t FunctionBase::stackDelta() const
{
	return returnType().fixedSize() - argumentsStackSize();
}

int32_t FunctionBase::argumentsStackSize() const
{
	return cat::range(_signature.parameterTypes())
			.map_c(LAMBDA(v){ return v.fixedSize(); })
			.join() + (isMethod() ? 1 : 0);
}

bool FunctionBase::isCallableWith(const CallArgTypes& argTypes) const
{
	return _signature.isCallableWith(argTypes);
}

cat::String FunctionBase::asCodeString() const {
	return cat::SW() << Member::asCodeString() << _signature.asCodeString();
}

cat::String FunctionBase::asQualifiedCodeString() const {
	return cat::SW() << Member::asCodeString() << _signature.asQualifiedCodeString();
}

cat::WriterObjectABC& Function::print(cat::WriterObjectABC& s) const {
	s+= cat::nlIndent;
	s += "func ";
	s += asCodeString();
	s.incIndent();
	body().print(s);
	s.decIndent();
	return s;
}

void Function::recalculteSideEffects()
{
	_hasSideEffect = body().hasSideEffect();
}

FunctionOverloads::FunctionOverloads()
{}


void FunctionOverloads::add(FunctionBasePtr&& function)
{
	_overloads.push_back(std::move(function));
}

bool FunctionOverloads::canAddOverload(const FunctionSignature& signature, cat::String& reasonOut) const
{
	foreach_c(overload, _overloads) {
			if (overload->signature().parameterTypes() == signature.parameterTypes()) {
				reasonOut = "overload " + overload->asCodeString() + " alredy exists.";
				return false;
			}
		}
	return true;
}


FunctionBaseCWeakPtr FunctionOverloads::tryGetOverload(const CallArgTypes& argTypes) const
{
	foreach_c(overload, _overloads) {
		if (overload->isCallableWith(argTypes)) {
			return overload.weak();
		}
	}
	return nullptr;
}

bool FunctionOverloads::hasOverload(const CallArgTypes& argTypes) const
{
	return tryGetOverload(argTypes) != nullptr;
}

FunctionOverloadsBuilder& FunctionOverloadsBuilder::add(FunctionBasePtr&& function)
{
	fo.add(std::move(function));
	return *this;
}

FunctionOverloads FunctionOverloadsBuilder::build()
{
	return std::move(fo);
}

cat::String CallArgTypes::asCodeString() const
{
	return cat::range(types).map_c(LAMBDA(t){ return t.asCodeString(); }).join(", ");
}

cat::String CallArgTypes::asQualifiedCodeString() const
{
	return cat::range(types).map_c(LAMBDA(t){ return t.asQualifiedCodeString(); }).join(", ");
}


}
