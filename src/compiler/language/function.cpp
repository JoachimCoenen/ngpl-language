#include "function.h"

#include "ranges.h"

namespace ngpl {



FunctionSignature::FunctionSignature(cat::DynArray<Parameter>&& parameters,
		TypeReference&& returnType)
	: //_name(std::move(name)),
	  _parameters(std::move(parameters)),
	  _returnType(std::move(returnType))
{}

bool FunctionSignature::isCallableWith(const CallArgs& args) const
{
	if (_parameters.size() != args.arguments().size()) {
		return false;
	}
	return cat::zip(_parameters, args.arguments())
			.all(LAMBDA(v) { return v.second.type().isAssignableTo(v.first.type()); });
	//return _parameterTypes == argTypes.types;
}

cat::String FunctionSignature::asCodeString(bool includeReturn) const
{
	cat::String args = cat::range(parameters()).map_c(LAMBDA(t){ return t.asCodeString(); }).join(", ");
	if (includeReturn) {
		return cat::SW() << "(" << args << ") -> " << _returnType.asCodeString();
	} else {
		return cat::SW() << "(" << args << ")";
	}
}

cat::String FunctionSignature::asQualifiedCodeString(bool includeReturn) const
{
	cat::String args = cat::range(parameters()).map_c(LAMBDA(t){ return t.asQualifiedCodeString(); }).join(", ");
	if (includeReturn) {
		return cat::SW() << "(" << args << ") -> " << _returnType.asQualifiedCodeString();
	} else {
		return cat::SW() << "(" << args << ")";
	}
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const FunctionSignature& v)
{
	s += "FunctionSignature";
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, parameters)
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
	return cat::range(_signature.parameters())
			.map_c(LAMBDA(v){ return v.fixedSize(); })
			.join()
			+ (isMethod() ? 1 : 0)
			+ (isCtor() ? returnType().fixedSize() : 0);
}

bool FunctionBase::isCallableWith(const CallArgs& args) const
{
	return _signature.isCallableWith(args);
}

cat::String FunctionBase::asCodeString() const {
	if (isCtor()) {
		return cat::SW() << returnType().baseType()->asCodeString() << _signature.asCodeString(false);
	}
	return cat::SW() << Member::asCodeString() << _signature.asCodeString();
}

cat::String FunctionBase::asQualifiedCodeString() const {
	if (isCtor()) {
		return cat::SW() << returnType().baseType()->asQualifiedCodeString() << _signature.asQualifiedCodeString(false);
	}
	return cat::SW() << Member::asQualifiedCodeString() << _signature.asQualifiedCodeString();
}

cat::WriterObjectABC& Function::print(cat::WriterObjectABC& s) const {
	s+= cat::nlIndent;

	if (isCtor()) {
		s += "ctor";
		s += _signature.asCodeString(false);
	}
	else if (isDtor()) {
		s += "dtor";
		s += _signature.asCodeString(false);
	}
	else {
		s += "func ";
		s += Member::asCodeString();
		s += _signature.asCodeString();
	}
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
			if (overload->signature().parameters() == signature.parameters()) {
				reasonOut = "overload " + overload->asCodeString() + " alredy exists.";
				return false;
			}
		}
	return true;
}


FunctionBaseCWeakPtr FunctionOverloads::tryGetOverload(const CallArgs& args) const
{
	foreach_c(overload, _overloads) {
		if (overload->isCallableWith(args)) {
			return overload.weak();
		}
	}
	return nullptr;
}

bool FunctionOverloads::hasOverload(const CallArgs& args) const
{
	return tryGetOverload(args) != nullptr;
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

cat::String CallArgs::asCodeString() const
{
	return cat::range(arguments()).map_c(LAMBDA(t){ return t.asCodeString(); }).join(", ");
}

cat::String CallArgs::asQualifiedCodeString() const
{
	return cat::range(arguments()).map_c(LAMBDA(t){ return t.asQualifiedCodeString(); }).join(", ");
}

Argument::Argument(TypeReference&& type)
	: _type(std::move(type))
{}

cat::String Argument::asCodeString() const
{
	return type().asCodeString();
}

cat::String Argument::asQualifiedCodeString() const
{
	return type().asQualifiedCodeString();
}

CallArgs::CallArgs(cat::DynArray<Argument>&& arguments)
	: _arguments(std::move(arguments))
{}

Parameter::Parameter(cat::String&& name, TypeReference&& type)
	: _name(std::move(name)),
	  _type(std::move(type))
{}

Parameter::Parameter(const cat::String& name, TypeReference&& type)
	: _name(name),
	  _type(std::move(type))
{}

uint64_t Parameter::fixedSize() const
{
	return type().fixedSize();
}

cat::String Parameter::asCodeString() const
{
	return cat::SW() << name() << ": " << type().asCodeString();
}

cat::String Parameter::asQualifiedCodeString() const
{
	return cat::SW() << name() << ": " << type().asQualifiedCodeString();
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const Parameter& v) {
	s += "Parameter";
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, name),
	MEMBER_PAIR_GET(v, type)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}


}
