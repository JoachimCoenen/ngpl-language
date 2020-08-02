#include "linker.h"

#include "function.h"
#include "type.h"
#include "../vm/instruction.h"

namespace ngpl {

Linker::Linker(std::vector<UnitCWeakPtr>&& units)
	: _units(std::move(units))
{}

void Linker::generateInstructionStream()
{
	_instructions.push_back(Instructions::Nop(Position()));
	foreach_c(unit, _units) {
		generateInstructionStream(unit->scope());
	}
	_instructions[0] = Instructions::JumpFA(_instructions.size(), Position());
	foreach_c(unit, _units) {
		generateInstructionStream(unit);
	}

}

void Linker::generateInstructionStream(InstructionsContainerCWeakPtr container)
{
	_instructions.insert(_instructions.end(), container->instructions.begin(), container->instructions.end());

	if (auto type = container.as<Type>()) {
		generateInstructionStream(type->scope());
	}
}

void Linker::generateInstructionStream(cat::WeakPtr<const Scope> scope)
{

	foreach_c(func, cat::range(scope->getFunctions())
			  .map_c(LAMBDA(funcsPair) { return cat::range(funcsPair.second); })
			  .flatten()
			  .map_c(LAMBDA(funcPair) { return funcPair.second.getRaw(); })
	) {
		_functionEntryPoints[func->asQualifiedCodeString()] = _instructions.size();
		generateInstructionStream(func);
	}

	foreach_c(type, cat::range(scope->getTypes())
			  .map_c(LAMBDA(typePair) { return typePair.second.getRaw(); })
	) {
		_functionEntryPoints[type->asQualifiedCodeString()] = _instructions.size();
		generateInstructionStream(type);
	}

}

void Linker::linkFunctions()
{
	cat::range(_instructions)
			.filter(LAMBDA(instr) { return instr.id() == InstructionID::CALL2; })
			.forEach([&](auto& instr){
						 Address funcAddress = _functionEntryPoints[instr.data().template getValue<std::string>()];
						 instr = Instructions::JumpFA(funcAddress, instr.pos());

					 });
}




}
