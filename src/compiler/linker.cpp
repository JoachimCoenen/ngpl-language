#include "linker.h"

#include "function.h"
#include "type.h"
#include "../vm/instruction.h"

#include "ranges.h"

namespace {
namespace itm = ngpl::intermediate;
}


namespace ngpl {

namespace Instrs = instructions;

Linker::Linker(std::vector<UnitCWeakPtr>&& units)
	: _units(std::move(units))
{}

void Linker::generateInstructionStream()
{
	_instructions.push_back(Instrs::Nop(Position()));
	foreach_c(unit, _units) {
		generateInstructionStream(unit->scope());
	}
	_instructions[0] = Instrs::JumpFA(_instructions.size(), Position());
	foreach_c(unit, _units) {
		generateInstructionStream(&unit->body());
	}

}

void Linker::generateInstructionStream(itm::IntermediateCodeContainerCWeakPtr container)
{
	foreach_c(code, container->instructions) {
		if (auto instr = code.as<itm::IntermediateSimpleInstruction>()) {
			if (instr->id() == InstructionID::CALL) {
				auto func = FunctionBaseCWeakPtr(static_cast<const FunctionBase*>(instr->data().template getValue<const void*>()));
				if (auto fn = func.as<Function>()) {
					_instructions.push_back(Instrs::PushCntrFR(+2, instr->pos()));
				}
			}
			_instructions.push_back(Instruction(instr->id(), instr->data(), instr->pos()));
		} else if (auto ifInstr = code.as<itm::IntermediateIf>()) {
			generateInstructionStreamIf(ifInstr);
		} else if (auto loopInstr = code.as<itm::IntermediateLoop>()) {
			generateInstructionStreamLoop(loopInstr);
		} else if (auto specialInstr = code.as<itm::IntermediateSpecial>()) {
			generateInstructionStreamSpecial(specialInstr);
		} else {
			throw cat::Exception("unhndeled IntermediateInstruction sub type.");
		}
	}

	if (auto type = container.as<Type>()) {
		generateInstructionStream(type->scope());
	}
}

void Linker::generateInstructionStream(ScopeCWeakPtr scope)
{

	foreach_c(func, cat::range(scope->getFunctions())
			  .map_c(LAMBDA(funcsPair) { return cat::range(funcsPair.second); })
			  .flatten()
			  .map_c(LAMBDA(funcPair) { return funcPair.second.getRaw(); })
	) {
		_functionEntryPoints[func->asQualifiedCodeString()] = _instructions.size();
		generateInstructionStream(&func->body());
	}

	foreach_c(type, cat::range(scope->getTypes())
			  .map_c(LAMBDA(typePair) { return typePair.second.getRaw(); })
	) {
		_functionEntryPoints[type->asQualifiedCodeString()] = _instructions.size();
		generateInstructionStream(type->scope());
		generateInstructionStream(&type->body());
	}

}

void Linker::generateInstructionStreamIf(intermediate::IntermediateIfCWeakPtr ifInstr)
{
	if (ifInstr->isInverted) {
		_instructions.push_back(Instrs::IfNotZero(ifInstr->pos()));
	} else {
		_instructions.push_back(Instrs::IfZero(ifInstr->pos()));
	}
	const auto jmpInstrPos_if = _instructions.size();
	_instructions.push_back(Instrs::Nop(ifInstr->pos()));

	generateInstructionStream(&ifInstr->ifCode);

	const auto jmpInstrPos_else = _instructions.size();
	if (not ifInstr->elseCode.instructions.empty()) {
		_instructions.push_back(Instrs::Nop(ifInstr->pos()));
	}

	_instructions[jmpInstrPos_if] = Instrs::JumpFR(_instructions.size() - jmpInstrPos_if, ifInstr->pos());

	if (not ifInstr->elseCode.instructions.empty()) {
		generateInstructionStream(&ifInstr->elseCode);
		_instructions[jmpInstrPos_else] = Instrs::JumpFR(_instructions.size() - jmpInstrPos_else, ifInstr->pos());
	}

}

void Linker::generateInstructionStreamLoop(intermediate::IntermediateLoopCWeakPtr loopInstr)
{
	const auto loopStart = _instructions.size();
	_loopControls.push({});
	generateInstructionStream(&loopInstr->code);
	const auto loopEnd = _instructions.size();

	for ( const auto [i, id] : _loopControls.pop()) {
		switch (id) {
		case itm::IntermediateSpecialId::CONTINUE:
			_instructions[i] = Instrs::JumpFR(loopStart - i, _instructions[i].pos());
			break;
		case itm::IntermediateSpecialId::BREAK:
			_instructions[i] = Instrs::JumpFR(loopEnd - i,  _instructions[i].pos());
			break;
		default:
			throw cat::Exception(cat::SW() << "Bad IntermediateSpecialId (" << id << ") in loop controls.");
		}
	}
}

void Linker::generateInstructionStreamSpecial(intermediate::IntermediateSpecialCWeakPtr specialInstr)
{
	switch (specialInstr->id) {
	case itm::IntermediateSpecialId::RETURN:
		_instructions.push_back(Instrs::PopCntr(specialInstr->pos()));
		_instructions.push_back(Instrs::JumpDA(specialInstr->pos()));
		break;
	case itm::IntermediateSpecialId::CONTINUE:
	case itm::IntermediateSpecialId::BREAK:
		_loopControls.peek().push_back({_instructions.size(), specialInstr->id});
		_instructions.push_back(Instrs::Nop(specialInstr->pos()));
		break;
	}
}

void Linker::linkFunctions()
{
	cat::range(_instructions)
			.filter(LAMBDA(instr) { return instr.id() == InstructionID::CALL2; })
			.forEach([&](auto& instr){
						 Address funcAddress = _functionEntryPoints[instr.data().template getValue<std::string>()];
						 instr = Instrs::JumpFA(funcAddress, instr.pos());

					 });
	cat::range(_instructions)
			.filter(LAMBDA(instr) { return instr.id() == InstructionID::CALL; })
			.forEach([&](auto& instr){
						auto func = FunctionBaseCWeakPtr(static_cast<const FunctionBase*>(instr.data().template getValue<const void*>()));
						if (auto fn = func.as<Function>()) {
							auto qualifiedCodeString = fn->asQualifiedCodeString();
							Address funcAddress = _functionEntryPoints.at(qualifiedCodeString);
							instr = Instrs::JumpFA(funcAddress, instr.pos());
						} else {
							auto fn2 = func.as<BuiltinFunction>();
							instr = Instrs::Call(&fn2->_body, instr.pos());
						}

					 });
}




}
