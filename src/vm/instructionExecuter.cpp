#include "instructionExecuter.h"

#include "toStringUtils.h"

#include <sstream>

namespace ngpl {

const size_t maxStackSize = 1ull << 5;
const size_t maxGlobalsSize = 100;

ExecutionError::ExecutionError(const cat::String& message, const Instruction& instruction)
	: Exception(cat::SW() << message << " at " << instruction << "."),
	  _instruction(instruction),
	  _rawMessage(message)
{}


using InstrID = InstructionID;

InstructionExecuter::InstructionExecuter(const std::vector<Instruction>& instructions, uint64_t programmCounter)
	: _instructions(instructions),
	  _programmCounter(programmCounter),
	  _stack( maxStackSize),
	  _globals({}, maxGlobalsSize, None{})
{}

void InstructionExecuter::executeInstruction()
{
	_overallCounter++;
	const auto oldPC = _programmCounter;
	const Instruction& instruction = _instructions[_programmCounter];
	const auto &data = instruction.data();

	switch (instruction.id()) {
	case InstrID::NOP: {
		++_programmCounter;
	} break;
	case InstrID::DUP: {
		_stack.push(_stack.peek());
		++_programmCounter;
	} break;
	case InstrID::SWP: {
		std::swap(_stack[_stack.size()-2], _stack[_stack.size()-1]);
		++_programmCounter;
	} break;
	case InstrID::CALL: {
		auto* func = static_cast<const std::function<Value(CallStack&)>*>(data.getValue<const void*>());

		auto result = (*func)(_stack);
		if (not result.hasValue<None>()) {
			_stack.push(std::move(result));
		}
		++_programmCounter;
	} break;

	case InstrID::READ_STCK_F: {
		Address addr = StackAddr(_stack.size()) - data.getValue<StackAddr>() - 1;
		_stack.push(_stack.at(addr));
		++_programmCounter;
	} break;
	case InstrID::READ_STCK_D: {
		Address addr = StackAddr(_stack.size()) - data.getValue<StackAddr>() - 1;
		const auto popedVal = _stack.pop().getValue<int64_t>();
		addr -= popedVal;
		_stack.push(_stack.at(addr));
		++_programmCounter;
	} break;

	case InstrID::WRITE_STCK_F: {
		Address addr = StackAddr(_stack.size()) - data.getValue<StackAddr>() - 1;
		_stack.at(addr) = _stack.pop();
		++_programmCounter;
	} break;
	case InstrID::WRITE_STCK_D: {
		Address addr = StackAddr(_stack.size()) - data.getValue<StackAddr>() - 1;
		const auto delta = _stack.pop().getValue<int64_t>();
		auto val = _stack.pop();
		addr -= delta;
		_stack.at(addr) = std::move(val);
		++_programmCounter;
	} break;

	case InstrID::READ_FA: {
		auto addr = Address(data.getValue<HeapAddr>());
		_stack.push(_globals->at(addr));
		++_programmCounter;
	} break;
/*	case InstrID::READ_DA: {
		auto addr = _stack.pop().getValue<int64_t>();
		_stack.push(_globals->at(addr));
		++_programmCounter;
	} break; */
	case InstrID::READ_FR: {
		const auto popedVal = _stack.pop().getValue<Reference>();
		if (popedVal.source() == nullptr) {
			throw nullPointerException(instruction);
		}
		auto addr = data.getValue<int64_t>();
		_stack.push(popedVal.at(addr));
		++_programmCounter;
	} break;
	case InstrID::READ_DR: {
		const auto popedVal1 = _stack.pop().getValue<Reference>();
		const auto addr = _stack.pop().getValue<int64_t>();
		_stack.push(popedVal1.at(addr));
		++_programmCounter;
	} break;

	case InstrID::WRITE_FA: {
		auto addr = Address(data.getValue<HeapAddr>());
		_globals->at(addr) = _stack.pop();
		++_programmCounter;
	} break;
//	case InstrID::WRITE_DA: {
//		auto addr = _stack.pop().getValue<int64_t>();
//		_globals->at(addr) = _stack.pop();
//		++_programmCounter;
//	} break;
	case InstrID::WRITE_FR: {
		auto reference = _stack.pop().getValue<Reference>();
		auto val = _stack.pop();
		if (reference.source() == nullptr) {
			throw nullPointerException(instruction);
		}
		const auto addr = data.getValue<int64_t>();
		reference.at(addr) = std::move(val);
		++_programmCounter;
	} break;
	case InstrID::WRITE_DR: {
		auto reference = _stack.pop().getValue<Reference>();
		if (reference.source() == nullptr) {
			throw nullPointerException(instruction);
		}
		const auto delta = _stack.pop().getValue<int64_t>();
		auto val = _stack.pop();
		reference.at(delta) = std::move(val);
		++_programmCounter;
	} break;

	case InstrID::POP_VAL: {
		_stack.pop();
		++_programmCounter;
	} break;

	case InstrID::PUSH_NULL_R: {
		_stack.push(Reference(nullptr, 0));
		++_programmCounter;
	} break;
	case InstrID::PUSH_GLBLS_R: {
		auto offset = data.getValue<int64_t>();
		_stack.push(Reference(_globals.weak(), offset));
		++_programmCounter;
	} break;
	case InstrID::PUSH_STACK_R: {
		auto offset = data.getValue<int64_t>();

		const auto fixedVal = 0;  // data.getValue<int64_t>()
		Address addr = Address(_stack.size()) - fixedVal - 1;
		addr -= offset;
		_stack.push(Reference(_stack.values(), addr));
		++_programmCounter;
	} break;

	case InstrID::PUSH_INT: {
		_stack.push(data.getValue<int64_t>());
		++_programmCounter;
	} break;
	case InstrID::PUSH_STR: {
		_stack.push(data.getValue<cat::String>());
		++_programmCounter;
	} break;


	case InstrID::POP_CNTR: {
		_stack.push(int64_t(_programmCounterStack.pop()));
		++_programmCounter;
	} break;
	case InstrID::PUSH_CNTR_FR: {
		_programmCounterStack.push(_programmCounter + data.getValue<int64_t>());
		++_programmCounter;
	} break;

	case InstrID::HEAP_ALLOC: {
		auto size = _stack.pop().getValue<int64_t>();
		auto* allocated = &_heap.emplace_back(size, None());
		_stack.push(Reference(allocated, 0));
		++_programmCounter;
	} break;

	case InstrID::INC_R: {
		auto offset = data.getValue<int64_t>();
		auto& ref = _stack.peek().getValue<Reference>();
		ref += offset;
		++_programmCounter;
	} break;

	case InstrID::ADD_R: {
		auto offset = _stack.pop().getValue<int64_t>();
		auto& ref = _stack.peek().getValue<Reference>();
		ref += offset;
		++_programmCounter;
	} break;
	case InstrID::ADD_SI: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs + rhs);
		++_programmCounter;
	} break;
	case InstrID::SUB_R: {
		auto offset = _stack.pop().getValue<int64_t>();
		auto& ref = _stack.peek().getValue<Reference>();
		ref -= offset;
		++_programmCounter;
	} break;
	case InstrID::SUB_SI: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs - rhs);
		++_programmCounter;
	} break;
	case InstrID::MUL_SI: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs * rhs);
		++_programmCounter;
	} break;
	case InstrID::DIV_SI: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs / rhs);
		++_programmCounter;
	} break;
	case InstrID::REM_SI: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs % rhs);
		++_programmCounter;
	} break;
	case InstrID::NEG_SI: {
		auto rhs = _stack.pop().getValue<int64_t>();
		_stack.push(-rhs);
		++_programmCounter;
	} break;
	case InstrID::SHR_SI: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs >> rhs);
		++_programmCounter;
	} break;
	case InstrID::SHL: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs << rhs);
		++_programmCounter;
	} break;
	case InstrID::AND: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs & rhs);
		++_programmCounter;
	} break;
	case InstrID::OR: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs | rhs);
		++_programmCounter;
	} break;
	case InstrID::XOR: {
		auto rhs = _stack.pop().getValue<int64_t>();
		auto lhs = _stack.pop().getValue<int64_t>();
		_stack.push(lhs ^ rhs);
		++_programmCounter;
	} break;
	case InstrID::NOT: {
		auto rhs = _stack.pop().getValue<int64_t>();
		_stack.push(~rhs);
		++_programmCounter;
	} break;



	case InstrID::IF_Z: {
		if (_stack.pop().getValue<int64_t>()) {
			_programmCounter += 2;
		} else {
			++_programmCounter;
		}
	} break;
	case InstrID::IF_NZ: {
		if (_stack.pop().getValue<int64_t>()) {
			++_programmCounter;
		} else {
			_programmCounter += 2;
		}
	} break;


	case InstrID::JMP_FR: {
		_programmCounter += data.getValue<int64_t>();
	} break;


	case InstrID::JMP_FA: {
		_programmCounter = data.getValue<int64_t>();
	} break;
	case InstrID::JMP_DA: {
		_funcCallCounter++;
		_programmCounter = _stack.pop().getValue<int64_t>();
	} break;

	default:
		throw ExecutionError("unknown Instruction ID", instruction);
	}

	if (_printStacklayout) {
		std::stringstream s;
		s << std::setfill(' ') << std::setw(3);
		s << oldPC << "  ";

		auto indentSize = 2 * _programmCounterStack.size();

		auto idStr = cat::String(indentSize, ' ');
		idStr += cat::String(cat::SW() << instruction.id());

		s << idStr;
		s << cat::String(std::max(1ll, 30 - int64_t(idStr.length())), ' ');
		s << " | ";

		s << std::setfill(' ') << std::setw(3);
		foreach_c(value, _stack) {
			cat::OW(s) << value << " | ";
		}
//		s << std::endl;
//		s << cat::String(35, ' ');
//		s << " | ";
//		foreach_c(value, _globals.get()) {
//			s << value << " | ";
//		}
		s.flush();
		std::cout << s.str() << std::endl;
	}
}


void InstructionExecuter::run()
{
	while (_programmCounter < _instructions.size()) {
		executeInstruction();
	}
}

ExecutionError InstructionExecuter::nullPointerException(const Instruction& instruction)
{
	return  ExecutionError("NullReferenceException!", instruction);
}

}
