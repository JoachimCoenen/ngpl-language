#include "instructionExecuter.h"

#include "toStringUtils.h"

namespace ngpl {

ExecutionError::ExecutionError(const std::string& message, const Instruction& instruction)
	: Exception(cat::SW() << message << " at " << instruction << "."),
	  _instruction(instruction),
	  _rawMessage(message)
{}


using InstrID = InstructionID;

InstructionExecuter::InstructionExecuter(const std::vector<Instruction>& instructions, uint64_t programmCounter)
	: _instructions(instructions),
	  _programmCounter(programmCounter)
{
	_variables = std::vector<Value>(100, None());
}

void InstructionExecuter::executeInstruction()
{
	_overallCounter++;
	const Instruction& instruction = _instructions[_programmCounter];
	const auto &data = instruction.data();

	switch (instruction.id()) {
	case InstrID::NOP: {
		++_programmCounter;
	} break;
	case InstrID::DUP: {
		_temporaryStack.push(_temporaryStack.peek());
		++_programmCounter;
	} break;
	case InstrID::SWP: {
		std::swap(_temporaryStack[_temporaryStack.size()-2], _temporaryStack[_temporaryStack.size()-1]);
		++_programmCounter;
	} break;
	case InstrID::CALL: {
		auto* func = static_cast<const std::function<Value(cat::Stack<Value>&)>*>(data.getValue<const void*>());
	//        for (auto i = func->argumentCount(); i --> 0;) {
	//            args.push_back(_temporaryStack.pop());
	//        }
		//std::string DBGVAL = cat::SW() << _temporaryStack.peek();
		auto result = (*func)(_temporaryStack);
		if (not result.hasValue<None>()) {
			_temporaryStack.push(std::move(result));
		}
		++_programmCounter;
	} break;

	case InstrID::READ_STCK_F: {
		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
		_temporaryStack.push(_temporaryStack.at(addr));
		++_programmCounter;
	} break;
	case InstrID::READ_STCK_D: {
		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
		addr -= _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(_temporaryStack.at(addr));
		++_programmCounter;
	} break;

	case InstrID::WRITE_STCK_F: {
		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
		_temporaryStack.at(addr) = _temporaryStack.pop();
		++_programmCounter;
	} break;
	case InstrID::WRITE_STCK_D: {
		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
		addr -= _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.at(addr) = _temporaryStack.pop();
		++_programmCounter;
	} break;

	case InstrID::READ_FA: {
		auto addr = data.getValue<int64_t>();
		_temporaryStack.push(_variables.at(addr));
		++_programmCounter;
	} break;
/*	case InstrID::READ_DA: {
		auto addr = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(_variables.at(addr));
		++_programmCounter;
	} break; */
	case InstrID::READ_FR: {
		auto addr = _temporaryStack.pop().getValue<int64_t>() + data.getValue<int64_t>();
		_temporaryStack.push(_variables.at(addr));
		++_programmCounter;
	} break;
	case InstrID::READ_DR: {
		auto addr = _temporaryStack.pop().getValue<int64_t>() + _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(_variables.at(addr));
		++_programmCounter;
	} break;

	case InstrID::WRITE_FA: {
		auto addr = data.getValue<int64_t>();
		_variables.at(addr) = _temporaryStack.pop();
		++_programmCounter;
	} break;
//	case InstrID::WRITE_DA: {
//		auto addr = _temporaryStack.pop().getValue<int64_t>();
//		_variables.at(addr) = _temporaryStack.pop();
//		++_programmCounter;
//	} break;
	case InstrID::WRITE_FR: {
		auto addr = _temporaryStack.pop().getValue<int64_t>() + data.getValue<int64_t>();
		_variables.at(addr) = _temporaryStack.pop();
		++_programmCounter;
	} break;
	case InstrID::WRITE_DR: {
		auto addr = _temporaryStack.pop().getValue<int64_t>() + _temporaryStack.pop().getValue<int64_t>();
		_variables.at(addr) = _temporaryStack.pop();
		++_programmCounter;
	} break;

	case InstrID::POP_VAL: {
		_temporaryStack.pop();
		++_programmCounter;
	} break;
	case InstrID::PUSH_INT: {
		_temporaryStack.push(data.getValue<int64_t>());
		++_programmCounter;
	} break;
	case InstrID::PUSH_STR: {
		_temporaryStack.push(data.getValue<std::string>());
		++_programmCounter;
	} break;


	case InstrID::POP_CNTR: {
		_temporaryStack.push(int64_t(_programmCounterStack.pop()));
		++_programmCounter;
	} break;
	case InstrID::PUSH_CNTR_FR: {
		_programmCounterStack.push(_programmCounter + data.getValue<int64_t>());
		++_programmCounter;
	} break;


	case InstrID::ADD_SI: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs + rhs);
		++_programmCounter;
	} break;
	case InstrID::SUB_SI: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs - rhs);
		++_programmCounter;
	} break;
	case InstrID::MUL_SI: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs * rhs);
		++_programmCounter;
	} break;
	case InstrID::DIV_SI: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs / rhs);
		++_programmCounter;
	} break;
	case InstrID::REM_SI: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs % rhs);
		++_programmCounter;
	} break;
	case InstrID::NEG_SI: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(-rhs);
		++_programmCounter;
	} break;
	case InstrID::SHR_SI: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs >> rhs);
		++_programmCounter;
	} break;
	case InstrID::SHL: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs << rhs);
		++_programmCounter;
	} break;
	case InstrID::AND: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs & rhs);
		++_programmCounter;
	} break;
	case InstrID::OR: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs | rhs);
		++_programmCounter;
	} break;
	case InstrID::XOR: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		auto lhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(lhs ^ rhs);
		++_programmCounter;
	} break;
	case InstrID::NOT: {
		auto rhs = _temporaryStack.pop().getValue<int64_t>();
		_temporaryStack.push(~rhs);
		++_programmCounter;
	} break;



	case InstrID::IF_Z: {
		if (_temporaryStack.pop().getValue<int64_t>()) {
			_programmCounter += 2;
		} else {
			++_programmCounter;
		}
	} break;
	case InstrID::IF_NZ: {
		if (_temporaryStack.pop().getValue<int64_t>()) {
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
		_programmCounter = _temporaryStack.pop().getValue<int64_t>();
	} break;

	default:
		throw ExecutionError("unknown Instruction ID", instruction);
	}
}


void InstructionExecuter::run()
{
	while (_programmCounter < _instructions.size()) {
		executeInstruction();
	}
}

}
