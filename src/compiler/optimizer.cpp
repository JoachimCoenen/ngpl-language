#include "optimizer.h"

#include "unit.h"
#include "intermediate/intermediateCode.h"
#include "../vm/instruction.h"
#include "../vm/value.h"

#include <optional>

//#include <vector>

namespace {
namespace itm = ngpl::intermediate;
}

namespace ngpl {
// ADD_PTR_TYPES

PTRS_FOR_STRUCT(TaggedInstruction)
PTRS_FOR_STRUCT(TaggedValue)
struct TaggedValue {
	int64_t id;
	std::optional<Value> constValue;

	TaggedInstructionPtr generatedBy;
	int64_t lamportTimestamp;
};

struct TaggedInstruction {
	Instruction instr;
	std::vector<TaggedValueWeakPtr> arguments;
	int64_t lamportTimestamp;

	TaggedInstruction(const Instruction& instr, std::vector<TaggedValueWeakPtr>&& arguments)
		: instr(instr),
		  arguments(std::move(arguments))
	{
		foreach_c(arg, arguments) {
			lamportTimestamp = std::max(arg->lamportTimestamp, lamportTimestamp);
		}
		lamportTimestamp += 1;
	}
};
/*
PTRS_FOR_STRUCT(TaggedCell)
struct TaggedCell {
	int64_t id;
	TaggedValueWeakPtr value;
};
*/
PTRS_FOR_STRUCT(ValueStore)
struct ValueStore
{
public:
	/*
	ValueStore() {}
	ValueStore(ValueStore&& other)
		: _nextValueId(std::move(other._nextValueId)),
		  _nextCellId(std::move(other._nextCellId)),
		  _allValues(std::move(other._allValues)),
		  _allCells(std::move(other._allCells)),
		  _stack(std::move(other._stack)),
		  _isValid(std::move(other._isValid))
	{}
	ValueStore(const ValueStore&) = delete;
	ValueStore& operator =(const ValueStore&) = delete;
	ValueStore& operator =(ValueStore&& other) {
		_nextValueId = std::move(other._nextValueId);
		_nextCellId = std::move(other._nextCellId);
		_allValues = std::move(other._allValues);
		_allCells = std::move(other._allCells);
		_stack = std::move(other._stack);
		_isValid = std::move(other._isValid);
		return *this;
	}
	*/
protected:
	int64_t _nextValueId = 1;
	int64_t _nextCellId = -1;
	std::vector<TaggedValuePtr> _allValues{};
	//std::vector<TaggedCellPtr> _allCells{};

public:
	TaggedValueWeakPtr getNewValue(std::optional<Value> constValue, TaggedInstructionPtr&& generatedBy) {
		auto lamportTimestamp = generatedBy ? generatedBy->lamportTimestamp : 0;
		auto* taggedValuePtr = new TaggedValue{_nextValueId++, constValue, std::move(generatedBy), lamportTimestamp};
		return _allValues.emplace_back(taggedValuePtr).getRaw();
	}
/*
	TaggedCellWeakPtr getNewCell(TaggedValueWeakPtr value = nullptr) {
		if (value != nullptr) {
			value = getNewValue();
		}
		auto* taggedCellPtr = new TaggedCell{_nextCellId--, value};
		return _allCells.emplace_back(taggedCellPtr).getRaw();;
	}
*/

};

struct StackLayoutScenario
{
public:
	StackLayoutScenario(ValueStoreWeakPtr&& valueStore)
		: _valueStore(valueStore)
	{}
protected:
	ValueStoreWeakPtr _valueStore;
	cat::Stack<TaggedValueWeakPtr> _stack{};
	bool _isValid = true;
public:
	void invalidate(){ _isValid = false; }
	bool isValid() const { return _isValid; }

	void pushValue(std::optional<Value> constValue, TaggedInstructionPtr&& generatedBy) {
		_stack.push(_valueStore->getNewValue(constValue, std::move(generatedBy)));
	}

	void pushExistingValue(TaggedValueWeakPtr value) {
		_stack.push(value);
	}

	TaggedValueWeakPtr popValue() {
		return _stack.pop();
	}

	TaggedValueWeakPtr peekValue() {
		return _stack.peek();
	}

	void readValue(Address addr) {
		const auto absoultueAddress = getAbsoluteAddress(addr);
		_stack.push(_stack.at(absoultueAddress));
	}

	void writeValue(Address addr) {
		const auto absoultueAddress = getAbsoluteAddress(addr);
		_stack.at(absoultueAddress) = _stack.pop();
	}

	void setValue(Address addr, TaggedValueWeakPtr value) {
		const auto absoultueAddress = getAbsoluteAddress(addr);
		_stack.at(absoultueAddress) = value;
	}

	TaggedValueWeakPtr getValue(Address addr) {
		const auto absoultueAddress = getAbsoluteAddress(addr);
		return _stack.at(absoultueAddress);
	}

	void retagStackValues() {
		invalidate();
		const auto stackSize = _stack.size();
		//_stack.clear();
		for (auto i = stackSize; i-->0;) {
			_stack.at(i) = _valueStore->getNewValue(std::nullopt, nullptr);
		}
	}

	const cat::Stack<TaggedValueWeakPtr>& getStack() const { return _stack; }

	Address getAbsoluteAddress(Address addr) {
		return  Address(_stack.size()) - addr - 1;
	}
	Address getRelativeAddress(Address addr) {
		return  Address(_stack.size()) - addr - 1;
	}


};


class OptimizerInternal
{
	using Instrs = Instructions;
	using InterInstr = intermediate::IntermediateInstruction;
public:
	OptimizerInternal(const UnitWeakPtr& unit)
		: _unit(unit)
	{}

	void optimize() {
		optimizeScope(_unit->scope().getRaw());
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		optimize(_unit, stackLayout);
	}

	void optimize(intermediate::IntermediateCodeContainerWeakPtr container, StackLayoutScenario& stackLayout) {
		for (size_t i = 0; i < container->instructions.size(); ++i) {
			auto& code = container->instructions[i];
			if (auto instr = code.as<itm::IntermediateInstruction>()) {
				optimizeInstruction(instr, stackLayout);
			} else if (auto ifInstr = code.as<itm::IntermediateIf>()) {
				optimizeIf(ifInstr, stackLayout);
			} else if (auto loopInstr = code.as<itm::IntermediateLoop>()) {
				optimizeLoop(loopInstr, stackLayout);
			} else if (auto specialInstr = code.as<itm::IntermediateSpecial>()) {
				optimizeSpecial(specialInstr);
				container->instructions.resize(i+1);
				break;
			} else {
				throw cat::Exception("unhndeled IntermediateCode sub type.");
			}
		}
	}

	void optimizeScope(ScopeWeakPtr scope) {

		foreach_v(func, cat::range(scope->getFunctions())
				  .map(LAMBDA_n(funcsPair) { return cat::range(funcsPair.second); })
				  .flatten()
				  .map(LAMBDA_n(funcPair) { return funcPair.second.getRaw(); })
		) {
			//_functionEntryPoints[func->asQualifiedCodeString()] = _instructions.size();
			optimizeFunc(func);
		}

		foreach_v(type, cat::range(scope->getTypes())
				  .map_c(LAMBDA_n(typePair) { return typePair.second.getRaw(); })
		) {
			//_functionEntryPoints[type->asQualifiedCodeString()] = _instructions.size();
			optimizeType(type);
		}

	}

	// ================================================================================================================

	void optimizeType(TypeWeakPtr type) {
		optimizeScope(type->scope().getRaw());
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		optimize(type, stackLayout);
	}

	void optimizeFunc(FunctionWeakPtr func) {
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		for (auto i = func->argumentsStackSize(); i-->0;) {
			stackLayout.pushValue(std::nullopt, nullptr);
		}
		StackLayoutScenario initialStackLayout(stackLayout);

		optimize(func, stackLayout);

		if (stackLayout.isValid()) {
			auto args = cat::IntRange(0ull, stackLayout.getStack().size()-1).map_c(LAMBDA2(stackLayout, i){ return stackLayout.getStack()[i]; }).toVector();

			std::vector<intermediate::IntermediateCodePtr> intermediateCode;
			TaggedInstruction instr = TaggedInstruction(Instrs::Nop(func->instructions[0]->pos()), std::move(args));
			if (generateInstructionSequenceFromReturn(&instr, initialStackLayout, intermediateCode)) {
				func->instructions = std::move(intermediateCode);
			}
		}
	}

	// ================================================================================================================

	void optimizeInstruction(itm::IntermediateInstructionWeakPtr instr, StackLayoutScenario& stackLayout) {
		using InstrID = InstructionID;

		if (not stackLayout.isValid()) {
			return;
		}

		switch (instr->instr.id()) {
		case InstrID::NOP: {
		} break;
		case InstrID::DUP: {
			stackLayout.readValue(0);
		} break;
		case InstrID::SWP: {
			auto v0 = stackLayout.getValue(0);
			auto v1 = stackLayout.getValue(1);
			stackLayout.setValue(0, v1);
			stackLayout.setValue(1, v0);
		} break;

		case InstrID::CALL: {
			auto* func = static_cast<const BuiltinFunction*>(instr->instr.data().getValue<const void*>());
			std::vector<TaggedValueWeakPtr> arguments = cat::IntRange(0, func->argumentsStackSize()-1)
					.map_c(LAMBDA2(&stackLayout, ) { return stackLayout.popValue(); })
					.toVector();
			arguments = cat::reversed(arguments).toVector();

			TaggedInstructionPtr taggedInstr = new TaggedInstruction(instr->instr, std::move(arguments));
			stackLayout.pushValue(std::nullopt, std::move(taggedInstr));
		} break;
		case InstrID::CALL2: {
			// TODO: resolve actual changes to stack!
			stackLayout.invalidate();
	//		auto* func = static_cast<const BuiltinFunction*>(instr->instr.data().getValue<const void*>());
	//		for (auto i = func->argumentsStackSize(); i-->0;) {
	//			stackLayout.popValue();
	//		}
	//		for (auto i = func->returnType()->fixedSize(); i-->0;) {
	//			stackLayout.pushValue();
	//		}
		} break;

		case InstrID::READ_STCK_F: {
			stackLayout.readValue(instr->instr.data().getValue<int64_t>());
		} break;
		case InstrID::READ_STCK_D: {
	//		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
	//		addr -= _temporaryStack.pop().getValue<int64_t>();
			// TODO: resolve actual changes to stack!
			stackLayout.invalidate();
//			stackLayout.popValue();
//			stackLayout.pushValue();
		} break;

		case InstrID::WRITE_STCK_F: {
			stackLayout.writeValue(instr->instr.data().getValue<int64_t>());
		} break;
		case InstrID::WRITE_STCK_D: {
	//		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
	//		addr -= _temporaryStack.pop().getValue<int64_t>();
			// TODO: resolve actual changes to stack!
			stackLayout.invalidate();
//			stackLayout.popValue();
//			stackLayout.retagStackValues();
		} break;

		case InstrID::READ_FA: {
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {}) );
		} break;
	/*	case InstrID::READ_DA: {
			auto addr = _temporaryStack.pop().getValue<int64_t>();
			_temporaryStack.push(_variables.at(addr));
			++_programmCounter;
		} break; */
		case InstrID::READ_FR: {
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1}) );
		} break;
		case InstrID::READ_DR: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}) );
		} break;

		case InstrID::WRITE_FA: {
			stackLayout.invalidate();
			stackLayout.popValue();
		} break;
	//	case InstrID::WRITE_DA: {
	//		auto addr = _temporaryStack.pop().getValue<int64_t>();
	//		_variables.at(addr) = _temporaryStack.pop();
	//		++_programmCounter;
	//	} break;
		case InstrID::WRITE_FR: {
			stackLayout.invalidate();
			stackLayout.popValue();
			stackLayout.popValue();
		} break;
		case InstrID::WRITE_DR: {
			stackLayout.invalidate();
			stackLayout.popValue();
			stackLayout.popValue();
			stackLayout.popValue();
		} break;

		case InstrID::POP_VAL: {
			stackLayout.popValue();
		} break;
		case InstrID::PUSH_INT: {
			stackLayout.pushValue(instr->instr.data().getValue<int64_t>(), new TaggedInstruction(instr->instr, {}));
		} break;
		case InstrID::PUSH_STR: {
			stackLayout.pushValue(instr->instr.data().getValue<std::string>(), new TaggedInstruction(instr->instr, {}));
		} break;


		case InstrID::POP_CNTR: {
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {}));
		} break;
		case InstrID::PUSH_CNTR_FR: {
			stackLayout.invalidate();
		} break;


		case InstrID::ADD_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::SUB_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::MUL_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::DIV_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::REM_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::NEG_SI: {
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1}));
		} break;
		case InstrID::SHR_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::SHL: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::AND: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::OR: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::XOR: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1, arg2}));
		} break;
		case InstrID::NOT: {
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, new TaggedInstruction(instr->instr, {arg1}));
		} break;



	//	case InstrID::IF_Z: {
	//		if (_temporaryStack.pop().getValue<int64_t>()) {
	//			_programmCounter += 2;
	//		} else {
	//			++_programmCounter;
	//		}
	//	} break;
	//	case InstrID::IF_NZ: {
	//		if (_temporaryStack.pop().getValue<int64_t>()) {
	//			++_programmCounter;
	//		} else {
	//			_programmCounter += 2;
	//		}
	//	} break;


	//	case InstrID::JMP_FR: {
	//		_programmCounter += data.getValue<int64_t>();
	//	} break;


	//	case InstrID::JMP_FA: {
	//		_programmCounter = data.getValue<int64_t>();
	//	} break;
	//	case InstrID::JMP_DA: {
	//		_funcCallCounter++;
	//		_programmCounter = _temporaryStack.pop().getValue<int64_t>();
	//	} break;

		default:
			throw cat::Exception(cat::SW() << "unknown Instruction ID. " << instr->instr);
		}
	}

	void optimizeIf(itm::IntermediateIfWeakPtr ifInstr, StackLayoutScenario& stackLayout) {
		auto ifStackLayout = stackLayout;
		optimize(&ifInstr->ifCode, ifStackLayout);
		auto elseStackLayout = stackLayout;
		optimize(&ifInstr->elseCode, elseStackLayout);
		// ifStackLayout and elseStackLayout should be the same
		if (not ifStackLayout.isValid()){
			stackLayout = ifStackLayout;
		} else if (not elseStackLayout.isValid()){
			stackLayout = elseStackLayout;
		} else {
			// TODO: merge both stackLayouts...!
			stackLayout = elseStackLayout;
			stackLayout.invalidate();
		}
	}

	void optimizeLoop(intermediate::IntermediateLoopWeakPtr loopInstr, StackLayoutScenario& stackLayout) {
		optimize(&loopInstr->code, stackLayout);
	}

	void optimizeSpecial(intermediate::IntermediateSpecialWeakPtr specialInstr) {
		switch (specialInstr->id) {
		case itm::IntermediateSpecialId::RETURN:
			break;
		case itm::IntermediateSpecialId::CONTINUE:
			break;
		case itm::IntermediateSpecialId::BREAK:
			break;
		}
	}

	/**
	 * @brief findRelativeIndexInStack
	 * @param stack
	 * @param value
	 * @return relative index of value in stack or -1 if not found
	 */
	int64_t findRelativeIndexInStack(const cat::Stack<TaggedValueWeakPtr>& stack, TaggedValueWeakPtr value) {
		for (size_t i = 0; i < stack.size(); ++i) {
			if (stack[i]->id == value->id) {
				return stack.size() - i - 1;
			}
		}
		return -1;
	}

	void generateValue(
			TaggedValueWeakPtr value, const Position& pos,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateCodePtr>& intermediateCode
	) {

		auto existingIndex = findRelativeIndexInStack(stackLayout.getStack(), value);

		if (existingIndex < 0) {
			if (value->generatedBy == nullptr) {
				throw cat::Exception("Cannot find value, but also doesnt know how to generate it.");
			} else {
				generateInstructionSequenceFromGraph(value->generatedBy.getRaw(), stackLayout, intermediateCode);
				stackLayout.pushExistingValue(value);
			}
		} else if (existingIndex == 0) {
			intermediateCode.push_back(new InterInstr(Instrs::Dup(0, pos)));
			stackLayout.readValue(0);
		} else {
			intermediateCode.push_back(new InterInstr(Instrs::ReadStackF(existingIndex, pos)));
			stackLayout.readValue(existingIndex);
		}

		NGPL_ASSERT(value->id == stackLayout.peekValue()->id)
	}

	void generateInstructionSequenceFromGraph(
			TaggedInstructionCWeakPtr taggedInstr,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateCodePtr>& intermediateCode
	) {
		/*
		 * Algorithm:
		 *  1. generate instructions for the values in return starting with the lowest lamportTimestamps, left to right
		 *
		 */


		auto& values = taggedInstr->arguments;
		// sort generation order of values:
		std::vector<size_t> orderOfGeneration = cat::IntRange(0ull, values.size()-1).toVector();
		// for now... cat::stableSort(orderOfGeneration, [&values](const size_t &a, const auto &b) { return values.at(a)->lamportTimestamp < values.at(b)->lamportTimestamp; });

		// Generate or find required values:
		foreach_c(index, orderOfGeneration) {
			auto value = values.at(index);
			generateValue(value, taggedInstr->instr.pos(), stackLayout, intermediateCode);
		}

		intermediateCode.push_back(new InterInstr(Instruction(taggedInstr->instr)));
		foreach_c(index [[maybe_unused]], orderOfGeneration) {
			stackLayout.popValue();
		}

	}


	bool generateInstructionSequenceFromReturn(
			TaggedInstructionCWeakPtr taggedInstr,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateCodePtr>& intermediateCode
	) {
		if (taggedInstr->arguments.size() == 0) {
			return false;
		}

		auto& values = taggedInstr->arguments;
		// sort generation order of values:
		std::vector<size_t> orderOfGeneration = cat::IntRange(0ull, values.size()-1).toVector();
		// for now... cat::stableSort(orderOfGeneration, [&values](const size_t &a, const auto &b) { return values.at(a)->lamportTimestamp < values.at(b)->lamportTimestamp; });

		// Generate or find required values:
		foreach_c(index, orderOfGeneration) {
			auto value = values.at(index);

			auto existingIndex = findRelativeIndexInStack(stackLayout.getStack(), value);
			if (existingIndex == stackLayout.getRelativeAddress(index)) {
				continue; // value already is at its final place
			}

			// get value that would be overwritten:
			{
				auto relativeIndex = stackLayout.getStack().size() - index - 1;
				intermediateCode.push_back(new InterInstr(Instrs::ReadStackF(relativeIndex, taggedInstr->instr.pos())));
				stackLayout.readValue(relativeIndex);
			}
			// generatethe actual value:
			generateValue(value, taggedInstr->instr.pos(), stackLayout, intermediateCode);

			// put it in place:
			{
				auto relativeIndex = stackLayout.getStack().size() - index - 1;
				intermediateCode.push_back(new InterInstr(Instrs::WriteStackF(relativeIndex, taggedInstr->instr.pos())));
				stackLayout.writeValue(relativeIndex);
			}


		}

		// clean up stack
		for (auto i = stackLayout.getStack().size() - values.size(); i --> 0; ) {
			intermediateCode.push_back(new InterInstr(Instrs::PopVal(taggedInstr->instr.pos())));
			stackLayout.popValue();
		}
		intermediateCode.push_back(new intermediate::IntermediateSpecial(intermediate::IntermediateSpecialId::RETURN, taggedInstr->instr.pos()));

		return true;
	}


	UnitWeakPtr _unit;
};




// ================================================================================================================
// ================================================================================================================
// ================================================================================================================


void Optimizer::optimize(const UnitWeakPtr& unit)
{
	OptimizerInternal(unit).optimize();
}

}
