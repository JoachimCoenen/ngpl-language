#include "optimizer.h"

#include "language/unit.h"
//#include "intermediate/intermediateCode.h"
#include "vm/value.h"

#include "ranges.h"

#include <iostream>
#include <optional>
#include <unordered_map>
#include <unordered_set>

//#include <vector>

namespace {
namespace itm = ngpl::intermediate;
}

namespace ngpl {
// ADD_PTR_TYPES

PTRS_FOR_STRUCT(ValueStore);
PTRS_FOR_STRUCT(TaggedInstruction);
PTRS_FOR_STRUCT(TaggedValue);

struct TaggedValue {
	int64_t id;
	std::optional<Value> constValue;

	TaggedInstructionWeakPtr generatedBy;
	int64_t lamportTimestamp;
	int32_t usageCount = 0;
	bool isAtFinalLocation = false;
	bool isVirtual = false;
	//cat::OwningPtr<std::unordered_map<TaggedValueWeakPtr, int32_t>> recursiveArguments = nullptr;
protected:
	friend struct ValueStore;
	TaggedValue(
			int64_t id,
			std::optional<Value>&& constValue,
			TaggedInstructionWeakPtr generatedBy,
			int64_t lamportTimestamp,
			int16_t usageCount = 0,
			bool isAtFinalLocation = false,
			bool isVirtual = false
	)
		: id(id),
		  constValue(std::move(constValue)),
		  generatedBy(generatedBy),
		  lamportTimestamp(lamportTimestamp),
		  usageCount(usageCount),
		  isAtFinalLocation(isAtFinalLocation),
		  isVirtual(isVirtual)
	{}
/*
	TaggedValue(
			int64_t id,
			std::optional<Value>&& constValue,
			TaggedInstructionPtr&& generatedBy,
			int64_t lamportTimestamp,
			int16_t usageCount = 0,
			bool isAtFinalLocation = false
	)
		: id(id),
		  constValue(std::move(constValue)),
		  generatedBy(std::move(generatedBy)),
		  lamportTimestamp(lamportTimestamp),
		  usageCount(usageCount),
		  isAtFinalLocation(isAtFinalLocation)
	{}
*/
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const TaggedValue& v) {
	s += "TaggedValue";
	auto tuple = std::make_tuple(
	MEMBER_PAIR(v, id),
	MEMBER_PAIR(v, generatedBy),
	MEMBER_PAIR(v, lamportTimestamp),
	MEMBER_PAIR(v, usageCount),
	MEMBER_PAIR(v, isAtFinalLocation),
	MEMBER_PAIR(v, isVirtual)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}

struct OperandUsege { // TODO: find beter name for struct OperandUsege
	TaggedValueWeakPtr val;
	bool usedByMultipleOperands;
	int32_t usageCount;
};

struct InstructionUsege { // TODO: find beter name for struct OperandUsege
	TaggedInstructionWeakPtr instr;
	bool usedByMultipleOperands;
	int32_t usageCount;
};

struct RequirementStats {
	// this could be an unordered_set, but we need the order here...:
	std::vector<OperandUsege> values;
	// this could be an unordered_set, but we need the order here...:
	std::vector<InstructionUsege> instructions;

};

struct TaggedInstruction {
	int64_t id;
	itm::IntermediateSimpleInstruction instr;
	std::vector<TaggedValueWeakPtr> operands;
	std::vector<TaggedValueWeakPtr> generatesValues;
	TaggedValueWeakPtr causesSideEffect = nullptr;
	int64_t lamportTimestamp;
	// this could be an opional, but Qt's debugger doesn't like them :'(
	cat::OwningPtr<RequirementStats> recursiveRequirements = nullptr;
	//TaggedValueWeakPtr result = nullptr;

protected:
	friend struct ValueStore;

	TaggedInstruction(int64_t id, const itm::IntermediateSimpleInstruction& instr, std::vector<TaggedValueWeakPtr>&& arguments)
		: id(id),
		  instr(instr),
		  operands(std::move(arguments))
	{
		lamportTimestamp = 0;
		foreach_n(arg, this->operands) {
			lamportTimestamp = std::max(arg->lamportTimestamp, lamportTimestamp);
			arg->usageCount += 1;
		}
		lamportTimestamp += 1;
	}
};

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const TaggedInstruction& v) {
	s += "TaggedInstruction";
	auto tuple = std::make_tuple(
	MEMBER_PAIR(v, id),
	MEMBER_PAIR(v, instr),
	MEMBER_PAIR(v, operands),
	//MEMBER_PAIR(v, generatesValues),
	//MEMBER_PAIR(v, causesSideEffect),
	MEMBER_PAIR(v, lamportTimestamp)
	//MEMBER_PAIR(v, recursivelyRequiredValues)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}

struct ValueStore {
protected:
	int64_t _nextValueId = 1;
	int64_t _nextInstrId = -1;
	std::vector<TaggedValuePtr> _allValues{};
	//std::vector<TaggedCellPtr> _allCells{};
	std::vector<TaggedInstructionPtr> _allInstructions{};

public:
	TaggedValueWeakPtr getNewValue(std::optional<Value>&& constValue, TaggedInstructionWeakPtr generatedBy, bool isVirtual = false) {
		auto lamportTimestamp = generatedBy ? generatedBy->lamportTimestamp : 0;
		auto* taggedValuePtr = new TaggedValue{_nextValueId++, std::move(constValue), generatedBy, lamportTimestamp, 0, false, isVirtual};
//		if (taggedValuePtr->generatedBy) {
//			taggedValuePtr->generatedBy->result = taggedValuePtr;
//		}
		auto result = _allValues.emplace_back(taggedValuePtr).getRaw();

		if (not result->isVirtual and result->generatedBy != nullptr) {
			generatedBy->generatesValues.push_back(result);
		}

		return result;
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
	TaggedInstructionWeakPtr getNewInstruction(const itm::IntermediateSimpleInstruction& instr, std::vector<TaggedValueWeakPtr>&& arguments) {
		TaggedInstructionPtr taggedInstr = new TaggedInstruction(_nextInstrId++, instr, std::move(arguments));
		TaggedInstructionWeakPtr result = taggedInstr.getRaw();
		_allInstructions.push_back(std::move(taggedInstr));
		return result;
	}

};

struct StackLayoutScenario {
public:
	StackLayoutScenario(ValueStoreWeakPtr&& valueStore)
		: _valueStore(valueStore)
	{}
protected:
	ValueStoreWeakPtr _valueStore;
	cat::Stack<TaggedValueWeakPtr> _stack{};
	std::vector<TaggedValueWeakPtr> _instructionsWithSideEffect;
	bool _isValid = true;
public:
	void invalidate(){ _isValid = false; }
	bool isValid() const { return _isValid; }

	void pushValue(std::optional<Value>&& constValue, TaggedInstructionWeakPtr generatedBy) {
		_stack.push(_valueStore->getNewValue(std::move(constValue), generatedBy));
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

	TaggedInstructionWeakPtr addInstruction(const itm::IntermediateSimpleInstruction& instr, std::vector<TaggedValueWeakPtr>&& arguments) {
		return _valueStore->getNewInstruction(instr, std::move(arguments));
	}

	TaggedInstructionWeakPtr addInstructionWithSideEffect(const itm::IntermediateSimpleInstruction& instr, std::vector<TaggedValueWeakPtr>&& arguments) {
		if (not _instructionsWithSideEffect.empty()) {
			arguments.push_back(_instructionsWithSideEffect.back());
		}
		auto taggedInstr = _valueStore->getNewInstruction(instr, std::move(arguments));
		taggedInstr->causesSideEffect =_valueStore->getNewValue(std::nullopt, taggedInstr, true);
		_instructionsWithSideEffect.push_back(taggedInstr->causesSideEffect);
		return taggedInstr;
	}

	void addExistingSideEffect(TaggedValueWeakPtr value) {
		//NGPL_ASSERT(value->generatedBy->);
		_instructionsWithSideEffect.push_back(value);
	}

	const cat::Stack<TaggedValueWeakPtr>& getStack() const { return _stack; }
	const std::vector<TaggedValueWeakPtr>& getAllInstructionsWithSideEffect() { return _instructionsWithSideEffect; }

	Address getAbsoluteAddress(Address addr) {
		return  Address(_stack.size()) - addr - 1;
	}
	Address getRelativeAddress(Address addr) {
		return  Address(_stack.size()) - addr - 1;
	}


};


class OptimizerInternal
{
	using Instrs = intermediate::Instructions;
	using InterInstr = intermediate::IntermediateSimpleInstruction;
	using InstrID = InstructionID;

public:
	OptimizerInternal(const UnitWeakPtr& unit)
		: _unit(unit)
	{}

	void optimize() {
		optimizeScope(_unit->scope().getRaw());
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		optimizeCodeContainer(&_unit->body(), stackLayout);
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

	void optimizeCodeContainer(intermediate::IntermediateCodeContainerWeakPtr container, StackLayoutScenario& stackLayout) {
		for (size_t i = 0; i < container->instructions.size(); ++i) {
			auto& code = container->instructions[i];
			if (auto instr = code.as<itm::IntermediateSimpleInstruction>()) {
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
				throw cat::Exception("unhndeled IntermediateInstruction sub type.");
			}
		}
	}

	// ================================================================================================================

	void optimizeType(TypeWeakPtr type) {
		optimizeScope(type->scope().getRaw());
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		optimizeCodeContainer(&type->body(), stackLayout);
	}

	void optimizeFunc(FunctionWeakPtr func) {
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		if (func->selfType()) {
			for (auto i = func->selfType()->fixedSize(); i-->0;) {
				stackLayout.pushValue(std::nullopt, nullptr);
			}
		}
		for (auto i = func->argumentsStackSize(); i-->0;) {
			stackLayout.pushValue(std::nullopt, nullptr);
		}
		StackLayoutScenario initialStackLayout(stackLayout);

		optimizeCodeContainer(&func->body(), stackLayout);
		cat::OW out(std::cout);
		out += cat::nl;
		out += " =============== =============== =============== ===============";
		out += cat::nl;
		out += cat::IntRange(0ull, stackLayout.getStack().size()).map(LAMBDA2(&stackLayout, i) { return *stackLayout.getStack()[i]; }).toVector();
		out += cat::nl;
		out += " =============== =============== =============== ===============";
		out += cat::nl;

		func->recalculteSideEffects();

		if (stackLayout.isValid()) {
			auto args = cat::IntRange(0ull, stackLayout.getStack().size()).map_c(LAMBDA2(&stackLayout, i){ return stackLayout.getStack()[i]; }).toVector();
//			if (not stackLayout.getAllInstructionsWithSideEffect().empty()) {
//				args.push_back(stackLayout.getAllInstructionsWithSideEffect().back());
//			}

			auto returnCount = args.size();
			std::vector<intermediate::IntermediateInstructionPtr> intermediateCode;
			TaggedInstructionWeakPtr instr = stackLayout.addInstructionWithSideEffect(Instrs::Nop(func->body().instructions[0]->pos()), std::move(args));

			if (generateInstructionSequenceFromReturn(instr, returnCount, initialStackLayout, intermediateCode)) {
				func->body().instructions = std::move(intermediateCode);
			}
		}
	}

	// ================================================================================================================

	void optimizeInstruction(itm::IntermediateSimpleInstructionWeakPtr instr, StackLayoutScenario& stackLayout) {
		if (not stackLayout.isValid()) {
			return;
		}

		switch (instr->id()) {
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
			auto* func = static_cast<const BuiltinFunction*>(instr->data().getValue<const void*>());
			Address selfTypeSize = func->selfType() ? func->selfType()->fixedSize() : 0;
			std::vector<TaggedValueWeakPtr> arguments = cat::IntRange(0ll, selfTypeSize + func->argumentsStackSize())
					.map_c(LAMBDA2(&stackLayout, ) { return stackLayout.popValue(); })
					.toVector();
			arguments = cat::reversed(arguments).toVector();

			TaggedInstructionWeakPtr taggedInstr;
			if (func->hasSideEffect()) {
				taggedInstr = stackLayout.addInstructionWithSideEffect(*instr, std::move(arguments));
			} else {
				taggedInstr = stackLayout.addInstruction(*instr, std::move(arguments));
			}

			for (auto i = func->returnType()->fixedSize() + selfTypeSize; i --> 0;) {
				stackLayout.pushValue(std::nullopt, taggedInstr);
			}
		} break;
		case InstrID::CALL2: {
			// TODO: resolve actual changes to stack!
			stackLayout.invalidate();
	//		auto* func = static_cast<const BuiltinFunction*>(instr->data().getValue<const void*>());
	//		for (auto i = func->argumentsStackSize(); i-->0;) {
	//			stackLayout.popValue();
	//		}
	//		for (auto i = func->returnType()->fixedSize(); i-->0;) {
	//			stackLayout.pushValue();
	//		}
		} break;

		case InstrID::READ_STCK_F: {
			stackLayout.readValue(instr->data().getValue<int64_t>());
		} break;
		case InstrID::READ_STCK_D: {
	//		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
	//		addr -= _temporaryStack.pop().getValue<int64_t>();
			// TODO: resolve actual changes to stack!
			auto instruct = InterInstr(instr->id(), stackLayout.getAbsoluteAddress(instr->data().getValue<Address>()), instr->pos());
			auto arg = stackLayout.popValue();

			stackLayout.pushValue(std::nullopt, stackLayout.addInstructionWithSideEffect(instruct, {arg}));
			//stackLayout.invalidate();
//			stackLayout.popValue();
//			stackLayout.pushValue();
		} break;

		case InstrID::WRITE_STCK_F: {
			stackLayout.writeValue(instr->data().getValue<int64_t>());
		} break;
		case InstrID::WRITE_STCK_D: {
	//		Address addr = Address(_temporaryStack.size()) - data.getValue<int64_t>() - 1;
	//		addr -= _temporaryStack.pop().getValue<int64_t>();
			// TODO: resolve actual changes to stack!
			auto instruct = InterInstr(instr->id(), stackLayout.getAbsoluteAddress(instr->data().getValue<Address>()), instr->pos());
			auto arg1 = stackLayout.popValue();
			stackLayout.addInstructionWithSideEffect(instruct, {arg1});
			//stackLayout.invalidate();
//			stackLayout.popValue();
//			stackLayout.retagStackValues();
		} break;

		case InstrID::READ_FA: {
			stackLayout.pushValue(std::nullopt, stackLayout.addInstructionWithSideEffect(*instr, {}));
		} break;
	/*	case InstrID::READ_DA: {
			auto addr = _temporaryStack.pop().getValue<int64_t>();
			_temporaryStack.push(_variables.at(addr));
			++_programmCounter;
		} break; */
		case InstrID::READ_FR: {
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstructionWithSideEffect(*instr, {arg1}));
		} break;
		case InstrID::READ_DR: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstructionWithSideEffect(*instr, {arg1, arg2}));
		} break;

		case InstrID::WRITE_FA: {
			auto arg1 = stackLayout.popValue();
			stackLayout.addInstructionWithSideEffect(*instr, {arg1});
		} break;
	//	case InstrID::WRITE_DA: {
	//		auto addr = _temporaryStack.pop().getValue<int64_t>();
	//		_variables.at(addr) = _temporaryStack.pop();
	//		++_programmCounter;
	//	} break;
		case InstrID::WRITE_FR: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.addInstructionWithSideEffect(*instr, {arg1, arg2});
		} break;
		case InstrID::WRITE_DR: {
			auto arg3 = stackLayout.popValue();
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.addInstructionWithSideEffect(*instr, {arg1, arg2, arg3});
		} break;

		case InstrID::POP_VAL: {
			stackLayout.popValue();
		} break;
		case InstrID::PUSH_INT: {
			stackLayout.pushValue(instr->data().getValue<int64_t>(), stackLayout.addInstruction(*instr, {}));
		} break;
		case InstrID::PUSH_STR: {
			stackLayout.pushValue(instr->data().getValue<std::string>(), stackLayout.addInstruction(*instr, {}));
		} break;


		case InstrID::POP_CNTR: {
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {}));
		} break;
		case InstrID::PUSH_CNTR_FR: {
			NGPL_ASSERT(false);
			stackLayout.invalidate();
		} break;


		case InstrID::ADD_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::SUB_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::MUL_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::DIV_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::REM_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::NEG_SI: {
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1}));
		} break;
		case InstrID::SHR_SI: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::SHL: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::AND: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::OR: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::XOR: {
			auto arg2 = stackLayout.popValue();
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1, arg2}));
		} break;
		case InstrID::NOT: {
			auto arg1 = stackLayout.popValue();
			stackLayout.pushValue(std::nullopt, stackLayout.addInstruction(*instr, {arg1}));
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
			throw cat::Exception(cat::SW() << "unknown Instruction ID. " << *instr);
		}
	}

	void optimizeIf(itm::IntermediateIfWeakPtr ifInstr, StackLayoutScenario& stackLayout) {
		auto ifStackLayout = stackLayout;
		optimizeCodeContainer(&ifInstr->ifCode, ifStackLayout);
		auto elseStackLayout = stackLayout;
		optimizeCodeContainer(&ifInstr->elseCode, elseStackLayout);
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
		optimizeCodeContainer(&loopInstr->code, stackLayout);
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

	// ================================================================================================================
	// ================================================================================================================
	// ================================================================================================================

	void cleanup() {
		cleanupScope(_unit->scope().getRaw());
		cleanupCodeContainer(&_unit->body());
	}

	void cleanupScope(ScopeWeakPtr scope) {
		foreach_v(func, cat::range(scope->getFunctions())
				  .map(LAMBDA_n(funcsPair) { return cat::range(funcsPair.second); })
				  .flatten()
				  .map(LAMBDA_n(funcPair) { return funcPair.second.getRaw(); })
		) {
			cleanupFunc(func);
		}

		foreach_v(type, cat::range(scope->getTypes())
				  .map_c(LAMBDA_n(typePair) { return typePair.second.getRaw(); })
		) {
			cleanupType(type);
		}
	}

	void cleanupCodeContainer(intermediate::IntermediateCodeContainerWeakPtr container) {
		for (size_t i = 0; i < container->instructions.size(); ++i) {
			auto& code = container->instructions[i];
			intermediate::IntermediateInstructionPtr replacementInstruction = nullptr;
			if (auto instr = code.as<itm::IntermediateSimpleInstruction>()) {
				replacementInstruction = cleanupInstruction(instr);
			} else if (auto ifInstr = code.as<itm::IntermediateIf>()) {
				replacementInstruction = cleanupIf(ifInstr);
			} else if (auto loopInstr = code.as<itm::IntermediateLoop>()) {
				replacementInstruction = cleanupLoop(loopInstr);
			} else if (auto specialInstr = code.as<itm::IntermediateSpecial>()) {
				replacementInstruction = cleanupSpecial(specialInstr);
				break;
			} else {
				throw cat::Exception("unhandeled IntermediateInstruction sub type.");
			}
			if (replacementInstruction != nullptr) {
				code = std::move(replacementInstruction);
			}
		}
	}

	// ================================================================================================================

	void cleanupType(TypeWeakPtr type) {
		cleanupScope(type->scope().getRaw());
		ValueStore valueStore;
		cleanupCodeContainer(&type->body());
	}

	void cleanupFunc(FunctionWeakPtr func) {
		cleanupCodeContainer(&func->body());
		func->recalculteSideEffects();

	}

	// ================================================================================================================

	intermediate::IntermediateSimpleInstructionPtr cleanupInstruction(itm::IntermediateSimpleInstructionWeakPtr instr) {
		auto& data = instr->data();
		switch (instr->id()) {
		case InstrID::NOP: {} break;
		case InstrID::DUP: {} break;
		case InstrID::SWP: {} break;

		case InstrID::CALL: {} break;
		case InstrID::CALL2: {} break;

		case InstrID::READ_STCK_F: {
			if (data.getValue<Address>() == 0) {
				return new intermediate::IntermediateSimpleInstruction(Instrs::Dup(0, instr->pos()));
			}
		} break;
		case InstrID::READ_STCK_D: {} break;

		case InstrID::WRITE_STCK_F: {} break;
		case InstrID::WRITE_STCK_D: {} break;

		case InstrID::READ_FA: {} break;
		case InstrID::READ_FR: {} break;
		case InstrID::READ_DR: {} break;

		case InstrID::WRITE_FA: {} break;
		case InstrID::WRITE_FR: {} break;
		case InstrID::WRITE_DR: {} break;

		case InstrID::POP_VAL: {} break;
		case InstrID::PUSH_INT: {} break;
		case InstrID::PUSH_STR: {} break;


		case InstrID::POP_CNTR: {} break;
		case InstrID::PUSH_CNTR_FR: {
			NGPL_ASSERT(false);
		} break;

		case InstrID::ADD_SI: {} break;
		case InstrID::SUB_SI: {} break;
		case InstrID::MUL_SI: {} break;
		case InstrID::DIV_SI: {} break;
		case InstrID::REM_SI: {} break;
		case InstrID::NEG_SI: {} break;
		case InstrID::SHR_SI: {} break;
		case InstrID::SHL: {} break;
		case InstrID::AND: {} break;
		case InstrID::OR: {} break;
		case InstrID::XOR: {} break;
		case InstrID::NOT: {} break;

/*
		case InstrID::IF_Z: {} break;
		case InstrID::IF_NZ: {} break;

		case InstrID::JMP_FR: {} break;

		case InstrID::JMP_FA: {} break;
		case InstrID::JMP_DA: {} break;
*/
		default:
			throw cat::Exception(cat::SW() << "unknown Instruction ID. " << *instr);
		}
		return nullptr;
	}

	intermediate::IntermediateInstructionPtr cleanupIf(itm::IntermediateIfWeakPtr ifInstr) {
		cleanupCodeContainer(&ifInstr->ifCode);
		cleanupCodeContainer(&ifInstr->elseCode);

		if (ifInstr->ifCode.isEmpty()) {
			if (ifInstr->elseCode.isEmpty()) {
				// this 'if' is completely useless, so remove it:

				intermediate::IntermediateInstructionWeakPtr replacementInstruction = ifInstr;
				return new intermediate::IntermediateSimpleInstruction(Instrs::Nop(ifInstr->pos()));
			} else {
				// elseCode exists but if Code doesn't, so swap them:
				std::swap(ifInstr->ifCode, ifInstr->elseCode);
				ifInstr->isInverted = not ifInstr->isInverted;
			}
		}
		return nullptr;
	}

	intermediate::IntermediateInstructionPtr cleanupLoop(intermediate::IntermediateLoopWeakPtr loopInstr) {
		cleanupCodeContainer(&loopInstr->code);
		return nullptr;
	}

	intermediate::IntermediateInstructionPtr cleanupSpecial(intermediate::IntermediateSpecialWeakPtr specialInstr) {
		switch (specialInstr->id) {
		case itm::IntermediateSpecialId::RETURN:
			break;
		case itm::IntermediateSpecialId::CONTINUE:
			break;
		case itm::IntermediateSpecialId::BREAK:
			break;
		}
		return nullptr;
	}


	// ================================================================================================================
	// ================================================================================================================
	// ================================================================================================================

	/**
	 * @brief findRelativeIndexInStack
	 * @param stack
	 * @param value
	 * @return relative index of value in stack or -1 if not found
	 */
	int64_t findRelativeIndexInStack(const cat::Stack<TaggedValueWeakPtr>& stack, TaggedValueWeakPtr value) {
		for (size_t i = stack.size(); i --> 0; ) {
			if (stack[i]->id == value->id) {
				return stack.size() - i - 1;
			}
		}
		return -1;
	}

	std::vector<size_t> getOrderOfGeneration(const std::vector<TaggedValueWeakPtr>& operands) {
		// sort generation order of values:
		std::vector<size_t> orderOfGeneration;
		if (operands.size() > 0 and operands.back()->isVirtual) {
			orderOfGeneration = cat::IntRange(operands.size()-1, operands.size()).andThen(cat::IntRange(0ull, operands.size()-1)).toVector();
		} else {
			orderOfGeneration = cat::IntRange(0ull, operands.size()).toVector();
		}
		// for now... cat::stableSort(orderOfGeneration, [&values](const size_t &a, const auto &b) { return values.at(a)->lamportTimestamp < values.at(b)->lamportTimestamp; });
		return orderOfGeneration;
	}

	/**
	 * @brief cacheAllRecursivelyRequiredValuesForGraph
	 * @param instruction
	 * @param alreadyGeneratedValues
	 * walks the DAG (Directed Acyclic Graph) in execution order and determines all values that are needed to calulate this 'subTree'[see: devDocs/dataFlowOptimization.drawio].
	 * already generated values are considered.
	 */
	void cacheAllRecursivelyRequiredValuesForGraph(TaggedInstructionWeakPtr instruction, std::unordered_set<TaggedValueWeakPtr>& alreadyGeneratedValues) {
		struct OperandsPreviouselySeenUsage{ uint32_t index; uint32_t usageCount; };


		if (instruction->recursiveRequirements != nullptr) {
			// we have analysed this previously already.
			return;
		}

		instruction->recursiveRequirements = new RequirementStats;
		auto& recursiveRequirements = *instruction->recursiveRequirements;
		std::unordered_map<TaggedValueWeakPtr, uint32_t> recursivelyRequiredValuesIndices;
		std::unordered_map<TaggedInstructionWeakPtr, uint32_t> recursivelyRequiredInstrsIndices;
		// integer is the index in recursivelyRequiredValues the operand should be inserted later:
		std::unordered_map<TaggedValueWeakPtr, OperandsPreviouselySeenUsage> operandsPreviouselySeen;
		std::unordered_map<TaggedInstructionWeakPtr, OperandsPreviouselySeenUsage> instructionsPreviouselySeen;

		const auto orderOfGeneration = getOrderOfGeneration(instruction->operands);
		std::vector<TaggedValueWeakPtr> orderedOperands = cat::range(orderOfGeneration).map(LAMBDA2_REF_n(&instruction, i) { return instruction->operands.at(i); }).toVector();

		foreach_n(operand, orderedOperands) {
			if (cat::contains(operandsPreviouselySeen, operand)) {
				continue;
			}
			if (cat::contains(instructionsPreviouselySeen, operand->generatedBy)) {
				NGPL_ASSERT(operand->generatedBy != nullptr);
				continue;
			}

			if (not cat::contains(alreadyGeneratedValues, operand)) {
				if (operand->generatedBy) {
					cacheAllRecursivelyRequiredValuesForGraph(operand->generatedBy, alreadyGeneratedValues);

					// Values:
					foreach_n(innerOperandUsage, operand->generatedBy->recursiveRequirements->values) {
						const auto& innerOperand = innerOperandUsage.val;
						if (auto* innerIndex = cat::find(recursivelyRequiredValuesIndices, innerOperand)) {
							// innerOperand was already needed previously, so set usedByMultipleOperands flag:
							recursiveRequirements.values.at(*innerIndex).usedByMultipleOperands = true;
							// checkfor integrity:
							NGPL_ASSERT(recursiveRequirements.values.at(*innerIndex).val->id == innerOperand->id);

						} else {
							recursiveRequirements.values.push_back({innerOperand, false, innerOperandUsage.usageCount});
							recursivelyRequiredValuesIndices[innerOperand] = recursiveRequirements.values.size() - 1;
						}
					}

					// Instructions:
					foreach_n(innerInstrUsage, operand->generatedBy->recursiveRequirements->instructions) {
						const auto& innerInstruction = innerInstrUsage.instr;
						if (auto* innerIndex = cat::find(recursivelyRequiredInstrsIndices, innerInstruction)) {
							// innerOperand was already needed previously, so set usedByMultipleOperands flag:
							recursiveRequirements.instructions.at(*innerIndex).usedByMultipleOperands = true;
							// checkfor integrity:
							NGPL_ASSERT(recursiveRequirements.instructions.at(*innerIndex).instr->id == innerInstruction->id);

						} else {
							recursiveRequirements.instructions.push_back({innerInstruction, false, innerInstrUsage.usageCount});
							recursivelyRequiredInstrsIndices[innerInstruction] = recursiveRequirements.instructions.size() - 1;
						}
					}
				}
			} else {
				alreadyGeneratedValues.insert(operand);
			}

			// set usedByMultipleOperands flag if if operand is already used, otherwse do nothing.
			// note: all operands will be inserted in a seperate loop after this one.
			// Value:
			if (auto* index = cat::find(recursivelyRequiredValuesIndices, operand)) {
				// innerOperand was already needed previously, so set usedByMultipleOperands flag:
				recursiveRequirements.values.at(*index).usedByMultipleOperands = true;
				// checkfor integrity:
				NGPL_ASSERT(recursiveRequirements.values.at(*index).val->id == operand->id);
			} else {
				// do nothing. (note: all operands will be inserted in a seperate loop after this one.)
				if (auto* usage = cat::find(operandsPreviouselySeen, operand)) {
					usage->usageCount += 1;
				} else {
					operandsPreviouselySeen[operand] = {uint32_t(recursiveRequirements.values.size()), 1};
				}
			}

			// set usedByMultipleOperands flag if if operand is already used, otherwse do nothing.
			// note: all operands will be inserted in a seperate loop after this one.
			// Instruction:
			if (auto generatedBy = operand->generatedBy) {
				if (auto* index = cat::find(recursivelyRequiredInstrsIndices, generatedBy)) {
					// innerOperand was already needed previously, so set usedByMultipleOperands flag:
					recursiveRequirements.instructions.at(*index).usedByMultipleOperands = true;
					// checkfor integrity:
					NGPL_ASSERT(recursiveRequirements.instructions.at(*index).instr->id == generatedBy->id);
				} else {
					// do nothing. (note: all operands will be inserted in a seperate loop after this one.)
					if (auto* usage = cat::find(instructionsPreviouselySeen, generatedBy)) {
						usage->usageCount += 1;
					} else {
						instructionsPreviouselySeen[generatedBy] = {uint32_t(recursiveRequirements.instructions.size()), 1};
					}
				}
			}
		}

		// isert all operands in recursivelyRequiredValues:
		foreach_n(operand, cat::reversed(orderedOperands)) {
			// Value:
			if (auto* index [[maybe_unused]] = cat::find(recursivelyRequiredValuesIndices, operand)) {
				// innerOperand was already needed previously, but this operand was earlier, so DONT set usedByMultipleOperands flag.
				// checkfor integrity:
				// NGPL_ASSERT(recursivelyRequiredValues.at(*index).val->id == operand->id);
			} else {
				auto* usage = cat::find(operandsPreviouselySeen, operand);
				NGPL_ASSERT(usage);
				recursiveRequirements.values.insert(recursiveRequirements.values.begin() + usage->index, {operand, false, int32_t(usage->usageCount)});
				recursivelyRequiredValuesIndices[operand] = recursiveRequirements.values.size() - 1;
			}
			// Instruction:
			if (auto generatedBy = operand->generatedBy) {
				if (auto* index [[maybe_unused]] = cat::find(recursivelyRequiredInstrsIndices, generatedBy)) {
					// innerOperand was already needed previously, but this generatedBy was earlier, so DONT set usedByMultipleOperands flag.
					// checkfor integrity:
					// NGPL_ASSERT(recursivelyRequiredValues.at(*index).val->id == generatedBy->id);
				} else {
					auto* usage = cat::find(instructionsPreviouselySeen, generatedBy);
					NGPL_ASSERT(usage);
					recursiveRequirements.instructions.insert(recursiveRequirements.instructions.begin() + usage->index, {generatedBy, false, int32_t(usage->usageCount)});
					recursivelyRequiredInstrsIndices[generatedBy] = recursiveRequirements.instructions.size() - 1;
				}
			}
		}

		foreach_c(generatedValue, instruction->generatesValues) {
			alreadyGeneratedValues.insert(generatedValue);
		}

		if (instruction->causesSideEffect) {
			alreadyGeneratedValues.insert(instruction->causesSideEffect);
		}
	}

	void moveValue(
			Address fromRel,
			Address toRel,
			const Position& pos,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateInstructionPtr>& intermediateCode
	) {
		NGPL_ASSERT(fromRel >= 0 and fromRel < Address(stackLayout.getStack().size()));
		NGPL_ASSERT(toRel >= -1 and toRel < Address(stackLayout.getStack().size()));
		NGPL_ASSERT(stackLayout.getValue(fromRel)->usageCount > 0);

		if (fromRel == toRel) {
				// pass; value already is where it needs to be
		} else {
			if (toRel == 0) {
				NGPL_ASSERT(stackLayout.peekValue()->usageCount == 0);
				intermediateCode.push_back(new InterInstr(Instrs::PopVal(pos)));
				stackLayout.popValue();
				fromRel -= 1; // account for changed stack;
				toRel -= 1; // account for changed stack;
			}

			if (fromRel > 0) {
				intermediateCode.push_back(new InterInstr(Instrs::ReadStackF(fromRel, pos)));
				stackLayout.readValue(fromRel);
				toRel += 1; // account for changed stack;
				// mark old position ov value as reusable, by setting its usageCount to 0,
				// without affecting the value itself:
				stackLayout.pushValue(std::nullopt, nullptr);
				stackLayout.writeValue(fromRel+2);
			}

			if (toRel == -1) {
				//
			} else if (toRel > 0) {
				NGPL_ASSERT(stackLayout.getValue(toRel)->usageCount == 0);
				intermediateCode.push_back(new InterInstr(Instrs::WriteStackF(toRel, pos)));
				stackLayout.writeValue(toRel);

			}
		}
	}

	void compressStack(
			uint32_t maxValueCount,
			const Position& pos,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateInstructionPtr>& intermediateCode
	) {
		auto& stack = stackLayout.getStack();
		NGPL_ASSERT(maxValueCount <= stack.size());

		std::vector<Address> moveLeftCounts(maxValueCount, 0);

		size_t currentMoveCnt = 0;

		const size_t firstIndex = stack.size() - maxValueCount;
		foreach_c(index, cat::IntRange(0u, maxValueCount)) {
			if (stack.at(index + firstIndex)->usageCount == 0) {
				currentMoveCnt += 1;
				moveLeftCounts.at(index) = -1;
			} else {
				moveLeftCounts.at(index) = currentMoveCnt;
			}
		}
		cat::Stack<size_t> toBeMoved;
		foreach_c(index, cat::IntRange(int32_t(maxValueCount)-1, -1, -1)) {
			NGPL_ASSERT(toBeMoved.empty());
			if (moveLeftCounts.at(index) == 0) {
				break;
			} /*else if (moveLeftCounts.at(index) == -1) {
				continue;
			}*/
			if (stackLayout.peekValue()->usageCount == 0) {
				intermediateCode.push_back(new InterInstr(Instrs::PopVal(pos)));
				stackLayout.popValue();
				continue;
			} else if (moveLeftCounts.at(index) == -1) {
				continue;
			}

			toBeMoved.push(index);
			while (stack.at( firstIndex + toBeMoved.peek() - moveLeftCounts.at(toBeMoved.peek()) )->usageCount > 0) {
				toBeMoved.push(toBeMoved.peek() - moveLeftCounts.at(toBeMoved.peek()));
			}
			while (not toBeMoved.empty()) {
				const auto fromIndex = toBeMoved.pop();
				const auto toIndex = fromIndex - moveLeftCounts.at(fromIndex);
				moveValue(
					stackLayout.getRelativeAddress(firstIndex + fromIndex),
					stackLayout.getRelativeAddress(firstIndex + toIndex),
					pos, stackLayout, intermediateCode
				);
			}
		}
	}

	void generateValue(
			TaggedValueWeakPtr value, const Position& pos,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateInstructionPtr>& intermediateCode,
			bool moveDontCopy = false
	) {
		NGPL_ASSERT2(stackLayout.getStack().empty() or stackLayout.peekValue()->usageCount > 0, "all unused values should have been removed from the stack...");
		if (value->isVirtual) {
			if (stackLayout.getAllInstructionsWithSideEffect().empty() or stackLayout.getAllInstructionsWithSideEffect().back()->id != value->id) {

				if (value->generatedBy == nullptr) {
					throw cat::Exception("Cannot find sideEffect, but also doesn't know how to generate it.");
				} else {
					generateInstructionSequenceFromGraph(value->generatedBy, stackLayout, intermediateCode);
					compressStack(value->generatedBy->generatesValues.size(), pos, stackLayout, intermediateCode);
					while (not stackLayout.getStack().empty() and stackLayout.peekValue()->usageCount <= 0) {
						NGPL_ASSERT(stackLayout.peekValue()->usageCount == 0);
						intermediateCode.push_back(new InterInstr(Instrs::PopVal(pos)));
						stackLayout.popValue();
					}


				}
			}
		} else {
			auto existingIndex = findRelativeIndexInStack(stackLayout.getStack(), value);

			if (existingIndex < 0) {
				if (value->generatedBy == nullptr) {
					throw cat::Exception("Cannot find value, but also doesn't know how to generate it.");
				} else {
					generateInstructionSequenceFromGraph(value->generatedBy, stackLayout, intermediateCode);
					compressStack(value->generatedBy->generatesValues.size(), pos, stackLayout, intermediateCode);
					while (not stackLayout.getStack().empty() and stackLayout.peekValue()->usageCount <= 0) {
						NGPL_ASSERT(stackLayout.peekValue()->usageCount == 0);
						intermediateCode.push_back(new InterInstr(Instrs::PopVal(pos)));
						stackLayout.popValue();
					}
				}
			} else if (existingIndex == 0) {
				if (moveDontCopy or value->usageCount == 1) {
					// pass; value already is where it needs to be
				} else {
					intermediateCode.push_back(new InterInstr(Instrs::Dup(0, pos)));
					stackLayout.readValue(0);
				}
			} else {
				intermediateCode.push_back(new InterInstr(Instrs::ReadStackF(existingIndex, pos)));
				stackLayout.readValue(existingIndex);
				if (moveDontCopy) {
					// mark old position ov value as reusable, by setting its usageCount to 0,
					// without affecting the value itself:
					stackLayout.pushValue(std::nullopt, nullptr);
					stackLayout.writeValue(existingIndex+2);
				}
			}
			NGPL_ASSERT(value->id == stackLayout.peekValue()->id);
		}
	}

	/*
	void preGenerateSomeValues(
			std::vector<TaggedValueWeakPtr> values,
			std::vector<size_t> orderOfGeneration,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateInstructionPtr>& intermediateCode
		) {

		std::vector<TaggedValueWeakPtr> preCalculatedValues;
		std::unordered_map<TaggedValueWeakPtr, int32_t> usedInArgsCount;
		foreach_c(index, (orderOfGeneration)) {
			auto value = values.at(index);
			if (usedInArgsCount.find(value) == usedInArgsCount.end()) {
				foreach_n(innerArg, *value->recursiveArguments) {
					auto& tmp = usedInArgsCount[innerArg.first];
					tmp += 1;
					if (tmp == 1) {
						preCalculatedValues.push_back(innerArg.first);
					} else {
						std::cout << "HELLO";
					}
				}
			}
			usedInArgsCount[value] += 1;
		}

		foreach_c(preCalcValue, preCalculatedValues) {
			if (usedInArgsCount[preCalcValue] > 1) {
				auto existingIndex = findRelativeIndexInStack(stackLayout.getStack(), preCalcValue);
				if (existingIndex < 0) {
					generateValue(preCalcValue, Position(), stackLayout, intermediateCode);
				}
			}
		}
	}
	*/

	void preGenerateValuesForInstruction(
			TaggedInstructionCWeakPtr instruction,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateInstructionPtr>& intermediateCode
		) {
		auto& stack = stackLayout.getStack();
		const auto isInStackPredicate = LAMBDA2(&, v) { return findRelativeIndexInStack(stack, v) >= 0; };
		foreach_n(recursivelyRequiredInstr, instruction->recursiveRequirements->instructions) {
			if (recursivelyRequiredInstr.usedByMultipleOperands) {
				auto& preCalcInstr = recursivelyRequiredInstr.instr;

				// auto existingIndex = findRelativeIndexInStack(stackLayout.getStack(), preCalcValue);
				// if (existingIndex < 0) {
				if (not cat::range(preCalcInstr->generatesValues).any(isInStackPredicate)) {
					generateValue(preCalcInstr->generatesValues.at(0), Position(), stackLayout, intermediateCode);
				} else {
					std::cout << "HELLO ///////////////////////////// HELLO \n";
				}
			}
		}
	}

	void generateInstructionSequenceFromGraph(
			TaggedInstructionCWeakPtr taggedInstr,
			StackLayoutScenario& stackLayout,
			std::vector<intermediate::IntermediateInstructionPtr>& intermediateCode
	) {
		/*
		 * Algorithm:
		 *  1. generate instructions for the values in return starting with the lowest lamportTimestamps, left to right
		 *
		 */


		auto& operands = taggedInstr->operands;
		const auto orderOfGeneration = getOrderOfGeneration(operands);


		/*
		preGenerateSomeValues(values, orderOfGeneration, stackLayout, intermediateCode);
		*/
		preGenerateValuesForInstruction(taggedInstr,stackLayout, intermediateCode);

		// Generate or find required values:
		bool isLastUsageStreak = true;
		Address lastIndex = -1;
		bool smartValueGenerationWasSuccessful = true;
		// smart generation:
		foreach_c(index, orderOfGeneration) {
			auto value = operands.at(index);
			NGPL_ASSERT( value->usageCount >= 0);

			if (isLastUsageStreak){
				const bool isLastUsage = value->usageCount <= 1;
				if (isLastUsage) {
					auto newIndex = findRelativeIndexInStack(stackLayout.getStack(), value);
					if (newIndex >= 0 and (lastIndex == -1 or lastIndex-1 == newIndex)) {
						lastIndex = newIndex;
					} else {
						isLastUsageStreak = false;
						if (lastIndex != 0) {
							smartValueGenerationWasSuccessful = false;
							break;
						}
					}
				} else {
					isLastUsageStreak = false;
					if (lastIndex != 0) {
						smartValueGenerationWasSuccessful = false;
						break;
					}
				}
			} else {
				generateValue(value, taggedInstr->instr.pos(), stackLayout, intermediateCode, value->usageCount <= 1);
				lastIndex = 0;
			}
		}
		smartValueGenerationWasSuccessful = smartValueGenerationWasSuccessful and lastIndex == 0;
		// if smart Generation was not successful:
		if (not smartValueGenerationWasSuccessful) {
			foreach_c(index, orderOfGeneration) {
				auto value = operands.at(index);
				NGPL_ASSERT( value->usageCount >= 0);
				generateValue(value, taggedInstr->instr.pos(), stackLayout, intermediateCode, value->usageCount <= 1);
			}
		}

		// decrement usageCount of all used operands:
		foreach_c(index, orderOfGeneration) {
			auto value = operands.at(index);
			value->usageCount -= 1;
		}



		auto instruct = taggedInstr->instr;
		if (cat::isAnyOf(instruct.id(), InstrID::READ_STCK_D, InstrID::WRITE_STCK_D)) {
			instruct = InterInstr(instruct.id(), stackLayout.getRelativeAddress(instruct.data().getValue<Address>()), instruct.pos());
		}

		intermediateCode.push_back(new InterInstr(instruct));

		foreach_c(index [[maybe_unused]], orderOfGeneration) {
			auto value = operands.at(index);
			if (not value->isVirtual) {
				stackLayout.popValue();
			}
		}

		foreach_c(generatedValue, taggedInstr->generatesValues) {
			stackLayout.pushExistingValue(generatedValue);
		}

		if (taggedInstr->causesSideEffect) {
			stackLayout.addExistingSideEffect(taggedInstr->causesSideEffect);
		}


	}


	bool generateInstructionSequenceFromReturn(
			TaggedInstructionWeakPtr taggedInstr,
			size_t returnCount,
			StackLayoutScenario& stackLayout,
			std::vector<itm::IntermediateInstructionPtr>& intermediateCode
	) {
		if (taggedInstr->operands.size() == 0) {
			return false;
		}

		auto& operands = taggedInstr->operands;
		// sort generation order of values:
		const auto orderOfGeneration = getOrderOfGeneration(operands);

		/*
		foreach_c(index, orderOfGeneration) {
			auto value = values.at(index);
			createRecursiveArgumentsForGraph(value);
		}
		*/
		{
			std::unordered_set<TaggedValueWeakPtr> alreadyGeneratedValues;
			cacheAllRecursivelyRequiredValuesForGraph(taggedInstr, alreadyGeneratedValues);
		}

		/*
		preGenerateSomeValues(values, orderOfGeneration, stackLayout, intermediateCode);
		*/
		preGenerateValuesForInstruction(taggedInstr,stackLayout, intermediateCode);

		// Generate or find required values:
		foreach_c(index, orderOfGeneration) {
			auto value = operands.at(index);

			if (not value->isVirtual) {
				auto existingIndex = findRelativeIndexInStack(stackLayout.getStack(), value);
				if (existingIndex > -1 and existingIndex == stackLayout.getRelativeAddress(index)) {
					continue; // value already is at its final place
				}
			}

			// get value that would be overwritten:
			if (not value->isVirtual and index < stackLayout.getStack().size()) {
				bool canSafelyOverride = stackLayout.getStack().at(index)->usageCount == 0;
				if (not canSafelyOverride and value->generatedBy) {
					foreach_n(recReqVal, value->generatedBy->recursiveRequirements->values) {
						if (recReqVal.val->id == stackLayout.getStack().at(index)->id) {
							canSafelyOverride = recReqVal.usageCount == stackLayout.getStack().at(index)->usageCount;
							NGPL_ASSERT(recReqVal.usageCount <= stackLayout.getStack().at(index)->usageCount);
							break;
						}
					}
				}

				if (not canSafelyOverride) { //stackLayout.peekValue()->usageCount > 0) {
					auto relativeIndex = stackLayout.getStack().size() - index - 1;
					stackLayout.readValue(relativeIndex);
					intermediateCode.push_back(new InterInstr(Instrs::ReadStackF(relativeIndex, taggedInstr->instr.pos())));
				}
			}
			// generatethe actual value:
			generateValue(value, taggedInstr->instr.pos(), stackLayout, intermediateCode, value->usageCount <= 1);

			// put it in place:
			if (not value->isVirtual) {
				NGPL_ASSERT(stackLayout.peekValue()->id == value->id);

				auto relativeIndex = stackLayout.getStack().size() - index - 1;
				if (relativeIndex > 0 ) {
					intermediateCode.push_back(new InterInstr(Instrs::WriteStackF(relativeIndex, taggedInstr->instr.pos())));
	//				stackLayout.pushValue(std::nullopt, nullptr);
	//				std::swap(*stackLayout.peekValue(), *value);
					stackLayout.writeValue(relativeIndex);
				}
			}


		}

		// clean up stack
		for (int64_t i = stackLayout.getStack().size() - (0); i --> returnCount; ) { // operands.size()-1 here because operands contain 1virtual value.
			intermediateCode.push_back(new InterInstr(Instrs::PopVal(taggedInstr->instr.pos())));
			const auto value = stackLayout.popValue();
			NGPL_ASSERT(cat::isAnyOf(value->usageCount, 0, 1));
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
	OptimizerInternal optimizer(unit);
	optimizer.optimize();
	optimizer.cleanup();
}

}
