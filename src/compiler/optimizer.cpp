#include "optimizer.h"
#include "CSSATree/cssatree.h"

#include "language/unit.h"
//#include "intermediate/intermediateCode.h"
#include "vm/value.h"

#include "ranges.h"
#include "cat_enumMap.h"
#include "cat_flagSet.h"

#include <iostream>
#include <optional>
#include <unordered_map>
#include <unordered_set>

//#include <vector>

namespace {
namespace itm = ngpl::intermediate;

using InstrID             = ngpl::InstructionID;
using Instrs              = ngpl::intermediate::Instructions;
using SimpleInstr         = ngpl::intermediate::IntermediateSimpleInstruction;
using SimpleInstrPtr      = ngpl::intermediate::IntermediateSimpleInstructionPtr;
using SimpleInstrWeakPtr  = ngpl::intermediate::IntermediateSimpleInstructionWeakPtr;
using SimpleInstrCWeakPtr = ngpl::intermediate::IntermediateSimpleInstructionCWeakPtr;
using namespace ngpl::CSSA;

}

namespace ngpl {
// ADD_PTR_TYPES

enum class BreakOut {
	NONE = 0,
	RETURN,
	CONTINUE,
	BREAK,
	__
};

using BreakOutSet = cat::FlagSet<BreakOut>;

using SLCSet = std::unordered_set<
	StackLayoutScenario,
	StackLayoutScenarioHash,
	StackLayoutScenarioEqualTo
>;

struct BreakOutInfo {
	cat::EnumMap<BreakOut, SLCSet> layouts;
};


//void phiSLC(SLCSet& slcs) {
//	if (slcs.size() == 0) {
//		return;
//	}
//	const auto slSize = slcs.cbegin()->getStack().size();
//	NGPL_ASSERT(cat::range(slcs).all_c(LAMBDA2(slSize, v) { return v.getStack().size() == slSize; }));

//	std::vector<TaggedValueWeakPtr> thenBranch;
//	std::vector<TaggedValueWeakPtr> elseBranch;

//	for (size_t i = 0; i < slSize; ++i) {

//		auto thenVal = thenStackLayout.getStack().at(i);
//		auto elseVal = elseStackLayout.getStack().at(i);
//		if (thenVal->id != elseVal->id) {

//			stackLayout.pushValue(std::nullopt, ifInstruct);
//			auto newVal = stackLayout.peekValue();
//			stackLayout.writeValue(stackLayout.getRelativeAddress(i));

//			thenBranch.push_back(thenVal);
//			elseBranch.push_back(elseVal);

//		}
//	}

//	ifInstruct->addBranch(std::move(thenBranch));
//	ifInstruct->addBranch(std::move(elseBranch));


//}


class Analyzer {


	bool _printStacklayout = true;
	cat::WriterObjectABCPtr s;

public:
	Analyzer(cat::WriterObjectABCPtr&& s, bool printStacklayout = true)
		: _printStacklayout(printStacklayout),
		  s(std::move(s))
	{}

	void analyzeFunc(FunctionWeakPtr func, StackLayoutScenario& stackLayout) {
		*s += cat::nlIndent;
		*s += "func ";
		*s += func->asCodeString();
		s->incIndent();
		analyzeCodeContainer(&func->body(), stackLayout);
	}

	BreakOutInfo analyzeCodeContainer(itm::IntermediateCodeContainerWeakPtr container, StackLayoutScenario& stackLayout) {
		for (size_t i = 0; i < container->instructions.size(); ++i) {
			auto& code = container->instructions[i];
			if (auto instr = code.as<SimpleInstr>()) {
				analyzeInstruction(instr, stackLayout);
			} else if (auto ifInstr = code.as<itm::IntermediateIf>()) {
				analyzeIf(ifInstr, stackLayout);
			} else if (auto loopInstr = code.as<itm::IntermediateLoop>()) {
				analyzeLoop(loopInstr, stackLayout);
			} else if (auto specialInstr = code.as<itm::IntermediateSpecial>()) {
				analyzeSpecial(specialInstr);
				container->instructions.resize(i+1);
				break;
			} else {
				throw cat::Exception("unhndeled IntermediateInstruction sub type.");
			}
		}
		return BreakOutInfo();
	}

	void analyzeInstruction(SimpleInstrWeakPtr instr, StackLayoutScenario& stackLayout) {
		// this method records the effects that a single instruction has. This is later used to build somethng
		// simiar to a Static single assignment form (https://en.wikipedia.org/wiki/Static_single_assignment_form).
		if (true and stackLayout.isValid()) {
			switch (instr->id()) {
			case InstrID::NOP: {
			} break;
			case InstrID::DUP: {
				stackLayout.readValue(0);
			} break;
			case InstrID::SWP: {
				stackLayout.swap();
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
					taggedInstr = stackLayout.addSimpleInstrWithSideEffect(new SimpleInstr(*instr), std::move(arguments));
				} else {
					taggedInstr = stackLayout.addSimpleInstr(new SimpleInstr(*instr), std::move(arguments));
				}

				for (auto i = func->returnType().fixedSize() + selfTypeSize; i --> 0;) {
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
				auto instruct = SimpleInstrPtr({}, instr->id(), stackLayout.getAbsoluteAddress(instr->data().getValue<Address>()), instr->pos());
				auto arg = stackLayout.popValue();

				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstrWithSideEffect(std::move(instruct), {arg}));
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
				auto instruct = SimpleInstrPtr({}, instr->id(), stackLayout.getAbsoluteAddress(instr->data().getValue<Address>()), instr->pos());
				auto arg1 = stackLayout.popValue();
				stackLayout.addSimpleInstrWithSideEffect(std::move(instruct), {arg1});
				//stackLayout.invalidate();
	//			stackLayout.popValue();
	//			stackLayout.retagStackValues();
			} break;

			case InstrID::READ_FA: {
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstrWithSideEffect(new SimpleInstr(*instr), {}));
			} break;
		/*	case InstrID::READ_DA: {
				auto addr = _temporaryStack.pop().getValue<int64_t>();
				_temporaryStack.push(_variables.at(addr));
				++_programmCounter;
			} break; */
			case InstrID::READ_FR: {
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstrWithSideEffect(new SimpleInstr(*instr), {arg1}));
			} break;
			case InstrID::READ_DR: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstrWithSideEffect(new SimpleInstr(*instr), {arg1, arg2}));
			} break;

			case InstrID::WRITE_FA: {
				auto arg1 = stackLayout.popValue();
				stackLayout.addSimpleInstrWithSideEffect(new SimpleInstr(*instr), {arg1});
			} break;
		//	case InstrID::WRITE_DA: {
		//		auto addr = _temporaryStack.pop().getValue<int64_t>();
		//		_variables.at(addr) = _temporaryStack.pop();
		//		++_programmCounter;
		//	} break;
			case InstrID::WRITE_FR: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.addSimpleInstrWithSideEffect(new SimpleInstr(*instr), {arg1, arg2});
			} break;
			case InstrID::WRITE_DR: {
				auto arg3 = stackLayout.popValue();
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.addSimpleInstrWithSideEffect(new SimpleInstr(*instr), {arg1, arg2, arg3});
			} break;

			case InstrID::POP_VAL: {
				stackLayout.popValue();
			} break;
			case InstrID::PUSH_INT: {
				stackLayout.pushValue(instr->data().getValue<int64_t>(), stackLayout.addSimpleInstr(new SimpleInstr(*instr), {}));
			} break;
			case InstrID::PUSH_STR: {
				stackLayout.pushValue(instr->data().getValue<cat::String>(), stackLayout.addSimpleInstr(new SimpleInstr(*instr), {}));
			} break;


			case InstrID::POP_CNTR: {
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {}));
			} break;
			case InstrID::PUSH_CNTR_FR: {
				NGPL_ASSERT(false);
				stackLayout.invalidate();
			} break;


			case InstrID::ADD_SI: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::SUB_SI: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::MUL_SI: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::DIV_SI: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::REM_SI: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::NEG_SI: {
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1}));
			} break;
			case InstrID::SHR_SI: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::SHL: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::AND: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::OR: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::XOR: {
				auto arg2 = stackLayout.popValue();
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1, arg2}));
			} break;
			case InstrID::NOT: {
				auto arg1 = stackLayout.popValue();
				stackLayout.pushValue(std::nullopt, stackLayout.addSimpleInstr(new SimpleInstr(*instr), {arg1}));
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

		if (_printStacklayout) {
			*s += cat::nlIndent;
			//s << std::setfill(' ') << std::setw(3);

			auto indentSize = 2 * s->getIndent();
			auto idStr = cat::String(cat::SW() << instr->id());

			*s << idStr;
			*s << cat::String(std::max(1ll, 30 - int64_t(idStr.length() + indentSize)), ' ');
			printStackLayout(s, stackLayout);
		}
	}

	void printStackLayout(cat::WriterObjectABCPtr& s, const StackLayoutScenario& stackLayout) {
			*s << " |";

			if (not stackLayout.isValid()) {
				*s << " invalid |";
			}

			foreach_c(value, stackLayout.getStack()) {
				if (value->isAtFinalLocation) {
					*s << "-" << cat::intToStr(value->id , 3, ' ') << "-|";
				} else  {
					*s << " " << cat::intToStr(value->id , 3, ' ') << " |";
				}
			}
		}

	BreakOutInfo analyzeIf(itm::IntermediateIfWeakPtr ifInstr, StackLayoutScenario& stackLayout) {
		auto condition = stackLayout.popValue();
		auto ifInstruct = stackLayout.addIfInstr(new SimpleInstr(Instrs::Nop(ifInstr->pos())), {condition});

		if (_printStacklayout) {
			*s += cat::nlIndent;
			*s += ifInstr->isInverted ? "if not:" : "if:";
			s->incIndent();
		}

		auto thenStackLayout = stackLayout.newScenarioFromThis();
		auto thenBOI = analyzeCodeContainer(&ifInstr->ifCode, thenStackLayout);

		if (_printStacklayout) {
			s->decIndent();
			if (not ifInstr->elseCode.instructions.empty()) {
				*s += cat::nlIndent;
				*s += "else:";
			}
			s->incIndent();
		}

		auto elseStackLayout = stackLayout.newScenarioFromThis();
		auto elseBOI = analyzeCodeContainer(&ifInstr->elseCode, elseStackLayout);

		if (_printStacklayout) {
			s->decIndent();
		}

		if (not thenStackLayout.isValid() or not elseStackLayout.isValid()){
			stackLayout.invalidate();
			return BreakOutInfo();
		}

		// thenStackLayout and elseStackLayout should have the same size:
		NGPL_ASSERT(thenStackLayout.getStack().size() == elseStackLayout.getStack().size());
		stackLayout = thenStackLayout.newScenarioFromThis();

		std::vector<TaggedValueWeakPtr> thenBranch;
		std::vector<TaggedValueWeakPtr> elseBranch;

		for (size_t i = 0; i < thenStackLayout.getStack().size(); ++i) {
			auto thenVal = thenStackLayout.getStack().at(i);
			auto elseVal = elseStackLayout.getStack().at(i);
			if (thenVal->id != elseVal->id) {

				stackLayout.pushValue(std::nullopt, ifInstruct);
				auto newVal = stackLayout.peekValue();
				stackLayout.writeValue(stackLayout.getRelativeAddress(i));

				thenBranch.push_back(thenVal);
				elseBranch.push_back(elseVal);

			}
		}

		ifInstruct->addBranch(std::move(thenBranch));
		ifInstruct->addBranch(std::move(elseBranch));


		// thenStackLayout and elseStackLayout should be the same
		if (not thenStackLayout.isValid()){
			stackLayout = std::move(thenStackLayout);
		} else if (not elseStackLayout.isValid()){
			stackLayout = std::move(elseStackLayout);
		} else {
			// TODO: merge both stackLayouts...!
			stackLayout = std::move(elseStackLayout);
			stackLayout.invalidate();
		}
		return BreakOutInfo();
	}

	BreakOutInfo analyzeLoop(itm::IntermediateLoopWeakPtr loopInstr, StackLayoutScenario& stackLayout) {
		if (_printStacklayout) {
			*s += cat::nlIndent;
			*s += "loop:";
			s->incIndent();
		}
		auto bos = analyzeCodeContainer(&loopInstr->code, stackLayout);
		if (1) {}

		if (_printStacklayout) {
			s->decIndent();
		}
		return bos;
	}

	BreakOutSet analyzeSpecial(itm::IntermediateSpecialWeakPtr specialInstr) {
		if (_printStacklayout) {
			specialInstr->print(*s);
		}
		switch (specialInstr->id) {
		case itm::IntermediateSpecialId::RETURN:
			return BreakOutSet(BreakOut::RETURN);
		case itm::IntermediateSpecialId::CONTINUE:
			return BreakOutSet(BreakOut::CONTINUE);
		case itm::IntermediateSpecialId::BREAK:
			return BreakOutSet(BreakOut::BREAK);
		}
		return BreakOutSet();
	}
};

/*
class OptimizerInternal
{

public:
	OptimizerInternal(const UnitWeakPtr& unit)
		: _unit(unit)
	{}

	void optimize() {
		optimizeScope(_unit->scope());
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		Analyzer(new cat::OW(std::cout)).analyzeCodeContainer(&_unit->body(), stackLayout);
	}

	void optimizeScope(ScopeWeakPtr scope) {
		// optimize each function:
		// (they are stored as a Dict[funcName, Dict[signature, function]])
		foreach_v(func, cat::range(scope->getFunctions())
				  .flatmap(LAMBDA_n(funcsPair) { return cat::range(funcsPair.second); })
				  .map(LAMBDA_n(funcPair) { return funcPair.second.weak(); })
		) {
			//_functionEntryPoints[func->asQualifiedCodeString()] = _instructions.size();
			optimizeFunc(func);
		}

		// optimize each type:
		foreach_v(type, cat::range(scope->getTypes())
				  .map_c(LAMBDA_n(typePair) { return typePair.second.weak(); })
		) {
			//_functionEntryPoints[type->asQualifiedCodeString()] = _instructions.size();
			optimizeType(type);
		}

	}


	// ================================================================================================================

	void optimizeType(TypeWeakPtr type) {
		optimizeScope(type->scope());
		ValueStore valueStore;
		StackLayoutScenario stackLayout(&valueStore);
		Analyzer(new cat::OW(std::cout)).analyzeCodeContainer(&type->body(), stackLayout);
	}


	StackLayoutScenario _getStackLayoutForFunction(FunctionWeakPtr func, ValueStore& valueStore) {
		StackLayoutScenario stackLayout(&valueStore);
		if (func->selfType()) {
			//function is a method
			for (auto i = func->selfType()->fixedSize(); i-->0;) {
				stackLayout.pushValue(std::nullopt, nullptr);
			}
		}
		for (auto i = func->argumentsStackSize(); i-->0;) {
			stackLayout.pushValue(std::nullopt, nullptr);
		}
		return stackLayout;
	}

	void optimizeFunc(FunctionWeakPtr func) {
		ValueStore valueStore;
		StackLayoutScenario initialStackLayout = _getStackLayoutForFunction(func, valueStore);
		StackLayoutScenario stackLayout1 = initialStackLayout.newScenarioFromThis();

		func->recalculteSideEffects();
		std::cout << "\n ================================================================\n";
		Analyzer(new cat::OW(std::cout)).analyzeFunc(func, stackLayout1);


		if (stackLayout1.isValid()) {
			auto returnArgs = cat::IntRange(0ull, stackLayout1.getStack().size()).map_c(LAMBDA2(&stackLayout1, i){ return stackLayout1.getStack()[i]; }).toVector();
//			if (not stackLayout.getAllInstructionsWithSideEffect().empty()) {
//				args.push_back(stackLayout.getAllInstructionsWithSideEffect().back());
//			}

			auto returnCount = returnArgs.size();
			IntermediateCode intermediateCode;
			TaggedInstructionWeakPtr instr = stackLayout1.addSimpleInstrWithSideEffect(
				new SimpleInstr(Instrs::Nop(func->body().instructions[0]->pos())), std::move(returnArgs)
			);

			if (generateInstructionSequenceFromReturn(instr, returnCount, initialStackLayout, intermediateCode)) {
				func->body().instructions = std::move(intermediateCode.vector);
			}

			std::cout << "\n ----------------\n";
			ValueStore valueStore2;
			StackLayoutScenario stackLayout2 = _getStackLayoutForFunction(func, valueStore2);
			Analyzer(new cat::OW(std::cout)).analyzeFunc(func, stackLayout2);
			std::cout << "\n ================================================================\n";

		}
	}

	// ================================================================================================================


	// ================================================================================================================
	// ================================================================================================================
	// ================================================================================================================

	void cleanup() {
		cleanupScope(_unit->scope());
		cleanupCodeContainer(&_unit->body());
	}

	void cleanupScope(ScopeWeakPtr scope) {
		foreach_v(func, cat::range(scope->getFunctions())
				  .flatmap(LAMBDA_n(funcsPair) { return cat::range(funcsPair.second); })
				  .map(LAMBDA_n(funcPair) { return funcPair.second.weak(); })
		) {
			cleanupFunc(func);
		}

		foreach_v(type, cat::range(scope->getTypes())
				  .map_c(LAMBDA_n(typePair) { return typePair.second.weak(); })
		) {
			cleanupType(type);
		}
	}

	void cleanupCodeContainer(itm::IntermediateCodeContainerWeakPtr container) {
		for (size_t i = 0; i < container->instructions.size(); ++i) {
			auto& code = container->instructions[i];
			itm::IntermediateInstructionPtr replacementInstruction = nullptr;
			if (auto instr = code.as<SimpleInstr>()) {
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
		cleanupScope(type->scope());
		ValueStore valueStore;
		cleanupCodeContainer(&type->body());
	}

	void cleanupFunc(FunctionWeakPtr func) {
		cleanupCodeContainer(&func->body());
		func->recalculteSideEffects();

	}

	// ================================================================================================================

	SimpleInstrPtr cleanupInstruction(SimpleInstrWeakPtr instr) {
		auto& data = instr->data();
		switch (instr->id()) {
		case InstrID::NOP: {} break;
		case InstrID::DUP: {} break;
		case InstrID::SWP: {} break;

		case InstrID::CALL: {} break;
		case InstrID::CALL2: {} break;

		case InstrID::READ_STCK_F: {
			if (data.getValue<Address>() == 0) {
				return new SimpleInstr(Instrs::Dup(0, instr->pos()));
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
* /
		default:
			throw cat::Exception(cat::SW() << "unknown Instruction ID. " << *instr);
		}
		return nullptr;
	}

	itm::IntermediateInstructionPtr cleanupIf(itm::IntermediateIfWeakPtr ifInstr) {
		cleanupCodeContainer(&ifInstr->ifCode);
		cleanupCodeContainer(&ifInstr->elseCode);

		if (ifInstr->ifCode.isEmpty()) {
			if (ifInstr->elseCode.isEmpty()) {
				// this 'if' is completely useless, so remove it:

				itm::IntermediateInstructionWeakPtr replacementInstruction = ifInstr;
				return new SimpleInstr(Instrs::Nop(ifInstr->pos()));
			} else {
				// elseCode exists but if Code doesn't, so swap them:
				std::swap(ifInstr->ifCode, ifInstr->elseCode);
				ifInstr->isInverted = not ifInstr->isInverted;
			}
		}
		return nullptr;
	}

	itm::IntermediateInstructionPtr cleanupLoop(itm::IntermediateLoopWeakPtr loopInstr) {
		cleanupCodeContainer(&loopInstr->code);
		return nullptr;
	}

	itm::IntermediateInstructionPtr cleanupSpecial(itm::IntermediateSpecialWeakPtr specialInstr) {
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
	 * /
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

	std::vector<TaggedValueWeakPtr> getOrderedOperands(const std::vector<TaggedValueWeakPtr>& operands) {
		const auto orderOfGeneration = getOrderOfGeneration(operands);

		std::vector<TaggedValueWeakPtr> orderedOperands;
		foreach_c(i, orderOfGeneration) {
			orderedOperands.push_back(operands[i]);
		}
		return orderedOperands;
		/*
		return cat::range(orderOfGeneration)
				.map(LAMBDA2(&, i) { return operands[i]; })
				.toVector();
		* /
	}





	/**
	 * @brief cacheAllRecursivelyRequiredValuesForGraph
	 * @param instruction
	 * @param alreadyGeneratedValues
	 * walks the DAG (Directed Acyclic Graph) in execution order and determines all values that are needed to calulate this 'subTree'[see: devDocs/dataFlowOptimization.drawio].
	 * already generated values are considered.
	 * /
	void cacheAllRecursivelyRequiredValuesForGraph(TaggedInstructionWeakPtr instruction, std::unordered_set<TaggedValueWeakPtr>& alreadyGeneratedValues) {
		struct OperandsPreviouselySeenUsage{ uint32_t index; int32_t usageCount; };


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
			if (operand->id == 18) {
				cat::OW(std::cout) << "good\n";
			}
			if (cat::contains(operandsPreviouselySeen, operand)) {
				continue;
			}
			if (cat::contains(instructionsPreviouselySeen, operand->generatedBy)) {
				NGPL_ASSERT(operand->generatedBy != nullptr);
				NGPL_ASSERT(cat::contains(alreadyGeneratedValues, operand));
				//continue;
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
				// ?? alreadyGeneratedValues.insert(operand);
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

		// insert all operands in recursivelyRequiredValues:
		foreach_n(operand, cat::reversed(orderedOperands)) {
			// Value:
			if (auto* index [[maybe_unused]] = cat::find(recursivelyRequiredValuesIndices, operand)) {
				// innerOperand was already needed previously, but this operand is earlier
				// (we're iteraing backwards), so DO NOT set usedByMultipleOperands flag.

				// checkfor integrity:
				// NGPL_ASSERT(recursivelyRequiredValues.at(*index).val->id == operand->id);
			} else {
				auto* usage = cat::find(operandsPreviouselySeen, operand);
				if (usage == nullptr) {
					cat::OW(std::cout) << "good :(\n";
				}
				NGPL_ASSERT(usage);
				// The recursive requirement's usage count cannot be larger than the
				// total usage count of its value (aka. operand, here) (d'oh!).
				NGPL_ASSERT(not (usage->usageCount > operand->usageCount));
				recursiveRequirements.values.insert(
						recursiveRequirements.values.begin() + usage->index,
						{operand, false, int32_t(usage->usageCount)}
				);
				recursivelyRequiredValuesIndices[operand] = recursiveRequirements.values.size() - 1;
			}
			// Instruction:
			if (auto generatedBy = operand->generatedBy) {
				if (auto* index [[maybe_unused]] = cat::find(recursivelyRequiredInstrsIndices, generatedBy)) {
					// generatedBy was already needed previously, but this generatedBy is earlier
					// (we're iteraing backwards), so DO NOT set usedByMultipleOperands flag.
					recursiveRequirements.instructions.at(*index).usedByMultipleOperands = true;
					// checkfor integrity:
					// NGPL_ASSERT(recursivelyRequiredValues.at(*index).val->id == generatedBy->id);
				} else {
					auto* usage = cat::find(instructionsPreviouselySeen, generatedBy);
					NGPL_ASSERT(usage);
					recursiveRequirements.instructions.insert(
							recursiveRequirements.instructions.begin() + usage->index,
							{generatedBy, false, int32_t(usage->usageCount)}
					);
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
			IntermediateCode& intermediateCode
	) {
		NGPL_ASSERT(fromRel >= 0 and fromRel < Address(stackLayout.getStack().size()));
		NGPL_ASSERT(toRel >= -1 and toRel < Address(stackLayout.getStack().size()));
		NGPL_ASSERT(stackLayout.getValue(fromRel)->usageCount > 0);

		if (fromRel == toRel) {
				// pass; value already is where it needs to be
		} else {
			if (toRel == 0) {
				NGPL_ASSERT(stackLayout.peekValue()->usageCount == 0);
				intermediateCode.push_back(new SimpleInstr(Instrs::PopVal(pos)));
				stackLayout.popValue();
				fromRel -= 1; // account for changed stack;
				toRel -= 1; // account for changed stack;
			}

			if (fromRel > 0) {
				intermediateCode.push_back(new SimpleInstr(Instrs::ReadStackF(fromRel, pos)));
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
				intermediateCode.push_back(new SimpleInstr(Instrs::WriteStackF(toRel, pos)));
				stackLayout.writeValue(toRel);

			}
		}
	}

	void popUnusedValues(
			const Position& pos,
			StackLayoutScenario& stackLayout,
			IntermediateCode& intermediateCode
	) {
		while (not stackLayout.getStack().empty() and stackLayout.peekValue()->usageCount <= 0) {
			NGPL_ASSERT(stackLayout.peekValue()->usageCount == 0);
			intermediateCode.push_back(new SimpleInstr(Instrs::PopVal(pos)));
			stackLayout.popValue();
		}
	}

	void compressStack(
			uint32_t maxValueCount,
			const Position& pos,
			StackLayoutScenario& stackLayout,
			IntermediateCode& intermediateCode
	) {
		auto& stack = stackLayout.getStack();
		NGPL_ASSERT(stackLayout.stackCompressionBarrier() <= stack.size());
		maxValueCount = std::min(maxValueCount, (uint32_t)stack.size() - stackLayout.stackCompressionBarrier());
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
			} /
			if (stackLayout.peekValue()->usageCount == 0) {
				intermediateCode.push_back(new SimpleInstr(Instrs::PopVal(pos)));
				stackLayout.popValue();
				continue;
			} else if (moveLeftCounts.at(index) == -1) {
				continue;
			}

			toBeMoved.push(index);
			while (true) {
				const auto toBeMovedPeek = toBeMoved.peek();
				const auto moveLeftCountsAtToBeMovedPeek = moveLeftCounts.at(toBeMovedPeek);
				const auto stackIndex = firstIndex + toBeMovedPeek - moveLeftCountsAtToBeMovedPeek;
				if (stack.at( stackIndex )->usageCount > 0) {
					toBeMoved.push(toBeMoved.peek() - moveLeftCounts.at(toBeMoved.peek()));
				} else {
					break;
				}
			}
			while (not toBeMoved.empty()) {
				const auto fromIndex = toBeMoved.pop();
				const auto toIndex = fromIndex - moveLeftCounts.at(fromIndex);
				moveValue(
					stackLayout.getRelativeAddress(firstIndex + fromIndex),
					stackLayout.getRelativeAddress(firstIndex + toIndex),
					pos, stackLayout, intermediateCode
				);
				moveLeftCounts.at(toIndex) = 0; // This value is at its final positin and doesn't need to be moved anymore.
			}
		}
	}

	void generateValue(
			TaggedValueWeakPtr value, const Position& pos,
			StackLayoutScenario& stackLayout,
			IntermediateCode& intermediateCode,
			bool moveDontCopy = false
	) {
		NGPL_ASSERT2(stackLayout.getStack().empty() or stackLayout.peekValue()->usageCount > 0, "all unused values should have been removed from the stack...");
		if (value->isVirtual) {
			if (not stackLayout.sideEffectHasHappened(value)) {
				if (value->generatedBy == nullptr) {
					throw cat::Exception("Cannot find sideEffect, but also doesn't know how to generate it.");
				} else {
					generateInstructionSequenceFromGraph(value->generatedBy, stackLayout, intermediateCode);
					compressStack(value->generatedBy->generatesValues.size()+1000, pos, stackLayout, intermediateCode);
					popUnusedValues(pos, stackLayout, intermediateCode);
				}
			}
		} else {
			if (value->id == 23) {
				cat::OW(std::cout) << "good :(\n";
			}
			auto existingIndex = findRelativeIndexInStack(stackLayout.getStack(), value);

			if (existingIndex < 0) {
				if (value->generatedBy == nullptr) {
					throw cat::Exception("Cannot find value, but also doesn't know how to generate it.");
				} else {
					generateInstructionSequenceFromGraph(value->generatedBy, stackLayout, intermediateCode);
					compressStack(value->generatedBy->generatesValues.size()+1000, pos, stackLayout, intermediateCode);
					popUnusedValues(pos, stackLayout, intermediateCode);
				}
			} else if (existingIndex == 0) {
				if (moveDontCopy or value->usageCount == 1) {
					// pass; value already is where it needs to be
				} else {
					intermediateCode.push_back(new SimpleInstr(Instrs::Dup(0, pos)));
					stackLayout.readValue(0);
				}
			}  else if (existingIndex == 1) {
				if (moveDontCopy or value->usageCount == 1) {
					intermediateCode.push_back(new SimpleInstr(Instrs::Swap(pos)));
					stackLayout.swap();
				} else {
					intermediateCode.push_back(new SimpleInstr(Instrs::ReadStackF(1, pos)));
					stackLayout.readValue(1);
				}
			} else {
				intermediateCode.push_back(new SimpleInstr(Instrs::ReadStackF(existingIndex, pos)));
				stackLayout.readValue(existingIndex);
				if (moveDontCopy) {
					// mark old position of value as reusable, by setting its usageCount to 0,
					// without affecting the value itself:
					stackLayout.pushValue(std::nullopt, nullptr);
					stackLayout.writeValue(existingIndex+2);
				}
			}
			if (value->id != stackLayout.peekValue()->id) {
				cat::OW(std::cout) << "bad :)\n";
			}
			// NGPL_ASSERT(value->id == stackLayout.peekValue()->id);
		}
	}

	/*
	void preGenerateSomeValues(
			std::vector<TaggedValueWeakPtr> values,
			std::vector<size_t> orderOfGeneration,
			StackLayoutScenario& stackLayout,
			IntermediateCode& intermediateCode
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
	* /

	void preGenerateValuesForInstruction(
			TaggedInstructionCWeakPtr instruction,
			StackLayoutScenario& stackLayout,
			IntermediateCode& intermediateCode
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

	/**
	 * @brief prepareValuesForInstruction
	 * @param taggedInstr
	 * @param stackLayout
	 * @param intermediateCode
	 * @return std::vector<TaggedValueWeakPtr> orderedOperands
	 * /
	std::vector<TaggedValueWeakPtr> prepareValuesForInstruction(
			TaggedInstructionCWeakPtr taggedInstr,
			StackLayoutScenario& stackLayout,
			IntermediateCode& intermediateCode
	){
		/*
		 * Algorithm:
		 *  1. generate instructions for the values in return starting with the lowest lamportTimestamps, left to right
		 *
		 * /
		const auto& operands = taggedInstr->operands;
		auto orderedOperands = getOrderedOperands(operands);

		/*
		preGenerateSomeValues(values, orderOfGeneration, stackLayout, intermediateCode);
		* /
		preGenerateValuesForInstruction(taggedInstr, stackLayout, intermediateCode);

		// Generate or find required values:
		bool isLastUsageStreak = true;
		Address lastIndex = -1;
		bool smartValueGenerationWasSuccessful = true;
		// smart generation:
		foreach_c(value, orderedOperands) {
			NGPL_ASSERT( value->usageCount >= 0);

			if (value->isVirtual) {
				// we analyze the virtuals later.
				continue;
			}

			const bool isLastUsage = value->usageCount <= 1;
			if (isLastUsageStreak){
				if (isLastUsage) {
					auto newIndex = findRelativeIndexInStack(stackLayout.getStack(), value);
					if (newIndex >= 0 and (lastIndex == -1 or lastIndex-1 == newIndex)) {
						lastIndex = newIndex;
					} else {
						isLastUsageStreak = false;
						if (lastIndex != 0) {
							smartValueGenerationWasSuccessful = false;
							//break;
						}
					}
				} else {
					isLastUsageStreak = false;
					if (lastIndex != 0) {
						smartValueGenerationWasSuccessful = false;
						//break;
					}
				}
			} else {
//				if (not isLastUsage) {
//					generateValue(value, taggedInstr->instr->pos(), stackLayout, intermediateCode, value->usageCount <= 1);
//					NGPL_ASSERT(value->isVirtual or value->id == stackLayout.peekValue()->id);
//					lastIndex = 0;
//				}
			}
		}
		smartValueGenerationWasSuccessful = smartValueGenerationWasSuccessful and lastIndex == 0;
		if (smartValueGenerationWasSuccessful) {
			foreach_c(value, orderedOperands) {
				NGPL_ASSERT( value->usageCount >= 0);
				if (not value->isVirtual) {
					// we just analyzed the non-virtuals.
					continue;
				}
				generateValue(value, taggedInstr->instr->pos(), stackLayout, intermediateCode, value->usageCount <= 1);
			}
		}

		// if smart Generation was not successful:
		if (not smartValueGenerationWasSuccessful) {
			foreach_c(value, orderedOperands) {
				NGPL_ASSERT( value->usageCount >= 0);
				generateValue(value, taggedInstr->instr->pos(), stackLayout, intermediateCode, value->usageCount <= 1);
				NGPL_ASSERT(value->isVirtual or value->id == stackLayout.peekValue()->id);
			}
		}

		// decrement usageCount of all used operands:
		foreach_n(value, orderedOperands) {
			value->usageCount -= 1;
		}

		return orderedOperands;
	}


	void generateInstructionSequenceFromGraph(
			TaggedInstructionCWeakPtr taggedInstr,
			StackLayoutScenario& stackLayout,
			IntermediateCode& intermediateCode
	) {
		const auto orderedOperands = prepareValuesForInstruction(taggedInstr, stackLayout, intermediateCode);

		auto instruct = (taggedInstr->instr.weak());
		if (cat::isAnyOf(instruct->id(), InstrID::READ_STCK_D, InstrID::WRITE_STCK_D)) {
			instruct = SimpleInstr(instruct.id(), stackLayout.getRelativeAddress(instruct.data().getValue<Address>()), instruct.pos());
		}

		intermediateCode.push_back(new SimpleInstr(instruct));

		foreach_c(value, orderedOperands) {
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
			IntermediateCode& intermediateCode
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
		* /
		{
			std::unordered_set<TaggedValueWeakPtr> alreadyGeneratedValues;
			cacheAllRecursivelyRequiredValuesForGraph(taggedInstr, alreadyGeneratedValues);
		}

		// all following operations expect a clean stack:
		popUnusedValues(taggedInstr->instr->pos(), stackLayout, intermediateCode);
		/*
		preGenerateSomeValues(values, orderOfGeneration, stackLayout, intermediateCode);
		* /
		preGenerateValuesForInstruction(taggedInstr, stackLayout, intermediateCode);

		const auto& stack = stackLayout.getStack();
		// Generate or find required values:
		foreach_c(index, orderOfGeneration) {
			// value should be at position index in the stack, when functin returns.
			const auto dstinationInStack = index;
			auto value = operands.at(index);

			if (not value->isVirtual) {
				auto existingIndex = findRelativeIndexInStack(stack, value);
				if (existingIndex > -1 and existingIndex == stackLayout.getRelativeAddress(index)) {
					continue; // Value is already in its final place
				}

				// get value that would be overwritten:
				if (dstinationInStack < stack.size()) {
					// is the
					const auto& valueAtDestination = stack.at(dstinationInStack);
					bool canSafelyOverride = valueAtDestination->usageCount == 0;

					if (not canSafelyOverride and value->generatedBy) {
						// maybe the valueAtDestination can be removed after this value is generated?
						foreach_n(recReqVal, value->generatedBy->recursiveRequirements->values) {
							if (recReqVal.val->id == valueAtDestination->id) {
								// The valueAtDestination is the recReqVal. So:
								// If the recursive requirement's usage count is the total usage count
								// of its value, then it's safe to overwrite it.
								canSafelyOverride = recReqVal.usageCount == recReqVal.val->usageCount;

								// The recursive requirement's usage count cannot be larger than the
								// total usage count of its value (d'oh!).
								NGPL_ASSERT(not (recReqVal.usageCount > recReqVal.val->usageCount));
								break;
							}
						}
					}

					if (not canSafelyOverride) { //stackLayout.peekValue()->usageCount > 0) {
						// TODO: maybe move this to after generateValue(...)? (using a SWAP instruction)
						auto relativeIndex = stack.size() - index - 1;
						stackLayout.readValue(relativeIndex);
						intermediateCode.push_back(new SimpleInstr(Instrs::ReadStackF(relativeIndex, taggedInstr->instr->pos())));
					}
				}
			}

			if (not value->isVirtual) {
				// set compressionBarrier:
				stackLayout.setStackCompressionBarrier(std::max(stackLayout.stackCompressionBarrier(), (uint32_t)index+1));
			}
			// generatethe actual value:
			generateValue(value, taggedInstr->instr->pos(), stackLayout, intermediateCode, value->usageCount <= 1);
			NGPL_ASSERT(value->isVirtual or value->id == stackLayout.peekValue()->id);

			// put it in place:
			if (not value->isVirtual) {
				NGPL_ASSERT(stackLayout.peekValue()->id == value->id);

				auto relativeIndex = stack.size() - index - 1;
				if (relativeIndex > 0 ) {
					intermediateCode.push_back(new SimpleInstr(Instrs::WriteStackF(relativeIndex, taggedInstr->instr->pos())));
					stackLayout.writeValue(relativeIndex);
	//				stackLayout.pushValue(std::nullopt, nullptr);
	//				std::swap(*stackLayout.peekValue(), *value);
				}
			}


		}

		// clean up stack
		for (size_t i = stack.size() - (0); i --> returnCount; ) { // operands.size()-1 here because operands contain 1virtual value.
			intermediateCode.push_back(new SimpleInstr(Instrs::PopVal(taggedInstr->instr->pos())));
			const auto value = stackLayout.popValue();
			NGPL_ASSERT(cat::isAnyOf(value->usageCount, 0, 1));
		}
		intermediateCode.push_back(new itm::IntermediateSpecial(itm::IntermediateSpecialId::RETURN, taggedInstr->instr->pos()));

		return true;
	}


	UnitWeakPtr _unit;
};
*/



// ================================================================================================================
// ================================================================================================================
// ================================================================================================================


void Optimizer::optimize(const UnitWeakPtr& unit)
{
//	OptimizerInternal optimizer(unit);
//	optimizer.optimize();
//	optimizer.cleanup();
}

}
