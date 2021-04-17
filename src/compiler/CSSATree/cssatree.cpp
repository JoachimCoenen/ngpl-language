#include "cssatree.h"

namespace ngpl::CSSA {


CSSATree::CSSATree()
{

}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const TaggedValue& v) {
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

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const TaggedInstruction& v) {
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

}
