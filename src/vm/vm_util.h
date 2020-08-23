#ifndef VM_UTIL_H
#define VM_UTIL_H

#include "../util/debug.h"

#include <inttypes.h>


namespace ngpl {

using InstructionPos = int64_t;
#define DECLARE_OPERATOR(rType, name, lhsType, rhsType) rType operator name (const lhsType& lhs, const rhsType& rhs)

#define IMPLEMENT_OPERATOR(rType, name, lhsType, rhsType, lhsCast, rhsCast) DECLARE_OPERATOR(rType, name, lhsType, rhsType) { \
	return rType(lhsCast(lhs) name rhsCast(rhs));  \
}

//struct Address {
//	explicit Address(uint64_t pos): pos(pos) {}
//	uint64_t pos;
//	explicit operator uint64_t() const { return pos; }
//};

//inline IMPLEMENT_OPERATOR(Address, + , Address, uint64_t, uint64_t, );
//inline IMPLEMENT_OPERATOR(Address, + , Address,  int64_t, uint64_t, );
//inline IMPLEMENT_OPERATOR(Address, - , Address, uint64_t, uint64_t, );
//inline IMPLEMENT_OPERATOR(Address, - , Address,  int64_t, uint64_t, );
//inline IMPLEMENT_OPERATOR(bool,    ==, Address, Address , uint64_t, uint64_t);
//inline IMPLEMENT_OPERATOR(bool,    !=, Address, Address , uint64_t, uint64_t);
//inline IMPLEMENT_OPERATOR(bool,    < , Address, Address , uint64_t, uint64_t);
//inline IMPLEMENT_OPERATOR(bool,    > , Address, Address , uint64_t, uint64_t);
//inline IMPLEMENT_OPERATOR(bool,    <=, Address, Address , uint64_t, uint64_t);
//inline IMPLEMENT_OPERATOR(bool,    >=, Address, Address , uint64_t, uint64_t);



//#define DECLARE_OPERATOR(rType, name, lhsType, rhsType) rType operator name (const lhsType& lhs, const rhsType& rhs)

//#define IMPLEMENT_OPERATOR(rType, name, lhsType, rhsType) DECLARE_OPERATOR(rType, name, lhsType, rhsType) { \
//    return lhs._p name rhs._p;  \
//}

//struct InstrPos { // InstructionPosition
//    InstrPos(uint64_t p)
//        : _p(p)
//    {}
////private:
//    uint64_t _p;

//};

//struct InstrDist { // InstructionDistance // is the result of the operation (InstructionPosition - InstructionPosition)
//    InstrDist(int64_t p)
//        : _p(p)
//    {}
////private:
//    int64_t _p;
//};


//inline IMPLEMENT_OPERATOR(InstrDist, +, InstrDist, InstrDist);
//inline IMPLEMENT_OPERATOR(InstrDist, -, InstrPos, InstrPos);
//inline DECLARE_OPERATOR(InstrPos, +, InstrPos, InstrDist) {
//    auto result = lhs._p + rhs._p;
//    NGPL_ASSERT2(lhs._p + rhs._p >= 0, "resulting absolut position must be greater than or equal to zero");
//    return result;
//}

//inline IMPLEMENT_OPERATOR(bool, ==, InstrPos, InstrPos);
//inline IMPLEMENT_OPERATOR(bool, !=, InstrPos, InstrPos);
//inline IMPLEMENT_OPERATOR(bool, <, InstrPos, InstrPos);
//inline IMPLEMENT_OPERATOR(bool, >, InstrPos, InstrPos);
//inline IMPLEMENT_OPERATOR(bool, <=, InstrPos, InstrPos);
//inline IMPLEMENT_OPERATOR(bool, >=, InstrPos, InstrPos);

//inline IMPLEMENT_OPERATOR(bool, ==, InstrDist, InstrDist);
//inline IMPLEMENT_OPERATOR(bool, !=, InstrDist, InstrDist);
//inline IMPLEMENT_OPERATOR(bool, <, InstrDist, InstrDist);
//inline IMPLEMENT_OPERATOR(bool, >, InstrDist, InstrDist);
//inline IMPLEMENT_OPERATOR(bool, <=, InstrDist, InstrDist);
//inline IMPLEMENT_OPERATOR(bool, >=, InstrDist, InstrDist);

}


#endif // VM_UTIL_H
