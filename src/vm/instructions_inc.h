//#ifndef INSRUCTIONS_INC_H
//#define INSRUCTIONS_INC_H

#ifndef INSTRUCTION_FACTORY0
	#define __INSTRUCTION_FACTORY0_WASNT_DEFINED__ 1
	#define INSTRUCTION_FACTORY0(instrName, stackDelta, funcName) instrName,
#endif

#ifndef INSTRUCTION_FACTORY1
	#define __INSTRUCTION_FACTORY1_WASNT_DEFINED__ 1
	#define INSTRUCTION_FACTORY1(instrName, stackDelta, funcName, argType, arg) instrName,

enum class InstructionID {
#endif
#define _STR_TYP_ const std::string&
	//                                                                           data: Type   | (result) <- (args topMost.. lastFromTop)   | Description:
	// -----------------------------------------------------------------------//--------------+--------------------------------------------+-------------------------------------------------------
	INSTRUCTION_FACTORY0(NOP,            0, Nop                             ) // --           | () <- ()                                   | Null operation / No operation
	INSTRUCTION_FACTORY1(CALL,           0, Call,       const void*, funcPtr) // addr: Int64  | (Any|None) <- (???)                        | Call a builtin function
	//																				          +											   +
	INSTRUCTION_FACTORY1(DUP,            +1, Dup,             Address, addr ) // pos: Int64   | (Any, Any) <- (Any)                        | Read from a position in the stack
	INSTRUCTION_FACTORY0(SWP,             0, Swap                           ) // --           | (Any2, Any1) <- (Any1, Any2)               | Read from a position in the stack

	INSTRUCTION_FACTORY1(READ_STCK_F,    +1, ReadStackF,      Address, addr ) // pos: Int64   | (Any)      <- ()                           | Read from a position in the stack
	INSTRUCTION_FACTORY1(READ_STCK_D,     0, ReadStackD,      Address, addr ) // pos: Int64   | (Any)      <- (delta: Int64)               | Read from a position in the stack
	INSTRUCTION_FACTORY1(WRITE_STCK_F,   -1, WriteStackF,     Address, addr ) // pos: Int64   | ()         <- (val: Any)                   | Write to a position in the stack
	INSTRUCTION_FACTORY1(WRITE_STCK_D,   -2, WriteStackD,     Address, addr ) // pos: Int64   | ()         <- (delta: Int64, val: Any)     | Write to a position in the stack
	//																						  +											   +
	INSTRUCTION_FACTORY1(READ_FA,       +1, ReadFA,           Address, addr ) // pos: Int64   | (Any) <- ()                                | Read from fixed absolute position
	INSTRUCTION_FACTORY1(READ_FR,        0, ReadFR,           Address, addr ) // pos: Int64   | (Any) <- (delta: Int64)                    | Read relative from a fixed position (p = pos + delta)
	INSTRUCTION_FACTORY0(READ_DR,       -1, ReadDR                          ) // --           | (Any) <- (pos: Int64, delta: Int64)        | Read from dynamic relative position (p = pos + delta)
	//																						  +											   +
	INSTRUCTION_FACTORY1(WRITE_FA,      -1, WriteFA,          Address, addr ) // pos: Int64   | () <- (val: Any)                           | Read from fixed absolute position
	//INSTRUCTION_FACTORY0(WRITE_DA,    -2, WriteDA                         ) // --           | () <- (pos: Int64, val: Any)               | Read from dynamic absolute position
	INSTRUCTION_FACTORY1(WRITE_FR,      -2, WriteFR,          Address, addr ) // pos: Int64   | () <- (delta: Int64, val: Any)             | Read from fixed relative position (p = pos + delta)
	INSTRUCTION_FACTORY0(WRITE_DR,      -3, WriteDR                         ) // --           | () <- (pos: Int64, delta: Int64, val: Any) | Read from dynamic relative position (p = pos + delta)
	//																						  +											   +
	INSTRUCTION_FACTORY0(POP_VAL,       -1, PopVal                          ) // --           | ()       <- (val: Any)                     |
	INSTRUCTION_FACTORY1(PUSH_INT,      +1, PushInt,          int64_t, v    ) // val: Int64   | (Int64)  <- ()                             |
	INSTRUCTION_FACTORY1(PUSH_STR,      +1, PushStr,   const std::string&, v) // val: String  | (String) <- ()                             |
	//																						  +											   +
	INSTRUCTION_FACTORY0(POP_CNTR,      +1, PopCntr                         ) //              |                                            |
	INSTRUCTION_FACTORY1(PUSH_CNTR_FR,   0, PushCntrFR,       int64_t, v    ) //              |                                            |
	//INSTRUCTION_FACTORY0(READ_CNTR,   +1, ReadCounter                     ) //              |                                            |
	// Arithmetic instructions:---------------------------------------------------------------+--------------------------------------------+-------------------------------------------------------
	INSTRUCTION_FACTORY0(ADD_SI,        -1, AddSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer addition
	INSTRUCTION_FACTORY0(ADD_UI,        -1, AddUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer addition
	INSTRUCTION_FACTORY0(ADD_F,         -1, AddF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) addition

	INSTRUCTION_FACTORY0(SUB_SI,        -1, SubSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer substraction
	INSTRUCTION_FACTORY0(SUB_UI,        -1, SubUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer substraction
	INSTRUCTION_FACTORY0(SUB_F,         -1, SubF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) substraction

	INSTRUCTION_FACTORY0(MUL_SI,        -1, MulSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer multiplication
	INSTRUCTION_FACTORY0(MUL_UI,        -1, MulUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer multiplication
	INSTRUCTION_FACTORY0(MUL_F,         -1, MulF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) multiplication

	INSTRUCTION_FACTORY0(DIV_SI,        -1, DivSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer division
	INSTRUCTION_FACTORY0(DIV_UI,        -1, DivUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer division
	INSTRUCTION_FACTORY0(DIV_F,         -1, DivF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) division

	INSTRUCTION_FACTORY0(REM_SI,        -1, RemSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer reminder from division
	INSTRUCTION_FACTORY0(REM_UI,        -1, RemUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer reminder from division
	INSTRUCTION_FACTORY0(REM_F,         -1, RemF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) reminder from division

	INSTRUCTION_FACTORY0(NEG_SI,         0, NegSI                           ) // --           | (Int64) <- (rhs: Int64)                    | negate a singned integer
	INSTRUCTION_FACTORY0(NEG_F,          0, NegF                            ) // --           | (Float) <- (rhs: Float)                    | negate a Float (=double in c++)

	INSTRUCTION_FACTORY0(SHR_SI,        -1, ShrSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer shift right (arithmetic shift)
	INSTRUCTION_FACTORY0(SHR_UI,        -1, ShrUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer shift right (logical shift)

	INSTRUCTION_FACTORY0(SHL,           -1, Shl                             ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | integer shift left (arithmetic/logical shift)

	INSTRUCTION_FACTORY0(AND,           -1, AndSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | bitwise and operation
	INSTRUCTION_FACTORY0(OR,            -1, OrSI                            ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | bitwise or operation
	INSTRUCTION_FACTORY0(XOR,           -1, XorSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | bitwise xnd operation

	INSTRUCTION_FACTORY0(NOT  ,          0, Not                             ) // --           | (Int64) <- (rhs: Int64)                    | bitwise not operation (~ in c++)
	// Jumping instructions:------------------------------------------------------------------+--------------------------------------------+-------------------------------------------------------
	INSTRUCTION_FACTORY0(IF_Z,          -1, IfZero                          ) // --           | () <- (condition: Bool)                    | Jump if zero, fixed Relative
	INSTRUCTION_FACTORY0(IF_NZ,         -1, IfNotZero                       ) // --           | () <- (condition: Bool)                    | Jump if not zero,  fixed Relative
	//																						  +											   +
	INSTRUCTION_FACTORY1(JMP_FR,         0, JumpFR,           int64_t, v    ) // jmpDst: Int64| () <- ()                                   | Jump fixed Relative
	INSTRUCTION_FACTORY1(JMP_FA,         0, JumpFA,           int64_t, v    ) // jmpDst: Int64| () <- ()                                   | Jump fixed Absolute
	INSTRUCTION_FACTORY0(JMP_DA,        -1, JumpDA                          ) // --           | () <- (jumpDestination: Int64)             | Jump dynamic Absolute

	// ================================================================================================================================

	INSTRUCTION_FACTORY1(CALL2,          0, Call,             const std::string&, funcName )

#undef _STR_TYP_
#ifdef __INSTRUCTION_FACTORY0_WASNT_DEFINED__
};
	#undef __INSTRUCTION_FACTORY0_WASNT_DEFINED__
	#undef INSTRUCTION_FACTORY0
#endif

#ifdef __INSTRUCTION_FACTORY1_WASNT_DEFINED__
	#undef __INSTRUCTION_FACTORY1_WASNT_DEFINED__
	#undef INSTRUCTION_FACTORY1
#endif

//#endif // INSRUCTIONS_INC_H
