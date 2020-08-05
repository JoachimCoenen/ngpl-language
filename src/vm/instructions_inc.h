//#ifndef INSRUCTIONS_INC_H
//#define INSRUCTIONS_INC_H

#ifndef INSTRUCTION_FACTORY0
	#define __INSTRUCTION_FACTORY0_WASNT_DEFINED__ 1
	#define INSTRUCTION_FACTORY0(instrName, stackDelta, hasSideEffect, funcName) instrName,
#endif

#ifndef INSTRUCTION_FACTORY1
	#define __INSTRUCTION_FACTORY1_WASNT_DEFINED__ 1
	#define INSTRUCTION_FACTORY1(instrName, stackDelta, hasSideEffect, funcName, argType, arg) instrName,

enum class InstructionID {
#endif
#define _STR_TYP_ const std::string&
	//                                                                           data: Type   | (result) <- (args topMost.. lastFromTop)   | Description:
	// -----------------------------------------------------------------------//--------------+--------------------------------------------+-------------------------------------------------------
	INSTRUCTION_FACTORY0(NOP,             0, false, Nop                            ) // --           | () <- ()                                   | Null operation / No operation
	INSTRUCTION_FACTORY1(CALL,            0, true,  Call,      const void*, funcPtr) // addr: Int64  | (Any|None) <- (???)                        | Call a builtin function
	//																				          +											   +
	INSTRUCTION_FACTORY1(DUP,            +1, false, Dup,             Address, addr ) // pos: Int64   | (Any, Any) <- (Any)                        | Read from a position in the stack
	INSTRUCTION_FACTORY0(SWP,             0, false, Swap                           ) // --           | (Any2, Any1) <- (Any1, Any2)               | Read from a position in the stack

	INSTRUCTION_FACTORY1(READ_STCK_F,    +1, false, ReadStackF,      Address, addr ) // pos: Int64   | (Any)      <- ()                           | Read from a position in the stack
	INSTRUCTION_FACTORY1(READ_STCK_D,     0, true,  ReadStackD,      Address, addr ) // pos: Int64   | (Any)      <- (delta: Int64)               | Read from a position in the stack
	INSTRUCTION_FACTORY1(WRITE_STCK_F,   -1, false, WriteStackF,     Address, addr ) // pos: Int64   | ()         <- (val: Any)                   | Write to a position in the stack
	INSTRUCTION_FACTORY1(WRITE_STCK_D,   -2, true,  WriteStackD,     Address, addr ) // pos: Int64   | ()         <- (delta: Int64, val: Any)     | Write to a position in the stack
	//																						  +											   +
	INSTRUCTION_FACTORY1(READ_FA,       +1, false, ReadFA,           Address, addr ) // pos: Int64   | (Any) <- ()                                | Read from fixed absolute position
	INSTRUCTION_FACTORY1(READ_FR,        0, false, ReadFR,           Address, addr ) // pos: Int64   | (Any) <- (delta: Int64)                    | Read relative from a fixed position (p = pos + delta)
	INSTRUCTION_FACTORY0(READ_DR,       -1, false, ReadDR                          ) // --           | (Any) <- (pos: Int64, delta: Int64)        | Read from dynamic relative position (p = pos + delta)
	//																						  +											   +
	INSTRUCTION_FACTORY1(WRITE_FA,      -1, true,  WriteFA,          Address, addr ) // pos: Int64   | () <- (val: Any)                           | Read from fixed absolute position
	//INSTRUCTION_FACTORY0(WRITE_DA,    -2, true,  WriteDA                         ) // --           | () <- (pos: Int64, val: Any)               | Read from dynamic absolute position
	INSTRUCTION_FACTORY1(WRITE_FR,      -2, true,  WriteFR,          Address, addr ) // pos: Int64   | () <- (delta: Int64, val: Any)             | Read from fixed relative position (p = pos + delta)
	INSTRUCTION_FACTORY0(WRITE_DR,      -3, true,  WriteDR                         ) // --           | () <- (pos: Int64, delta: Int64, val: Any) | Read from dynamic relative position (p = pos + delta)
	//																						  +											   +
	INSTRUCTION_FACTORY0(POP_VAL,       -1, false, PopVal                          ) // --           | ()       <- (val: Any)                     |
	INSTRUCTION_FACTORY1(PUSH_INT,      +1, false, PushInt,          int64_t, v    ) // val: Int64   | (Int64)  <- ()                             |
	INSTRUCTION_FACTORY1(PUSH_STR,      +1, false, PushStr,   const std::string&, v) // val: String  | (String) <- ()                             |
	//																						  +											   +
	INSTRUCTION_FACTORY0(POP_CNTR,      +1, true,  PopCntr                         ) //              |                                            |
	INSTRUCTION_FACTORY1(PUSH_CNTR_FR,   0, true,  PushCntrFR,       int64_t, v    ) //              |                                            |
	//INSTRUCTION_FACTORY0(READ_CNTR,   +1, ReadCounter                     ) //              |                                            |
	// Arithmetic instructions:---------------------------------------------------------------+--------------------------------------------+-------------------------------------------------------
	INSTRUCTION_FACTORY0(ADD_SI,        -1, false, AddSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer addition
	INSTRUCTION_FACTORY0(ADD_UI,        -1, false, AddUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer addition
	INSTRUCTION_FACTORY0(ADD_F,         -1, false, AddF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) addition

	INSTRUCTION_FACTORY0(SUB_SI,        -1, false, SubSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer substraction
	INSTRUCTION_FACTORY0(SUB_UI,        -1, false, SubUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer substraction
	INSTRUCTION_FACTORY0(SUB_F,         -1, false, SubF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) substraction

	INSTRUCTION_FACTORY0(MUL_SI,        -1, false, MulSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer multiplication
	INSTRUCTION_FACTORY0(MUL_UI,        -1, false, MulUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer multiplication
	INSTRUCTION_FACTORY0(MUL_F,         -1, false, MulF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) multiplication

	INSTRUCTION_FACTORY0(DIV_SI,        -1, false, DivSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer division
	INSTRUCTION_FACTORY0(DIV_UI,        -1, false, DivUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer division
	INSTRUCTION_FACTORY0(DIV_F,         -1, false, DivF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) division

	INSTRUCTION_FACTORY0(REM_SI,        -1, false, RemSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer reminder from division
	INSTRUCTION_FACTORY0(REM_UI,        -1, false, RemUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer reminder from division
	INSTRUCTION_FACTORY0(REM_F,         -1, false, RemF                            ) // --           | (Float) <- (rhs: Float, lhs: Float)        | Float (=double in c++) reminder from division

	INSTRUCTION_FACTORY0(NEG_SI,         0, false, NegSI                           ) // --           | (Int64) <- (rhs: Int64)                    | negate a singned integer
	INSTRUCTION_FACTORY0(NEG_F,          0, false, NegF                            ) // --           | (Float) <- (rhs: Float)                    | negate a Float (=double in c++)

	INSTRUCTION_FACTORY0(SHR_SI,        -1, false, ShrSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | singned integer shift right (arithmetic shift)
	INSTRUCTION_FACTORY0(SHR_UI,        -1, false, ShrUI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | unsingned integer shift right (logical shift)

	INSTRUCTION_FACTORY0(SHL,           -1, false, Shl                             ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | integer shift left (arithmetic/logical shift)

	INSTRUCTION_FACTORY0(AND,           -1, false, AndSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | bitwise and operation
	INSTRUCTION_FACTORY0(OR,            -1, false, OrSI                            ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | bitwise or operation
	INSTRUCTION_FACTORY0(XOR,           -1, false, XorSI                           ) // --           | (Int64) <- (rhs: Int64, lhs: Int64)        | bitwise xnd operation

	INSTRUCTION_FACTORY0(NOT  ,          0, false, Not                             ) // --           | (Int64) <- (rhs: Int64)                    | bitwise not operation (~ in c++)
	// Jumping instructions:------------------------------------------------------------------+--------------------------------------------+-------------------------------------------------------
	INSTRUCTION_FACTORY0(IF_Z,          -1, true,  IfZero                          ) // --           | () <- (condition: Bool)                    | Jump if zero, fixed Relative
	INSTRUCTION_FACTORY0(IF_NZ,         -1, true,  IfNotZero                       ) // --           | () <- (condition: Bool)                    | Jump if not zero,  fixed Relative
	//																						  +											   +
	INSTRUCTION_FACTORY1(JMP_FR,         0, true,  JumpFR,           int64_t, v    ) // jmpDst: Int64| () <- ()                                   | Jump fixed Relative
	INSTRUCTION_FACTORY1(JMP_FA,         0, true,  JumpFA,           int64_t, v    ) // jmpDst: Int64| () <- ()                                   | Jump fixed Absolute
	INSTRUCTION_FACTORY0(JMP_DA,        -1, true,  JumpDA                          ) // --           | () <- (jumpDestination: Int64)             | Jump dynamic Absolute

	// ================================================================================================================================

	INSTRUCTION_FACTORY1(CALL2,          0, false, Call,             const std::string&, funcName )

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
