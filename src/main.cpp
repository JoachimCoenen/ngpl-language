//#include <QCoreApplication>

#include "compiler/tokenizer.h"
#include "compiler/parser.h"
#include "compiler/syntaxError.h"
#include "compiler/codeGenerator.h"
#include "compiler/linker.h"
#include "compiler/optimizer.h"
//#include "vm/evaluationContext.h"
#include "vm/instructionExecuter.h"

#include "mainHelper.h"

#include "toStringUtils.h"
#include "cat_utils.h"
#include "cat_string.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <chrono>


using TimeVar = std::chrono::high_resolution_clock::time_point;
using Duration = std::chrono::high_resolution_clock::duration;

inline auto duration(Duration a) {
	return std::chrono::duration_cast<std::chrono::nanoseconds>(a).count();
}
#define timeNow() std::chrono::high_resolution_clock::now()

template<typename F, typename... Args>
double funcTime(F func, Args&&... args){
	TimeVar t1=timeNow();
	func(std::forward<Args>(args)...);
	return duration(timeNow()-t1);
}


template<typename F, typename... Args>
inline auto funcTime2(volatile double &t, int n, F func, Args&&... args){
	//func(std::forward<Args>(args)...);
	//func(std::forward<Args>(args)...);
	TimeVar t1=timeNow();
	decltype(func(std::forward<Args>(args)...)) result;
	for (int i = 0; i < n; i++) {
	result = func(std::forward<Args>(args)...);
	}
	t = duration(timeNow()-t1) / n;
	return result;
}


struct Timer {
	TimeVar _t1;
	long long *durationNanoSecs;
	Timer(long long *_durationNanoSecs): _t1(timeNow()), durationNanoSecs(_durationNanoSecs) { }

	~Timer() {
	*durationNanoSecs = duration(timeNow() - _t1);
	}
};

using cat::range;
using cat::String;


void writeSrcStr(cat::WriterObjectABC& s, const cat::String& src) {
	s += cat::nlIndent;
	s += src;
	s += cat::nlIndent;
	s += cat::IntRange(0ull, src.length()).map( LAMBDA_s(v, char(v % 10) + '0') ).toContainer<cat::String>();
	s += cat::nlIndent;
	s += cat::IntRange(0ull, src.length()).map( LAMBDA_s(v, v % 10 == 0 ? char(v / 10) + '0' : ' ') ).toContainer<cat::String>();
}

void writeFinalAssembler(cat::WriterObjectABC& s, const std::vector<ngpl::Instruction>& instructions, const std::vector<cat::String>& lines) {

	for (size_t i = 0; i < instructions.size(); ++i) {
		cat::String iStr = std::to_string(i);
		auto paddLength = std::max<int64_t>(0, 4 - iStr.length());
		s += cat::nl;
		s += cat::String(paddLength, ' ');
		s += i;
		s += "   ";
		auto instrString = instructions[i].toString();
		s += instrString;

		paddLength = std::max<int64_t>(0, 80 - instrString.length());
		s += cat::String(paddLength, ' ');
		s += "";
		s += lines.at(instructions[i].pos().line());
	}
}


ngpl::RootPtr parseFile(const cat::String str) {

	ngpl::Parser p{ngpl::Tokenizer(str)};

	return p.parseRoot();
}

int main([[maybe_unused]] int argc, [[maybe_unused]] char* argv[])
{

	using namespace ngpl::main;
	cat::String str1 = "hello beautiful world!";
	cat::String str2 = "print(759)kkkkkkkkkkkkkkk";
	cat::OW out = cat::OW(std::cout);

	if (argc != 2) {
		out += "NGPL interpreter\n";
		out += "  usage:\n";
		out += "  ngpl <path to file>\n";
		return 0;
	}


	String unitSourcePath = String(argv[1]); // TEST_UNIT_PATH
	String unitPath = unitSourcePath.removeSuffix(NGPL_SOURCE_EXTENSION);

	cat::String src;
	{
		std::ifstream infile(unitPath + NGPL_SOURCE_EXTENSION);
		std::stringstream buffer;
		buffer << infile.rdbuf();
		src = buffer.str();
	}


	{
		bool compiledSuccessfull = false;
		ngpl::compiler::CodeGenerator ctx;
		ngpl::UnitPtr unit = nullptr;
		try {
			auto astRoot = parseFile(src);
			unit = ctx.evalUnitDeclaration(astRoot->statements[0].as<ngpl::UnitDeclaration>());
			compiledSuccessfull = true;
		} catch (ngpl::SyntaxError& e) {

			out += e.what();

			if (e.pos()) {
				auto errorLine = range(src)
					.split_c(LAMBDA(v){ return v == '\n'; })
					.dropN(e.pos()->line())
					.first_c()
					.toString();

				uint32_t columnPos;
				{
					auto substr = errorLine.substr(0, e.pos()->column());
					columnPos = range(substr).map_c(LAMBDA(c){ return c == '\t' ? 4ul : 1ul; }).join();
				}
				cat::String errorMarkerLine;
				if (columnPos >= 2) {
					errorMarkerLine = cat::String(columnPos - 2, ' ') + "~~^~~";
				} else {
					errorMarkerLine = cat::String(columnPos, ' ') + "^~~";
				}

				auto errorLineForPrint = range(errorLine)
					.split_c(LAMBDA(v){ return v == '\t'; })
					.map_c(LAMBDA(v){ return v.toString(); })
					.addSeparator_c([](){ return "    "; })
					.join();

				out += cat::nlIndent;
				out += errorLineForPrint;
				out += cat::nlIndent;
				out += errorMarkerLine;
			}
		} /*catch (const ngpl::util::debug::AssertionError& e) {
			{
				auto outFile = cat::FW(new std::ofstream(unitPath + NGPL_COMPILED_EXTENSION));
				unit->print(outFile);
			}
		}*/

		if (compiledSuccessfull) {
			//write to File:
			{
				auto outFile = cat::FW(new std::ofstream(unitPath + NGPL_COMPILED_EXTENSION));
				unit->print(outFile);
			}

//			// optimizing:
//			if (DO_OPTIMIZE_CODE) {
//				ngpl::Optimizer().optimize(unit.weak());
//			}

			//write to File:
			{
				auto outFile = cat::FW(new std::ofstream(unitPath + "_opt" + NGPL_COMPILED_EXTENSION));
				unit->print(outFile);
			}

			// linking:
			ngpl::Linker linker({unit.weak()});
			linker.generateInstructionStream();
			linker.linkFunctions();


			// print final assembler:
			auto lines = range(src)
				.split_c(LAMBDA(v){ return v == '\n'; })
				.map_c(LAMBDA(v){ return v.toString(); })
				.toVector();
			{
				auto outFile = cat::FW(new std::ofstream(unitPath + "_asm" + NGPL_COMPILED_EXTENSION));
				writeFinalAssembler(outFile, linker._instructions, lines);
			}
			writeFinalAssembler(out, linker._instructions, lines);
			out += cat::nl;
			out += cat::nl;
			out += "----------------";
			out += cat::nl;

			ngpl::InstructionExecuter executer(linker._instructions);
			long long duration = 0;

			{
				Timer t(&duration);
				executer.run();
			}

			out += cat::nl;
			out += "----------------";
			out += cat::nlIndent;
			out += executer._overallCounter;
			out += " instructions executed (total) \n";
			out += executer._funcCallCounter;
			out += " functions called (total) \n";

			std::cout << std::setw(8) << std::fixed << std::setprecision(2) <<  duration / 1e6 << " ms\n";
			std::cout << std::setw(8) << std::fixed << std::setprecision(2) <<  executer._overallCounter/(duration / 1e6) << " instr/ms";
		}
		out += cat::nlIndent;
		out += "FINISHED ";
		out += unitPath + NGPL_SOURCE_EXTENSION;

	}
	out += cat::nlIndent;

	// qDebug() << "hello beautiful world!" << endl << endl;
	// QCoreApplication a(argc, argv);

	return 0; //a.exec();
}

