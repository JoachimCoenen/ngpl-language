//#include <QCoreApplication>

#include "compiler/tokenizer.h"
#include "compiler/parser.h"
#include "compiler/syntaxError.h"
#include "compiler/codeGenerator.h"
#include "compiler/linker.h"
//#include "vm/evaluationContext.h"
#include "vm/instructionExecuter.h"

#include "toStringUtils.h"
#include "cat_utils.h"

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

void writeSrcStr(cat::WriterObjectABC& s, const std::string& src) {
	s += cat::nlIndent;
	s += src;
	s += cat::nlIndent;
	s += cat::IntRange(0ull, src.length()-1).map( LAMBDA_s(v, char(v % 10) + '0') ).toContainer<std::string>();
	s += cat::nlIndent;
	s += cat::IntRange(0ull, src.length()-1).map( LAMBDA_s(v, v % 10 == 0 ? char(v / 10) + '0' : ' ') ).toContainer<std::string>();
}


ngpl::RootPtr parseFile(const std::string str) {

	ngpl::Parser p{ngpl::Tokenizer(str)};

	return p.parseRoot();
}

static const std::string NGPL_SOURCE_EXTENSION = ".ngpl";
static const std::string NGPL_COMPILED_EXTENSION = ".ngcu";
static const std::string TEST_UNITS_DIR = "C:/Users/Joachim Coenen/Documents/Privat/Programieren/NGPL_lang/testUnits/";
static const std::string TEST_UNIT1_PATH = TEST_UNITS_DIR + "testUnit1";

int main([[maybe_unused]] int argc, [[maybe_unused]] char* argv[])
{

	std::string str1 = "hello beautiful world!";
	std::string str2 = "print(759)kkkkkkkkkkkkkkk";
	cat::OW out = cat::OW(std::cout);


	std::string src;
	{
		std::ifstream infile(TEST_UNIT1_PATH + NGPL_SOURCE_EXTENSION);
		std::stringstream buffer;
		buffer << infile.rdbuf();
		src = buffer.str();
	}


	{
		bool compiledSuccessfull = false;
		ngpl::CodeGenerator ctx;
		ngpl::UnitPtr unit = nullptr;
		try {
			auto astRoot = parseFile(src);
			unit = ctx.evalUnitDeclaration(astRoot.getRaw()->statements[0].as<ngpl::UnitDeclaration>());
			compiledSuccessfull = true;
		} catch (ngpl::SyntaxError e) {

			auto errorLine = range(src)
				.split_c(LAMBDA(v){ return v == '\n'; })
				.dropN(e.pos().line())
				.first_c()
				.toString();

			uint32_t columnPos;
			{
				auto substr = errorLine.substr(0, e.pos().column());
				columnPos = range(substr).map_c(LAMBDA(c){ return c == '\t' ? 4ul : 1ul; }).join();
			}
			std::string errorMarkerLine;
			if (columnPos >= 2) {
				errorMarkerLine = std::string(columnPos - 2, ' ') + "~~^~~";
			} else {
				errorMarkerLine = std::string(columnPos, ' ') + "^~~";
			}

			auto errorLineForPrint = range(errorLine)
				.split_c(LAMBDA(v){ return v == '\t'; })
				.map_c(LAMBDA(v){ return v.toString(); })
				.addSeparator_c([](){ return "    "; })
				.join();

			out += e.what();
			out += cat::nlIndent;
			out += errorLineForPrint;
			out += cat::nlIndent;
			out += errorMarkerLine;
		}
		if (compiledSuccessfull) {
			//write to File:
			{
				auto outFile = cat::FW(new std::ofstream(TEST_UNIT1_PATH + NGPL_COMPILED_EXTENSION));
				unit->print(outFile);
			}

			// linking:
			ngpl::Linker linker({unit.getRaw()});
			linker.generateInstructionStream();
			linker.linkFunctions();


			// print final assembler:
			auto lines = range(src)
				.split_c(LAMBDA(v){ return v == '\n'; })
				.map_c(LAMBDA(v){ return v.toString(); })
				.toVector();
			for (size_t i = 0; i < linker._instructions.size(); ++i) {

				std::string iStr = std::to_string(i);
				auto paddLength = 4 - iStr.length();
				out += cat::nl;
				out += std::string(paddLength, ' ');
				out += i;
				out += "   ";
				auto instrString = linker._instructions[i].toString();
				out += instrString;

				paddLength = 80 - instrString.length();
				out += std::string(paddLength, ' ');
				out += "";
				out += lines.at(linker._instructions[i].pos().line());
			}
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
		out += "FINISHED !";

	}
	out += cat::nlIndent;

	// qDebug() << "hello beautiful world!" << endl << endl;
	// QCoreApplication a(argc, argv);

	return 0; //a.exec();
}
