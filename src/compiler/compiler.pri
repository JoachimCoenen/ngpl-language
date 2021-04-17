
#include(CSSATree/CSSATree.pri)
include(language/language.pri)
#include(optimizer2/optimizer2.pri)


SOURCES += \
	src/compiler/codeGenerator.cpp \
	src/compiler/linker.cpp \
	src/compiler/optimizer.cpp \
	src/compiler/parser.cpp \
	src/compiler/syntaxError.cpp \
	src/compiler/tokenizer.cpp \

HEADERS += \
	src/compiler/codeGenerator.h \
	src/compiler/linker.h \
	src/compiler/optimizer.h \
	src/compiler/parser.h \
	src/compiler/syntaxError.h \
	src/compiler/tokenizer.h \
