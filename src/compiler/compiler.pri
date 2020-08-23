
include(intermediate/intermediate.pri)
include(language/language.pri)


SOURCES += \
	src/compiler/codeGenerator.cpp \
	src/compiler/linker.cpp \
	src/compiler/optimizer.cpp \
	src/compiler/parser.cpp \
	src/compiler/stackFrame.cpp \
	src/compiler/syntaxError.cpp \
	src/compiler/tokenizer.cpp \

HEADERS += \
	src/compiler/codeGenerator.h \
	src/compiler/linker.h \
	src/compiler/optimizer.h \
	src/compiler/parser.h \
	src/compiler/stackFrame.h \
	src/compiler/syntaxError.h \
	src/compiler/tokenizer.h \
