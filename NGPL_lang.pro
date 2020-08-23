QT -= gui

CONFIG += c++17 console
CONFIG -= app_bundle

# The following define makes your compiler emit warnings if you use
# any Qt feature that has been marked deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if it uses deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

win32:CONFIG(release, debug|release): QMAKE_CXXFLAGS += -O3 -Wno-comment
win32:CONFIG(debug,   debug|release): QMAKE_CXXFLAGS += -Wno-comment

INCLUDEPATH += src

include(src/compiler/compiler.pri)

SOURCES += \
	src/language/ast.cpp \
	src/language/token.cpp \
	src/main.cpp \
	src/mainHelper.cpp \
	src/util/instructionID.cpp \
	src/vm/instruction.cpp \
	src/vm/instructionExecuter.cpp \
	src/vm/object.cpp \
	src/vm/value.cpp


HEADERS += \
	src/language/ast.h \
	src/language/position.h \
	src/language/token.h \
	src/language/unitNature.h \
	src/mainHelper.h \
	src/util/debug.h \
	src/util/instructionID.h \
	src/util/instructions_inc.h \
	src/util/types.h \
	src/vm/instruction.h \
	src/vm/instructionExecuter.h \
	src/vm/object.h \
	src/vm/value.h \
	src/vm/vm_util.h

# SUBDIRS = language



INCLUDEPATH += ../Cat/src
win32:CONFIG(release, debug|release): LIBS += -L../../Cat/build-Cat-MinGW-Release/release -lCat
win32:CONFIG(debug,   debug|release): LIBS += -L../../Cat/build-Cat-MinGW-Debug/debug -lCat

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target







