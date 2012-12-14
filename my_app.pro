TEMPLATE     = app
CONFIG      += no_keywords uitools release
INCLUDEPATH += /home/martin/code/eql/src
QMAKE_LIBDIR += /home/martin/code/eql
LIBS        += -lecl -L. -lmy_lib -L.. -leql
TARGET       = my_app
DESTDIR      = ./
OBJECTS_DIR  = ./tmp/
MOC_DIR      = ./tmp/

include(/home/martin/code/eql/src/windows.pri)

SOURCES += main.cpp
