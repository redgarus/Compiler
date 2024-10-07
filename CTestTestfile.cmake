# CMake generated Testfile for 
# Source directory: /home/redgar/Compiler
# Build directory: /home/redgar/Compiler
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test([=[test1.red]=] "bash" "-c" "./build/compiler /home/redgar/Compiler/tests/samples/test1.red && g++ test1.o -o test1 && ./test1")
set_tests_properties([=[test1.red]=] PROPERTIES  _BACKTRACE_TRIPLES "/home/redgar/Compiler/CMakeLists.txt;40;add_test;/home/redgar/Compiler/CMakeLists.txt;0;")
add_test([=[test2.red]=] "bash" "-c" "./build/compiler /home/redgar/Compiler/tests/samples/test2.red && g++ test2.o -o test2 && ./test2")
set_tests_properties([=[test2.red]=] PROPERTIES  _BACKTRACE_TRIPLES "/home/redgar/Compiler/CMakeLists.txt;40;add_test;/home/redgar/Compiler/CMakeLists.txt;0;")
add_test([=[test3.red]=] "bash" "-c" "./build/compiler /home/redgar/Compiler/tests/samples/test3.red && g++ test3.o -o test3 && ./test3")
set_tests_properties([=[test3.red]=] PROPERTIES  _BACKTRACE_TRIPLES "/home/redgar/Compiler/CMakeLists.txt;40;add_test;/home/redgar/Compiler/CMakeLists.txt;0;")
