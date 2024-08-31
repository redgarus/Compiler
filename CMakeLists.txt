cmake_minimum_required(VERSION 3.20.0)

project(REDCOMP VERSION 1.0 LANGUAGES CXX)

set(EXECUTABLE_OUTPUT_PATH "${PROJECT_SOURCE_DIR}/build")
SET(TESTS ${CMAKE_SOURCE_DIR}/tests)
SET(FILENAMES)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

add_executable(compiler src/compiler.cpp)

if(EBUG_MODE)
    message(STATUS "DEBUG MODE: ACTIVATED")
    target_compile_definitions(compiler PRIVATE DEBUG_MODE)
endif()

llvm_map_components_to_libnames(llvm_libs -18)
target_include_directories(compiler PUBLIC "src/headers/")
target_link_libraries(compiler PRIVATE ${llvm_libs})

enable_testing()

file(GLOB testFiles "${TESTS}/samples/*.red")

foreach(file ${testFiles})
    get_filename_component(fileNameEXT ${file} NAME)
    get_filename_component(fileName ${file} NAME_WE)

    set(FILENAMES ${FILENAMES} "${CMAKE_SOURCE_DIR}/${fileName}")
    set(FILENAMES ${FILENAMES} "${CMAKE_SOURCE_DIR}/${fileName}.o")
    
    add_test(${fileNameEXT} bash -c "./build/compiler ${file} && g++ ${fileName}.o -o ${fileName} && ./${fileName}")
endforeach()

add_custom_target(clear COMMAND rm -f ${FILENAMES})