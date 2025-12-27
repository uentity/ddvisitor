cmake_minimum_required(VERSION 3.31)

project(ddvisitor
	VERSION 0.9
	DESCRIPTION "Modern C++ double dispatch visitor"
	LANGUAGES CXX)

set(DDVISITOR_NAMESPACE ddv::)

add_library(ddvisitor INTERFACE)
add_library(ddv::ddvisitor ALIAS ddvisitor)
target_compile_features(ddvisitor INTERFACE cxx_std_20)

set(DDV_HEADERS
	"deduce_callable.h"
	"serial_visitor.h"
	"visitable.h"
	"visitor_common.h"
)
list(TRANSFORM DDV_HEADERS PREPEND "include/ddv/")
target_sources(
	ddvisitor PUBLIC
	FILE_SET HEADERS
	BASE_DIRS include
	FILES ${DDV_HEADERS}
)

# Check if we are a sub-project and if the dependency has been fetched
if (NOT CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND DEFINED tpack_BINARY_DIR)
	# This ensures find_package(tpack) succeeds by finding the build-tree targets.
	list(APPEND CMAKE_PREFIX_PATH ${tpack_BINARY_DIR})
endif()

find_package(tpack CONFIG REQUIRED)
target_link_libraries(ddvisitor INTERFACE tp::tpack)

include(CTest)
if ((CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME) AND BUILD_TESTING)
	Include(FetchContent)

	FetchContent_Declare(
		Catch2
		GIT_REPOSITORY https://github.com/catchorg/Catch2.git
		GIT_TAG        v3.11.0
	)
	FetchContent_MakeAvailable(Catch2)
	include(Catch)
	
	add_executable(
		ddv_tests
		${CMAKE_CURRENT_SOURCE_DIR}/tests/test_serial_visitor.cpp
		${CMAKE_CURRENT_SOURCE_DIR}/tests/test_ddv.cpp
	)
	target_link_libraries(ddv_tests PRIVATE Catch2::Catch2WithMain)
	target_link_libraries(ddv_tests PRIVATE ddvisitor)
	target_compile_options(ddv_tests PRIVATE
		$<$<CXX_COMPILER_ID:MSVC>:/W4>
		$<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra -Wpedantic>
		$<$<CXX_COMPILER_ID:Clang>:-ftime-trace>
	)
	set_property(TARGET ddv_tests PROPERTY COMPILE_WARNING_AS_ERROR ON)
	catch_discover_tests(ddv_tests)

	add_executable(dynamic_cast_test ${CMAKE_CURRENT_SOURCE_DIR}/tests/dynamic_cast_test.cpp)
	target_link_libraries(dynamic_cast_test PRIVATE Catch2::Catch2WithMain)
	target_link_libraries(dynamic_cast_test PRIVATE ddvisitor)
	add_test(NAME "[benchmark] dynamic_cast vs ddv" COMMAND dynamic_cast_test)
endif ()

## Install and export
set(PACKAGE_CONFIG_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

# Lib artifacts which is only headers installed into default location
install(
	TARGETS ddvisitor
	EXPORT ddvisitor-targets
	FILE_SET HEADERS
)
# Install generated package definition file
install(
	EXPORT ddvisitor-targets
	NAMESPACE ${DDVISITOR_NAMESPACE}
	FILE ddvisitor-targets.cmake
	DESTINATION ${PACKAGE_CONFIG_DESTINATION}
)

# Generate CMake package config files
include(CMakePackageConfigHelpers)

configure_package_config_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/cmake/ddvisitor-config.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/ddvisitor-config.cmake"
	INSTALL_DESTINATION ${PACKAGE_CONFIG_DESTINATION}
)
write_basic_package_version_file(
	"${CMAKE_CURRENT_BINARY_DIR}/ddvisitor-version.cmake"
	COMPATIBILITY ExactVersion
)

# Install generated config files
install(
	FILES
		"${CMAKE_CURRENT_BINARY_DIR}/ddvisitor-config.cmake"
		"${CMAKE_CURRENT_BINARY_DIR}/ddvisitor-version.cmake"
	DESTINATION ${PACKAGE_CONFIG_DESTINATION}
)

set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
	APPEND PROPERTY EXPORT_PACKAGE_REGISTRY ${PROJECT_NAME}
)
set(CMAKE_EXPORT_PACKAGE_REGISTRY ON)
export(EXPORT ddvisitor-targets NAMESPACE ${DDVISITOR_NAMESPACE} FILE "${CMAKE_CURRENT_BINARY_DIR}/ddvisitor-targets.cmake")

if (DEFINED CMAKE_USER_BUILD_DEV_MODE AND CMAKE_USER_BUILD_DEV_MODE)
	message(STATUS "Local Package Registry ENABLED for development.")
	export(PACKAGE ddvisitor)
endif ()
