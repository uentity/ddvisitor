cmake_minimum_required(VERSION 3.23)

project(ddvisitor
	DESCRIPTION "Modern C++ double dispatch visitor"
	LANGUAGES CXX)

add_library(ddvisitor INTERFACE)
add_library(ddvisitor::ddvisitor ALIAS ddvisitor)
target_compile_features(ddvisitor INTERFACE cxx_std_20)

target_include_directories(
	ddvisitor INTERFACE
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
	$<INSTALL_INTERFACE:include>
)

set(DDV_HEADERS
	"deduce_callable.h"
	"serial_visitor.h"
	"visitable.h"
	"visitor_common.h"
)
list(TRANSFORM DDV_HEADERS PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/include/ddv/")
target_sources(
	ddvisitor INTERFACE
	FILE_SET HEADERS
	BASE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include
	FILES ${DDV_HEADERS}
)

find_package(tpack)
target_link_libraries(ddvisitor INTERFACE tp::tpack)

include(CTest)
if ((CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME) AND BUILD_TESTING)
	Include(FetchContent)

	FetchContent_Declare(
	  Catch2
	  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
	  GIT_TAG        v3.4.0
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
	)
	set_property(TARGET ddv_tests PROPERTY COMPILE_WARNING_AS_ERROR ON)
	catch_discover_tests(ddv_tests)
endif ()

include(GNUInstallDirs)
install(
	TARGETS ddvisitor
	EXPORT ddv_targets
	INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
	FILE_SET HEADERS DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(
	EXPORT ddv_targets NAMESPACE ddvisitor::
	DESTINATION ${CMAKE_INSTALL_LIB_DIR}/cmake/ddvisitor
)
export(EXPORT ddv_targets NAMESPACE ddvisitor:: FILE ddv_targets.cmake)
#set(CMAKE_EXPORT_PACKAGE_REGISTRY ON)
export(PACKAGE ddvisitor)