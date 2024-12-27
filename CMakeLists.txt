cmake_minimum_required(VERSION 3.30)

project(
	FormalSystem
	LANGUAGES CXX
	VERSION 1.0
)

file(GLOB SRC "./src/*cxx")
file(GLOB IFC "./module/*ixx")

add_executable(${PROJECT_NAME})

target_sources(
	${PROJECT_NAME}
	PRIVATE
	${SRC} ${IFC}
)

target_sources(
	${PROJECT_NAME}
	PRIVATE
	FILE_SET CXX_MODULES FILES ${IFC}
)

target_include_directories(
	${PROJECT_NAME}
	PRIVATE
	"./include/"
)

target_compile_features(
	${PROJECT_NAME}
	PRIVATE
	cxx_std_20
)

###

set_source_files_properties(
	${IFC}
	PROPERTIES
	VS_SETTINGS "CompileAs=CompileAsCppModule"
)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/templates/.clangd.in
	${CMAKE_CURRENT_SOURCE_DIR}/.clangd
	@ONLY
)

set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR})

install(
	TARGETS ${CMAKE_PROJECT_NAME}
	RUNTIME DESTINATION bin
)

install(
	DIRECTORY data/
	DESTINATION bin/
)