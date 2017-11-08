cmake_minimum_required(VERSION 3.2)
project(impVm VERSION 0.1 LANGUAGES C)

include(GNUInstallDirs)

set(CMAKE_C_FLAGS "-std=gnu99 -Wall -Wextra -pedantic -O3 -Wno-unused-parameter")

file(GLOB SOURCES "${PROJECT_SOURCE_DIR}/src/*.c")

#MESSAGE( STATUS "SRC_DIR:   " ${PROJECT_SOURCE_DIR})
MESSAGE( STATUS "SOURCES:   " ${SOURCES})

add_executable(impVM ${SOURCES})
#TARGET_LINK_LIBRARIES(impVM ubsan)

install(TARGETS impVM EXPORT impVMConfig
	RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
#install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/lauSim/)
install(EXPORT impVMConfig DESTINATION share/lauSim/cmake)
