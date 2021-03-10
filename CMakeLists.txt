cmake_minimum_required(VERSION 3.10)

project(ncix)

enable_language(Fortran)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}")
find_package(CDF)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
	set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -Wall -cpp -fdefault-real-8")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -free -fpp -fpconst")
endif()

set_source_files_properties(cdf_mod.f90 PROPERTIES Fortran_FORMAT FIXED)

set(DATETIME OFF CACHE BOOL "Enable datetime-fortran support")
if (DATETIME)
    add_definitions("-DDATETIME_FORTRAN=1")
	SET(DATETIME_LIBRARIES datetime-fortran CACHE STRING "datetime-fortran library name")
	message(STATUS "datetime_fortran enabled : ${DATETIME_LIBRARIES}")
else()
	message(STATUS "Datetime not found")
    SET(DATETIME_LIBRARIES "")
endif()

add_library(ncix ncix.f90 cdf_mod.f90 ncix_epoch.f90)
target_include_directories(ncix PUBLIC ${CDF_INCLUDES})
target_link_libraries(ncix ${CDF_LIBRARIES} ${DATETIME_LIBRARIES})

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
	add_executable(demo demo.f90)
	target_link_libraries(demo ncix ${CDF_LIBRARIES})
endif()
