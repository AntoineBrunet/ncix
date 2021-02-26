cmake_minimum_required(VERSION 3.10)

project(ncix)

enable_language(Fortran)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}")
find_package(CDF)

set_source_files_properties(cdf_mod.f90 PROPERTIES Fortran_FORMAT FIXED)
add_library(ncix ncix.f90 cdf_mod.f90 ncix_epoch.f90)
target_include_directories(ncix PUBLIC ${CDF_INCLUDES})
target_link_libraries(ncix ${CDF_LIBRARIES})

add_executable(demo demo.f90)
target_link_libraries(demo ncix ${CDF_LIBRARIES})
