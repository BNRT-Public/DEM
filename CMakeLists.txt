# CMakeLists.txt para contruir el ejecutable del DEM
cmake_minimum_required(VERSION 3.10)

set(CMAKE_Fortran_COMPILER "ifort")
set(CMAKE_VERBOSE_MAKEFILE "ON")

project(DEM_program LANGUAGES Fortran)


set(DEMsrc ${PROJECT_SOURCE_DIR}/src)
set(DEMconstits ${PROJECT_SOURCE_DIR}/src/constits)
set(DEMfunction ${PROJECT_SOURCE_DIR}/src/function)
set(DEMmain ${PROJECT_SOURCE_DIR}/src/main)
set(DEMrandomness ${PROJECT_SOURCE_DIR}/src/randomness)
set(DEMsubrutines ${PROJECT_SOURCE_DIR}/src/subrutines)
set(DEMwritefiles ${PROJECT_SOURCE_DIR}/src/writefiles)

# Lista de archivos de módulos
set(MODULE_FILES
    ${DEMfunction}/GAMA.f90
    ${DEMsubrutines}/SIZEPOLO.f90
    ${DEMrandomness}/CAMPOROH.f90
    ${DEMsubrutines}/POSPOLO.f90
    ${DEMsubrutines}/DOMINF.f90
    ${DEMmain}/WINDOWS.f90
    ${DEMfunction}/STA101.f90
    ${DEMfunction}/SPECTRALROH.f90
    ${DEMwritefiles}/CLOSE_FILES.f90
    ${DEMsrc}/VARIABLES_DATA.f90
)

set(MAIN_FILE
    ${DEMsrc}/DEM.f90
)


# Añadir los módulos como una biblioteca
add_library(modulos STATIC ${MODULE_FILES})

# Crear el ejecutable principal y vincularlo con la biblioteca de módulos
add_executable(DEM ${MAIN_FILE})
target_link_libraries(DEM PRIVATE modulos)

# Configuración de OpenMP si es necesario
find_package(OpenMP REQUIRED)
if(OpenMP_Fortran_FOUND)
    target_link_libraries(DEM PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Copiar archivo data
file(COPY ${DEMsrc}/DATA.dat
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
