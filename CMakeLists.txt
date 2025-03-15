cmake_minimum_required(VERSION 3.28)
project(FawnMemory VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CXX_SCAN_FOR_MODULES ON)
set(CXX_STANDARD_REQUIRED ON)
set(CXX_EXTENSIONS OFF)

option(FawnMemory_Test "If the tests need to be build to (Default OFF)" ON)

add_library(${PROJECT_NAME})
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

set_target_properties(${PROJECT_NAME} PROPERTIES CXX_STANDARD_REQUIRED ON)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_EXTENSIONS OFF)

target_sources(${PROJECT_NAME}
        PRIVATE
        Source/Container/ContainerCompatibleRange.hpp
        Source/Container/ValueTraits.hpp
        PUBLIC
        FILE_SET CXX_MODULES TYPE CXX_MODULES FILES
        source/FawnMemory.ixx
        source/Allocators/AffixAllocator.ixx
        source/Allocators/AllocatorWithStats.ixx
        #        source/Allocators/BitmappedBlock.ixx
        source/Allocators/Buckitizer.ixx
        #        source/Allocators/CascadingAllocator.ixx
        source/Allocators/Common.ixx
        source/Allocators/DeerAllocator.ixx
        source/Allocators/FallbackAllocator.ixx
        source/Allocators/Freelist.ixx
        source/Allocators/Mallocator.ixx
        source/Allocators/Segregator.ixx
        source/Allocators/StackAllocator.ixx
        source/Allocators/Utilities.ixx

        source/Container/CircularBuffer.ixx
        source/Container/CompressedPair.ixx
        source/Container/DeerContainer.ixx
        source/Container/DenseMap.ixx
        source/Container/DenseSet.ixx
        source/Container/FixedString.ixx
        source/Container/Pair.ixx
        source/Container/SparseSet.ixx
        source/Container/Vector.ixx

#        source/EntityComponent/DeerEntity.ixx
#        source/EntityComponent/ECManager.ixx
#        source/EntityComponent/Storage.ixx
)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(compiler_flags)
set(FUNCTION_NAME "get_compile_flags_${CMAKE_BUILD_TYPE}_${CMAKE_CXX_COMPILER_ID}")
if(EXISTS ${FUNCTION_NAME})
    execute_process(COMMAND ${FUNCTION_NAME} COMPILER_FLAGS)
endif()
target_compile_options(${PROJECT_NAME} PRIVATE ${COMPILER_FLAGS} )

target_link_options(${PROJECT_NAME}
        PRIVATE
        $<$<CXX_COMPILER_ID:Clang>: $<$<BOOL:${USE_LLD}>:-fuse-ld=lld>>>
        $<$NOT<$<BOOL:${USE_LLD}>>:
            $<$<CXX_COMPILER_ID:MSVC>:
                /Gw /Gy /GL
            >
            $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:
                -flto=full
            >
        >
)

target_compile_definitions(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-DBALBINO_DEBUG>)
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_23)

if (DEFINED FawnMemory_Test AND FawnMemory_Test)
    add_subdirectory(tests)
endif ()
