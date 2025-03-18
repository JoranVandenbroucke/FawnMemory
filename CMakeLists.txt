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
        Source/container/containerCompatibleRange.hpp
        Source/container/ValueTraits.hpp
        PUBLIC
        FILE_SET CXX_MODULES TYPE CXX_MODULES FILES
        source/FawnMemory.ixx
        source/allocators/AffixAllocator.ixx
        source/allocators/AllocatorWithStats.ixx
        #        source/allocators/BitmappedBlock.ixx
        source/allocators/Buckitizer.ixx
        #        source/allocators/CascadingAllocator.ixx
        source/allocators/Common.ixx
        source/allocators/DeerAllocator.ixx
        source/allocators/FallbackAllocator.ixx
        source/allocators/Freelist.ixx
        source/allocators/Mallocator.ixx
        source/allocators/Segregator.ixx
        source/allocators/StackAllocator.ixx
        source/allocators/Utilities.ixx

        source/container/CircularBuffer.ixx
        source/container/CompressedPair.ixx
        source/container/Deercontainer.ixx
        source/container/DenseMap.ixx
        source/container/DenseSet.ixx
        source/container/FixedString.ixx
        source/container/Pair.ixx
        source/container/SparseSet.ixx
        source/container/Vector.ixx

#        source/entity_component/DeerEntity.ixx
#        source/entity_component/ECManager.ixx
#        source/entity_component/Storage.ixx
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
