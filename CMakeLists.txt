cmake_minimum_required(VERSION 3.28)
project(FawnMemory VERSION 0.1.0 LANGUAGES CXX C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(FawnMemory_Test "If the tests need to be build to (Default OFF)" OFF)

add_library(${PROJECT_NAME}
        Source/Container/ContainerCompatibleRange.hpp
)
target_sources(${PROJECT_NAME} PUBLIC
        FILE_SET fawn_memory_modules TYPE CXX_MODULES FILES
        Source/FawnMemory.ixx
        Source/Allocators/AffixAllocator.ixx
        Source/Allocators/AllocatorWithStats.ixx
        #        Source/Allocators/BitmappedBlock.ixx
        Source/Allocators/Buckitizer.ixx
        #        Source/Allocators/CascadingAllocator.ixx
        Source/Allocators/Common.ixx
        Source/Allocators/DeerAllocator.ixx
        Source/Allocators/FallbackAllocator.ixx
        Source/Allocators/Freelist.ixx
        Source/Allocators/Mallocator.ixx
        Source/Allocators/Segregator.ixx
        Source/Allocators/StackAllocator.ixx
        Source/Allocators/Utilities.ixx

        Source/Container/DeerContainer.ixx
        Source/Container/Map.ixx
        Source/Container/Pair.ixx
        Source/Container/Set.ixx
        Source/Container/Vector.ixx
        Source/Container/ValueTraits.ixx

        Source/EntityComponent/DeerEntity.ixx
        Source/EntityComponent/ECManager.ixx
        Source/EntityComponent/System.ixx
)
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

target_compile_options(${PROJECT_NAME}
        PRIVATE
        $<$<CONFIG:Debug>:${DEFAULT_CXX_DEBUG_INFORMATION_FORMAT} ${DEFAULT_CXX_EXCEPTION_HANDLING}>
        $<$<CXX_COMPILER_ID:MSVC>:/W4 /WX /MP >
        $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra -Wpedantic -Werror>
        PUBLIC
        $<$<CXX_COMPILER_ID:GNU>:-fmodules-ts>
        # todo remove -Wdepricated -Wdepricated-declarations
        #todo --embed-dir=${CMAKE_SOURCE_DIR}/Embedded/
        $<$<CXX_COMPILER_ID:Clang>: -fembed-bitcode>
)
target_compile_definitions(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-DBALBINO_DEBUG>)
target_compile_features(${PROJECT_NAME} PRIVATE
        cxx_aggregate_default_initializers
        cxx_alignof
        cxx_attributes
        cxx_auto_type
        cxx_binary_literals
        cxx_constexpr
        cxx_contextual_conversions
        cxx_defaulted_functions
        cxx_defaulted_move_initializers
        cxx_delegating_constructors
        cxx_deleted_functions
        cxx_enum_forward_declarations
        cxx_explicit_conversions
        cxx_final
        cxx_generalized_initializers
        cxx_generic_lambdas
        cxx_inheriting_constructors
        cxx_inline_namespaces
        cxx_lambda_init_captures
        cxx_lambdas
        cxx_long_long_type
        cxx_noexcept
        cxx_nonstatic_member_init
        cxx_nullptr
        cxx_override
        cxx_range_for
        cxx_raw_string_literals
        cxx_right_angle_brackets
        cxx_rvalue_references
        cxx_sizeof_member
        cxx_static_assert
        cxx_std_23
        cxx_strong_enums
        cxx_thread_local
        cxx_uniform_initialization
        cxx_unrestricted_unions
        cxx_user_literals
)

if (DEFINED FawnMemory_Test AND FawnMemory_Test)
    add_subdirectory(tests)
endif ()