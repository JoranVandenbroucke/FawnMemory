add_executable(${PROJECT_NAME}_test
        container/CompressedPair.cpp
        container/DenseMap.cpp
        container/DenseSet.cpp
        container/Pair.cpp
        container/SparseSet.cpp
        container/Vector.cpp
)
target_link_libraries(${PROJECT_NAME}_test PRIVATE
        ${PROJECT_NAME}::${PROJECT_NAME}
)

set_target_properties(${PROJECT_NAME}_test PROPERTIES CXX_STANDARD_REQUIRED ON)
set_target_properties(${PROJECT_NAME}_test PROPERTIES CXX_EXTENSIONS OFF)

include(compiler_flags)
set(FUNCTION_NAME "get_compile_flags_${CMAKE_BUILD_TYPE}_${CMAKE_CXX_COMPILER_ID}")

# Call the dynamically constructed function
if(EXISTS ${FUNCTION_NAME})
    execute_process(COMMAND ${FUNCTION_NAME} COMPILER_FLAGS)
endif()
target_compile_options(${PROJECT_NAME}_test
        PRIVATE
            ${COMPILER_FLAGS}
)

target_compile_features(${PROJECT_NAME}_test PRIVATE cxx_std_23)

enable_testing()
include(setup_catch)
setup_catch(${PROJECT_NAME}_test)
