//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

#define CATCH_CONFIG_MAIN
#include <catch2/catch_all.hpp>

import FawnMemory;
TEST_CASE( "DeerContainer Vecotr Basic Operations", "[DeerContainerVector]" )
 {
    DeerContainer::CVector<int> vec;

    SECTION( "Push back and size" ) {
        REQUIRE( vec.size() == 0 );

        vec.push_back(1);
        REQUIRE( vec.size() == 1 );
        REQUIRE( vec[0] == 1 );

        vec.push_back(2);
        vec.push_back(3);
        REQUIRE( vec.size() == 3 );
        REQUIRE( vec[1] == 2 );
        REQUIRE( vec[2] == 3 );
    }

    SECTION( "Element access with at() and operator[]" ) {
        vec.push_back(10);
        vec.push_back(20);
        vec.push_back(30);

        REQUIRE( vec[0] == 10 );
        REQUIRE( vec[1] == 20 );
        REQUIRE( vec[2] == 30 );

        REQUIRE( vec.at(0) == 10 );
        REQUIRE( vec.at(1) == 20 );
        REQUIRE( vec.at(2) == 30 );

        REQUIRE_THROWS_AS( vec.at(3), std::out_of_range );
    }

    SECTION( "Empty vector and clear()" ) {
        REQUIRE( vec.empty() );

        vec.push_back(5);
        REQUIRE_FALSE( vec.empty() );

        vec.clear();
        REQUIRE( vec.empty() );
        REQUIRE( vec.size() == 0 );
    }

    SECTION( "Resize" ) {
        vec.resize(5);
        REQUIRE( vec.size() == 5 );
        for (int i = 0; i < 5; ++i) {
            REQUIRE( vec[i] == int() );
        }

        vec.resize(3);
        REQUIRE( vec.size() == 3 );
    }

    SECTION( "Insert and erase" ) {
        vec.push_back(1);
        vec.push_back(2);
        vec.push_back(4);

        vec.insert(vec.begin() + 2, 3);
        REQUIRE( vec.size() == 4 );
        REQUIRE( vec[2] == 3 );
        vec.erase(vec.cbegin() + 1);
        REQUIRE( vec.size() == 3 );
        REQUIRE( vec[1] == 3 );
    }

    SECTION( "Iteration" ) {
        vec.push_back(1);
        vec.push_back(2);
        vec.push_back(3);

        int sum = 0;
        for (const auto& val : vec) {
            sum += val;
        }
        REQUIRE( sum == 6 );

        sum = 0;
        for (auto it = vec.begin(); it != vec.end(); ++it) {
            sum += *it;
        }
        REQUIRE( sum == 6 );
    }
}

// TEST_CASE("DeerContainer::Vector Comparison", "[DeerContainer::Vector]") {
//     DeerContainer::Vector<int> vec1 = {1, 2, 3};
//     DeerContainer::Vector<int> vec2 = {1, 2, 3};
//     DeerContainer::Vector<int> vec3 = {1, 2, 4};
//
//     SECTION("Equality and inequality") {
//         REQUIRE(vec1 == vec2);
//         REQUIRE_FALSE(vec1 == vec3);
//         REQUIRE(vec1 != vec3);
//     }
// }
