//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

#define CATCH_CONFIG_MAIN
#include <string>

#include <catch2/catch_all.hpp>

import FawnMemory;

TEST_CASE( "CDenseMap: Constructors and Basic Access" )
{
    DeerContainer::CDenseMap<int, std::string> map;
    REQUIRE( map.empty() );
    REQUIRE( map.size() == 0 );

    DeerContainer::CDenseMap<int, std::string> map2{{1, "one"}, {2, "two"}};
    REQUIRE( map2.size() == 2 );
    REQUIRE( map2.at(1) == "one" );
    REQUIRE( map2[2] == "two" );
}

TEST_CASE( "CDenseMap: Operator[] and At" )
{
    DeerContainer::CDenseMap<int, std::string> map;
    map[ 1 ] = "one";
    REQUIRE( map[1] == "one" );

    map[ 2 ] = "two";
    REQUIRE( map[2] == "two" );

    REQUIRE_THROWS_AS( map.at(3), std::out_of_range );
}

TEST_CASE( "CDenseMap: Insertion and Emplace" )
{
    DeerContainer::CDenseMap<int, std::string> map;

    // Insert with value
    auto [ it, inserted ] = map.insert( std::pair<int, std::string>{3, "three"} );

    // const auto& [ it, inserted ]{map.insert( DeerContainer::SPair{3, "three"} )};
    REQUIRE( inserted );
    REQUIRE( it->first == 3 );
    REQUIRE( it->second == "three" );

    // Insert with emplace
    auto [ it2, inserted2 ] = map.emplace( 4, "four" );
    REQUIRE( inserted2 );
    REQUIRE( it2->first == 4 );
    REQUIRE( it2->second == "four" );

    // Emplace with hint
    auto itHint{map.emplace_hint( map.cbegin(), 5, "five" )};
    REQUIRE( itHint->first == 5 );
    REQUIRE( itHint->second == "five" );
}

TEST_CASE( "CDenseMap: Modifiers - Insert or Assign and Try Emplace" )
{
    DeerContainer::CDenseMap<int, std::string> map{{1, "one"}};

    // Insert or Assign
    // auto [it, inserted] = map.insert_or_assign(1, "ONE");
    std::map<int, std::string> tmp{{1, "one"}};
    const auto [ it, inserted ]{map.insert_or_assign( 1, "ONE" )};
    REQUIRE( !inserted );
    REQUIRE( it->first == 1 );
    REQUIRE( it->second == "ONE" );

    // Try Emplace
    const auto [ it2, inserted2 ]{map.try_emplace( 2, "two" )};
    REQUIRE( inserted2 );
    REQUIRE( it2->first == 2 );
    REQUIRE( it2->second == "two" );
}

TEST_CASE( "CDenseMap: Iterators" )
{
    DeerContainer::CDenseMap<int, std::string> map{{1, "one"}, {2, "two"}, {3, "three"}};

    auto it = map.begin();
    REQUIRE( it->first == 1 );
    ++it;
    REQUIRE( it->first == 2 );

    const auto rit = map.rbegin();
    REQUIRE( rit->first == 3 );
}

TEST_CASE( "CDenseMap: Erase and Clear" )
{
    DeerContainer::CDenseMap<int, std::string> map{{1, "one"}, {2, "two"}};

    map.erase( 1 );
    REQUIRE( map.size() == 1 );
    REQUIRE( !map.contains(1) );

    map.clear();
    REQUIRE( map.empty() );
}

TEST_CASE( "CDenseMap: Lookup Functions" )
{
    DeerContainer::CDenseMap<int, std::string> map{{1, "one"}, {2, "two"}};

    REQUIRE( map.find(1) != map.end() );
    REQUIRE( map.find(3) == map.end() );
    REQUIRE( map.contains(2) );
    REQUIRE( !map.contains(3) );
}
