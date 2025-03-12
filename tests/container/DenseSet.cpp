//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

#define CATCH_CONFIG_MAIN
#include <string>

#include <catch2/catch_all.hpp>
import FawnMemory;

TEST_CASE("CDenseSet basic operations", "[CDenseSet]") {
    DeerContainer::CDenseSet<int> set;

    SECTION("Insertion and size") {
        REQUIRE(set.empty());
        REQUIRE(set.size() == 0);

        set.insert(1);
        set.insert(2);
        set.insert(3);

        REQUIRE_FALSE(set.empty());
        REQUIRE(set.size() == 3);
    }

    SECTION("Element access") {
        set.insert(1);
        set.insert(2);
        set.insert(3);

        auto it = set.begin();
        REQUIRE(*it == 1);
        ++it;
        REQUIRE(*it == 2);
        ++it;
        REQUIRE(*it == 3);
    }

    SECTION("Erase elements") {
        set.insert(1);
        set.insert(2);
        set.insert(3);

        set.erase(2);
        REQUIRE(set.size() == 2);

        auto it = set.begin();
        REQUIRE(*it == 1);
        ++it;
        REQUIRE(*it == 3);
    }

    SECTION("Clear set") {
        set.insert(1);
        set.insert(2);
        set.insert(3);

        set.clear();
        REQUIRE(set.empty());
        REQUIRE(set.size() == 0);
    }

    SECTION("Swap sets") {
        DeerContainer::CDenseSet<int> otherSet;
        otherSet.insert(4);
        otherSet.insert(5);

        set.insert(1);
        set.insert(2);

        set.swap(otherSet);

        REQUIRE(set.size() == 2);
        REQUIRE(otherSet.size() == 2);

        auto it = set.begin();
        REQUIRE(*it == 4);
        ++it;
        REQUIRE(*it == 5);

        it = otherSet.begin();
        REQUIRE(*it == 1);
        ++it;
        REQUIRE(*it == 2);
    }
}
