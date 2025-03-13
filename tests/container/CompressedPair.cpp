//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//
#define CATCH_CONFIG_MAIN
#include <catch2/catch_all.hpp>
#include <string>

import FawnMemory;

TEST_CASE("SCompressedPair: Basic Construction", "[SCompressedPair]")
{
    const DeerContainer::SCompressedPair<int, std::string> p1{1, "Test"};
    REQUIRE(p1.first == 1);
    REQUIRE(p1.second == "Test");
}

TEST_CASE("SCompressedPair: Copy and Assignment", "[SCompressedPair]")
{
    const DeerContainer::SCompressedPair<int, std::string> p1{1, "Copy"};
    const DeerContainer::SCompressedPair<int, std::string> p2 = p1; // Copy
    REQUIRE(p2.first == 1);
    REQUIRE(p2.second == "Copy");

    DeerContainer::SCompressedPair<int, std::string> p3{2, "Assign"};
    p3 = p1; // Assignment
    REQUIRE(p3.first == 1);
    REQUIRE(p3.second == "Copy");
}

TEST_CASE("SCompressedPair: Move Semantics", "[SCompressedPair]")
{
    const DeerContainer::SCompressedPair<int, std::string> p1{1, "Move"};
    const DeerContainer::SCompressedPair<int, std::string> p2 = std::move(p1);

    REQUIRE(p2.first == 1);
    REQUIRE(p2.second == "Move");
}

TEST_CASE("SCompressedPair: make_compressed_pair Helper", "[SCompressedPair]")
{
    const auto p = DeerContainer::make_compressed_pair<int, std::string>(3, "Helper");
    REQUIRE(p.first == 3);
    REQUIRE(p.second == "Helper");
}

TEST_CASE("SCompressedPair: Comparisons", "[SCompressedPair]")
{
    const DeerContainer::SCompressedPair p1{1, 2};
    const DeerContainer::SCompressedPair p2{1, 2};
    const DeerContainer::SCompressedPair p3{2, 1};

    const bool isEqual{p1 == p2};
    const bool isNotEqual{p1 != p3};
    const bool isLess{p1 < p3};
    const bool isLessEqual{p1 <= p2};
    const bool isGreaterEqual{p3 > p1};
    const bool isGreater{p3 >= p2};
    REQUIRE(isEqual);
    REQUIRE(isNotEqual);
    REQUIRE(isLess);
    REQUIRE(isLessEqual);
    REQUIRE(isGreaterEqual);
    REQUIRE(isGreater);
}

TEST_CASE("SCompressedPair: Swap", "[SCompressedPair]")
{
    DeerContainer::SCompressedPair<int, std::string> p1{1, "One"};
    DeerContainer::SCompressedPair<int, std::string> p2{2, "Two"};
    p1.swap(p2);

    REQUIRE(p1.first == 2);
    REQUIRE(p1.second == "Two");
    REQUIRE(p2.first == 1);
    REQUIRE(p2.second == "One");
}

TEST_CASE("SCompressedPair: std::get Access", "[SCompressedPair]")
{
    DeerContainer::SCompressedPair<int, std::string> p{42, "Answer"};

    REQUIRE(std::get<0>(p) == 42);
    REQUIRE(std::get<1>(p) == "Answer");

    std::get<0>(p) = 99;
    std::get<1>(p) = "Updated";

    REQUIRE(p.first == 99);
    REQUIRE(p.second == "Updated");
}

TEST_CASE("SCompressedPair: tuple_size and tuple_element", "[SCompressedPair]")
{
    using P = DeerContainer::SCompressedPair<int, double>;
    REQUIRE(std::tuple_size<P>::value == 2);

    const std::tuple_element_t<0, P> i = 42;
    const std::tuple_element_t<1, P> d = 3.14;
    REQUIRE(i == 42);
    REQUIRE(d == Catch::Approx(3.14));
}
