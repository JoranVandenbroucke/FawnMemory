//
// Copyright (c) 2025.
// Author: Joran Vandenbroucke.
//

#pragma once
#include <ranges>

namespace DeerContainer
{
template <class Range, class T>
concept ContainerCompatibleRange = std::ranges::input_range<Range> && std::convertible_to<std::ranges::range_reference_t<Range>, T>;
} // namespace DeerContainer
