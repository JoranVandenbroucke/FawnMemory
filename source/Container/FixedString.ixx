//
// Copyright (c) 2025.
// Author: Joran Vandenbroucke.
//

module;
#include <string_view>
#include <algorithm>
#include <cstddef>
#include <string>

export module DeerContainer:FexedString;

export template <std::size_t N>
class CFixedString
{
  public:
    constexpr CFixedString() = default;
    constexpr CFixedString(const char* str)
    {
        std::copy_n(str, std::char_traits<char>::length(str), m_data);
    }
    [[nodiscard]] constexpr std::size_t size() const noexcept
    {
        return N;
    }
    std::string_view view() const noexcept
    {
        return {m_data, N};
    }

  private:
    char m_data[N]{};
};

export template <std::size_t N>
constexpr auto make_fixed_string(const char (&str)[N])
{
    return CFixedString<N>(str);
}
