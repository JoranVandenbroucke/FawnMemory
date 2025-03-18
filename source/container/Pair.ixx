//
// Copyright (c) 2025.
// Author: Joran Vandenbroucke.
//

module;
#include <compare>
#include <cstddef>
#include <format>
#include <tuple>
#include <type_traits>
#include <utility>

export module DeerContainer:Pair;

namespace DeerContainer
{

export template <class T1, class T2>
struct SPair
{
    using first_type  = T1;
    using second_type = T2;
    first_type  first;
    second_type second;

    constexpr explicit(!std::is_default_constructible_v<first_type> || !std::is_default_constructible_v<second_type>) SPair() = default;
    constexpr explicit(!std::is_convertible_v<const first_type&, first_type> || !std::is_convertible_v<const second_type&, second_type>)
        SPair(const first_type& x, const second_type& y)
        requires(std::is_copy_constructible_v<first_type> && std::is_copy_constructible_v<second_type>)
        : first{x}, second{y}
    {
    }
    template <typename U1 = first_type, typename U2 = second_type>
    constexpr explicit(!std::is_convertible_v<U1, first_type> || !std::is_convertible_v<U2, second_type>) SPair(U1&& x, U2&& y)
        requires(std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>)
        : first{std::forward<U1>(x)}, second{std::forward<U2>(y)}
    {
    }
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<U1&, first_type> || !std::is_convertible_v<U2&, second_type>) SPair(SPair<U1, U2>& p)
        requires(std::is_constructible_v<first_type, U1&> && std::is_constructible_v<second_type, U2&>)
        : first{p.first}, second{p.second}
    {
    }
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<const U1&, first_type> || !std::is_convertible_v<const U2&, second_type>)
        SPair(const SPair<U1, U2>& p)
        requires(std::is_constructible_v<first_type, const U1&> && std::is_constructible_v<second_type, const U2&>)
        : first{p.first}, second{p.second}
    {
    }
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<U1, first_type> || !std::is_convertible_v<U2, second_type>) SPair(SPair<U1, U2>&& p)
        requires(std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>)
        : first{std::forward<U1>(p.first)}, second{std::forward<U2>(p.second)}
    {
    }
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<const U1, first_type> || !std::is_convertible_v<const U2, second_type>)
        SPair(const SPair<U1, U2>&& p)
        requires(std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>)
        : first{std::forward<const U1>(p.first)}, second{std::forward<const U2>(p.second)}
    {
    }
    template <typename P>
    constexpr explicit(!std::is_convertible_v<decltype(std::get<0>(std::declval<P>())), first_type>
                       || !std::is_convertible_v<decltype(std::get<1>(std::declval<P>())), second_type>) SPair(P&& p)
        requires(std::is_constructible_v<first_type, decltype(std::get<0>(std::declval<P>()))>
                 && std::is_constructible_v<second_type, decltype(std::get<1>(std::declval<P>()))>)
        : first{std::get<0>(std::forward_as_tuple(p))}, second{std::get<1>(std::forward_as_tuple(p))}
    {
    }

    template <typename... U1, typename... U2>
    constexpr SPair(std::piecewise_construct_t /**/, std::tuple<U1...> first_arg, std::tuple<U2...> second_arg) :
        first{std::make_from_tuple<first_type>(std::move(first_arg))}, second{std::make_from_tuple<second_type>(std::move(second_arg))}
    {
    }

    constexpr SPair(const SPair&) = default;
    constexpr SPair(SPair&&)      = default;

    constexpr SPair& operator=(const SPair& other)
        requires(std::is_copy_assignable_v<first_type> && std::is_copy_assignable_v<second_type>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    constexpr const SPair& operator=(const SPair& other) const
        requires(std::is_copy_assignable_v<const first_type> && std::is_copy_assignable_v<const second_type>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    template <typename U1, typename U2>
    constexpr SPair& operator=(const SPair<U1, U2>& other)
        requires(std::is_assignable_v<first_type&, const U1&> && std::is_assignable_v<second_type&, const U2&>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    template <typename U1, typename U2>
    constexpr const SPair& operator=(const SPair<U1, U2>& other) const
        requires(std::is_assignable_v<const first_type&, const U1&> && std::is_assignable_v<const second_type&, const U2&>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    constexpr SPair& operator=(SPair&& other) noexcept(std::is_nothrow_move_assignable_v<first_type>
                                                       && std::is_nothrow_move_assignable_v<second_type>)
        requires(std::is_move_assignable_v<first_type> && std::is_move_assignable_v<second_type>)
    {
        first  = std::move(other.first);
        second = std::move(other.second);
        return *this;
    }

    constexpr const SPair& operator=(SPair&& other) const
        requires(std::is_assignable_v<const first_type&, first_type> && std::is_assignable_v<const second_type&, second_type>)
    {
        first  = std::move(other.first);
        second = std::move(other.second);
        return *this;
    }

    template <typename U1, typename U2>
    constexpr SPair& operator=(SPair<U1, U2>&& other)
        requires(std::is_assignable_v<first_type&, U1> && std::is_assignable_v<second_type&, U2>)
    {
        first  = std::forward<U1>(other.first);
        second = std::forward<U2>(other.second);
        return *this;
    }

    template <typename U1, typename U2>
    constexpr const SPair& operator=(SPair<U1, U2>&& other) const
        requires(std::is_assignable_v<const first_type&, U1> && std::is_assignable_v<const second_type&, U2>)
    {
        first  = std::forward<U1>(other.first);
        second = std::forward<U2>(other.second);
        return *this;
    }

    template <typename P>
    constexpr SPair& operator=(P&& other)
        requires(!std::same_as<std::remove_cvref_t<P>, SPair>
                 && std::is_assignable_v<first_type&, decltype(std::get<0>(std::forward<P>(other)))>
                 && std::is_assignable_v<second_type&, decltype(std::get<1>(std::forward<P>(other)))>)
    {
        first  = std::get<0>(std::forward<P>(other));
        second = std::get<1>(std::forward<P>(other));
        return *this;
    }

    template <typename P>
    constexpr const SPair& operator=(P&& other) const
        requires(!std::same_as<std::remove_cvref_t<P>, SPair>
                 && std::is_assignable_v<const first_type&, decltype(std::get<0>(std::forward<P>(other)))>
                 && std::is_assignable_v<const second_type&, decltype(std::get<1>(std::forward<P>(other)))>)
    {
        first  = std::get<0>(std::forward<P>(other));
        second = std::get<1>(std::forward<P>(other));
        return *this;
    }

    constexpr void swap(SPair& other) noexcept(std::is_nothrow_swappable_v<first_type> && std::is_nothrow_swappable_v<second_type>)
        requires(!std::is_swappable_v<first_type> || std::is_swappable_v<second_type>)
    {
        using std::swap;
        swap(first, other.first);
        swap(second, other.second);
    }
    constexpr void swap(const SPair& other) const
        noexcept(std::is_nothrow_swappable_v<const first_type> && std::is_nothrow_swappable_v<const second_type>)
        requires(!std::is_swappable_v<const first_type> || std::is_swappable_v<const second_type>)
    {
        using std::swap;
        swap(first, other.first);
        swap(second, other.second);
    }
};
export template <class T1, class T2>
constexpr SPair<std::unwrap_ref_decay_t<T1>, std::unwrap_ref_decay_t<T2>> make_pair(T1&& x, T2&& y)
{
    return SPair<std::unwrap_ref_decay_t<T1>, std::unwrap_ref_decay_t<T2>>(std::forward<T1>(x), std::forward<T2>(y));
}
} // namespace DeerContainer

export template <class T1, class T2, class U1, class U2>
    requires(std::is_convertible_v<decltype(std::declval<T1>() == std::declval<U1>()), bool>
             && std::is_convertible_v<decltype(std::declval<T2>() == std::declval<U2>()), bool>)
constexpr bool operator==(const DeerContainer::SPair<T1, T2>& lhs, const DeerContainer::SPair<U1, U2>& rhs)
{
    return (lhs.first == rhs.first) && (lhs.second == rhs.second);
}
export template <class T1, class T2, class U1, class U2>
constexpr auto operator<=>(const DeerContainer::SPair<T1, T2>& lhs, const DeerContainer::SPair<U1, U2>& rhs)
{
    if (const auto val{lhs.first <=> rhs.first}; val != 0)
    {
        return val;
    }
    return lhs.second <=> rhs.second;
}

namespace std
{
export template <class T1, class T2>
    requires(std::is_swappable_v<T1> && std::is_swappable_v<T2>)
constexpr void swap(DeerContainer::SPair<T1, T2>& x, DeerContainer::SPair<T1, T2>& y) noexcept(noexcept(x.swap(y)))
{
    x.swap(y);
}
export template <class T1, class T2>
    requires(std::is_swappable_v<const T1> && std::is_swappable_v<const T2>)
constexpr void swap(const DeerContainer::SPair<T1, T2>& x, const DeerContainer::SPair<T1, T2>& y) noexcept(noexcept(x.swap(y)))
{
    x.swap(y);
}

export template <std::size_t I, class T1, class T2>
constexpr typename std::tuple_element<I, DeerContainer::SPair<T1, T2>>::type& get(DeerContainer::SPair<T1, T2>& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SPair)");
    if constexpr (I == 0)
    {
        return p.first;
    }
    else
    {
        return p.second;
    }
}

export template <std::size_t I, class T1, class T2>
constexpr const typename std::tuple_element<I, DeerContainer::SPair<T1, T2>>::type& get(const DeerContainer::SPair<T1, T2>& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SPair)");
    if constexpr (I == 0)
    {
        return p.first;
    }
    else
    {
        return p.second;
    }
}

export template <std::size_t I, class T1, class T2>
constexpr typename std::tuple_element<I, DeerContainer::SPair<T1, T2>>::type&& get(DeerContainer::SPair<T1, T2>&& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SPair)");
    if constexpr (I == 0)
    {
        return std::move(p.first);
    }
    else
    {
        return std::move(p.second);
    }
}

export template <std::size_t I, class T1, class T2>
constexpr const typename std::tuple_element<I, DeerContainer::SPair<T1, T2>>::type&& get(const DeerContainer::SPair<T1, T2>&& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SPair)");
    if constexpr (I == 0)
    {
        return std::move(p.first);
    }
    else
    {
        return std::move(p.second);
    }
}

export template <class T, class U>
constexpr T& get(DeerContainer::SPair<T, U>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return p.first;
}

export template <class T, class U>
constexpr const T& get(const DeerContainer::SPair<T, U>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return p.first;
}

export template <class T, class U>
constexpr T&& get(DeerContainer::SPair<T, U>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return std::move(p.first);
}

export template <class T, class U>
constexpr const T&& get(const DeerContainer::SPair<T, U>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return std::move(p.first);
}

export template <class T, class U>
constexpr T& get(DeerContainer::SPair<U, T>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return p.second;
}

export template <class T, class U>
constexpr const T& get(const DeerContainer::SPair<U, T>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return p.second;
}

export template <class T, class U>
constexpr T&& get(DeerContainer::SPair<U, T>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return std::move(p.second);
}

export template <class T, class U>
constexpr const T&& get(const DeerContainer::SPair<U, T>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SPair), T and U cannot be the same");
    return std::move(p.second);
}

export template <class T1, class T2>
struct tuple_size<DeerContainer::SPair<T1, T2>> : std::integral_constant<std::size_t, 2>
{
};

export template <std::size_t I, class T1, class T2>
struct tuple_element<I, DeerContainer::SPair<T1, T2>>
{
    static_assert(I < 2, "DeerContainer::SPair has only 2 elements!");
};

export template <class T1, class T2>
struct tuple_element<0, DeerContainer::SPair<T1, T2>>
{
    using type = T1;
};
export template <class T1, class T2>
struct tuple_element<1, DeerContainer::SPair<T1, T2>>
{
    using type = T2;
};

export template <class T1, class T2, class U1, class U2, template <class> class TQual, template <class> class UQual>
    requires requires {
        typename DeerContainer::SPair<std::common_reference_t<TQual<T1>, UQual<U1>>, std::common_reference_t<TQual<T2>, UQual<U2>>>;
    }
struct basic_common_reference<DeerContainer::SPair<T1, T2>, DeerContainer::SPair<U1, U2>, TQual, UQual>
{
    using type = DeerContainer::SPair<std::common_reference_t<TQual<T1>, UQual<U1>>, std::common_reference_t<TQual<T2>, UQual<U2>>>;
};
export template <class T1, class T2, class U1, class U2>

    requires requires { typename DeerContainer::SPair<std::common_type_t<T1, U1>, std::common_type_t<T2, U2>>; }
struct common_type<DeerContainer::SPair<T1, T2>, DeerContainer::SPair<U1, U2>>
{
    using type = DeerContainer::SPair<std::common_type_t<T1, U1>, std::common_type_t<T2, U2>>;
};

export template <class CharT, std::formattable<CharT>... Ts>
struct formatter<DeerContainer::SPair<Ts...>, CharT>
{
    constexpr void set_separator(std::basic_string_view<CharT> sep) noexcept
    {
        separator_ = sep;
    }
    constexpr void set_brackets(std::basic_string_view<CharT> opening, std::basic_string_view<CharT> closing) noexcept
    {
        opening_bracket_ = opening;
        closing_bracket_ = closing;
    }
    template <class ParseContext>
    constexpr auto parse(ParseContext& ctx) -> typename ParseContext::iterator
    {
        auto it  = ctx.begin();
        auto end = ctx.end();

        // Parse any custom format specifiers here (e.g., changing separators or brackets)
        if (it != end && *it != closing_bracket_)
        {
            // Example: parse a single character as a separator
            set_separator(std::basic_string_view<CharT>(&*it, 1));
            ++it;
        }

        // Parse format specifiers for underlying types
        if (it != end && *it == separator_)
        {
            ++it;
            it = std::get<0>(underlying_).parse(ctx);
            it = std::get<1>(underlying_).parse(ctx);
        }

        // Ensure the parse context is valid
        if (it != end && *it != closing_bracket_)
        {
            throw std::format_error("Invalid format");
        }

        return it;
    }

    template <class FormatContext>
    auto format(const DeerContainer::SPair<Ts...>& pair, FormatContext& ctx) const -> typename FormatContext::iterator
    {
        auto out = ctx.out();

        // Write opening bracket
        out = std::copy(opening_bracket_.begin(), opening_bracket_.end(), out);

        // Format and write the first element
        out = std::get<0>(underlying_).format(pair.first, ctx);

        // Write separator
        out = std::copy(separator_.begin(), separator_.end(), out);

        // Format and write the second element
        out = std::get<1>(underlying_).format(pair.second, ctx);

        // Write closing bracket
        out = std::copy(closing_bracket_.begin(), closing_bracket_.end(), out);

        return out;
    }

  private:
    std::tuple<std::formatter<std::remove_cvref_t<Ts>, CharT>...> underlying_;
    std::basic_string_view<CharT>                                 separator_       = ", ";
    std::basic_string_view<CharT>                                 opening_bracket_ = "(";
    std::basic_string_view<CharT>                                 closing_bracket_ = ")";
};
} // namespace std
