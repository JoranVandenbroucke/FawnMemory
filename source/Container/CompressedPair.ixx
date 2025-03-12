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

// todo : put this in the config/compiler.hpp from other projects
#if defined(_MSC_VER) && (_MSC_VER >= 1920) // Visual Studio 2019 and later
#define NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#elif defined(__has_cpp_attribute)
#if __has_cpp_attribute(no_unique_address)
#define NO_UNIQUE_ADDRESS [[no_unique_address]]
#else
#define NO_UNIQUE_ADDRESS
#endif
#else
#define NO_UNIQUE_ADDRESS
#endif

export module DeerContainer:CompressedPair;

namespace DeerContainer
{
/**
 * @brief A lightweight pair container with empty base optimization, designed for efficient memory usage.
 *
 * @details `SCompressedPair` is a utility class designed to store two heterogeneous objects (`first` and `second`)
 * with potential empty base optimization using `[[msvc::no_unique_address]]`. It is a compact alternative to
 * `std::pair`, particularly beneficial when either type is an empty class, saving memory space by avoiding
 * redundant storage.
 *
 * This class supports various constructors for different initialization scenarios, including copy, move,
 * and perfect forwarding. It also provides assignment operators and swap functionality for seamless value
 * manipulation and interoperability.
 *
 * @tparam T1 Type of the first object in the pair.
 * @tparam T2 Type of the second object in the pair.
 *
 * @note `SCompressedPair` leverages C++20 features such as `requires` and `explicit` specifiers for enhanced
 * type safety and flexibility. It is particularly useful when working with custom containers like `CDenseMap`
 * to optimize memory layout and cache locality.
 *
 * @warning `SCompressedPair` is not inherently thread-safe. Use appropriate synchronization mechanisms if
 * accessing from multiple threads.
 *
 * @see [std::pair](https://en.cppreference.com/w/cpp/utility/pair)
 */
export template <class T1, class T2>
struct SCompressedPair
{
    using first_type  = T1; ///< Type of the first element.
    using second_type = T2; ///< Type of the second element.

    NO_UNIQUE_ADDRESS first_type  first;  ///< First object in the pair.
    NO_UNIQUE_ADDRESS second_type second; ///< Second object in the pair.

    /**
     * @brief Default constructor. Constructs both elements using their default constructors.
     */
    constexpr explicit(!std::is_default_constructible_v<first_type> || !std::is_default_constructible_v<second_type>)
        SCompressedPair() = default;

    /**
     * @brief Copy constructor for lvalue references.
     * @param x The first element to copy.
     * @param y The second element to copy.
     */
    constexpr explicit(!std::is_convertible_v<const first_type&, first_type> || !std::is_convertible_v<const second_type&, second_type>)
        SCompressedPair(const first_type& x, const second_type& y)
        requires(std::is_copy_constructible_v<first_type> && std::is_copy_constructible_v<second_type>)
        : first{x}, second{y}
    {
    }

    /**
     * @brief Perfect-forwarding constructor for rvalue references.
     * @tparam U1 Type of the first argument.
     * @tparam U2 Type of the second argument.
     * @param x The first element to forward.
     * @param y The second element to forward.
     */
    template <typename U1 = first_type, typename U2 = second_type>
    constexpr explicit(!std::is_convertible_v<U1, first_type> || !std::is_convertible_v<U2, second_type>) SCompressedPair(U1&& x, U2&& y)
        requires(std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>)
        : first{std::forward<U1>(x)}, second{std::forward<U2>(y)}
    {
    }

    /**
     * @brief Perfect-forwarding constructor for rvalue references.
     * @tparam U1 Type of the first argument.
     * @tparam U2 Type of the second argument.
     * @param x The first element to assign.
     * @param y The second element to assign.
     */
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<U1&, first_type> || !std::is_convertible_v<U2&, second_type>)
        SCompressedPair(SCompressedPair<U1, U2>& p)
        requires(std::is_constructible_v<first_type, U1&> && std::is_constructible_v<second_type, U2&>)
        : first{p.first}, second{p.second}
    {
    }

    /**
     * @brief Perfect-forwarding constructor for rvalue references.
     * @tparam U1 Type of the first argument.
     * @tparam U2 Type of the second argument.
     * @param x The first element to assign.
     * @param y The second element to assign.
     */
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<const U1&, first_type> || !std::is_convertible_v<const U2&, second_type>)
        SCompressedPair(const SCompressedPair<U1, U2>& p)
        requires(std::is_constructible_v<first_type, const U1&> && std::is_constructible_v<second_type, const U2&>)
        : first{p.first}, second{p.second}
    {
    }

    /**
     * @brief Perfect-forwarding constructor for rvalue references.
     * @tparam U1 Type of the first argument.
     * @tparam U2 Type of the second argument.
     * @param x The first element to forward.
     * @param y The second element to forward.
     */
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<U1, first_type> || !std::is_convertible_v<U2, second_type>)
        SCompressedPair(SCompressedPair<U1, U2>&& p)
        requires(std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>)
        : first{std::forward<U1>(p.first)}, second{std::forward<U2>(p.second)}
    {
    }

    /**
     * @brief Perfect-forwarding constructor for rvalue references.
     * @tparam U1 Type of the first argument.
     * @tparam U2 Type of the second argument.
     * @param x The first element to forward.
     * @param y The second element to forward.
     */
    template <typename U1, typename U2>
    constexpr explicit(!std::is_convertible_v<const U1, first_type> || !std::is_convertible_v<const U2, second_type>)
        SCompressedPair(const SCompressedPair<U1, U2>&& p)
        requires(std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>)
        : first{std::forward<const U1>(p.first)}, second{std::forward<const U2>(p.second)}
    {
    }

    /**
     * @brief Perfect-forwarding constructor for rvalue references.
     * @tparam P Type of a pair like object.
     * @param p The pair like object to forward.
     */
    template <typename P>
    constexpr explicit(!std::is_convertible_v<decltype(std::get<0>(std::declval<P>())), first_type>
                       || !std::is_convertible_v<decltype(std::get<1>(std::declval<P>())), second_type>) SCompressedPair(P&& p)
        requires(std::is_constructible_v<first_type, decltype(std::get<0>(std::declval<P>()))>
                 && std::is_constructible_v<second_type, decltype(std::get<1>(std::declval<P>()))>)
        : first{std::get<0>(std::forward(p))}, second{std::get<1>(std::forward(p))}
    {
    }

    /**
     * @brief Perfect-forwarding constructor for rvalue references.
     * @tparam P Type of a pair like object.
     * @param p The pair like object to forward.
     */
    template <typename... U1, typename... U2>
    constexpr SCompressedPair(std::piecewise_construct_t /**/, std::tuple<U1...> first_arg, std::tuple<U2...> second_arg) :
        first{std::forward_as_tuple(first_arg)}, second{std::forward_as_tuple(second_arg)}
    {
    }

    constexpr SCompressedPair(const SCompressedPair&) = default;
    constexpr SCompressedPair(SCompressedPair&&)      = default;

    /**
     * @brief Copy assignment operator.
     * @param other The pair to copy from.
     * @return Reference to the assigned pair.
     */
    constexpr SCompressedPair& operator=(const SCompressedPair& other)
        requires(std::is_copy_assignable_v<first_type> && std::is_copy_assignable_v<second_type>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    /**
     * @brief Copy assignment operator.
     * @param other The pair to copy from.
     * @return Reference to the assigned pair.
     */
    constexpr const SCompressedPair& operator=(const SCompressedPair& other) const
        requires(std::is_copy_assignable_v<const first_type> && std::is_copy_assignable_v<const second_type>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    /**
     * @brief Copy assignment operator.
     * @param other The pair to copy from.
     * @return Reference to the assigned pair.
     */
    template <typename U1, typename U2>
    constexpr SCompressedPair& operator=(const SCompressedPair<U1, U2>& other)
        requires(std::is_assignable_v<first_type&, const U1&> && std::is_assignable_v<second_type&, const U2&>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    /**
     * @brief Copy assignment operator.
     * @param other The pair to copy from.
     * @return Reference to the assigned pair.
     */
    template <typename U1, typename U2>
    constexpr const SCompressedPair& operator=(const SCompressedPair<U1, U2>& other) const
        requires(std::is_assignable_v<const first_type&, const U1&> && std::is_assignable_v<const second_type&, const U2&>)
    {
        first  = other.first;
        second = other.second;
        return *this;
    }

    /**
     * @brief Move assignment operator.
     * @param other The pair to move from.
     * @return Reference to the assigned pair.
     */
    constexpr SCompressedPair& operator=(SCompressedPair&& other) noexcept(std::is_nothrow_move_assignable_v<first_type>
                                                                           && std::is_nothrow_move_assignable_v<second_type>)
        requires(std::is_move_assignable_v<first_type> && std::is_move_assignable_v<second_type>)
    {
        first  = std::move(other.first);
        second = std::move(other.second);
        return *this;
    }

    /**
     * @brief Move assignment operator.
     * @param other The pair to move from.
     * @return Reference to the assigned pair.
     */
    constexpr const SCompressedPair& operator=(SCompressedPair&& other) const
        requires(std::is_assignable_v<const first_type&, first_type> && std::is_assignable_v<const second_type&, second_type>)
    {
        first  = std::move(other.first);
        second = std::move(other.second);
        return *this;
    }

    /**
     * @brief Move assignment operator.
     * @tparam U1 The type of the first element
     * @tparam U2 The type of the second element
     * @param other The pair to move from.
     * @return Reference to the assigned pair.
     */
    template <typename U1, typename U2>
    constexpr SCompressedPair& operator=(SCompressedPair<U1, U2>&& other)
        requires(std::is_assignable_v<first_type&, U1> && std::is_assignable_v<second_type&, U2>)
    {
        first  = std::forward<U1>(other.first);
        second = std::forward<U2>(other.second);
        return *this;
    }

    /**
     * @brief Move assignment operator.
     * @tparam U1 The type of the first element
     * @tparam U2 The type of the second element
     * @param other The pair to move from.
     * @return Reference to the assigned pair.
     */
    template <typename U1, typename U2>
    constexpr const SCompressedPair& operator=(SCompressedPair<U1, U2>&& other) const
        requires(std::is_assignable_v<const first_type&, U1> && std::is_assignable_v<const second_type&, U2>)
    {
        first  = std::forward<U1>(other.first);
        second = std::forward<U2>(other.second);
        return *this;
    }

    /**
     * @brief Move assignment operator.
     * @tparam P A pair like type.
     * @param other The pair to move from.
     * @return Reference to the assigned pair.
     */
    template <typename P>
    constexpr SCompressedPair& operator=(P&& other)
        requires(!std::same_as<std::remove_cvref_t<P>, SCompressedPair>
                 && std::is_assignable_v<first_type&, decltype(std::get<0>(std::forward<P>(other)))>
                 && std::is_assignable_v<second_type&, decltype(std::get<1>(std::forward<P>(other)))>)
    {
        first  = std::get<0>(std::forward<P>(other));
        second = std::get<1>(std::forward<P>(other));
        return *this;
    }

    /**
     * @brief Move assignment operator.
     * @tparam P A pair like type.
     * @param other The pair to move from.
     * @return Reference to the assigned pair.
     */
    template <typename P>
    constexpr const SCompressedPair& operator=(P&& other) const
        requires(!std::same_as<std::remove_cvref_t<P>, SCompressedPair>
                 && std::is_assignable_v<const first_type&, decltype(std::get<0>(std::forward<P>(other)))>
                 && std::is_assignable_v<const second_type&, decltype(std::get<1>(std::forward<P>(other)))>)
    {
        first  = std::get<0>(std::forward<P>(other));
        second = std::get<1>(std::forward<P>(other));
        return *this;
    }

    /**
     * @brief Swaps the contents of this pair with another.
     * @param other The other pair to swap with.
     */
    constexpr void swap(SCompressedPair& other) noexcept(std::is_nothrow_swappable_v<first_type>
                                                         && std::is_nothrow_swappable_v<second_type>)
        requires(!std::is_swappable_v<first_type> || std::is_swappable_v<second_type>)
    {
        using std::swap;
        swap(first, other.first);
        swap(second, other.second);
    }
};

/**
 * @brief Utility function to create an `SCompressedPair`.
 * @tparam T1 Type of the first object.
 * @tparam T2 Type of the second object.
 * @param x The first object to forward.
 * @param y The second object to forward.
 * @return An instance of `SCompressedPair`.
 */
export template <class T1, class T2>
constexpr SCompressedPair<std::unwrap_ref_decay_t<T1>, std::unwrap_ref_decay_t<T2>> make_compressed_pair(T1&& x, T2&& y)
{
    return SCompressedPair<std::unwrap_ref_decay_t<T1>, std::unwrap_ref_decay_t<T2>>(std::forward<T1>(x), std::forward<T2>(y));
}
} // namespace DeerContainer

export template <class T1, class T2, class U1, class U2>
    requires(std::is_convertible_v<decltype(std::declval<T1>() == std::declval<U1>()), bool>
             && std::is_convertible_v<decltype(std::declval<T2>() == std::declval<U2>()), bool>)
constexpr bool operator==(const DeerContainer::SCompressedPair<T1, T2>& lhs, const DeerContainer::SCompressedPair<U1, U2>& rhs)
{
    return lhs.first == rhs.first && lhs.second == rhs.second;
}
/**
 * @brief Utility function to create an `SCompressedPair`.
 * @tparam T1 Type of the first element of the first object.
 * @tparam T2 Type of the second element of the first object.
 * @tparam U1 Type of the first element of the second object.
 * @tparam U2 Type of the second element of the second object.
 * @param lhs The first object to compare.
 * @param rhs The second object to compare.
 * @return result of three way comparison between lhs and rhs.
 */
export template <class T1, class T2, class U1, class U2>
constexpr std::common_comparison_category_t<std::compare_three_way_result_t<T1, U1>, std::compare_three_way_result_t<T2, U2>>
operator<=>(const DeerContainer::SCompressedPair<T1, T2>& lhs, const DeerContainer::SCompressedPair<U1, U2>& rhs)
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
constexpr void swap(DeerContainer::SCompressedPair<T1, T2>& x, DeerContainer::SCompressedPair<T1, T2>& y) noexcept(noexcept(x.swap(y)))
{
    x.swap(y);
}

export template <std::size_t I, class T1, class T2>
constexpr typename std::tuple_element<I, DeerContainer::SCompressedPair<T1, T2>>::type&
get(DeerContainer::SCompressedPair<T1, T2>& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SCompressedPair)");
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
constexpr const typename std::tuple_element<I, DeerContainer::SCompressedPair<T1, T2>>::type&
get(const DeerContainer::SCompressedPair<T1, T2>& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SCompressedPair)");
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
constexpr typename std::tuple_element<I, DeerContainer::SCompressedPair<T1, T2>>::type&&
get(DeerContainer::SCompressedPair<T1, T2>&& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SCompressedPair)");
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
constexpr const typename std::tuple_element<I, DeerContainer::SCompressedPair<T1, T2>>::type&&
get(const DeerContainer::SCompressedPair<T1, T2>&& p) noexcept
{
    static_assert(I <= 1, "Index is bigger than 1. Only values 0, and 1 are allowed When calling std::get<>(SCompressedPair)");
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
constexpr T& get(DeerContainer::SCompressedPair<T, U>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return p.first;
}

export template <class T, class U>
constexpr const T& get(const DeerContainer::SCompressedPair<T, U>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return p.first;
}

export template <class T, class U>
constexpr T&& get(DeerContainer::SCompressedPair<T, U>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return std::move(p.first);
}

export template <class T, class U>
constexpr const T&& get(const DeerContainer::SCompressedPair<T, U>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return std::move(p.first);
}

export template <class T, class U>
constexpr T& get(DeerContainer::SCompressedPair<U, T>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return p.second;
}

export template <class T, class U>
constexpr const T& get(const DeerContainer::SCompressedPair<U, T>& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return p.second;
}

export template <class T, class U>
constexpr T&& get(DeerContainer::SCompressedPair<U, T>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return std::move(p.second);
}

export template <class T, class U>
constexpr const T&& get(const DeerContainer::SCompressedPair<U, T>&& p) noexcept
{
    static_assert(!std::is_same_v<T, U>, "When calling get<calss T, class U>(SCompressedPair), T and U cannot be the same");
    return std::move(p.second);
}

export template <class T1, class T2>
struct tuple_size<DeerContainer::SCompressedPair<T1, T2>> : std::integral_constant<std::size_t, 2>
{
};

export template <std::size_t I, class T1, class T2>
struct tuple_element<I, DeerContainer::SCompressedPair<T1, T2>>
{
    static_assert(I < 2, "DeerContainer::SCompressedPair has only 2 elements!");
};

export template <class T1, class T2>
struct tuple_element<0, DeerContainer::SCompressedPair<T1, T2>>
{
    using type = T1;
};
export template <class T1, class T2>
struct tuple_element<1, DeerContainer::SCompressedPair<T1, T2>>
{
    using type = T2;
};

export template <class T1, class T2, class U1, class U2, template <class> class TQual, template <class> class UQual>
    requires requires {
        typename DeerContainer::SCompressedPair<std::common_reference_t<TQual<T1>, UQual<U1>>,
                                                std::common_reference_t<TQual<T2>, UQual<U2>>>;
    }
struct basic_common_reference<DeerContainer::SCompressedPair<T1, T2>, DeerContainer::SCompressedPair<U1, U2>, TQual, UQual>
{
    using type =
        DeerContainer::SCompressedPair<std::common_reference_t<TQual<T1>, UQual<U1>>, std::common_reference_t<TQual<T2>, UQual<U2>>>;
};
export template <class T1, class T2, class U1, class U2>

    requires requires { typename DeerContainer::SCompressedPair<std::common_type_t<T1, U1>, std::common_type_t<T2, U2>>; }
struct common_type<DeerContainer::SCompressedPair<T1, T2>, DeerContainer::SCompressedPair<U1, U2>>
{
    using type = DeerContainer::SCompressedPair<std::common_type_t<T1, U1>, std::common_type_t<T2, U2>>;
};

export template <class CharT, std::formattable<CharT>... Ts>
struct formatter<DeerContainer::SCompressedPair<Ts...>, CharT>
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
    auto format(const DeerContainer::SCompressedPair<Ts...>& pair, FormatContext& ctx) const -> typename FormatContext::iterator
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
