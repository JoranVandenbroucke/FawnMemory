//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

export module DeerContainer.Pair;

namespace DeerContainer
{
template<typename Type>
concept PairLike = requires
        {
            typename std::tuple_size<Type>::type;
        } && std::tuple_size_v<Type> == 2 &&
        (
            std::same_as<Type, std::pair<typename Type::first_type, typename Type::second_type>> ||// std::SPair specialization
            requires { typename std::tuple_element_t<0, Type>; } ||                                // std::tuple specialization
            std::same_as<Type, std::array<typename Type::value_type, 2>> ||                        // std::array of size 2
            std::ranges::range<Type>                                                               // std::ranges::subrange with 2 elements
        );

template<class Type, char Element, bool CanBeEmptyBase = std::is_empty_v<Type> && !std::is_final_v<Type>>
struct SPairElement
{
    using value_type = Type;
    using reference = Type&;
    using const_reference = const Type&;

    constexpr SPairElement() noexcept(std::is_nothrow_constructible_v<Type>) requires std::is_default_constructible_v<Type>
        : m_value{}
    {
    }

    template<typename P>
        requires ( !std::is_same_v<SPairElement, std::decay_t<P>> )
    explicit constexpr SPairElement( P&& value ) noexcept(std::is_nothrow_constructible_v<Type, P>)
        : m_value{std::forward<P>( value )}
    {
    }

    template<typename... Args, std::size_t... Indices>
    constexpr explicit SPairElement( std::piecewise_construct_t /*piecewise_construct*/, std::tuple<Args...> args, std::index_sequence<Indices...> /*indices*/ ) noexcept(std::is_nothrow_constructible_v<Type, Args...>)
        : m_value( std::forward<Args>( std::get<Indices>( args ) )... )
    {
    }

    constexpr auto get() noexcept -> reference
    {
        return m_value;
    }

    constexpr auto get() const noexcept -> const_reference
    {
        return m_value;
    }

private:
    Type m_value;
};

template<class Type, char Element>
struct SPairElement<Type, Element, true> : Type
{
    using value_type = Type;
    using reference = Type&;
    using const_reference = const Type&;

    constexpr SPairElement() = default;

    constexpr SPairElement() noexcept(std::is_nothrow_constructible_v<Type>) requires std::is_default_constructible_v<Type>
        : Type{}
    {
    }

    template<typename P>
        requires ( !std::is_same_v<SPairElement, std::decay<P>> )
    explicit constexpr SPairElement( P&& value ) noexcept(std::is_nothrow_constructible_v<Type, P>)
        : Type{std::forward<P>( value )}
    {
    }

    template<typename... Args, std::size_t... Indices>
    constexpr explicit SPairElement( std::piecewise_construct_t /*piecewise_construct*/, std::tuple<Args...> args, std::index_sequence<Indices...> /*indices*/ ) noexcept(std::is_nothrow_constructible_v<Type, Args...>)
        : Type( std::forward<Args>( std::get<Indices>( args ) )... )
    {
    }

    constexpr auto get() noexcept -> reference
    {
        return *this;
    }

    constexpr auto get() const noexcept -> const_reference
    {
        return *this;
    }
};

export template<class Type1, class Type2>
struct SCompressedPair : SPairElement<Type1, 0>, private SPairElement<Type2, 1>
{
    using first_type = SPairElement<Type1, 0>;
    using second_type = SPairElement<Type2, 1>;

    using first_value_type = Type1;
    using second_value_type = Type2;

    constexpr explicit(std::is_default_constructible_v<first_type> && std::is_default_constructible_v<second_type>)
        SCompressedPair()
            noexcept(std::is_nothrow_constructible_v<first_type> && std::is_nothrow_constructible_v<second_type>)
            requires( std::is_default_constructible_v<first_type> && std::is_default_constructible_v<second_type> )
        : first_type()
        , second_type()
    {
    }

    constexpr explicit(!( std::is_convertible_v<const first_type&, first_type> && std::is_convertible_v<const second_type&, second_type> ))
        SCompressedPair( const first_type& left, const second_type& right )
            noexcept(std::is_nothrow_copy_constructible_v<first_type> && std::is_nothrow_copy_constructible_v<second_type>)
            requires std::is_default_constructible_v<first_type> && std::is_default_constructible_v<second_type>
        : first_type{left}
        , second_type{right}
    {
    }

    template<class U1 = first_type, class U2 = second_type>
    constexpr explicit(!( std::is_convertible_v<U1, first_type> && std::is_convertible_v<U2, second_type> ))
        SCompressedPair( U1&& left, U2&& right )
            noexcept(std::is_nothrow_constructible_v<first_type, U1&&> && std::is_nothrow_constructible_v<second_type, U2&&>)
            requires std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>

        : first_type{std::forward<U1>( left )}
        , second_type{std::forward<U2>( right )}
    {
    }

    template<class U1, class U2>
    constexpr explicit(!( std::is_convertible_v<U1&, first_type> && std::is_convertible_v<U2&, second_type> ))
        SCompressedPair( SCompressedPair<U1, U2>& SPair )
            noexcept(std::is_nothrow_constructible_v<first_type, U1&> && std::is_nothrow_constructible_v<second_type, U2&>)
            requires std::is_constructible_v<first_type, U1&> && std::is_constructible_v<second_type, U2&>
        : first_type{SPair.first}
        , second_type{SPair.second}
    {
    }

    template<class U1, class U2>
    constexpr explicit(!( std::is_convertible_v<const U1&, first_type> && std::is_convertible_v<const U2&, second_type> ))
        SCompressedPair( const SCompressedPair<U1, U2>& pair )
            noexcept(std::is_nothrow_constructible_v<first_type, const U1&> && std::is_nothrow_constructible_v<second_type, const U2&>)
            requires std::is_constructible_v<first_type, const U1&> && std::is_constructible_v<second_type, const U2&>
        : first_type{pair.first}
        , second_type{pair.second}
    {
    }

    template<class U1, class U2>
        requires std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>
    constexpr explicit(!( std::is_convertible_v<U1, first_type> && std::is_convertible_v<U2, second_type> ))
        SCompressedPair( SCompressedPair<U1, U2>&& pair )
        : first_type{std::forward<U1>( pair.first )}
        , second_type{std::forward<U2>( pair.second )}
    {
    }

    template<class U1, class U2>
    constexpr explicit(!( std::is_convertible_v<const U1&&, first_type> && std::is_convertible_v<const U2&&, second_type> ))
        SCompressedPair( const SCompressedPair<U1, U2>&& pair )
            noexcept(std::is_nothrow_constructible_v<first_type, const U1&&> && std::is_nothrow_constructible_v<second_type, const U2&&>)
            requires std::is_constructible_v<first_type, const U1&&> && std::is_constructible_v<second_type, const U2&&>
        : first_type{std::move( pair.first )}
        , second_type{std::move( pair.second )}
    {
    }

    template<PairLike P>
        requires std::is_constructible_v<first_type, decltype(std::get<0>( std::declval<P&&>() ))> && std::is_constructible_v<second_type, decltype(std::get<1>( std::declval<P&&>() ))>
    constexpr explicit(!( std::is_convertible_v<decltype(std::get<0>( std::declval<P&&>() )), first_type> && std::is_convertible_v<decltype(std::get<1>( std::declval<P&&>() )), second_type> ))
        SCompressedPair( P&& SPair )
        : first_type( std::get<0>( std::forward<P>( SPair ) ) )
        , second_type( std::get<1>( std::forward<P>( SPair ) ) )
    {
    }

    template<class... Args1, class... Args2>
    constexpr SCompressedPair( std::piecewise_construct_t /*piecewise_construct*/, std::tuple<Args1...> first_args, std::tuple<Args2...> second_args )
        : first_type{std::forward<Args1>( std::get<Args1>( first_args ) )...}
        , second_type{std::forward<Args2>( std::get<Args2>( second_args ) )...}
    {
    }

    constexpr auto operator=( const SCompressedPair& other ) -> SCompressedPair& requires std::is_copy_assignable_v<first_type> && std::is_copy_assignable_v<second_type>
    {
        if ( this != &other )
        {
            first() = other.first();
            second() = other.second();
        }
        return *this;
    }

    constexpr auto operator=( const SCompressedPair& ) -> SCompressedPair& requires std::is_copy_assignable_v<const first_type> && std::is_copy_assignable_v<const second_type>
    = default;

    template<class U1, class U2>
    constexpr auto operator=( const SCompressedPair<U1, U2>& other ) -> SCompressedPair& requires std::is_assignable_v<first_type&, const U1&> && std::is_assignable_v<second_type&, const U2&>
    {
        if ( this != &other )
        {
            first() = other.first();
            second() = other.second();
        }
        return *this;
    }

    template<class U1, class U2>
    constexpr auto operator=( const SCompressedPair<U1, U2>& other ) const -> const SCompressedPair& requires std::is_assignable_v<const first_type&, const U1&> && std::is_assignable_v<const second_type&, const U2&>
    {
        if ( this != &other )
        {
            first() = other.first();
            second() = other.second();
        }
        return *this;
    }

    constexpr auto operator=( SCompressedPair&& other )
        noexcept(std::is_nothrow_assignable_v<const first_type&, first_type> && std::is_nothrow_assignable_v<const second_type&, second_type>) -> SCompressedPair& requires std::is_move_assignable_v<first_type&> && std::is_move_assignable_v<
        second_type&>
    {
        first() = std::move( other ).first();
        second() = std::move( other ).second();
        return *this;
    }

    constexpr auto operator=( SCompressedPair&& other ) const
        noexcept(std::is_nothrow_assignable_v<const first_type&, first_type> && std::is_nothrow_assignable_v<const second_type&, second_type>) -> const SCompressedPair& requires std::is_assignable_v<const first_type&, first_type> &&
        std::is_assignable_v
        <const second_type&, second_type>
    {
        first() = std::forward<first_type>( other.first() );
        second() = std::forward<second_type>( other.second() );
        return *this;
    }

    template<class U1, class U2>
    constexpr auto operator=( SCompressedPair<U1, U2>&& other ) -> SCompressedPair& requires std::is_assignable_v<const first_type&, U1> && std::is_assignable_v<const second_type&, U2>
    {
        first() = std::forward<first_type>( other.first() );
        second() = std::forward<second_type>( other.second() );
        return *this;
    }

    template<class U1, class U2>
    constexpr auto operator=( SCompressedPair<U1, U2>&& other ) const -> const SCompressedPair& requires std::is_assignable_v<const first_type&, const U1&> && std::is_assignable_v<const second_type&, const U2&>
    {
        first() = std::forward<first_type>( other.first() );
        second() = std::forward<second_type>( other.second() );
        return *this;
    }

    template<PairLike P>
    constexpr auto operator=( P&& other ) -> SCompressedPair& requires( std::is_assignable_v<first_type&, decltype(std::get<0>( std::declval<P>() ))> && std::is_assignable_v<second_type&, decltype(std::get<1>( std::declval<P>() ))> )
    {
        first() = std::get<0>( std::forward<P>( other ) );
        second() = std::get<1>( std::forward<P>( other ) );
        return *this;
    }

    template<PairLike P>
    constexpr auto operator=( P&& other ) const -> const SCompressedPair& requires( std::is_assignable_v<first_type&, decltype(std::get<0>( std::declval<P>() ))> && std::is_assignable_v<second_type&, decltype(std::get<1>( std::declval<P>() ))> )
    {
        first() = std::get<0>( std::forward<P>( other ) );
        second() = std::get<1>( std::forward<P>( other ) );
        return *this;
    }

    constexpr ~SCompressedPair() = default;
    constexpr SCompressedPair( const SCompressedPair& other ) = default;
    constexpr SCompressedPair( SCompressedPair&& other ) noexcept = default;

    constexpr auto first() noexcept -> typename first_type::reference
    {
        return static_cast<first_type&>(*this).get();
    }

    constexpr auto first() const noexcept -> typename first_type::const_reference
    {
        return static_cast<const first_type&>(*this).get();
    }

    constexpr auto second() noexcept -> typename second_type::reference
    {
        return static_cast<second_type&>(*this).get();
    }

    constexpr auto second() const noexcept -> typename second_type::const_reference
    {
        return static_cast<second_type const&>(*this).get();
    }

    template<std::size_t Index>
    [[nodiscard]] constexpr auto get() noexcept
    {
        if constexpr ( Index == 0U )
        {
            return first();
        }
        else
        {
            static_assert( Index == 1U, "Index out of bounds" );
            return second();
        }
    }

    template<std::size_t Index>
    [[nodiscard]] constexpr auto get() const noexcept
    {
        if constexpr ( Index == 0U )
        {
            return first();
        }
        else
        {
            static_assert( Index == 1U, "Index out of bounds" );
            return second();
        }
    }

    friend constexpr auto operator==( const SCompressedPair& lhs, const SCompressedPair& rhs ) -> bool
    {
        return lhs.first() == rhs.first()
                && lhs.second() == rhs.second();
    }

    friend constexpr auto operator!=( const SCompressedPair& lhs, const SCompressedPair& rhs ) -> bool
    {
        return !( lhs == rhs );
    }

    friend constexpr auto operator<( const SCompressedPair& lhs, const SCompressedPair& rhs ) -> bool
    {
        if ( lhs.first() < rhs.first() )
        {
            return true;
        }
        if ( rhs.first() < lhs.first() )
        {
            return false;
        }
        return lhs.second() < rhs.second();
    }

    friend constexpr auto operator<=( const SCompressedPair& lhs, const SCompressedPair& rhs ) -> bool
    {
        return !( rhs < lhs );
    }

    friend constexpr auto operator>( const SCompressedPair& lhs, const SCompressedPair& rhs ) -> bool
    {
        return rhs < lhs;
    }

    friend constexpr auto operator>=( const SCompressedPair& lhs, const SCompressedPair& rhs ) -> bool
    {
        return !( lhs < rhs );
    }

    constexpr void swap( SCompressedPair& other ) noexcept(std::is_nothrow_swappable_v<first_value_type> && std::is_nothrow_swappable_v<second_value_type>)
    {
        using std::swap;
        swap( first(), other.first() );
        swap( second(), other.second() );
    }

    friend constexpr void swap( SCompressedPair& lhs, SCompressedPair& rhs ) noexcept(std::is_nothrow_swappable_v<first_value_type> && std::is_nothrow_swappable_v<second_value_type>)
    {
        lhs.swap( rhs );
    }
};

export template<class Type1, class Type2>
struct SPair final
{
    using first_type = Type1;
    using second_type = Type2;

    first_type first{};
    second_type second{};

    constexpr explicit(std::is_default_constructible_v<first_type> && std::is_default_constructible_v<second_type>) SPair()
        : first()
        , second()
    {
    }

    constexpr explicit(!( std::is_convertible_v<const first_type&, first_type> && std::is_convertible_v<const second_type&, second_type> )) SPair( const first_type& left, const second_type& right )
        requires std::is_default_constructible_v<first_type> && std::is_default_constructible_v<second_type>
        : first{left}
        , second{right}
    {
    }

    template<class U1 = first_type, class U2 = second_type>
        requires std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>
    constexpr explicit(!( std::is_convertible_v<U1, first_type> && std::is_convertible_v<U2, second_type> )) SPair( U1&& left, U2&& right )
        : first{std::forward<U1>( left )}
        , second{std::forward<U2>( right )}
    {
    }

    template<class U1, class U2>
        requires std::is_constructible_v<first_type, U1&> && std::is_constructible_v<second_type, U2&>
    constexpr explicit(!( std::is_convertible_v<U1&, first_type> && std::is_convertible_v<U2&, second_type> )) SPair( SPair<U1, U2>& pair )
        : first{pair.first}
        , second{pair.second}
    {
    }

    template<class U1, class U2>
        requires std::is_constructible_v<first_type, const U1&> && std::is_constructible_v<second_type, const U2&>
    constexpr explicit(!( std::is_convertible_v<const U1&, first_type> && std::is_convertible_v<const U2&, second_type> )) SPair( const SPair<U1, U2>& pair )
        : first{pair.first}
        , second{pair.second}
    {
    }

    template<class U1, class U2>
        requires std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>
    constexpr explicit(!( std::is_convertible_v<U1, first_type> && std::is_convertible_v<U2, second_type> )) SPair( SPair<U1, U2>&& pair )
        : first{std::forward<U1>( pair.first )}
        , second{std::forward<U2>( pair.second )}
    {
    }

    template<class U1, class U2>
        requires std::is_constructible_v<first_type, U1> && std::is_constructible_v<second_type, U2>
    constexpr explicit(!( std::is_convertible_v<const U1&, first_type> && std::is_convertible_v<const U2&, second_type> )) SPair( const SPair<U1, U2>&& pair )
        : first{std::forward<const U1>( pair.first )}
        , second{std::forward<const U2>( pair.second )}
    {
    }

    template<PairLike P>
    constexpr explicit(!( std::is_convertible_v<decltype(std::get<0>( std::declval<P&&>() )), first_type> && std::is_convertible_v<decltype(std::get<1>( std::declval<P&&>() )), second_type> ))
        SPair( P&& SPair )
            requires std::is_constructible_v<first_type, decltype(std::get<0>( std::declval<P&&>() ))> && std::is_constructible_v<second_type, decltype(std::get<1>( std::declval<P&&>() ))>
        : first( std::get<0>( std::forward<P>( SPair ) ) )
        , second( std::get<1>( std::forward<P>( SPair ) ) )
    {
    }

    template<class... Args1, class... Args2>
    constexpr SPair( std::piecewise_construct_t /*unused*/, std::tuple<Args1...> first_args, std::tuple<Args2...> second_args )
        noexcept(std::is_nothrow_constructible_v<first_type, Args1...> && std::is_nothrow_constructible_v<second_type, Args2...>)
        : SPair( std::piecewise_construct, first_args, second_args, std::make_index_sequence<sizeof...( Args1 )>{}, std::make_index_sequence<sizeof...( Args2 )>{} )
    {
    }

    template<class... Args1, class... Args2, size_t... I1, size_t... I2>
    constexpr SPair( std::piecewise_construct_t /*unused*/, std::tuple<Args1...>& first_args, std::tuple<Args2...>& second_args, std::index_sequence<I1...> /*unused*/, std::index_sequence<I2...> /*unused*/ )
        : first( std::forward<Args1>( std::get<I1>( first_args ) )... )
        , second( std::forward<Args2>( std::get<I2>( second_args ) )... )
    {
    }

    constexpr auto operator=( const SPair& other ) -> SPair& requires std::is_copy_assignable_v<first_type> && std::is_copy_assignable_v<second_type>
    {
        if ( this != &other )
        {
            first = other.first;
            second = other.second;
        }
        return *this;
    }

    constexpr auto operator=( const SPair& ) -> SPair& requires std::is_copy_assignable_v<const first_type> && std::is_copy_assignable_v<const second_type>
    = default;

    template<class U1, class U2>
    constexpr auto operator=( const SPair<U1, U2>& other ) -> SPair& requires std::is_assignable_v<first_type&, const U1&> && std::is_assignable_v<second_type&, const U2&>
    {
        if ( this != &other )
        {
            first = other.first;
            second = other.second;
        }
        return *this;
    }

    template<class U1, class U2>
    constexpr auto operator=( const SPair<U1, U2>& other ) const -> const SPair& requires std::is_assignable_v<const first_type&, const U1&> && std::is_assignable_v<const second_type&, const U2&>
    {
        if ( this != &other )
        {
            first = other.first;
            second = other.second;
        }
        return *this;
    }

    constexpr auto operator=( SPair&& other ) noexcept( std::is_nothrow_move_assignable_v<first_type> && std::is_nothrow_move_assignable_v<second_type>) -> SPair& requires std::is_move_assignable_v<first_type&> && std::is_move_assignable_v<
        second_type&>
    {
        first = std::move( other ).first;
        second = std::move( other ).second;
        return *this;
    }

    constexpr auto operator=( SPair&& other ) const noexcept( std::is_nothrow_move_assignable_v<const first_type> && std::is_nothrow_move_assignable_v<const second_type>) -> const SPair& requires std::is_assignable_v<const first_type&, first_type> &&
        std::is_assignable_v<const second_type&, second_type>
    {
        first = std::forward<first_type>( other.first );
        second = std::forward<second_type>( other.second );
        return *this;
    }

    template<class U1, class U2>
    constexpr auto operator=( SPair<U1, U2>&& other ) -> SPair& requires std::is_assignable_v<const first_type&, U1> && std::is_assignable_v<const second_type&, U2>
    {
        first = std::forward<first_type>( other.first );
        second = std::forward<second_type>( other.second );
        return *this;
    }

    template<class U1, class U2>
    constexpr auto operator=( SPair<U1, U2>&& other ) const -> const SPair& requires std::is_assignable_v<const first_type&, const U1&> && std::is_assignable_v<const second_type&, const U2&>
    {
        first = std::forward<first_type>( other.first );
        second = std::forward<second_type>( other.second );
        return *this;
    }

    template<PairLike P>
    constexpr auto operator=( P&& other ) -> SPair& requires( std::is_assignable_v<first_type&, decltype(std::get<0>( std::declval<P>() ))> && std::is_assignable_v<second_type&, decltype(std::get<1>( std::declval<P>() ))> )
    {
        first = std::get<0>( std::forward<P>( other ) );
        second = std::get<1>( std::forward<P>( other ) );
        return *this;
    }

    template<PairLike P>
    constexpr auto operator=( P&& other ) const -> const SPair& requires( std::is_assignable_v<first_type&, decltype(std::get<0>( std::declval<P>() ))> && std::is_assignable_v<second_type&, decltype(std::get<1>( std::declval<P>() ))> )
    {
        first = std::get<0>( std::forward<P>( other ) );
        second = std::get<1>( std::forward<P>( other ) );
        return *this;
    }

    constexpr ~SPair() = default;
    constexpr SPair( const SPair& other ) = default;
    constexpr SPair( SPair&& other ) noexcept = default;

    friend constexpr auto operator==( const SPair& lhs, const SPair& rhs ) -> bool
    {
        return lhs.first == rhs.first
                && lhs.second == rhs.second;
    }

    friend constexpr auto operator!=( const SPair& lhs, const SPair& rhs ) -> bool
    {
        return !( lhs == rhs );
    }

    friend auto operator<( const SPair& lhs, const SPair& rhs ) -> bool
    {
        if ( lhs.first < rhs.first )
        {
            return true;
        }
        if ( rhs.first < lhs.first )
        {
            return false;
        }
        return lhs.second < rhs.second;
    }

    friend auto operator<=( const SPair& lhs, const SPair& rhs ) -> bool
    {
        return !( rhs < lhs );
    }

    friend auto operator>( const SPair& lhs, const SPair& rhs ) -> bool
    {
        return rhs < lhs;
    }

    friend auto operator>=( const SPair& lhs, const SPair& rhs ) -> bool
    {
        return !( lhs < rhs );
    }

    constexpr void swap( SPair& other ) noexcept
    {
        using std::swap;
        swap( first, other.first );
        swap( second, other.second );
    }

    friend constexpr void swap( SPair& lhs, SPair& rhs ) noexcept
    {
        using std::swap;
        lhs.swap( rhs );
    }
};

export template<class T1, class T2>
constexpr auto MakeCompactPair( T1&& value1, T2&& value2 ) -> SCompressedPair<std::decay_t<T1>, std::decay_t<T2>>
{
    return SCompressedPair<std::decay_t<T1>, std::decay_t<T2>>( std::forward<T1>( value1 ), std::forward<T2>( value2 ) );
}

export template<class T1, class T2>
constexpr auto MakePair( T1&& value1, T2&& value2 ) -> SPair<std::decay_t<T1>, std::decay_t<T2>>
{
    return SPair<std::decay_t<T1>, std::decay_t<T2>>( std::forward<T1>( value1 ), std::forward<T2>( value2 ) );
}


export template<typename Type, typename Other>
SCompressedPair( Type&&, Other&& ) -> SCompressedPair<std::decay_t<Type>, std::decay_t<Other>>;
export template<typename Type, typename Other>
SPair( Type&&, Other&& ) -> SPair<std::decay_t<Type>, std::decay_t<Other>>;
}// namespace DeerContainer

export namespace std
{
template<typename First, typename Second>
struct tuple_size<DeerContainer::SCompressedPair<First, Second>> : integral_constant<size_t, 2U>
{
};

template<size_t Index, typename First, typename Second>
struct tuple_element<Index, DeerContainer::SCompressedPair<First, Second>> : conditional<Index == 0U, First, Second>
{
    static_assert( Index < 2U, "Index out of bounds" );
};

template<typename Type1, typename Type2>
struct tuple_element<0, DeerContainer::SCompressedPair<Type1, Type2>>
{
    using type = Type1;
};

template<typename Type1, typename Type2>
struct tuple_element<1, DeerContainer::SCompressedPair<Type1, Type2>>
{
    using type = Type2;
};


template<typename First, typename Second>
struct tuple_size<DeerContainer::SPair<First, Second>> : integral_constant<size_t, 2U>
{
};

template<size_t Index, typename First, typename Second>
struct tuple_element<Index, DeerContainer::SPair<First, Second>> : conditional<Index == 0U, First, Second>
{
    static_assert( Index < 2U, "Index out of bounds" );
};

template<typename Type1, typename Type2>
struct tuple_element<0, DeerContainer::SPair<Type1, Type2>>
{
    using type = Type1;
};

template<typename Type1, typename Type2>
struct tuple_element<1, DeerContainer::SPair<Type1, Type2>>
{
    using type = Type2;
};

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( DeerContainer::SCompressedPair<Type1, Type2>& pair ) noexcept -> std::tuple_element_t<Index, DeerContainer::SCompressedPair<Type1, Type2>>&
{
    if constexpr ( Index == 0 )
    {
        return pair.first();
    }
    else
    {
        return pair.second();
    }
}

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( const DeerContainer::SCompressedPair<Type1, Type2>& pair ) noexcept -> const std::tuple_element_t<Index, DeerContainer::SCompressedPair<Type1, Type2>>&
{
    if constexpr ( Index == 0 )
    {
        return pair.first();
    }
    else
    {
        return pair.second();
    }
}

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( DeerContainer::SCompressedPair<Type1, Type2>&& pair ) noexcept -> std::tuple_element_t<Index, DeerContainer::SCompressedPair<Type1, Type2>>&&
{
    if constexpr ( Index == 0 )
    {
        return std::move( pair.first() );
    }
    else
    {
        return std::move( pair.second() );
    }
}

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( const DeerContainer::SCompressedPair<Type1, Type2>&& pair ) noexcept -> const std::tuple_element_t<Index, DeerContainer::SCompressedPair<Type1, Type2>>&&
{
    if constexpr ( Index == 0 )
    {
        return std::move( pair.first() );
    }
    else
    {
        return std::move( pair.second() );
    }
}

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( DeerContainer::SPair<Type1, Type2>& pair ) noexcept -> std::tuple_element_t<Index, DeerContainer::SPair<Type1, Type2>>&
{
    if constexpr ( Index == 0 )
    {
        return pair.first;
    }
    else
    {
        return pair.second;
    }
}

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( const DeerContainer::SPair<Type1, Type2>& pair ) noexcept -> const std::tuple_element_t<Index, DeerContainer::SPair<Type1, Type2>>&
{
    if constexpr ( Index == 0 )
    {
        return pair.first;
    }
    else
    {
        return pair.second;
    }
}

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( DeerContainer::SPair<Type1, Type2>&& pair ) noexcept -> std::tuple_element_t<Index, DeerContainer::SPair<Type1, Type2>>&&
{
    if constexpr ( Index == 0 )
    {
        return std::move( pair.first );
    }
    else
    {
        return std::move( pair.second );
    }
}

template<std::size_t Index, typename Type1, typename Type2>
constexpr auto get( const DeerContainer::SPair<Type1, Type2>&& pair ) noexcept -> const std::tuple_element_t<Index, DeerContainer::SPair<Type1, Type2>>&&
{
    if constexpr ( Index == 0 )
    {
        return std::move( pair.first );
    }
    else
    {
        return std::move( pair.second );
    }
}

}// namespace std
