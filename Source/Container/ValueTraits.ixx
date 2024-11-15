//
// Copyright (c) 2024.
// Author: Joran.
//

module;
#include <bit>
#include <cstdint>
#include <type_traits>

export module DeerContainer.ValueTraits;

namespace DeerContainer
{
export template<typename, typename = void>
struct SValueTraits;

export template<typename Type>
struct SValueTraits<Type, std::enable_if_t<std::is_enum_v<Type>>>
        : SValueTraits<std::underlying_type_t<Type>>
{
    using value_type = Type;
};

export template<typename Type>
struct SValueTraits<Type, std::enable_if_t<std::is_class_v<Type>>>
        : SValueTraits<typename Type::value_type>
{
    using value_type = Type;
};

export template<>
struct SValueTraits<std::uint32_t>
{
    using value_type = std::uint32_t;
    using version_type = std::uint16_t;

    static constexpr value_type g_VALUE_MASK{0xFFFFF};
    static constexpr value_type g_VERSION_MASK{0xFFF};
};

export template<>
struct SValueTraits<std::uint64_t>
{
    using value_type = std::uint64_t;
    using version_type = std::uint32_t;

    static constexpr value_type g_VALUE_MASK{0xFFFFFFFF};
    static constexpr value_type g_VERSION_MASK{0xFFFFFFFF};
};

template<typename Traits>
class CValueTraitsHelper
{
    static constexpr auto g_MASK_LENGTH{std::popcount( Traits::value_type )};

    static_assert( Traits::g_VALUE_MASK && ( ( Traits::g_VALUE_MASK & ( Traits::g_VALUE_MASK + 1 ) ) == 0 ), "Invalid value mask" );
    static_assert( ( Traits::g_VALUE_MASK & ( Traits::g_VALUE_MASK + 1 ) ) == 0, "Invalid version mask" );

public:
    using value_type = typename Traits::value_type;
    using version_type = typename Traits::version_type;

    static constexpr value_type g_VALUE_TYPE = Traits::g_VALUE_MASK;
    static constexpr value_type g_VERSION_MASK = Traits::g_version_mask;

    [[nodiscard]] static constexpr auto ConvertToIntegral( const value_type value ) noexcept -> value_type
    {
        return static_cast<value_type>(value);
    }

    [[nodiscard]] static constexpr auto GetValue( const value_type value ) noexcept -> value_type
    {
        return ConvertToIntegral( value ) & g_VALUE_TYPE;
    }

    [[nodiscard]] static constexpr auto GetVersion( const value_type value ) noexcept -> version_type
    {
        if constexpr ( Traits::version_mask == 0U )
        {
            return version_type{};
        }
        else
        {
            return ( static_cast<version_type>(ConvertToIntegral( value ) >> g_MASK_LENGTH) & g_VERSION_MASK );
        }
    }

    [[nodiscard]] static constexpr auto IncrementVersion( const value_type value ) noexcept -> value_type
    {
        const auto newVersion = GetVersion( value ) + 1;
        return CreateValue( ConvertToIntegral( value ), static_cast<version_type>(newVersion + ( newVersion == g_VERSION_MASK )) );
    }

    [[nodiscard]] static constexpr auto CreateValue( const value_type value, const version_type version ) noexcept -> value_type
    {
        if constexpr ( Traits::g_VERSION_MASK == 0U )
        {
            return value_type{value & g_VALUE_TYPE};
        }
        else
        {
            return value_type{( value & g_VALUE_TYPE ) | ( static_cast<value_type>(version & g_VERSION_MASK) << g_MASK_LENGTH )};
        }
    }

    [[nodiscard]] static constexpr auto CombineValueVersion( const value_type value, const value_type version ) noexcept -> value_type
    {
        if constexpr ( Traits::g_VERSION_MASK == 0U )
        {
            return value_type{value & g_VALUE_TYPE};
        }
        else
        {
            return value_type{( value & g_VALUE_TYPE ) | ( version & ( g_VERSION_MASK << g_MASK_LENGTH ) )};
        }
    }
};

export template<typename Type>
struct SValueTraits : CValueTraitsHelper<SValueTraits<Type>>
{
    using base_type = CValueTraitsHelper<SValueTraits<Type>>;
    static constexpr std::size_t g_PAGE_SIZE{4096};
};

export template<typename Value>
[[nodiscard]] constexpr auto ToIntegral( const Value value ) noexcept -> typename SValueTraits<Value>::value_type
{
    return SValueTraits<Value>::ToIntegral( value );
}

export template<typename Value>
[[nodiscard]] constexpr auto ToValue( const Value value ) noexcept -> typename SValueTraits<Value>::value_type
{
    return SValueTraits<Value>::ToValue( value );
}

export template<typename Value>
[[nodiscard]] constexpr auto ToVersion( const Value value ) noexcept -> typename SValueTraits<Value>::version_type
{
    return SValueTraits<Value>::ToVersion( value );
}

template<typename To, typename From>
struct SConstnessAss
{
    using type = std::remove_const_t<To>;
};

template<typename To, typename From>
struct SConstnessAss<To, const From>
{
    using type = const To;
};

template<typename To, typename From>
struct SConstnessAss_t<To, const From>
{
    using type = const To;
};
}// namespace DeerContainer
