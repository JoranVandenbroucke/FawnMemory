#pragma once
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace DeerContainer
{
namespace internal
{
template <typename Type>
    requires std::is_unsigned_v<Type>
[[nodiscard]] constexpr int popcount(const Type value) noexcept
{
    return value ? (int(value & 1) + popcount(static_cast<Type>(value >> 1))) : 0;
}

template <typename Type>
    requires std::is_signed_v<Type>
[[nodiscard]] constexpr int popcount(const Type value) noexcept
{
    // Treat signed values by working with their absolute value
    return popcount(static_cast<std::make_unsigned_t<Type>>(std::abs(value)));
}

template <typename, typename = void>
struct key_traits;

template <typename Type>
    requires std::is_enum_v<Type>
struct key_traits<Type> : key_traits<std::underlying_type_t<Type>>
{
    using value_type = Type;
};

template <typename Type>
    requires std::is_class_v<Type>
struct key_traits<Type> : key_traits<typename Type::value_type>
{
    using value_type = Type;
};

template <>
struct key_traits<std::uint32_t>
{
    using value_type = std::uint32_t;

    using key_type     = std::uint32_t;
    using version_type = std::uint16_t;

    static constexpr key_type key_mask{0xFFFFF};
    static constexpr key_type version_mask{0xFFF};
};
template <>
struct key_traits<std::int32_t>
{
    using value_type = std::int32_t;

    using key_type     = std::int32_t;
    using version_type = std::int16_t;

    static constexpr key_type key_mask{0xFFFFF};
    static constexpr key_type version_mask{0xFFF};
};
template <>
struct key_traits<std::uint64_t>
{
    using value_type = std::uint64_t;

    using key_type     = std::uint64_t;
    using version_type = std::uint32_t;

    static constexpr key_type key_mask{0xFFFFFFFF};
    static constexpr key_type version_mask{0xFFFFFFFF};
};

template <>
struct key_traits<std::int64_t>
{
    using value_type = std::int64_t;

    using key_type     = std::int64_t;
    using version_type = std::int32_t;

    static constexpr key_type key_mask{0xFFFFFFFF};
    static constexpr key_type version_mask{0xFFFFFFFF};
};

} // namespace internal

template <typename Traits>
class basic_key_traits
{
    static constexpr auto length = internal::popcount(Traits::key_mask);

    static_assert(Traits::key_mask && ((Traits::key_mask & (Traits::key_mask + 1)) == 0), "Invalid key mask");
    static_assert((Traits::version_mask & (Traits::version_mask + 1)) == 0, "Invalid version mask");

  public:
    using value_type   = typename Traits::value_type;
    using key_type     = typename Traits::key_type;
    using version_type = typename Traits::version_type;

    static constexpr key_type key_mask     = Traits::key_mask;
    static constexpr key_type version_mask = Traits::version_mask;

    [[nodiscard]] static constexpr key_type to_integral(const key_type key) noexcept
    {
        return static_cast<key_type>(key);
    }

    [[nodiscard]] static constexpr key_type to_key(const key_type key) noexcept
    {
        return (to_integral(key) & key_mask);
    }

    [[nodiscard]] static constexpr version_type to_version(const key_type key) noexcept
    {
        if constexpr (Traits::version_mask == 0u)
        {
            return version_type{};
        }
        else
        {
            return (static_cast<version_type>(to_integral(key) >> length) & version_mask);
        }
    }

    [[nodiscard]] static constexpr value_type next(const value_type value) noexcept
    {
        const auto vers = to_version(value) + 1;
        return construct(to_integral(value), static_cast<version_type>(vers + (vers == version_mask)));
    }

    [[nodiscard]] static constexpr value_type construct(const key_type key, const version_type version) noexcept
    {
        if constexpr (Traits::version_mask == 0u)
        {
            return value_type{key & key_mask};
        }
        else
        {
            return value_type{(key & key_mask) | (static_cast<value_type>(version & version_mask) << length)};
        }
    }

    [[nodiscard]] static constexpr value_type combine(const key_type lhs, const key_type rhs) noexcept
    {
        if constexpr (Traits::version_mask == 0u)
        {
            return value_type{lhs & key_mask};
        }
        else
        {
            return value_type{(lhs & key_mask) | (rhs & (version_mask << length))};
        }
    }
};

template <typename Type>
struct key_traits : basic_key_traits<internal::key_traits<Type>>
{
    using base_type = basic_key_traits<internal::key_traits<Type>>;
    static constexpr std::size_t page_size{4096ULL};
};

template <typename Key>
[[nodiscard]] constexpr typename key_traits<Key>::value_type to_integral(const Key key) noexcept
{
    return key_traits<Key>::to_integral(key);
}

template <typename Key>
[[nodiscard]] constexpr typename key_traits<Key>::value_type to_key(const Key key) noexcept
{
    return key_traits<Key>::to_key(key);
}

template <typename Key>
[[nodiscard]] constexpr typename key_traits<Key>::version_type to_version(const Key key) noexcept
{
    return key_traits<Key>::to_version(key);
}

struct null_t
{
    template <typename Key>
    [[nodiscard]] constexpr operator Key() const noexcept
    {
        using traits_type  = key_traits<Key>;
        constexpr auto key = traits_type::construct(traits_type::key_mask, traits_type::version_mask);
        return key;
    }

    [[nodiscard]] constexpr bool operator==([[maybe_unused]] const null_t other) const noexcept
    {
        return true;
    }

    [[nodiscard]] constexpr bool operator!=([[maybe_unused]] const null_t other) const noexcept
    {
        return false;
    }

    template <typename Key>
    [[nodiscard]] constexpr bool operator==(const Key key) const noexcept
    {
        using traits_type = key_traits<Key>;
        return traits_type::to_key(key) == traits_type::to_key(*this);
    }

    template <typename Key>
    [[nodiscard]] constexpr bool operator!=(const Key key) const noexcept
    {
        return !(key == *this);
    }
};

template <typename Key>
[[nodiscard]] constexpr bool operator==(const Key lhs, const null_t rhs) noexcept
{
    return rhs.operator==(lhs);
}

template <typename Key>
[[nodiscard]] constexpr bool operator!=(const Key lhs, const null_t rhs) noexcept
{
    return !(rhs == lhs);
}

struct tombstone_t
{
    template <typename Key>
    [[nodiscard]] constexpr operator Key() const noexcept
    {
        using traits_type  = key_traits<Key>;
        constexpr auto key = traits_type::construct(traits_type::key_mask, traits_type::version_mask);
        return key;
    }

    [[nodiscard]] constexpr bool operator==([[maybe_unused]] const tombstone_t other) const noexcept
    {
        return true;
    }

    [[nodiscard]] constexpr bool operator!=([[maybe_unused]] const tombstone_t other) const noexcept
    {
        return false;
    }

    template <typename Key>
    [[nodiscard]] constexpr bool operator==(const Key key) const noexcept
    {
        using traits_type = key_traits<Key>;

        if constexpr (traits_type::version_mask == 0u)
        {
            return false;
        }
        else
        {
            return (traits_type::to_version(key) == traits_type::to_version(*this));
        }
    }

    template <typename Key>
    [[nodiscard]] constexpr bool operator!=(const Key key) const noexcept
    {
        return !(key == *this);
    }
};

template <typename Key>
[[nodiscard]] constexpr bool operator==(const Key lhs, const tombstone_t rhs) noexcept
{
    return rhs.operator==(lhs);
}

template <typename Key>
[[nodiscard]] constexpr bool operator!=(const Key lhs, const tombstone_t rhs) noexcept
{
    return !(rhs == lhs);
}

inline constexpr null_t null{};

inline constexpr tombstone_t tombstone{};

} // namespace DeerContainer
