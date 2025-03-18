//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//
module;
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

export module DeerAllocator:Utilities;

namespace DeerAllocator
{
template <typename Type>
struct uses_allocator_construction
{
    template <typename Allocator, typename... Params>
    static constexpr auto args([[maybe_unused]] const Allocator& allocator, Params&&... params) noexcept
    {
        if constexpr (!std::uses_allocator_v<Type, Allocator> && std::is_constructible_v<Type, Params...>)
        {
            return std::forward_as_tuple(std::forward<Params>(params)...);
        }
        else
        {
            static_assert(std::uses_allocator_v<Type, Allocator>, "Ill-formed request");

            if constexpr (std::is_constructible_v<Type, std::allocator_arg_t, const Allocator&, Params...>)
            {
                return std::tuple<std::allocator_arg_t, const Allocator&, Params&&...>{std::allocator_arg,
                                                                                       allocator,
                                                                                       std::forward<Params>(params)...};
            }
            else
            {
                static_assert(std::is_constructible_v<Type, Params..., const Allocator&>, "Ill-formed request");
                return std::forward_as_tuple(std::forward<Params>(params)..., allocator);
            }
        }
    }
};

template <typename Type, typename Other>
struct uses_allocator_construction<std::pair<Type, Other>>
{
    using type = std::pair<Type, Other>;

    template <typename Allocator, typename First, typename Second>
    static constexpr auto args(const Allocator& allocator, std::piecewise_construct_t, First&& first, Second&& second) noexcept
    {
        return std::make_tuple(
            std::piecewise_construct,
            std::apply([&allocator](auto&&... curr)
                       { return uses_allocator_construction<Type>::args(allocator, std::forward<decltype(curr)>(curr)...); },
                       std::forward<First>(first)),
            std::apply([&allocator](auto&&... curr)
                       { return uses_allocator_construction<Other>::args(allocator, std::forward<decltype(curr)>(curr)...); },
                       std::forward<Second>(second)));
    }

    template <typename Allocator>
    static constexpr auto args(const Allocator& allocator) noexcept
    {
        return uses_allocator_construction<type>::args(allocator, std::piecewise_construct, std::tuple<>{}, std::tuple<>{});
    }

    template <typename Allocator, typename First, typename Second>
    static constexpr auto args(const Allocator& allocator, First&& first, Second&& second) noexcept
    {
        return uses_allocator_construction<type>::args(allocator,
                                                       std::piecewise_construct,
                                                       std::forward_as_tuple(std::forward<First>(first)),
                                                       std::forward_as_tuple(std::forward<Second>(second)));
    }

    template <typename Allocator, typename First, typename Second>
    static constexpr auto args(const Allocator& allocator, const std::pair<First, Second>& value) noexcept
    {
        return uses_allocator_construction<type>::args(allocator,
                                                       std::piecewise_construct,
                                                       std::forward_as_tuple(value.first),
                                                       std::forward_as_tuple(value.second));
    }

    template <typename Allocator, typename First, typename Second>
    static constexpr auto args(const Allocator& allocator, std::pair<First, Second>&& value) noexcept
    {
        return uses_allocator_construction<type>::args(allocator,
                                                       std::piecewise_construct,
                                                       std::forward_as_tuple(std::move(value.first)),
                                                       std::forward_as_tuple(std::move(value.second)));
    }
};

export template <typename Type, typename Allocator, typename... Args>
constexpr Type MakeObjUsingAllocator(const Allocator& allocator, Args&&... args)
{
    return std::make_from_tuple<Type>(uses_allocator_construction<Type>::args(allocator, std::forward<Args>(args)...));
}
} // namespace DeerAllocator
