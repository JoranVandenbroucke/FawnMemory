//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//
module;
#include <tuple>

export module DeerAllocator.Utilities;

namespace DeerAllocator
{
template<typename Type>
struct SUsesAllocatorConstruction
{
    template<typename Allocator, typename... Params>
    static constexpr auto Args( [[maybe_unused]] const Allocator& allocator, Params&&... params ) noexcept
    {
        if constexpr ( !std::uses_allocator_v<Type, Allocator> && std::is_constructible_v<Type, Params...> )
        {
            return std::forward_as_tuple( std::forward<Params>( params )... );
        }
        else
        {
            static_assert( std::uses_allocator_v<Type, Allocator>, "Ill-formed request" );

            if constexpr ( std::is_constructible_v<Type, std::allocator_arg_t, const Allocator&, Params...> )
            {
                return std::tuple<std::allocator_arg_t, const Allocator&, Params&&...>{std::allocator_arg, allocator, std::forward<Params>( params )...};
            }
            else
            {
                static_assert( std::is_constructible_v<Type, Params..., const Allocator&>, "Ill-formed request" );
                return std::forward_as_tuple( std::forward<Params>( params )..., allocator );
            }
        }
    }
};

export template<typename Type, typename Allocator, typename... Args>
constexpr auto MakeObjUsingAllocator(const Allocator& allocator, Args&&... args) -> Type
{
    return std::make_from_tuple<Type>(SUsesAllocatorConstruction<Type>::Args(allocator, std::forward<Args>(args)...));
}
}// namespace DeerAllocator
