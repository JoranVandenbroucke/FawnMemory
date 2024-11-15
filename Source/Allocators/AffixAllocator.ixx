//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <bit>
#include <cstddef>
#include <type_traits>
export module DeerAllocator.AffixAllocator;
import DeerAllocator.Common;

namespace DeerAllocator
{
export template<class Allocator, class Prefix, class Suffix = void>
class CAffixAllocator
{
public:
    constexpr auto allocate( const std::size_t size ) noexcept -> SBlock
    {
        std::size_t totalSize = size + sizeof( Prefix ) + ( std::is_void_v<Suffix> ? 0 : sizeof( Suffix ) );
        const SBlock block{Allocator::allocate( totalSize )};
        return {.pData = std::bit_cast<unsigned char*>( block.pData ) + sizeof( Prefix ), .size = size};
    }

    [[nodiscard]] constexpr auto owns( const SBlock& block ) const noexcept -> bool
    {
        return Allocator::owns( std::bit_cast<unsigned char*>( block.pData ) - sizeof( Prefix ) );
    }

    constexpr void deallocate( const SBlock& block ) noexcept
    {
        if ( block.pData != nullptr )
        {
            std::size_t totalSize = block.size + sizeof( Prefix ) + ( std::is_void_v<Suffix> ? 0 : sizeof( Suffix ) );
            Allocator::deallocate( {std::bit_cast<unsigned char*>( block.pData ) - sizeof( Prefix ), totalSize} );
        }
    }
};
} // namespace DeerAllocator
