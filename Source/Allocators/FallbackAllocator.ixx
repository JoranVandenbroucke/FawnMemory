//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <cstddef>
export module DeerAllocator.FallbackAllocator;
import DeerAllocator.Common;

namespace DeerAllocator
{
export template<class Primary, class Fallback>
class CFallbackAllocator : Primary, Fallback
{
public:
    constexpr auto allocate( std::size_t size ) noexcept -> SBlock
    {
        const SBlock block{Primary::allocate( size )};
        if ( block.pData == nullptr )
        {
            return Fallback::allocate( size );
        }
        return block;
    }

    [[nodiscard]] constexpr auto owns( const SBlock& block ) const noexcept -> bool
    {
        return Primary::owns( block ) || Fallback::owns( block );
    }

    constexpr void deallocate( SBlock& block ) noexcept
    {
        if ( Primary::owns( block ) )
        {
            Primary::deallocate( block );
        }
        else if ( Fallback::owns( block ) )
        {
            Fallback::deallocate( block );
        }
    }
};
}// namespace DeerAllocator
