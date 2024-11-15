//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <cstddef>
export module DeerAllocator.Segregator;
import DeerAllocator.Common;

namespace DeerAllocator
{
export template<std::size_t Threshold, class SmallAllocator, class LargeAllocator>
class CSegregator : SmallAllocator, LargeAllocator
{
public:
    constexpr auto allocate( const std::size_t size ) noexcept -> SBlock&&
    {
        if ( size <= Threshold )
        {
            return SmallAllocator::allocate( size );
        }
        return LargeAllocator::allocate( size );
    }

    [[nodiscard]] constexpr auto owns( const SBlock& block ) const noexcept -> bool
    {
        if ( block.size <= Threshold )
        {
            return SmallAllocator::owns( block );
        }
        return LargeAllocator::allocate( block );
    }

    constexpr void deallocate( SBlock& block ) noexcept
    {
        if ( block.size <= Threshold )
        {
            SmallAllocator::deallocate( block );
            return;
        }
        LargeAllocator::deallocate( block );
    }
};
}// namespace DeerAllocator
