//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <cstddef>
export module DeerAllocator:Buckitizer;
import :Common;

namespace DeerAllocator
{
// [Min, Min + Step)
// [Min + Step, Min + 2 * Step) ...
export template<class Allocator, std::size_t Min, std::size_t Max, std::size_t Step>
class CBucketizer
{
public:
    constexpr auto allocate( const std::size_t size ) noexcept -> SBlock
    {
        if ( size >= Min && size <= Max )
        {
            return m_buckets[ ( size - Min ) / Step ].allocate( size );
        }
        return {.pData=nullptr, .size=0};
    }

    [[nodiscard]] constexpr auto owns( const SBlock& block ) const noexcept -> bool
    {
        if ( block.size >= Min && block.size <= Max )
        {
            return m_buckets[ ( block.size - Min ) / Step ].owns( block );
        }
        return false;
    }

    constexpr void deallocate( SBlock& block ) noexcept
    {
        if ( block.size >= Min && block.size <= Max )
        {
            return m_buckets[ ( block.size - Min ) / Step ].allocate( block );
        }
    }

private:
    Allocator m_buckets[ ( Max - Min ) / Step ];
};
}// namespace DeerAllocator
