//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <cstddef>
#include <cstdlib>
export module DeerAllocator:Mallocator;
import :Common;

namespace DeerAllocator
{
export class CMallocator
{
public:
    auto allocate( const std::size_t size ) noexcept -> SBlock
    {
        return {.pData = malloc( size ), .size = size};
    }

    [[nodiscard]] constexpr auto owns( const SBlock& block ) noexcept -> bool
    {
        return block.pData != nullptr;
    }

    void deallocate( SBlock& block ) noexcept
    {
        free( block.pData );
        block.pData = nullptr;
        block.size = 0;
    }
};
}// namespace DeerAllocator
