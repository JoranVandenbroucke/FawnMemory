//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <cstddef>
export module DeerAllocator.Common;

namespace DeerAllocator
{
    export struct SBlock
    {
        void* pData{nullptr};
        std::size_t size{0};
    };
} // namespace DeerAllocator

/*
static constexpr unsigned alignment
static constexpr goodSize(size_t);
SBlock allocate(size_t);
SBlock allocateAll()
SBlock alignedAllocate(size_t, unsigned);
bool expand(SBlock&, size_t delta);
void reallocate(SBlock&, size_t);
void alignedReallocate(SBlock&, size_t, unsigned);
bool owns(const SBlock&);
void deallocate(Blk&);
void deallocateAll();
*/
