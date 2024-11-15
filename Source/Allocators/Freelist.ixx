//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <bit>
#include <cstddef>
export module DeerAllocator.Freelist;
import DeerAllocator.Common;

namespace DeerAllocator
{
export template<class Allocator, std::size_t Size, std::size_t Min = Size, std::size_t Max = Size, std::size_t MaxFree = 1024>
class CFreelist : Allocator
{
public:
    constexpr auto allocate( const std::size_t size ) noexcept -> SBlock
    {
        if ( size >= Min && size <= Max && m_root )
        {
            const SBlock block{m_root, size};
            m_root = m_root.next;
            --m_freeSize;
            return block;
        }
        return Allocator::allocate( size );
    }

    [[nodiscard]] constexpr auto owns( const SBlock& block ) const noexcept -> bool
    {
        return ( block.size >= Min && block.size <= Max ) || Allocator::owns( block );
    }

    constexpr void deallocate( SBlock& block )
    {
        if ( block.size != Size || m_freeSize == MaxFree )
        {
            Allocator::deallocate( block );
            return;
        }
        SNode* pNode{std::bit_cast<SNode*>( block.pData )};
        pNode->next = m_root;
        m_root = pNode;
        ++m_freeSize;
        block.pData = nullptr;
        block.size = 0;
    }

private:
    std::size_t m_freeSize{};

    struct SNode
    {
        SNode* next;
    } m_root;
};
}// namespace DeerAllocator
