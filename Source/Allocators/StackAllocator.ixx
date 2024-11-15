//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <bit>
#include <cstddef>
export module DeerAllocator.StackAllocator;
import DeerAllocator.Common;

namespace DeerAllocator
{
    export template<std::size_t Size>
    class CStackAllocator
    {
    public:
        CStackAllocator()
            : m_p( m_d )
        {
        }

        constexpr auto allocate( const std::size_t size ) noexcept -> SBlock
        {
            const std::size_t newSize{RoundToAlign(size)};
            if(newSize > m_d + Size - m_p)
            {
                return SBlock{.pData=nullptr, .size=0U};
            }
            const SBlock result{.pData=m_p, .size=size};
            m_p += newSize;
            return result;
        }

        [[nodiscard]] constexpr auto owns( const SBlock& block ) const noexcept -> bool
        {
            return block.pData >= m_d && block.pData <= m_d + Size;
        }

        constexpr void deallocate( SBlock& block ) noexcept
        {
            if ( std::bit_cast<unsigned char*>(block.pData) + RoundToAlign( block.size ) == m_p )
            {
                m_p = std::bit_cast<unsigned char*>(block.pData);
                block.pData = nullptr;
                block.size = 0;
            }
        }

        constexpr void dealloc_all()
        {
            m_p = m_d;
        }

    private:
        unsigned char m_d[ Size ]{};
        unsigned char* m_p;

        static constexpr auto RoundToAlign( const std::size_t size ) -> std::size_t
        {
            constexpr std::size_t alignment { sizeof( void* ) - 1};// Typically 4 or 8 bytes
            return ( size + alignment ) & ~alignment;
        }
    };
}// namespace DeerAllocator
