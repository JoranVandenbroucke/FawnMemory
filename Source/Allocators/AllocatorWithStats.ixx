//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <source_location>
export module DeerAllocator:AllocatorWithStats;
import :Common;

namespace DeerAllocator
{
enum statistic_options : uint16_t
{
    num_owns = 1U << 0,
    num_allocate = 1U << 1,
    num_allocate_ok = 1U << 2,
    num_deallocate = 1U << 4,
    num_all = num_owns | num_allocate | num_allocate_ok | num_deallocate,
    bytes_allocated = 1U << 5,
    bytes_deallocated = 1U << 6,
    bytes_high_tide = 1U << 7,
    bytes_all = bytes_allocated | bytes_deallocated | bytes_high_tide,
    caller_size = 1U << 8,
    caller_file = 1U << 9,
    caller_function = 1U << 10,
    caller_line = 1U << 11,
    caller_time = 1U << 12,
    caller_all = caller_size | caller_file | caller_function | caller_line | caller_time,
    all = num_all | bytes_all | caller_all
};

export template<class Allocator, statistic_options Flags>
class CAllocatorWithStats
{
public:
    constexpr auto allocate( const std::size_t size, const std::source_location& location = std::source_location::current() ) noexcept -> SBlock
    {
        const SBlock block = m_allocator.allocate( size );

        if constexpr ( ( Flags & num_allocate ) != 0 )
        {
            ++m_numAllocations;
        }
        if constexpr ( ( ( Flags & num_allocate_ok ) != 0 ) && ( block.pData != nullptr ) )
        {
            ++m_numAllocationsOk;
        }
        if constexpr ( ( Flags & bytes_allocated ) != 0 )
        {
            m_bytesAllocated += size;
        }
        if constexpr ( ( ( Flags & bytes_high_tide ) != 0 ) && m_bytesAllocated > m_highTide )
        {
            m_highTide = m_bytesAllocated;
        }
        if constexpr ( ( Flags & caller_size ) != 0 )
        {
            g_pHead->callerSize = size;
        }
        if constexpr ( ( Flags & caller_file ) != 0 )
        {
            g_pHead->callerFile = location.file_name();
        }
        if constexpr ( ( Flags & caller_function ) != 0 )
        {
            g_pHead->callerLine = location.function_name();
        }
        if constexpr ( ( Flags & caller_line ) != 0 )
        {
            g_pHead->callerLine = location.line();
        }
        if constexpr ( ( Flags & caller_time ) != 0 )
        {
            g_pHead->callerTime = std::chrono::system_clock::now();
        }

        // if ( (Flags & num_owns) != 0 )
        // ++numOwns;

        // if (flags & caller_all) {
        //     AllocationInfo* info = new AllocationInfo();
        //     info->next = head;
        //     head->previous = info;
        //     head = info;
        // }

        return block;
    }

    [[nodiscard]] constexpr auto owns( const SBlock& block ) const noexcept -> bool
    {
        return m_allocator.owns( block );
    }

    constexpr void deallocate( const SBlock& block ) noexcept
    {
        m_allocator.deallocate( block );

        if constexpr ( ( Flags & num_deallocate ) != 0 )
        {
            ++m_numDeallocations;
        }
        if constexpr ( ( Flags & bytes_deallocated ) != 0 )
        {
            m_bytesDeallocated += block.size;
        }
        if constexpr ( ( Flags & bytes_allocated ) != 0 )
        {
            m_bytesAllocated -= block.size;
        }
    }

    static void PrintStats()
    {
        SAllocationInfo* pCurrent{g_pHead};
        while ( pCurrent )
        {
            std::cout << "Size: " << pCurrent->callerSize
                    << ", File: " << pCurrent->callerFile
                    << ", Function: " << pCurrent->callerFunction
                    << ", Line: " << pCurrent->callerLine
                    << ", Time: " << std::chrono::duration_cast<std::chrono::microseconds>( pCurrent->callerTime.time_since_epoch() ).count()
                    << " microseconds since epoch\n";
            pCurrent = pCurrent->next;
        }
    }

private:
    struct SAllocationInfo
    {
        std::size_t callerSize{};
        std::chrono::time_point<std::chrono::system_clock> callerTime;
        SAllocationInfo* previous;
        SAllocationInfo* next;
        const char* callerFile{};
        const char* callerFunction{};
        int callerLine{};

        friend auto operator==( const SAllocationInfo& lhs, const SAllocationInfo& rhs ) -> bool
        {
            return lhs.callerSize == rhs.callerSize
                    && ( lhs.callerFile == rhs.callerFile || std::strcmp( lhs.callerFile, rhs.callerFile ) != 0 )
                    && ( std::strcmp( lhs.callerFunction, rhs.callerFunction ) != 0 || std::strcmp( lhs.callerFunction, rhs.callerFunction ) != 0 )
                    && lhs.callerLine == rhs.callerLine;
        }

        friend auto operator!=( const SAllocationInfo& lhs, const SAllocationInfo& rhs ) -> bool
        {
            return !( lhs == rhs );
        }
    };

    Allocator m_allocator;
    std::size_t m_numAllocations{};
    std::size_t m_numAllocationsOk{};
    std::size_t m_numDeallocations{};
    std::size_t m_numOwns{};
    std::size_t m_bytesAllocated{};
    std::size_t m_bytesDeallocated{};
    std::size_t m_highTide{};
    static SAllocationInfo* g_pHead;
};

template<class Allocator, statistic_options Flags>
typename CAllocatorWithStats<Allocator, Flags>::SAllocationInfo* CAllocatorWithStats<Allocator, Flags>::g_pHead = nullptr;
}// namespace DeerAllocator
