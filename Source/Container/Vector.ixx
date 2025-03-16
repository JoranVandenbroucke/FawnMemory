//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <algorithm>
#include <cassert>
#include <memory>
#include <stdexcept>

#include "ContainerCompatibleRange.hpp"
export module DeerContainer:Vector;
import :CompressedPair;

namespace DeerContainer
{
/**
 * @cond TURN_OFF_DOXYGEN
 */
template <class Itr>
class CVectorIterator
{
  public:
    using value_type        = typename std::iterator_traits<Itr>::value_type;
    using difference_type   = typename std::iterator_traits<Itr>::difference_type;
    using pointer           = typename std::iterator_traits<Itr>::pointer;
    using reference         = typename std::iterator_traits<Itr>::reference;
    using iterator_category = typename std::iterator_traits<Itr>::iterator_category;
    using iterator_concept  = std::contiguous_iterator_tag;

    constexpr CVectorIterator() noexcept : m_itr{} {}

    constexpr explicit CVectorIterator(Itr value) noexcept : m_itr{value} {}

    template <typename Other>
        requires std::is_constructible_v<Itr, Other>
    CVectorIterator(const CVectorIterator<Other>& other) noexcept : m_itr{other.base()}
    {
    }

    constexpr auto operator++() noexcept -> CVectorIterator&
    {
        ++m_itr;
        return *this;
    }

    constexpr auto operator++(int) const noexcept -> CVectorIterator
    {
        CVectorIterator orig{*this};
        ++*this;
        return orig;
    }

    constexpr auto operator--() noexcept -> CVectorIterator&
    {
        --m_itr;
        return *this;
    }

    constexpr auto operator--(int) const noexcept -> CVectorIterator
    {
        CVectorIterator orig{*this};
        --*this;
        return orig;
    }

    constexpr auto operator+(difference_type value) const noexcept -> CVectorIterator
    {
        CVectorIterator temp{*this};
        temp += value;
        return temp;
    }

    constexpr auto operator+=(difference_type value) noexcept -> CVectorIterator&
    {
        m_itr += value;
        return *this;
    }

    constexpr auto operator-(difference_type value) const noexcept -> CVectorIterator
    {
        return *this + (-value);
    }

    constexpr auto operator-=(difference_type value) noexcept -> CVectorIterator&
    {
        *this += (-value);
        return *this;
    }

    friend constexpr auto operator-(const CVectorIterator& lhs, const CVectorIterator& rhs) noexcept -> difference_type
    {
        return lhs.base() - rhs.base();
    }

    friend constexpr auto operator+(difference_type lhs, CVectorIterator rhs) noexcept -> CVectorIterator
    {
        rhs += lhs;
        return rhs;
    }

    [[nodiscard]] constexpr auto operator->() const noexcept -> pointer
    {
        return std::to_address(m_itr);
    }

    [[nodiscard]] constexpr auto operator*() const noexcept -> reference
    {
        return *m_itr;
    }

    [[nodiscard]] constexpr auto operator[](const difference_type value) const noexcept -> reference
    {
        return m_itr[value];
    }

    friend auto operator==(const CVectorIterator& lhs, const CVectorIterator& rhs) -> bool
    {
        return lhs.base() == rhs.base();
    }

    friend auto operator!=(const CVectorIterator& lhs, const CVectorIterator& rhs) -> bool
    {
        return !(lhs == rhs);
    }

    friend auto operator<(const CVectorIterator& lhs, const CVectorIterator& rhs) -> bool
    {
        return lhs.base() < rhs.base();
    }

    friend auto operator<=(const CVectorIterator& lhs, const CVectorIterator& rhs) -> bool
    {
        return !(rhs < lhs);
    }

    friend auto operator>(const CVectorIterator& lhs, const CVectorIterator& rhs) -> bool
    {
        return rhs < lhs;
    }

    friend auto operator>=(const CVectorIterator& lhs, const CVectorIterator& rhs) -> bool
    {
        return !(lhs < rhs);
    }

    constexpr auto base() const noexcept -> Itr
    {
        return m_itr;
    }

  private:
    Itr m_itr;
};
/**
 * @endcond
 */

export template <class T, class Allocator = std::allocator<T>>
class CVector
{
  public:
    using value_type             = T;
    using allocator_type         = Allocator;
    using size_type              = typename std::allocator_traits<allocator_type>::size_type;
    using difference_type        = typename std::allocator_traits<allocator_type>::difference_type;
    using pointer                = typename std::allocator_traits<allocator_type>::pointer;
    using const_pointer          = typename std::allocator_traits<allocator_type>::const_pointer;
    using reference              = value_type&;
    using const_reference        = const value_type&;
    using iterator               = CVectorIterator<pointer>;
    using const_iterator         = CVectorIterator<const_pointer>;
    using reverse_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    constexpr CVector() noexcept = default;

    constexpr explicit CVector(const Allocator& alloc) noexcept : m_max{nullptr, alloc} {}

    constexpr CVector(size_type count, const value_type& value, const Allocator& alloc = Allocator()) : m_max{nullptr, alloc}
    {
        if (count < 0)
        {
            return;
        }
        Allocate(count);
        do_work(count, value);
    }

    constexpr explicit CVector(size_type count, const Allocator& alloc = Allocator()) : m_max{nullptr, alloc}
    {
        Allocate(count);
    }

    template <class InputIt>
    constexpr CVector(InputIt first, InputIt last, const Allocator& alloc = Allocator()) : m_max{nullptr, alloc}
    {
        for (; first != last; ++first)
        {
            emplace_back(*first);
        }
    }

    ~CVector()
    {
        clear();
        std::allocator_traits<allocator_type>::deallocate(m_max.second, m_begin, capacity());
        m_begin = m_end = m_max.first = nullptr;
    }

    CVector(const CVector& other)                        = default;
    CVector(CVector&& other) noexcept                    = default;
    auto operator=(const CVector& other) -> CVector&     = default;
    auto operator=(CVector&& other) noexcept -> CVector& = default;

    constexpr auto operator=(std::initializer_list<value_type> iList) -> CVector&
    {
        if (iList.size())
        {
            Allocate(iList.size());
            ConstructData(iList.begin(), iList.end(), iList.size());
        }

        return *this;
    }

    constexpr auto get_allocator() const -> allocator_type
    {
        return m_max.second;
    }

    constexpr void swap(CVector& other) noexcept
    {
        using std::swap;
        swap(m_begin, other.m_begin);
        swap(m_end, other.m_end);
        swap(m_max, other.m_max);
    }

    friend constexpr void swap(CVector& lhs, CVector& rhs) noexcept
    {
        lhs.swap(rhs);
    }

    constexpr void assign(size_type count, const value_type& value)
    {
        clear();
        if (capacity() < count)
        {
            reserve(count);
        }

        for (size_type i = 0; i < count; ++i)
        {
            std::allocator_traits<allocator_type>::construct(m_max.second, std::to_address(m_end), value);
            ++m_end;
        }
    }

    template <class InputIt>
    constexpr void assign(InputIt first, InputIt last)
    {
        clear();

        const size_type count = std::distance(first, last);
        if (capacity() < count)
        {
            reserve(count);
        }

        for (; first != last; ++first)
        {
            std::allocator_traits<allocator_type>::construct(m_max.second, std::to_address(m_end), *first);
            ++m_end;
        }
    }

    constexpr void assign(std::initializer_list<value_type> iList)
    {
        assign(iList.begin(), iList.end());
    }

    constexpr auto get_allocator() -> allocator_type
    {
        return m_max.second;
    }

    constexpr auto at(size_type pos) -> reference
    {
        if (pos >= size())
        {
            throw std::out_of_range("Vector::at out of range.");
        }
        return m_begin[pos];
    }

    constexpr auto at(size_type pos) const -> const_reference
    {
        if (pos >= size())
        {
            throw std::out_of_range("Vector::at out of range.");
        }
        return m_begin[pos];
    }

    constexpr auto operator[](size_type idx) noexcept -> reference
    {
        return m_begin[idx];
    }

    constexpr auto operator[](size_type idx) const noexcept -> const_reference
    {
        return m_begin[idx];
    }

    constexpr auto f() -> const_reference
    {
        return at(0);
    }

    constexpr auto back() -> const_reference
    {
        return at(empty() ? 0 : size() - 1);
    }

    constexpr auto begin() noexcept -> iterator
    {
        return iterator(m_begin);
    }

    constexpr auto begin() const noexcept -> const_iterator
    {
        return const_iterator(m_begin);
    }

    constexpr auto cbegin() const noexcept -> const_iterator
    {
        return const_iterator(m_begin);
    }

    constexpr auto end() noexcept -> iterator
    {
        return iterator(m_end);
    }

    constexpr auto end() const noexcept -> const_iterator
    {
        return const_iterator(m_end);
    }

    constexpr auto cend() const noexcept -> const_iterator
    {
        return const_iterator(m_end);
    }

    constexpr auto rbegin() noexcept -> reverse_iterator
    {
        return reverse_iterator(end());
    }

    constexpr auto rbegin() const noexcept -> const_reverse_iterator
    {
        return const_reverse_iterator(end());
    }

    constexpr auto crbegin() const noexcept -> const_reverse_iterator
    {
        return const_reverse_iterator(cend());
    }

    constexpr auto rend() noexcept -> reverse_iterator
    {
        return reverse_iterator(begin());
    }

    constexpr auto rend() const noexcept -> const_reverse_iterator
    {
        return const_reverse_iterator(begin());
    }

    constexpr auto crend() const noexcept -> const_reverse_iterator
    {
        return const_reverse_iterator(cbegin());
    }

    [[nodiscard]] constexpr auto empty() const noexcept -> bool
    {
        return m_begin == m_end;
    }

    constexpr auto size() const noexcept -> size_type
    {
        return m_end - m_begin;
    }

    constexpr auto max_size() const noexcept -> size_type
    {
        return std::min<size_type>(std::allocator_traits<allocator_type>::max_size(m_max.second),
                                   std::numeric_limits<difference_type>::max());
    }

    constexpr auto capacity() const noexcept -> size_type
    {
        return m_max.first - m_begin;
    }

    constexpr void reserve(size_type newCap)
    {
        if (newCap <= capacity())
        {
            return;
        }
        if (newCap > max_size())
        {
            throw std::length_error("Vector");
        }
        const pointer newBegin{std::allocator_traits<allocator_type>::allocate(m_max.second, newCap)};
        pointer       newEnd{newBegin};

        for (pointer pos{m_begin}; pos != m_end; ++pos, ++newEnd)
        {
            std::allocator_traits<allocator_type>::construct(m_max.second, std::to_address(newEnd), std::move_if_noexcept(*pos));
            std::allocator_traits<allocator_type>::destroy(m_max.second, std::to_address(pos));
        }

        std::allocator_traits<allocator_type>::deallocate(m_max.second, m_begin, capacity());
        m_begin     = newBegin;
        m_end       = newEnd;
        m_max.first = m_begin + newCap;
    }

    constexpr void shrink_to_fit()
    {
        std::allocator_traits<allocator_type>::deallocate(m_max.second, m_max.second(), m_max.second() - m_end);
        m_max.first = m_end;
    }

    constexpr void clear()
    {
        pointer currentEnd = m_end;
        while (m_begin != currentEnd)
        {
            std::allocator_traits<allocator_type>::destroy(m_max.second, std::to_address(--currentEnd));
        }
        m_end = currentEnd;
    }

    constexpr auto insert(const_iterator pos, const_reference value) -> iterator
    {
        pointer pntr{m_begin + (pos - begin())};
        if (m_end < m_max.first)
        {
            if (pntr == m_end)
            {
                ConstructOneData(value);
            }
            else
            {
                MoveRange(pntr, m_end, pntr + 1);
                const_pointer pntrTo{std::pointer_traits<const_pointer>::pointer_to(value)};
                if (std::less()(std::to_address(pntr), std::addressof(value))
                    && !std::less()(std::to_address(m_end), std::addressof(value)))
                {
                    ++pntrTo;
                }
                *pntr = *pntrTo;
            }
        }
        else
        {
            allocator_type& alloc{m_max.second}; // Use allocator for buffer growth

            // Calculate new capacity
            size_type newCapacity{Recommend(size() + 1)};

            // Allocate new buffer
            pointer newBegin{alloc.allocate(newCapacity)};
            pointer newEnd{newBegin};

            // Move elements to the new buffer
            pointer oldPos{m_begin};
            while (oldPos != pntr)
            {
                *newEnd = *oldPos;
                ++oldPos;
                ++newEnd;
            }

            // Insert the new element at the correct position
            std::allocator_traits<allocator_type>::construct(alloc, std::to_address(newEnd), value);
            ++newEnd;

            // Move remaining elements after position `pntr`
            while (oldPos != m_end)
            {
                *newEnd = *oldPos;
                ++oldPos;
                ++newEnd;
            }

            std::allocator_traits<allocator_type>::deallocate(alloc, m_begin, capacity());

            // Update pointers to new buffer
            m_begin     = newBegin;
            m_end       = newEnd;
            m_max.first = newBegin + newCapacity;
        }

        return iterator(pntr);
    }

    constexpr auto insert(const_iterator pos, value_type&& value) -> iterator
    {
        pointer pntr{m_begin + (pos - begin())};
        if (m_end < m_max.first)
        {
            if (pntr == m_end)
            {
                ConstructOneData(std::move(value));
            }
            else
            {
                MoveRange(pntr, m_end, pntr + 1);
                *pntr = std::move(value);
            }
        }
        else
        {
            allocator_type& alloc{m_max.second}; // Use allocator for buffer growth

            // Calculate new capacity
            size_type newCapacity{Recommend(size() + 1)};

            // Allocate new buffer
            pointer newBegin{alloc.allocate(newCapacity)};
            pointer newEnd{newBegin};

            // Move elements to the new buffer
            pointer oldPos{m_begin};
            while (oldPos != pntr)
            {
                *newEnd = *oldPos;
                ++oldPos;
                ++newEnd;
            }

            // Insert the new element at the correct position
            std::allocator_traits<allocator_type>::construct(alloc, std::to_address(newEnd), value);
            ++newEnd;

            // Move remaining elements after position `pntr`
            while (oldPos != m_end)
            {
                *newEnd = *oldPos;
                ++oldPos;
                ++newEnd;
            }

            std::allocator_traits<allocator_type>::deallocate(alloc, m_begin, capacity());

            // Update pointers to new buffer
            m_begin     = newBegin;
            m_end       = newEnd;
            m_max.first = newBegin + newCapacity;
        }

        return iterator(pntr);
    }

    constexpr auto insert(const_iterator pos, size_type count, const value_type& value) -> iterator
    {
        pointer pntr{m_begin + (pos - begin())};
        if (count > 0)
        {
            // We can't compare unrelated pointers inside constant expressions
            if (!std::is_constant_evaluated() && count <= static_cast<size_type>(m_max.first - m_end))
            {
                size_type oldCount{count};
                pointer   oldLast{m_end};
                if (count > static_cast<size_type>(m_end - pntr))
                {
                    size_type number{count - (m_end - pntr)};
                    ConstructData(number, value);
                    count -= number;
                }
                if (count > 0)
                {
                    MoveRange(pntr, oldLast, pntr + oldCount);
                    const_pointer valuePntr = std::pointer_traits<const_pointer>::pointer_to(value);
                    if (pntr <= valuePntr && valuePntr < m_end)
                    {
                        valuePntr += oldCount;
                    }
                    std::fill_n(pntr, value, *valuePntr);
                }
            }
            else
            {
                allocator_type& alloc{m_max.second}; // Use allocator for buffer growth

                // Calculate new capacity
                size_type newCapacity{Recommend(size() + count)};

                // Allocate new buffer
                pointer newBegin{alloc.allocate(newCapacity)};
                pointer newEnd{newBegin};

                // Move elements to the new buffer
                pointer oldPos{m_begin};
                while (oldPos != pntr)
                {
                    *newEnd = *oldPos;
                    ++oldPos;
                    ++newEnd;
                }

                // Insert the new element at the correct position
                ConstructData(count, value);
                ++newEnd;

                // Move remaining elements after position `pntr`
                while (oldPos != m_end)
                {
                    *newEnd = *oldPos;
                    ++oldPos;
                    ++newEnd;
                }

                std::allocator_traits<allocator_type>::deallocate(alloc, m_begin, capacity());

                // Update pointers to new buffer
                m_begin     = newBegin;
                m_end       = newEnd;
                m_max.first = newBegin + newCapacity;
            }
        }
        return iterator(pntr);
    }

    template <class InputIt>
    constexpr auto insert(const_iterator pos, InputIt first, InputIt last) -> iterator
    {
        difference_type offset{pos - begin()};
        pointer         pPointer{m_begin + offset};
        allocator_type& allocator{m_max.second};
        pointer         oldLast = m_end;
        for (; m_end != m_max.first && first != last; ++first)
        {
            ConstructOneData(*first);
        }
        if (first != last)
        {
            // Calculate new capacity
            size_type extraSize{last - first};
            size_type newCapacity{Recommend(size() + extraSize)};

            // Allocate new buffer
            pointer newBegin{allocator.allocate(newCapacity)};
            pointer newEnd{newBegin};

            // Move elements to the new buffer
            pointer oldPos{m_begin};
            while (oldPos != pPointer)
            {
                *newEnd = *oldPos;
                ++oldPos;
                ++newEnd;
            }

            // Insert the new element at the correct position
            while (first != last)
            {
                *newEnd = *first;
                ++first;
                ++newEnd;
            }
            newEnd += extraSize;

            // Move remaining elements after position `pntr`
            while (oldPos != m_end)
            {
                *newEnd = *oldPos;
                ++oldPos;
                ++newEnd;
            }

            std::allocator_traits<allocator_type>::deallocate(allocator, m_begin, capacity());

            // Update pointers to new buffer
            m_begin     = newBegin;
            m_end       = newEnd;
            m_max.first = newBegin + newCapacity;
        }
        pos = std::rotate(pos, oldLast, m_end);
        return begin() + offset;
    }

    constexpr auto insert(const_iterator pos, std::initializer_list<value_type> ilist) -> iterator
    {
        return insert(pos, ilist.begin(), ilist.end());
    }

    template <class... Args>
    constexpr auto emplace(const_iterator pos, Args&&... args) -> iterator
    {
        pointer pointer = m_begin + (pos - begin());
        if (pointer == m_end)
        {
            ConstructData(std::forward<Args>(args)...);
        }
        else
        {
            value_type temp;
            std::allocator_traits<allocator_type>::construct(m_max.second, &temp, std::forward<Args>(args)...);
            MoveRange(pointer, m_end, pointer + 1);
            *pointer = std::move(temp);
        }

        return iterator(pointer);
    }

    constexpr auto erase(const_iterator pos) -> iterator
    {
        pointer pointerPosition = m_begin + (pos - cbegin());
        DeconstructData(std::move(pointerPosition + 1, m_end, pointerPosition));
        return iterator(pointerPosition);
    }

    constexpr auto erase(const_iterator first, const_iterator last) -> iterator
    {
        if (first == last)
        {
            return first;
        }
        pointer pPointer = m_begin + (first - cbegin());
        DeconstructData(std::move(pPointer + (last - first), m_end, pPointer));
        return iterator(pPointer);
    }

    constexpr void push_back(const_reference value)
    {
        if (m_end >= m_max.first)
        {
            reserve(std::min((capacity() + 1) * 2, max_size()));
        }
        ConstructOneData(std::move(value));
    }

    constexpr void push_back(value_type&& value)
    {
        if (m_end >= m_max.first)
        {
            reserve(std::min((capacity() + 1) * 2, max_size()));
        }
        ConstructOneData(std::move(value));
    }

    template <class... Args>
    constexpr auto emplace_back(Args&&... args) -> reference
    {
        if (m_end >= m_max.first)
        {
            reserve(std::min((capacity() + 1) * 2, max_size()));
        }
        ConstructOneData(std::forward<Args>(args)...);
        return *(m_end - 1);
    }

    template <ContainerCompatibleRange<T> R>
    constexpr void append_range(R&& range)
    {
        const auto range_size = std::ranges::distance(std::forward<R>(range));

        if (range_size <= 0)
        {
            return;
        }

        if (size() + range_size > capacity())
        {
            reserve(size() + range_size);
        }

        for (auto&& elem : range)
        {
            ConstructOneData(std::forward<decltype(elem)>(elem));
        }
    }

    constexpr void pop_back()
    {
        // todo : add assert
        assert(!empty() && "SVector::pop_back called on an empty vector");
        DeconstructData(m_end - 1);
    }

    constexpr void resize(size_type count)
    {
        const size_type current_size = size();

        if (count > current_size)
        {
            if (count > capacity())
            {
                reserve(count);
            }
            ConstructData(count - current_size);
        }
        else if (count < current_size)
        {
            pointer new_end = m_begin + count;
            for (pointer pos = new_end; pos != m_end; ++pos)
            {
                std::allocator_traits<allocator_type>::destroy(m_max.second, std::to_address(pos));
            }
            m_end = new_end;
        }
    }

    constexpr void resize(size_type count, const value_type& value)
    {
        const size_type current_size = size();

        if (count > current_size)
        {
            if (count > capacity())
            {
                reserve(count);
            }
            ConstructData(count - current_size, value);
        }
        else if (count < current_size)
        {
            pointer new_end = m_begin + count;
            for (pointer pos = new_end; pos != m_end; ++pos)
            {
                std::allocator_traits<allocator_type>::destroy(m_max.second, std::to_address(pos));
            }
            m_end = new_end;
        }
    }

  private:
    pointer                                  m_begin{nullptr};
    pointer                                  m_end{nullptr};
    SCompressedPair<pointer, allocator_type> m_max{nullptr, allocator_type{}};

    constexpr auto Recommend(size_type newSize) const -> size_type
    {
        const size_type maxSize{max_size()};
        if (newSize > maxSize)
        {
            throw std::length_error("SVector: asking for to much memory");
        }
        const size_type cap{capacity()};
        if (cap >= maxSize / 2)
        {
            return maxSize;
        }
        return std::max<size_type>(2 * cap, newSize);
    }

    constexpr void Allocate(size_type size)
    {
        const auto allocation{std::allocator_traits<allocator_type>::allocate_at_least(m_max.second, size)};
        m_begin     = allocation.ptr;
        m_end       = allocation.ptr;
        m_max.first = m_begin + allocation.count;
    }

    constexpr void ConstructData(size_type count)
    {
        for (pointer pos{m_end}; pos != m_end + count; ++pos)
        {
            std::allocator_traits<allocator_type>::construct(this->m_max.second, std::to_address(pos));
        }
        m_end += count;
    }

    constexpr void ConstructData(size_type count, const value_type& value)
    {
        for (pointer pos{m_end}; pos != m_end + count; ++pos)
        {
            std::allocator_traits<allocator_type>::construct(this->m_max.second, std::to_address(pos), value);
        }
        m_end += count;
    }

    template <class IteratorType>
    constexpr void ConstructData(IteratorType first, IteratorType last /*, size_type size*/)
    {
        pointer pos{m_end};
        for (; first != last; ++first, ++pos)
        {
            std::allocator_traits<allocator_type>::construct(m_max.second, std::to_address(pos), *first);
        }

        m_end = pos;
    }

    template <class... Args>
    constexpr void ConstructOneData(Args&&... args)
    {
        std::allocator_traits<allocator_type>::construct(m_max.second, std::to_address(m_end), std::forward<Args>(args)...);
        ++m_end;
    }

    constexpr void DeconstructData(pointer newEnd) noexcept
    {
        pointer soonToBeEnd{m_end};
        while (soonToBeEnd != newEnd)
        {
            std::allocator_traits<allocator_type>::destroy(this->m_max.second, std::to_address(--soonToBeEnd));
        }
        m_end = soonToBeEnd;
    }

    constexpr void MoveRange(pointer begin, pointer end, pointer destination)
    {
        pointer         oldEnd{m_end};
        difference_type number = oldEnd - destination;
        pointer         index  = begin + number;

        pointer editableEnd{m_end};
        for (pointer pos{editableEnd}; index < end; ++index, (void) ++pos, editableEnd = pos)
        {
            std::allocator_traits<allocator_type>::construct(m_max.second, pos, *index);
        }
        m_end = editableEnd;
        std::move_backward(begin, begin + number, oldEnd);
    }
};
} // namespace DeerContainer
