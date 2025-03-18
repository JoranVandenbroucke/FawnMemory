//
// Copyright (c) 2024.
// Author: Joran.
//

// based on https://github.com/skypjack/key/blob/master/src/key/entity/sparse_set.hpp
module;
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <memory>
#include <type_traits>
#include <utility>

#include "ValueTraits.hpp"
export module DeerContainer:SparseSet;
import :CompressedPair;
import :Pair;
import :Vector;

namespace DeerContainer
{
enum class deletion_policy : std::uint8_t
{
    swap_and_pop = 0u,
    in_place     = 1u,
    swap_only    = 2u,
    unspecified  = swap_and_pop,
};

template <typename Container>
class CSparseSetIterator final
{

  public:
    using value_type        = typename Container::value_type;
    using pointer           = typename Container::const_pointer;
    using reference         = typename Container::const_reference;
    using difference_type   = typename Container::difference_type;
    using iterator_category = std::random_access_iterator_tag;

    constexpr CSparseSetIterator() noexcept : m_packed{}, m_offset{} {}

    constexpr CSparseSetIterator(const Container& ref, const difference_type idx) noexcept : m_packed{&ref}, m_offset{idx} {}

    // todo : EnTT does operator--()  and operator++() the otherway around. Double check performance!
    constexpr CSparseSetIterator& operator++() noexcept
    {
        return ++m_offset, *this;
    }

    constexpr CSparseSetIterator operator++(int) noexcept
    {
        const CSparseSetIterator orig{*this};
        operator++();
        return orig;
    }

    constexpr CSparseSetIterator& operator--() noexcept
    {
        --m_offset;
        return *this;
    }

    constexpr CSparseSetIterator operator--(int) noexcept
    {
        const CSparseSetIterator orig{*this};
        operator--();
        return orig;
    }

    // todo : EnTT does operator-=()  and operator+=() the otherway around. Double check performance!
    constexpr CSparseSetIterator& operator+=(const difference_type value) noexcept
    {
        m_offset += value;
        return *this;
    }

    constexpr CSparseSetIterator operator+(const difference_type value) const noexcept
    {
        CSparseSetIterator copy{*this};
        copy += value;
        return copy;
    }

    constexpr CSparseSetIterator& operator-=(const difference_type value) noexcept
    {
        *this += -value;
        return *this;
    }

    constexpr CSparseSetIterator operator-(const difference_type value) const noexcept
    {
        return (*this + -value);
    }

    [[nodiscard]] constexpr reference operator[](const difference_type value) const noexcept
    {
        return (*m_packed)[static_cast<typename Container::size_type>(index() + value)];
    }

    [[nodiscard]] constexpr pointer operator->() const noexcept
    {
        return std::addressof(operator[](0));
    }

    [[nodiscard]] constexpr reference operator*() const noexcept
    {
        return operator[](0);
    }

    [[nodiscard]] constexpr pointer data() const noexcept
    {
        return m_packed ? m_packed->data() : nullptr;
    }

    [[nodiscard]] constexpr difference_type index() const noexcept
    {
        return m_offset;
    }

    friend auto operator==(const CSparseSetIterator& lhs, const CSparseSetIterator& rhs) -> bool
    {
        return std::tie(lhs.m_packed, lhs.m_offset) == std::tie(rhs.m_packed, rhs.m_offset);
    }

    friend auto operator!=(const CSparseSetIterator& lhs, const CSparseSetIterator& rhs) -> bool
    {
        return !(lhs == rhs);
    }

    friend auto operator<(const CSparseSetIterator& lhs, const CSparseSetIterator& rhs) -> bool
    {
        return std::tie(lhs.m_packed, lhs.m_offset) < std::tie(rhs.m_packed, rhs.m_offset);
    }

    friend auto operator<=(const CSparseSetIterator& lhs, const CSparseSetIterator& rhs) -> bool
    {
        return !(rhs < lhs);
    }

    friend auto operator>(const CSparseSetIterator& lhs, const CSparseSetIterator& rhs) -> bool
    {
        return rhs < lhs;
    }

    friend auto operator>=(const CSparseSetIterator& lhs, const CSparseSetIterator& rhs) -> bool
    {
        return !(lhs < rhs);
    }

  private:
    const Container* m_packed;
    difference_type  m_offset;
};

export template <class Value, class Allocator = std::allocator<Value>>
class CSparseSet final
{
    using alloc_traits = std::allocator_traits<Allocator>;
    static_assert(std::is_same_v<typename alloc_traits::value_type, Value>, "Invalid key type");

    using sparse_container_type =
        CVector<typename alloc_traits::pointer, typename alloc_traits::template rebind_alloc<typename alloc_traits::pointer>>;
    using packed_container_type = CVector<Value, Allocator>;
    using type_traits           = key_traits<Value>;

  public:
    using type_key     = typename type_traits::key_type;
    using type_version = typename type_traits::version_type;

    using key_type        = type_key;
    using value_type      = Value;
    using size_type       = typename Allocator::size_type;
    using difference_type = typename Allocator::difference_type;
    // using key_compare       = Compare;
    // using value_compare     = Compare;
    using allocator_type    = Allocator;
    using reference         = value_type&;
    using const_reference   = const value_type&;
    using reverse_reference = const value_type&;

    using pointer                = std::allocator_traits<Allocator>::pointer;
    using const_pointer          = std::allocator_traits<Allocator>::const_pointer;
    using iterator               = CSparseSetIterator<packed_container_type>;
    using const_iterator         = const CSparseSetIterator<packed_container_type>;
    using reverse_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    using node_type = SPair<key_type, value_type>;

    CSparseSet()  = default;
    ~CSparseSet() = default;

    // todo : find a way to unionize all allocators, so that sending one is sending all ...
    constexpr allocator_type get_allocator() const noexcept
    {
        return m_sparse.get_allocator();
    }

    // todo : EnTT does begin() and end() the otherway around. Double check performance!
    constexpr iterator begin() noexcept
    {
        return iterator{m_packed, {}};
    }
    constexpr const_iterator begin() const noexcept
    {
        return const_iterator{m_packed, {}};
    }
    constexpr const_iterator cbegin() const noexcept
    {
        return const_iterator{m_packed, {}};
    }
    constexpr iterator end() noexcept
    {
        const auto pos{static_cast<typename iterator::difference_type>(m_packed.size())};
        return iterator{m_packed, pos};
    }
    constexpr const_iterator end() const noexcept
    {
        const auto pos{static_cast<typename iterator::difference_type>(m_packed.size())};
        return const_iterator{m_packed, pos};
    }
    constexpr const_iterator cend() const noexcept
    {
        const auto pos{static_cast<typename iterator::difference_type>(m_packed.size())};
        return const_iterator{m_packed, pos};
    }
    constexpr reverse_iterator rbegin() noexcept
    {
        return std::make_reverse_iterator(end());
    }
    constexpr const_reverse_iterator rbegin() const noexcept
    {
        return std::make_reverse_iterator(cend());
    }
    constexpr const_reverse_iterator rcbegin() const noexcept
    {
        return std::make_reverse_iterator(cend());
    }
    constexpr reverse_iterator rend() noexcept
    {
        return std::make_reverse_iterator(begin());
    }
    constexpr const_reverse_iterator rend() const noexcept
    {
        return std::make_reverse_iterator(cbegin());
    }
    constexpr const_reverse_iterator rcend() const noexcept
    {
        return std::make_reverse_iterator(cbegin());
    }
    constexpr bool empty() const noexcept
    {
        return m_packed.empty();
    }
    constexpr size_type size() const noexcept
    {
        return m_packed.size();
    }
    constexpr size_type max_size() const noexcept
    {
        return m_packed.max_size();
    }
    constexpr void clear() noexcept
    {
        PopAll();
        m_head = PolicyToHead();
        m_packed.clear();
    }
    constexpr SPair<iterator, bool> insert(const key_type& key) noexcept
    {
        auto&     element = AssureAtLeast(key);
        size_type position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], key));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(key);
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }

        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    constexpr SPair<iterator, bool> insert(key_type&& key) noexcept
    {
        auto&     element = AssureAtLeast(key);
        size_type position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], std::move(key)));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(std::move(key)));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(std::move(key)));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(std::move(key));
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }
        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    constexpr iterator insert(const_iterator pos, const key_type& key) noexcept
    {
        // todo : find a proper way to implement this instead of just ignoring things that aren't cend()
        if (pos != cend())
        {
            return pos;
        }
        auto&     element = AssureAtLeast(key);
        size_type position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], key));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(key);
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }

        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    constexpr iterator insert(const_iterator pos, const value_type&& key) noexcept
    {
        // todo : find a proper way to implement this instead of just ignoring things that aren't cend()
        if (pos != cend())
        {
            return pos;
        }
        auto&     element = AssureAtLeast(key);
        size_type position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], std::move(key)));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(std::move(key)));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(std::move(key)));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(std::move(key));
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }

        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    template <class InputIt>
    constexpr void insert(InputIt first, InputIt last) noexcept
    {
        for (auto& pos{first}; pos != last; ++pos)
        {
            const key_type key{*pos};
            auto&          element = AssureAtLeast(key);
            size_type      position{size()};
            switch (m_mode)
            {
            case deletion_policy::in_place:
                [[fallthrough]];
            case deletion_policy::swap_and_pop:
                m_packed.push_back(key);
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(std::move(key)));
                break;
            case deletion_policy::swap_only:
                if (element == null)
                {
                    m_packed.push_back(key);
                    element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(std::move(key)));
                }
                else
                {
                    assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                    Bump(std::move(key));
                }

                position = m_head++;
                SwapAt(KeyToPosition(element), position);
                break;
            }
        }
    }
    constexpr void insert(std::initializer_list<value_type> ilist) noexcept
    {
        insert(ilist.begin(), ilist.end());
    }
    template <class K>
    std::pair<iterator, bool> insert(K&& x) noexcept
    {
        const key_type key{static_cast<key_type>(std::forward<K>(x))};
        auto&          element = AssureAtLeast(key);
        size_type      position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], key));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(key);
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }

        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    template <class K>
    iterator insert(const_iterator pos, K&& x) noexcept
    {
        // todo : find a proper way to implement this instead of just ignoring things that aren't cend()
        if (pos != cend())
        {
            return pos;
        }
        const key_type key{static_cast<key_type>(std::forward<K>(x))};
        auto&          element = AssureAtLeast(key);
        size_type      position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], key));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(key);
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }

        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    template <class... Args>
    std::pair<iterator, bool> emplace(Args&&... args) noexcept
    {
        key_type  key{std::forward<Args>(args)...};
        auto&     element = AssureAtLeast(key);
        size_type position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], key));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(key);
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }

        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    template <class... Args>
    iterator emplace_hint(const_iterator hint, Args&&... args) noexcept
    {
        // todo : find a proper way to implement this instead of just ignoring things that aren't cend()
        if (hint != cend())
        {
            return hint;
        }
        key_type  key{std::forward<Args>(args)...};
        auto&     element = AssureAtLeast(key);
        size_type position{size()};
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != max_size())
            {
                position = m_head;
                assert(element == null && "Slot not available");
                element = type_traits::combine(static_cast<type_key>(m_head), type_traits::to_integral(key));
                m_head  = KeyToPosition(std::exchange(m_packed[position], key));
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_and_pop:
            m_packed.push_back(key);
            assert(element == null && "Slot not available");
            element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            break;
        case deletion_policy::swap_only:
            if (element == null)
            {
                m_packed.push_back(key);
                element = type_traits::combine(static_cast<type_key>(m_packed.size() - 1u), type_traits::to_integral(key));
            }
            else
            {
                assert(!(KeyToPosition(element) < m_head) && "Slot not available");
                Bump(key);
            }

            position = m_head++;
            SwapAt(KeyToPosition(element), position);
            break;
        }

        return {begin() + static_cast<typename iterator::difference_type>(position), true};
    }
    constexpr iterator erase(iterator pos) noexcept
        requires(!std::same_as<iterator, const_iterator>)
    {
        const auto diff{begin() - pos};
        switch (m_mode)
        {
        case deletion_policy::swap_and_pop:
            SwapAndPop(pos);
            break;
        case deletion_policy::in_place:
            InPlacePop(pos);
            break;
        case deletion_policy::swap_only:
            SwapOnly(pos);
            break;
        }
        return begin() + diff;
    }
    constexpr iterator erase(const_iterator pos) noexcept
    {
        const auto diff{begin() - pos};
        switch (m_mode)
        {
        case deletion_policy::swap_and_pop:
            SwapAndPop(pos);
            break;
        case deletion_policy::in_place:
            InPlacePop(pos);
            break;
        case deletion_policy::swap_only:
            SwapOnly(pos);
            break;
        }
        return begin() + diff;
    }
    constexpr iterator erase(const_iterator first, const_iterator last) noexcept
    {
        const auto diff{begin() - first};
        for (; first != last; ++first)
        {
            switch (m_mode)
            {
            case deletion_policy::swap_and_pop:
                SwapAndPop(first);
                break;
            case deletion_policy::in_place:
                InPlacePop(first);
                break;
            case deletion_policy::swap_only:
                SwapOnly(first);
                break;
            }
        }
        return begin() + diff;
    }
    constexpr size_type erase(const key_type&& key) noexcept
    {
        const auto startSize{size()};
        const auto pos{ToIterator(std::forward(key))};
        switch (m_mode)
        {
        case deletion_policy::swap_and_pop:
            SwapAndPop(pos);
            break;
        case deletion_policy::in_place:
            InPlacePop(pos);
            break;
        case deletion_policy::swap_only:
            SwapOnly(pos);
            break;
        }
        return startSize - size();
    }
    template <typename K>
    constexpr size_type erase(K&& x) noexcept
    {
        const auto startSize{size()};
        const auto pos{ToIterator(std::forward<K>(x))};
        switch (m_mode)
        {
        case deletion_policy::swap_and_pop:
            SwapAndPop(pos);
            break;
        case deletion_policy::in_place:
            InPlacePop(pos);
            break;
        case deletion_policy::swap_only:
            SwapOnly(pos);
            break;
        }
        return startSize - size();
    }
    [[nodiscard]] constexpr size_type count(const key_type& key) const noexcept
    {
        return contains(key) ? 1 : 0;
    }
    template <typename K>
    [[nodiscard]] constexpr size_type count(K&& x) const noexcept
    {
        return contains(std::forward<K>(x)) ? 1 : 0;
    }
    [[nodiscard]] iterator find(const key_type key) noexcept
    {
        return contains(key) ? ToIterator(key) : end();
    }
    [[nodiscard]] const_iterator find(const key_type key) const noexcept
    {
        return contains(key) ? ToIterator(key) : cend();
    }
    template <typename K>
    [[nodiscard]] iterator find(K&& x) noexcept
    {
        const key_type key{std::forward<K>(x)};
        return contains(key) ? ToIterator(key) : end();
    }
    template <typename K>
    [[nodiscard]] const_iterator find(K&& x) const noexcept
    {
        const key_type key{std::forward<K>(x)};
        return contains(key) ? ToIterator(key) : cend();
    }
    [[nodiscard]] constexpr bool contains(const key_type key) const noexcept
    {
        const auto*    elem{SparsePtr(key)};
        constexpr auto cap{type_traits::key_mask};
        constexpr auto mask{type_traits::to_integral(null) & ~cap};
        // testing versions permits to avoid accessing the m_packed array
        return elem && (((mask & type_traits::to_integral(key)) ^ type_traits::to_integral(*elem)) < cap);
    }
    constexpr void swap(CSparseSet& other) noexcept
    {
        using std::swap;
        swap(m_sparse, other.m_sparse);
        swap(m_packed, other.m_packed);
        swap(m_mode, other.m_mode);
        swap(m_head, other.m_head);
    }

  private:
    static constexpr auto MAX_SIZE{static_cast<std::size_t>(type_traits::to_key(null))};

    sparse_container_type m_sparse{};
    packed_container_type m_packed{};
    deletion_policy       m_mode{};
    size_type             m_head{};

    [[nodiscard]] constexpr std::size_t PolicyToHead() const noexcept
    {
        return static_cast<size_type>(MAX_SIZE
                                      * static_cast<std::remove_const_t<decltype(MAX_SIZE)>>(m_mode != deletion_policy::swap_only));
    }
    [[nodiscard]] constexpr auto KeyToPosition(const key_type key) const noexcept
    {
        return static_cast<size_type>(type_traits::to_key(key));
    }
    [[nodiscard]] constexpr auto PosToPage(const std::size_t position) const noexcept
    {
        return static_cast<size_type>(position / type_traits::page_size);
    }
    [[nodiscard]] constexpr auto SparsePtr(const key_type key) const noexcept
    {
        const auto pos{KeyToPosition(key)};
        const auto page{PosToPage(pos)};
        return (page < m_sparse.size() && m_sparse[page]) ? (m_sparse[page] + (pos & (type_traits::page_size - 1))) : nullptr;
    }
    [[nodiscard]] constexpr auto& SparseRef(const key_type key) const noexcept
    {
        assert(SparsePtr(key) && "Invalid element");
        const auto position{KeyToPosition(key)};
        return m_sparse[PosToPage(position)][position & (type_traits::page_size - 1)];
    }
    [[nodiscard]] size_type Index(const key_type key) const noexcept
    {
        assert(contains(key) && "Set does not contain entity");
        return KeyToPosition(SparseRef(key));
    }
    // todo : compare performance with EnTT verions
    [[nodiscard]] constexpr auto ToIterator(const key_type key) const
    {
        return (begin() + static_cast<difference_type>(Index(key)));
    }
    constexpr auto& AssureAtLeast(const key_type key) noexcept
    {
        const auto position{KeyToPosition(key)};
        const auto page{PosToPage(position)};

        if (!(page < m_sparse.size()))
        {
            m_sparse.resize(page + 1u, nullptr);
        }

        if (!m_sparse[page])
        {
            static constexpr auto init{null};

            auto pageAllocator{m_packed.get_allocator()};
            m_sparse[page] = alloc_traits::allocate(pageAllocator, type_traits::page_size);
            std::uninitialized_fill(m_sparse[page], m_sparse[page] + type_traits::page_size, init);
        }

        return m_sparse[page][position & (type_traits::page_size - 1)];
    }
    constexpr void SwapAt(const size_type lhs, const size_type rhs) noexcept
    {
        auto& from{m_packed[lhs]};
        auto& to{m_packed[rhs]};

        SparseRef(from) = type_traits::combine(static_cast<type_key>(rhs), type_traits::to_integral(from));
        SparseRef(to)   = type_traits::combine(static_cast<type_key>(lhs), type_traits::to_integral(to));

        std::swap(from, to);
    }

    constexpr type_version Bump(const key_type key) noexcept
    {
        auto& element{SparseRef(key)};
        assert(key != null && element != tombstone && "Cannot set the required version");
        element                          = type_traits::combine(type_traits::to_integral(element), type_traits::to_integral(key));
        m_packed[KeyToPosition(element)] = key;
        return type_traits::to_version(key);
    }
    constexpr void SwapAndPop(const_iterator pos) noexcept
    {
        assert(m_mode == deletion_policy::swap_and_pop && "Deletion policy mismatch");
        auto&      self{SparseRef(*pos)};
        const auto key{type_traits::to_key(self)};
        SparseRef(m_packed.back())            = type_traits::combine(key, type_traits::to_integral(m_packed.back()));
        m_packed[static_cast<size_type>(key)] = m_packed.back();

        // lazy self-assignment guard
        self = null;
        m_packed.pop_back();
    }
    constexpr void InPlacePop(const_iterator pos) noexcept
    {
        assert(m_mode == deletion_policy::in_place && "Deletion policy mismatch");
        const auto position{KeyToPosition(std::exchange(SparseRef(*pos), null))};
        m_packed[position] = type_traits::combine(static_cast<type_key>(std::exchange(m_head, position)), tombstone);
    }
    constexpr void SwapOnly(const_iterator pos) noexcept
    {
        assert(m_mode == deletion_policy::swap_only && "Deletion policy mismatch");
        const auto position{Index(*pos)};
        Bump(type_traits::next(*pos));
        SwapAt(position, m_head -= (position < m_head));
    }
    constexpr void PopAll() noexcept
    {
        switch (m_mode)
        {
        case deletion_policy::in_place:
            if (m_head != MAX_SIZE)
            {
                for (auto&& elem : m_packed)
                {
                    if (elem != tombstone)
                    {
                        SparseRef(elem) = null;
                    }
                }
                break;
            }
            [[fallthrough]];
        case deletion_policy::swap_only:
        case deletion_policy::swap_and_pop:
            for (auto&& elem : m_packed)
            {
                SparseRef(elem) = null;
            }
            break;
        }

        m_head = PolicyToHead();
        m_packed.clear();
    }
};
} // namespace DeerContainer
