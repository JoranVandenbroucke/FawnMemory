//
// Copyright (c) 2024.
// Author: Joran.
//

module;
#include <algorithm>
#include <cstddef>
#include <cmath>
#include <memory>
#include <ranges>
#include <utility>

#include "ContainerCompatibleRange.hpp"
export module DeerContainer:DenseSet;
import :CompressedPair;
import :Pair;
import :Vector;

namespace DeerContainer
{
/**
 * @cond TURN_OFF_DOXYGEN
 */
template <typename Itr>
class CDenseSetIterator final
{
    template <typename>
    friend class CDenseSetIterator;

  public:
    using value_type        = typename Itr::value_type::second_type;
    using pointer           = const value_type*;
    using reference         = const value_type&;
    using difference_type   = typename Itr::difference_type;
    using iterator_category = std::random_access_iterator_tag;

    constexpr CDenseSetIterator() noexcept : m_itr{} {}

    explicit constexpr CDenseSetIterator(const Itr iter) noexcept : m_itr{iter} {}

    template <typename Other>
        requires(!std::is_same_v<Itr, Other>) && std::is_constructible_v<Itr, Other>
    constexpr explicit CDenseSetIterator(const CDenseSetIterator<Other>& other) noexcept : m_itr{other.iter}
    {
    }

    constexpr auto operator++() noexcept -> CDenseSetIterator&
    {
        ++m_itr;
        return *this;
    }

    constexpr auto operator++(int) const noexcept -> CDenseSetIterator
    {
        CDenseSetIterator orig{*this};
        ++*this;
        return orig;
    }

    constexpr auto operator--() noexcept -> CDenseSetIterator&
    {
        --m_itr;
        return *this;
    }

    constexpr auto operator--(int) const noexcept -> CDenseSetIterator
    {
        CDenseSetIterator orig{*this};
        --*this;
        return orig;
    }

    constexpr auto operator+(difference_type value) const noexcept -> CDenseSetIterator
    {
        CDenseSetIterator temp{*this};
        temp += value;
        return temp;
    }

    constexpr auto operator+=(difference_type value) noexcept -> CDenseSetIterator&
    {
        m_itr += value;
        return *this;
    }

    constexpr auto operator-(difference_type value) const noexcept -> CDenseSetIterator
    {
        return *this + (-value);
    }

    constexpr auto operator-=(difference_type value) noexcept -> CDenseSetIterator&
    {
        *this += (-value);
        return *this;
    }

    friend constexpr auto operator-(const CDenseSetIterator& lhs, const CDenseSetIterator& rhs) noexcept -> difference_type
    {
        return lhs.m_itr - rhs.m_itr;
    }

    friend constexpr auto operator+(difference_type lhs, CDenseSetIterator rhs) noexcept -> CDenseSetIterator
    {
        rhs += lhs;
        return rhs;
    }

    [[nodiscard]] constexpr auto operator->() const noexcept -> pointer
    {
        return std::addressof(operator[](0));
    }

    [[nodiscard]] constexpr auto operator*() const noexcept -> reference
    {
        return operator[](0);
    }

    [[nodiscard]] constexpr auto operator[](const difference_type value) const noexcept -> reference
    {
        return m_itr[value].second;
    }

    friend auto operator==(const CDenseSetIterator& lhs, const CDenseSetIterator& rhs) -> bool
    {
        return lhs.m_itr == rhs.m_itr;
    }

    friend auto operator!=(const CDenseSetIterator& lhs, const CDenseSetIterator& rhs) -> bool
    {
        return !(lhs == rhs);
    }

    friend auto operator<(const CDenseSetIterator& lhs, const CDenseSetIterator& rhs) -> bool
    {
        return lhs.m_itr < rhs.m_itr;
    }

    friend auto operator<=(const CDenseSetIterator& lhs, const CDenseSetIterator& rhs) -> bool
    {
        return !(rhs < lhs);
    }

    friend auto operator>(const CDenseSetIterator& lhs, const CDenseSetIterator& rhs) -> bool
    {
        return rhs < lhs;
    }

    friend auto operator>=(const CDenseSetIterator& lhs, const CDenseSetIterator& rhs) -> bool
    {
        return !(lhs < rhs);
    }

    friend auto operator-(CDenseSetIterator lhs, const CDenseSetIterator& rhs) -> CDenseSetIterator
    {
        return (lhs -= rhs);
    }

    friend auto operator+(CDenseSetIterator lhs, const CDenseSetIterator& rhs) -> CDenseSetIterator
    {
        return (lhs += rhs);
    }

  private:
    Itr m_itr;
};

template <typename Itr>
class CDenseSetLocalIterator final
{
    template <typename>
    friend class CDenseSetLocalIterator;

  public:
    using value_type        = typename Itr::value_type::second_type;
    using pointer           = const value_type*;
    using reference         = const value_type&;
    using difference_type   = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;

    constexpr CDenseSetLocalIterator() noexcept : m_itr{}, m_offset{} {}

    constexpr CDenseSetLocalIterator(Itr iter, const std::size_t pos) noexcept : m_itr{iter}, m_offset{pos} {}

    template <typename Other>
        requires(!std::is_same_v<Itr, Other>) && std::is_constructible_v<Itr, Other>
    explicit constexpr CDenseSetLocalIterator(const CDenseSetLocalIterator<Other>& other) noexcept :
        m_itr{other.iter}, m_offset{other.offset}
    {
    }

    constexpr auto operator++() noexcept -> CDenseSetLocalIterator&
    {
        ++m_itr;
        return *this;
    }

    constexpr auto operator++(int) const noexcept -> CDenseSetLocalIterator
    {
        CDenseSetLocalIterator orig{*this};
        ++*this;
        return orig;
    }

    constexpr auto operator--() noexcept -> CDenseSetLocalIterator&
    {
        --m_itr;
        return *this;
    }

    constexpr auto operator--(int) const noexcept -> CDenseSetLocalIterator
    {
        CDenseSetLocalIterator orig{*this};
        --*this;
        return orig;
    }

    constexpr auto operator+(difference_type value) const noexcept -> CDenseSetLocalIterator
    {
        CDenseSetLocalIterator temp{*this};
        temp += value;
        return temp;
    }

    constexpr auto operator+=(difference_type value) noexcept -> CDenseSetLocalIterator&
    {
        m_itr += value;
        return *this;
    }

    constexpr auto operator-(const difference_type value) const noexcept -> CDenseSetLocalIterator
    {
        return *this + (-value);
    }

    constexpr auto operator-=(const difference_type value) noexcept -> CDenseSetLocalIterator&
    {
        *this += (-value);
        return *this;
    }

    friend constexpr auto operator-(const CDenseSetLocalIterator& lhs, const CDenseSetLocalIterator& rhs) noexcept -> difference_type
    {
        return lhs.m_itr - rhs.m_itr;
    }

    friend constexpr auto operator+(difference_type lhs, CDenseSetLocalIterator rhs) noexcept -> CDenseSetLocalIterator
    {
        rhs += lhs;
        return rhs;
    }

    [[nodiscard]] constexpr auto index() const noexcept -> std::size_t
    {
        return m_offset;
    }

    [[nodiscard]] constexpr pointer operator->() const noexcept
    {
        return std::addressof(m_itr[m_offset].second);
    }

    [[nodiscard]] constexpr reference operator*() const noexcept
    {
        return *operator->();
    }

    friend auto operator==(const CDenseSetLocalIterator& lhs, const CDenseSetLocalIterator& rhs) -> bool
    {
        return std::tie(lhs.m_itr, lhs.m_offset) == std::tie(rhs.m_itr, rhs.m_offset);
    }

    friend auto operator!=(const CDenseSetLocalIterator& lhs, const CDenseSetLocalIterator& rhs) -> bool
    {
        return !(lhs == rhs);
    }

    friend auto operator<(const CDenseSetLocalIterator& lhs, const CDenseSetLocalIterator& rhs) -> bool
    {
        return std::tie(lhs.m_itr, lhs.m_offset) < std::tie(rhs.m_itr, rhs.m_offset);
    }

    friend auto operator<=(const CDenseSetLocalIterator& lhs, const CDenseSetLocalIterator& rhs) -> bool
    {
        return !(rhs < lhs);
    }

    friend auto operator>(const CDenseSetLocalIterator& lhs, const CDenseSetLocalIterator& rhs) -> bool
    {
        return rhs < lhs;
    }

    friend auto operator>=(const CDenseSetLocalIterator& lhs, const CDenseSetLocalIterator& rhs) -> bool
    {
        return !(lhs < rhs);
    }

  private:
    Itr         m_itr;
    std::size_t m_offset;
};
/**
 * @endcond
 */

export template <class Key, class Hash = std::identity, class Compare = std::equal_to<>, class Allocator = std::allocator<Key>>
class CDenseSet
{
    static constexpr float       g_DEFAULT_THRESHOLD{0.875F};
    static constexpr std::size_t g_MINIMUM_CAPACITY{8U};

    using alloc_traits = std::allocator_traits<Allocator>;
    static_assert(std::is_same_v<typename alloc_traits::value_type, Key>, "Invalid value type");

  public:
    using key_type        = Key;
    using value_type      = key_type;
    using hash_type       = Hash;
    using size_type       = typename alloc_traits::size_type;
    using difference_type = typename alloc_traits::difference_type;
    using key_compare     = Compare;
    using value_compare   = Compare;
    using allocator_type  = Allocator;
    using reference       = value_type&;
    using const_reference = const value_type&;
    using pointer         = typename alloc_traits::pointer;
    using const_pointer   = typename alloc_traits::const_pointer;

  private:
    using node_type             = SPair<size_type, Key>;
    using sparse_container_type = CVector<size_type, typename alloc_traits::template rebind_alloc<size_type>>;
    using packed_container_type = CVector<node_type, typename alloc_traits::template rebind_alloc<node_type>>;

  public:
    using iterator               = CDenseSetIterator<typename packed_container_type::iterator>;
    using const_iterator         = CDenseSetIterator<typename packed_container_type::const_iterator>;
    using reverse_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<iterator>;

    using local_iterator               = CDenseSetLocalIterator<typename packed_container_type::iterator>;
    using const_local_iterator         = CDenseSetLocalIterator<typename packed_container_type::const_iterator>;
    using reverse_local_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_local_iterator = std::reverse_iterator<const_iterator>;

    struct SInsertReturnType
    {
        iterator  position; // Points to the inserted element or the existing one
        bool      inserted; // True if insertion succeeded
        node_type node;     // Holds the node if insertion failed, empty if iter succeeded
    };

    constexpr CDenseSet() : CDenseSet{key_compare{}} {}

    constexpr explicit CDenseSet(const allocator_type& alloc) : CDenseSet{key_compare{}, alloc} {}

    constexpr explicit CDenseSet(const key_compare& comp, const allocator_type& alloc = allocator_type{}) :
        CDenseSet{g_MINIMUM_CAPACITY, comp, alloc}
    {
    }

    constexpr CDenseSet(const size_type cnt, const allocator_type& alloc) : CDenseSet{cnt, hash_type{}, alloc} {}

    constexpr CDenseSet(const size_type cnt, const key_compare& comp, const allocator_type& alloc) :
        CDenseSet{cnt, hash_type{}, comp, alloc}
    {
    }

    constexpr CDenseSet(const size_type cnt, const hash_type& hash, const allocator_type& alloc) :
        CDenseSet{cnt, hash, key_compare{}, alloc}
    {
    }

    constexpr CDenseSet(const size_type cnt, const hash_type& hash, const key_compare& comp, const allocator_type& alloc) :
        m_sparse{alloc, hash}, m_packed{alloc, comp}
    {
        Rehash(cnt);
    }

    template <class InputIt>
    constexpr CDenseSet(InputIt first, InputIt last, const allocator_type& alloc) : CDenseSet{first, last, key_compare{}, alloc}
    {
    }

    template <class InputIt>
    constexpr CDenseSet(InputIt               first,
                        InputIt               last,
                        const hash_type&      hash  = hash_type{},
                        const key_compare&    comp  = key_compare{},
                        const allocator_type& alloc = allocator_type{}) :
        m_sparse{alloc, hash}, m_packed{alloc, comp}
    {
        insert(first, last);
    }

    constexpr CDenseSet(const CDenseSet& other, const allocator_type& alloc) :
        m_sparse{std::piecewise_construct,
                 std::forward_as_tuple(other.m_sparse.first, alloc),
                 std::forward_as_tuple(other.m_sparse.second)},
        m_packed{std::piecewise_construct,
                 std::forward_as_tuple(other.m_packed.first, alloc),
                 std::forward_as_tuple(other.m_packed.second)},
        m_threshold{other.m_threshold}
    {
    }

    constexpr CDenseSet(CDenseSet&& other, const allocator_type& alloc) :
        m_sparse{std::piecewise_construct,
                 std::forward_as_tuple(std::move(other.m_sparse.first), alloc),
                 std::forward_as_tuple(std::move(other.m_sparse.second))},
        m_packed{std::piecewise_construct,
                 std::forward_as_tuple(std::move(other.m_packed.first), alloc),
                 std::forward_as_tuple(std::move(other.m_packed.second))},
        m_threshold{other.m_threshold}
    {
    }

    constexpr CDenseSet(std::initializer_list<value_type> init, const allocator_type& alloc) :
        CDenseSet{init, hash_type{}, key_compare{}, alloc}
    {
    }

    constexpr CDenseSet(std::initializer_list<value_type> init, const hash_type& hasher) :
        CDenseSet{init, hasher, key_compare{}, allocator_type{}}
    {
    }

    constexpr CDenseSet(std::initializer_list<value_type> init, const key_compare& comp = key_compare{}) :
        CDenseSet{init, hash_type{}, comp, allocator_type{}}
    {
    }

    constexpr CDenseSet(std::initializer_list<value_type> init,
                        const hash_type&                  hasher,
                        const key_compare&                comp,
                        const allocator_type&             alloc) :
        m_sparse{alloc, hasher}, m_packed{alloc, comp}
    {
        Rehash(init.size());
        insert(init);
    }

    template <ContainerCompatibleRange<value_type> R>
    constexpr CDenseSet(std::from_range_t /*from_range*/, R&& range, const allocator_type& alloc) :
        CDenseSet(std::from_range, std::forward<R>(range), key_compare{}, alloc)
    {
    }

    template <ContainerCompatibleRange<value_type> R>
    constexpr CDenseSet(std::from_range_t /*from_range*/,
                        R&&                   range,
                        const key_compare&    comp  = key_compare{},
                        const allocator_type& alloc = allocator_type{}) :
        m_sparse{alloc, hash_type{}}, m_packed{alloc, comp}
    {
        insert(std::begin(std::forward<R>(range)), std::end(std::forward<R>(range)));
    }

    CDenseSet(const CDenseSet&)                        = default;
    CDenseSet(CDenseSet&&)                             = default;
    auto operator=(const CDenseSet&) -> CDenseSet&     = default;
    auto operator=(CDenseSet&&) noexcept -> CDenseSet& = default;
    ~CDenseSet()                                       = default;

    constexpr auto begin() -> iterator
    {
        return iterator(m_packed.first.begin());
    }

    constexpr auto begin() const -> const_iterator
    {
        return const_iterator(m_packed.first.cbegin());
    }

    constexpr auto cbegin() const noexcept -> const_iterator
    {
        return const_iterator(m_packed.first.cbegin());
    }

    constexpr auto end() -> iterator
    {
        return iterator(m_packed.first.end());
    }

    constexpr auto end() const -> const_iterator
    {
        return const_iterator(m_packed.first.cend());
    }

    constexpr auto cend() const noexcept -> const_iterator
    {
        return const_iterator(m_packed.first.cend());
    }

    constexpr auto rbegin() -> reverse_iterator
    {
        return reverse_iterator(m_packed.first.end());
    }

    constexpr auto rbegin() const -> const_reverse_iterator
    {
        return const_reverse_iterator(m_packed.first.cend());
    }

    constexpr auto crbegin() const noexcept -> const_reverse_iterator
    {
        return const_reverse_iterator(m_packed.first.cend());
    }

    constexpr auto rend() -> reverse_iterator
    {
        return reverse_iterator(m_packed.first.begin());
    }

    constexpr auto rend() const -> const_reverse_iterator
    {
        return const_reverse_iterator(m_packed.first.cend());
    }

    constexpr auto crend() const noexcept -> const_reverse_iterator
    {
        return const_reverse_iterator(m_packed.first.cend());
    }

    constexpr auto empty() noexcept -> bool
    {
        return m_packed.first.empty();
    }

    constexpr auto size() noexcept -> size_type
    {
        return m_packed.first.size();
    }

    constexpr auto max_size() noexcept -> size_type
    {
        return m_packed.first.max_size();
    }

    constexpr void clear() noexcept
    {
        m_sparse.first.clear();
        m_packed.first.clear();
        Rehash(0U);
    }

    constexpr auto insert(const value_type& value) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing(value.first, value.second);
    }

    template <typename P>
    constexpr auto insert(P&& value) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing(std::forward<P>(value).first, std::forward<P>(value).second);
    }

    constexpr auto insert(value_type&& value) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing(std::move(value));
    }

    constexpr auto insert(const_iterator pos, const value_type& value) -> iterator
    {
        if (pos != end() && m_packed.second(*pos, value))
        {
            return pos;
        }
        return InsertOrDoNothing(value).first;
    }

    template <typename P>
    constexpr auto insert(const_iterator pos, P&& value) -> iterator
    {
        if (pos != end() && m_packed.second(pos->first, value.first))
        {
            return pos;
        }
        return InsertOrDoNothing(std::forward<P>(value).first, std::forward<P>(value).second).first;
    }

    constexpr auto insert(const_iterator pos, value_type&& value) -> iterator
    {
        if (pos != end() && m_packed.second(pos->first, value.first))
        {
            return pos;
        }
        return InsertOrDoNothing(std::move(value).first, std::move(value).second).first;
    }

    template <typename InputIt>
    constexpr void insert(InputIt first, InputIt last)
    {
        for (auto itr{first}; itr != last; ++itr)
        {
            InsertOrDoNothing(*itr);
        }
    }

    constexpr void insert(std::initializer_list<value_type> ilist)
    {
        std::ranges::for_each(ilist, [this](const value_type& value) { InsertOrDoNothing(value.first, value.second); });
    }

    auto insert(node_type&& nodeHandle) -> SInsertReturnType
    {
        const auto index{ValueToBucket(nodeHandle.first)};
        if (auto itr{FindInBucket(nodeHandle.first, index)}; itr != end())
        {
            return {itr, false, std::move(nodeHandle)};
        }

        m_packed.first.emplace_back(m_sparse.first[index], std::move(nodeHandle));
        m_sparse.first[index] = m_packed.first.size() - 1;
        RehashIfRequired();

        return {--end(), true, {}};
    }

    constexpr auto insert(const_iterator pos, node_type&& nodeHandle) -> iterator
    {
        const auto& key{nodeHandle.first};
        auto        bucketIndex{ValueToBucket(key)};

        if (pos != end() && ValueToBucket(pos->first) == bucketIndex && pos->first == key)
        {
            return pos;
        }

        if (auto itr{FindInBucket(key, bucketIndex)}; itr != end())
        {
            return itr;
        }

        m_packed.first.emplace_back(m_sparse.first[bucketIndex], std::move(nodeHandle));
        m_sparse.first[bucketIndex] = m_packed.first.size() - 1;

        RehashIfRequired();

        return --end();
    }

    template <ContainerCompatibleRange<value_type> R>
    constexpr void insert_range(R&& range)
    {
        std::ranges::for_each(std::forward<R>(range), [this](const value_type& value) { InsertOrDoNothing(value.first, value.second); });
    }

    template <typename... Args>
    constexpr auto emplace(Args&&... args) -> SPair<iterator, bool>
    {
        if constexpr (sizeof...(Args) == 1U)
        {
            return InsertOrDoNothing(std::forward<Args>(args)...);
        }
        else
        {
            auto&           node{m_packed.first.emplace_back(m_packed.first.size(), std::forward<Args>(args)...)};
            const size_type index{ValueToBucket(node.second)};

            if (auto iterator{FindInBucket(node.second, index)}; iterator != end())
            {
                m_packed.first.pop_back();
                return SPair{iterator, false};
            }

            std::swap(node.first, m_sparse.first[index]);
            RehashIfRequired();

            return SPair{--end(), true};
        }
    }

    template <typename... Args>
    constexpr auto emplace_hint(const_iterator hint, Args&&... args) -> iterator
    {
        if constexpr (sizeof...(Args) == 1U)
        {
            const auto      tuple{std::forward_as_tuple(args...)};
            const size_type index{std::get<0>(tuple).second};
            if (hint != end() && m_packed.second(hint, index))
            {
                return hint;
            }
            const key_type value{std::get<0>(tuple).first};
            return InsertOrDoNothing(index, value).first;
        }
        else
        {
            auto&           node{m_packed.first.emplace_back(m_packed.first.size(), std::forward<Args>(args)...)};
            const size_type index{ValueToBucket(node.second)};

            if (auto iter{FindInBucket(node.second, index)}; iter != end())
            {
                m_packed.first.pop_back();
                return iter;
            }

            std::swap(node.first, m_sparse.first[index]);
            RehashIfRequired();

            return --end();
        }
    }

    constexpr auto erase(const_iterator pos) -> iterator
    {
        const auto diff{pos - cbegin()};
        erase(*pos);
        return begin() + diff;
    }

    constexpr auto erase(const_iterator first, const_iterator last) -> iterator
    {
        const auto diff{first - cbegin()};
        for (auto from{last - cbegin()}; from != diff; --from)
        {
            erase(m_packed.first[from - 1U].second);
        }
        return begin() + diff;
    }

    constexpr auto erase(const Key& key) -> size_type
    {
        for (size_type* pCurr{&m_sparse.first[ValueToBucket(key)]}; *pCurr != (std::numeric_limits<size_type>::max)();
             pCurr = &m_packed.first[*pCurr].first)
        {
            if (m_packed.second(m_packed.first[*pCurr].second, key))
            {
                const auto index{*pCurr};
                *pCurr = m_packed.first[index].first;
                MoveAndPop(index);
                return 1U;
            }
        }

        return 0U;
    }

    template <typename V>
    constexpr auto erase(V&& value) -> size_type
    {
        for (size_type* pCurr{&m_sparse.first[ValueToBucket(std::forward<V>(value))]}; *pCurr != (std::numeric_limits<size_type>::max)();
             pCurr = &m_packed.first[*pCurr].first)
        {
            if (m_packed.second(m_packed.first[*pCurr].second, std::forward<V>(value)))
            {
                const auto index{*pCurr};
                *pCurr = m_packed.first[*pCurr].first;
                MoveAndPop(index);
                return 1U;
            }
        }

        return 0U;
    }

    void swap(CDenseSet& other) noexcept
    {
        using std::swap;
        swap(m_sparse, other.m_sparse);
        swap(m_packed, other.m_packed);
        swap(m_threshold, other.m_threshold);
    }

    friend void swap(CDenseSet& lhs, CDenseSet& rhs) noexcept
    {
        lhs.swap(rhs);
    }

    constexpr auto extract(const_iterator position) -> node_type
    {
        if (position == cend())
        {
            return {};
        }

        auto& key{position->first};
        auto  bucketIndex{ValueToBucket(key)};

        node_type extractedNode{*position};
        auto      lastPosition{std::prev(m_packed.first.end())};

        if (position != lastPosition)
        {
            *position                                            = std::move(*lastPosition);
            m_sparse.first[ValueToBucket(lastPosition->first)] = std::distance(m_packed.first.begin(), position);
        }

        m_packed.first.pop_back();
        m_sparse.first[bucketIndex] = 0;

        return extractedNode;
    }

    constexpr auto extract(const Key& key) -> node_type
    {
        const size_type bucket_index{ValueToBucket(key)};
        const_iterator  iter{FindInBucket(key, bucket_index)};

        if (iter == cend())
        {
            return {};
        }

        return extract(iter);
    }

    template <typename K>
        requires std::is_invocable_v<key_compare, K, Key>
    constexpr auto extract(K&& key) -> node_type
    {
        const size_type bucketIndex{ValueToBucket(key)};
        const_iterator  iter{FindInBucket(std::forward<K>(key), bucketIndex)};

        if (iter == cend())
        {
            return {};
        }

        return extract(iter);
    }

    constexpr auto count(const Key& key) const -> size_type
    {
        return find(key) != cend();
    }

    template <typename K>
    constexpr auto count(const K& key) const -> size_type
    {
        return find(key) != cend();
    }

    constexpr auto find(const Key& key) -> iterator
    {
        return FindInBucket(key, ValueToBucket(key));
    }

    constexpr auto find(const Key& key) const -> const_iterator
    {
        return FindInBucket(key, ValueToBucket(key));
    }

    template <typename K>
    constexpr auto find(const K& key) -> iterator
    {
        return FindInBucket(key, ValueToBucket(key));
    }

    template <typename K>
    constexpr auto find(const K& key) const -> const_iterator
    {
        return FindInBucket(key, ValueToBucket(key));
    }

    constexpr auto contains(const Key& key) const -> bool
    {
        return find(key) != cend();
    }

    template <typename K>
    constexpr auto contains(const K& key) const -> bool
    {
        return find(key) != cend();
    }

  private:
    SCompressedPair<sparse_container_type, hash_type>   m_sparse;
    SCompressedPair<packed_container_type, key_compare> m_packed;
    float                                               m_threshold{g_DEFAULT_THRESHOLD};

    constexpr auto lbegin(const size_type index) -> local_iterator
    {
        return {m_packed.first.begin(), m_sparse.first[index]};
    }

    constexpr auto lbegin(const size_type index) const -> const_local_iterator
    {
        return {m_packed.first.begin(), m_sparse.first[index]};
    }

    constexpr auto lcbegin(const size_type index) const -> const_local_iterator
    {
        return {m_packed.first.cbegin(), m_sparse.first[index]};
    }

    constexpr auto lend(const size_type index) -> local_iterator
    {
        return {m_packed.first.end(), m_sparse.first[index]};
    }

    constexpr auto lend(const size_type index) const -> const_local_iterator
    {
        return {m_packed.first.end(), m_sparse.first[index]};
    }

    constexpr auto lcend(const size_type index) const -> const_local_iterator
    {
        return {m_packed.first.cend(), m_sparse.first[index]};
    }

    constexpr auto lrbegin(const size_type index) -> reverse_local_iterator
    {
        return {m_packed.first.rbegin(), m_sparse.first[index]};
    }

    constexpr auto lrbegin(const size_type index) const -> const_reverse_local_iterator
    {
        return {m_packed.first.rbegin(), m_sparse.first[index]};
    }

    constexpr auto lcrbegin(const size_type index) const -> const_reverse_local_iterator
    {
        return {m_packed.first.crbegin(), m_sparse.first[index]};
    }

    constexpr auto lrend(const size_type index) -> reverse_local_iterator
    {
        return {m_packed.first.rend(), m_sparse.first[index]};
    }

    constexpr auto lrend(const size_type index) const -> const_reverse_local_iterator
    {
        return {m_packed.first.rend(), m_sparse.first[index]};
    }

    constexpr auto lcrend(const size_type index) const -> const_reverse_local_iterator
    {
        return {m_packed.first.crend(), m_sparse.first[index]};
    }

    constexpr auto BucketCount() const -> size_type
    {
        return m_sparse.first.size();
    }

    constexpr auto MaxBucketCount() const -> size_type
    {
        return m_sparse.first.max_size();
    }

    constexpr auto BucketSize(const size_type index) const -> size_type
    {
        return static_cast<size_type>(std::distance(begin(index), lend(index)));
    }

    constexpr auto Bucket(const key_type& key) const -> size_type
    {
        return ValueToBucket(key);
    }

    [[nodiscard]] constexpr auto LoadFactor() const -> float
    {
        return size() / static_cast<float>(BucketCount());
    }

    [[nodiscard]] constexpr auto MaxLoadFactor() const -> float
    {
        return m_threshold;
    }

    constexpr void MaxLoadFactor(const float value)
    {
        m_threshold = value;
        Rehash(0U);
    }

    constexpr void reserve(const size_type cnt)
    {
        m_packed.first.reserve(cnt);
        Rehash(static_cast<size_type>(std::ceil(cnt / MaxLoadFactor())));
    }

    constexpr auto hash_function() const -> hash_type
    {
        return m_sparse.second;
    }

    constexpr auto NextPowerOfTwo(size_type value) const -> size_type
    {
        value -= value ? 1 : 0;
        for (size_type i{1}; i < std::numeric_limits<size_type>::digits; i <<= 1)
        {
            value |= value >> i;
        }
        return ++value;
    }

    template <typename Other>
    constexpr auto ValueToBucket(const Other& value) const noexcept -> size_type
    {
        return static_cast<size_type>(m_sparse.second(value)) & (BucketCount() - 1);
    }

    constexpr void Rehash(const size_type count)
    {
        const size_type value{
            std::max<size_type>(count, std::max<size_type>(g_MINIMUM_CAPACITY, static_cast<size_type>(size() / MaxLoadFactor())))};
        if (const size_type nextPow{NextPowerOfTwo(value)}; nextPow != BucketCount())
        {
            m_sparse.first.resize(nextPow);

            std::ranges::for_each(m_sparse.first, [](size_type& elem) { elem = (std::numeric_limits<size_type>::max)(); });
            for (size_type pos{}, last{size()}; pos < last; ++pos)
            {
                const size_type index{ValueToBucket(m_packed.first[pos].second)};
                m_packed.first[pos].first = std::exchange(m_sparse.first[index], pos);
            }
        }
    }

    void RehashIfRequired()
    {
        if (size() > BucketCount() * MaxLoadFactor())
        {
            Rehash(BucketCount() * 2U);
        }
    }

    template <typename Other>
    constexpr auto FindInBucket(const Other& value, const std::size_t bucket) -> iterator
    {
        for (local_iterator iter{lbegin(bucket)}, last{lend(bucket)}; iter != last; ++iter)
        {
            if (m_packed.second(*iter, value))
            {
                return begin() + static_cast<difference_type>(iter.index());
            }
        }
        return end();
    }

    template <typename Other>
    constexpr auto FindInBucket(const Other& value, const std::size_t bucket) const -> const_iterator
    {
        for (local_iterator iter{lbegin(bucket)}, last{lend(bucket)}; iter != last; ++iter)
        {
            if (m_packed.second(*iter, value))
            {
                return begin() + static_cast<difference_type>(iter.index());
            }
        }
        return end();
    }

    template <typename Other>
    constexpr auto InsertOrDoNothing(Other&& value) -> SPair<iterator, bool>
    {
        const size_type index{ValueToBucket(value)};

        if (iterator iter{FindInBucket(value, index)}; iter != end())
        {
            return SPair{iter, false};
        }

        m_packed.first.emplace_back(m_sparse.first[index], std::forward<Other>(value));
        m_sparse.first[index] = m_packed.first.size() - 1U;
        RehashIfRequired();

        return SPair{--end(), true};
    }

    constexpr void MoveAndPop(const std::size_t pos)
    {
        if (const auto last{size() - 1U}; pos != last)
        {
            size_type* pCurr{&m_sparse.first[ValueToBucket(m_packed.first.back().second)]};
            m_packed.first[pos] = std::move(m_packed.first.back());
            for (; *pCurr != last; pCurr = &m_packed.first[*pCurr].first)
            {
            }
            *pCurr = pos;
        }

        m_packed.first.pop_back();
    }
};

} // namespace DeerContainer
