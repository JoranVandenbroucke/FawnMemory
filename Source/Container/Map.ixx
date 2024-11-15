//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <memory>
#include <ranges>
#include <stdexcept>
#include <utility>

#include "ContainerCompatibleRange.hpp"
export module DeerContainer.Map;
import DeerContainer.Pair;
import DeerContainer.Vector;
import DeerAllocator.Utilities;

namespace DeerContainer
{
template<typename Type>
struct SInputIteratorPointer final
{
    using value_type = Type;
    using pointer = Type*;
    using reference = Type&;

    explicit constexpr SInputIteratorPointer( value_type&& val ) noexcept(std::is_nothrow_move_constructible_v<value_type>)
        : m_value{std::move( val )}
    {
    }

    [[nodiscard]] constexpr auto operator->() noexcept -> pointer
    {
        return std::addressof( m_value );
    }

    [[nodiscard]] constexpr auto operator*() noexcept -> reference
    {
        return m_value;
    }

private:
    Type m_value;
};

template<typename Key, typename Type>
struct SDenseMapNode final
{
    using value_type = SPair<Key, Type>;

    template<typename... Args>
    SDenseMapNode( const std::size_t pos, Args&&... args )
        : next{pos}
        , element{std::forward<Args>( args )...}
    {
    }

    template<typename Allocator, typename... Args>
    SDenseMapNode( std::allocator_arg_t, const Allocator& allocator, const std::size_t pos, Args&&... args )
        : next{pos}
        , element{DeerAllocator::MakeObjUsingAllocator<value_type>( allocator, std::forward<Args>( args )... )}
    {
    }

    template<typename Allocator>
    SDenseMapNode( std::allocator_arg_t, const Allocator& allocator, const SDenseMapNode& other )
        : next{other.next}
        , element{DeerAllocator::MakeObjUsingAllocator<value_type>( allocator, other.element )}
    {
    }

    template<typename Allocator>
    SDenseMapNode( std::allocator_arg_t, const Allocator& allocator, SDenseMapNode&& other )
        : next{other.next}
        , element{DeerAllocator::MakeObjUsingAllocator<value_type>( allocator, std::move( other.element ) )}
    {
    }

    std::size_t next;
    value_type element;
};

template<typename Itr>
class CDenseMapIterator final
{
    template<typename>
    friend class CDenseMapIterator;

    using first_type = decltype(( std::as_const( std::declval<Itr>()->element.first ) ));
    using second_type = decltype(( std::declval<Itr>()->element.second ));

public:
    using value_type = SPair<first_type, second_type>;
    using pointer = SInputIteratorPointer<value_type>;
    using reference = value_type;
    using difference_type = typename Itr::difference_type;
    using iterator_category = std::input_iterator_tag;
    using iterator_concept = std::random_access_iterator_tag;

    constexpr CDenseMapIterator() noexcept
        : m_itr{}
    {
    }

    explicit constexpr CDenseMapIterator( const Itr iter ) noexcept
        : m_itr{iter}
    {
    }

    template<typename Other>
        requires ( !std::is_same_v<Itr, Other> ) && std::is_constructible_v<Itr, Other>
    constexpr explicit CDenseMapIterator( const CDenseMapIterator<Other>& other ) noexcept
        : m_itr{other.iter}
    {
    }

    constexpr auto operator++() noexcept -> CDenseMapIterator&
    {
        ++m_itr;
        return *this;
    }

    constexpr auto operator++( int ) const noexcept -> CDenseMapIterator
    {
        CDenseMapIterator orig{*this};
        ++*this;
        return orig;
    }

    constexpr auto operator--() noexcept -> CDenseMapIterator&
    {
        --m_itr;
        return *this;
    }

    constexpr auto operator--( int ) const noexcept -> CDenseMapIterator
    {
        CDenseMapIterator orig{*this};
        --*this;
        return orig;
    }

    constexpr auto operator+( difference_type value ) const noexcept -> CDenseMapIterator
    {
        CDenseMapIterator temp{*this};
        temp += value;
        return temp;
    }

    constexpr auto operator+=( difference_type value ) noexcept -> CDenseMapIterator&
    {
        m_itr += value;
        return *this;
    }

    constexpr auto operator-( difference_type value ) const noexcept -> CDenseMapIterator
    {
        return *this + ( -value );
    }

    constexpr auto operator-=( difference_type value ) noexcept -> CDenseMapIterator&
    {
        *this += ( -value );
        return *this;
    }

    friend constexpr auto operator-( const CDenseMapIterator& lhs, const CDenseMapIterator& rhs ) noexcept -> difference_type
    {
        return lhs.m_itr - rhs.m_itr;
    }

    friend constexpr auto operator+( difference_type lhs, CDenseMapIterator rhs ) noexcept -> CDenseMapIterator
    {
        rhs += lhs;
        return rhs;
    }

    [[nodiscard]] constexpr auto operator->() const noexcept -> pointer
    {
        return pointer( operator*() );
    }

    [[nodiscard]] constexpr auto operator*() const noexcept -> reference
    {
        return operator[]( 0 );
    }

    [[nodiscard]] constexpr auto operator[]( const difference_type value ) const noexcept -> reference
    {
        return SPair<first_type, second_type>{m_itr[ value ].element.first, m_itr[ value ].element.second};
    }

    friend auto operator==( const CDenseMapIterator& lhs, const CDenseMapIterator& rhs ) -> bool
    {
        return lhs.m_itr == rhs.m_itr;
    }

    friend auto operator!=( const CDenseMapIterator& lhs, const CDenseMapIterator& rhs ) -> bool
    {
        return !( lhs == rhs );
    }

    friend auto operator<( const CDenseMapIterator& lhs, const CDenseMapIterator& rhs ) -> bool
    {
        return lhs.m_itr < rhs.m_itr;
    }

    friend auto operator<=( const CDenseMapIterator& lhs, const CDenseMapIterator& rhs ) -> bool
    {
        return !( rhs < lhs );
    }

    friend auto operator>( const CDenseMapIterator& lhs, const CDenseMapIterator& rhs ) -> bool
    {
        return rhs < lhs;
    }

    friend auto operator>=( const CDenseMapIterator& lhs, const CDenseMapIterator& rhs ) -> bool
    {
        return !( lhs < rhs );
    }

    friend auto operator-( CDenseMapIterator lhs, const CDenseMapIterator& rhs ) -> CDenseMapIterator
    {
        return ( lhs -= rhs );
    }

    friend auto operator+( CDenseMapIterator lhs, const CDenseMapIterator& rhs ) -> CDenseMapIterator
    {
        return ( lhs += rhs );
    }

private:
    Itr m_itr;
};

template<typename Itr>
class CDenseMapLocalIterator final
{
    template<typename>
    friend class CDenseMapLocalIterator;

    using first_type = decltype(std::as_const( std::declval<Itr>()->element.first ));
    using second_type = decltype(( std::declval<Itr>()->element.second ));

public:
    using value_type = SPair<first_type, second_type>;
    using pointer = SInputIteratorPointer<value_type>;
    using reference = value_type;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::input_iterator_tag;
    using iterator_concept = std::forward_iterator_tag;

    constexpr CDenseMapLocalIterator() noexcept
        : m_itr{}
        , m_offset{}
    {
    }

    constexpr CDenseMapLocalIterator( Itr iter, const std::size_t pos ) noexcept
        : m_itr{iter}
        , m_offset{pos}
    {
    }

    template<typename Other>
        requires ( !std::is_same_v<Itr, Other> ) && std::is_constructible_v<Itr, Other>
    explicit constexpr CDenseMapLocalIterator( const CDenseMapLocalIterator<Other>& other ) noexcept
        : m_itr{other.iter}
        , m_offset{other.offset}
    {
    }

    constexpr auto operator++() noexcept -> CDenseMapLocalIterator&
    {
        ++m_itr;
        return *this;
    }

    constexpr auto operator++( int ) const noexcept -> CDenseMapLocalIterator
    {
        CDenseMapLocalIterator orig{*this};
        ++*this;
        return orig;
    }

    constexpr auto operator--() noexcept -> CDenseMapLocalIterator&
    {
        --m_itr;
        return *this;
    }

    constexpr auto operator--( int ) const noexcept -> CDenseMapLocalIterator
    {
        CDenseMapLocalIterator orig{*this};
        --*this;
        return orig;
    }

    constexpr auto operator+( difference_type value ) const noexcept -> CDenseMapLocalIterator
    {
        CDenseMapLocalIterator temp{*this};
        temp += value;
        return temp;
    }

    constexpr auto operator+=( difference_type value ) noexcept -> CDenseMapLocalIterator&
    {
        m_itr += value;
        return *this;
    }

    constexpr auto operator-( const difference_type value ) const noexcept -> CDenseMapLocalIterator
    {
        return *this + ( -value );
    }

    constexpr auto operator-=( const difference_type value ) noexcept -> CDenseMapLocalIterator&
    {
        *this += ( -value );
        return *this;
    }

    friend constexpr auto operator-( const CDenseMapLocalIterator& lhs, const CDenseMapLocalIterator& rhs ) noexcept -> difference_type
    {
        return lhs.m_itr - rhs.m_itr;
    }

    friend constexpr auto operator+( difference_type lhs, CDenseMapLocalIterator rhs ) noexcept -> CDenseMapLocalIterator
    {
        rhs += lhs;
        return rhs;
    }

    [[nodiscard]] constexpr auto operator->() const noexcept -> pointer
    {
        return SInputIteratorPointer( operator*() );
    }

    [[nodiscard]] constexpr auto operator*() const noexcept -> reference
    {
        return {m_itr[ m_offset ].element.first, m_itr[ m_offset ].element.second};
    }

    [[nodiscard]] constexpr auto index() const noexcept -> std::size_t
    {
        return m_offset;
    }

    friend auto operator==( const CDenseMapLocalIterator& lhs, const CDenseMapLocalIterator& rhs ) -> bool
    {
        return std::tie( lhs.m_itr, lhs.m_offset ) == std::tie( rhs.m_itr, rhs.m_offset );
    }

    friend auto operator!=( const CDenseMapLocalIterator& lhs, const CDenseMapLocalIterator& rhs ) -> bool
    {
        return !( lhs == rhs );
    }

    friend auto operator<( const CDenseMapLocalIterator& lhs, const CDenseMapLocalIterator& rhs ) -> bool
    {
        return std::tie( lhs.m_itr, lhs.m_offset ) < std::tie( rhs.m_itr, rhs.m_offset );
    }

    friend auto operator<=( const CDenseMapLocalIterator& lhs, const CDenseMapLocalIterator& rhs ) -> bool
    {
        return !( rhs < lhs );
    }

    friend auto operator>( const CDenseMapLocalIterator& lhs, const CDenseMapLocalIterator& rhs ) -> bool
    {
        return rhs < lhs;
    }

    friend auto operator>=( const CDenseMapLocalIterator& lhs, const CDenseMapLocalIterator& rhs ) -> bool
    {
        return !( lhs < rhs );
    }

private:
    Itr m_itr;
    std::size_t m_offset;
};

/**
 * @brief A high-performance dense hash map for mapping unique keys to values, based on skypjack/entt's design.
 *
 * @details CDenseMap is designed as a memory-efficient alternative to `std::map`, with dense storage for
 * optimized performance. Using custom container types—`SPair`, `SCompactPair`, and `CVector`—it enhances
 * control over memory layout and performance characteristics. This map fulfills most `std::map` requirements
 * but focuses on dense, cache-friendly data storage inspired by `skypjack/entt`.
 *
 * @tparam Key The type used for keys in the dense map.
 * @tparam Type The type of values associated with each key.
 * @tparam Hash The hashing function used for generating hash values from keys. Defaults to `std::identity`.
 * @tparam Equals The function used to compare keys for equality. Defaults to `std::equal_to<>`.
 * @tparam Allocator The memory allocator used for storing pairs of keys and values, defaulting to `std::allocator<SPair<const Key, Type>>`.
 *
 * @author Joran
 * @date 2024-11-03
 * @since 1.0
 *
 * @note This class relies on `SPair`, `SCompactPair`, and `CVector` for internal storage and handling,
 * offering a more customized approach than standard containers.
 * @warning Not thread-safe. External synchronization is required in concurrent contexts.
 *
 * @see std::map, skypjack/entt, SPair, SCompactPair, CVector
 *
 * @example
 * CDenseMap<int, std::string> map;
 * map.insert(1, "one");
 * auto value = map.find(1);
 */
export template<typename Key, typename Type, typename Hash = std::identity, typename Equals = std::equal_to<>, typename Allocator = std::allocator<SPair<const Key, Type>>>
class CDenseMap
{
    static constexpr float g_DEFAULT_THRESHOLD{0.875F};
    static constexpr std::size_t g_MINIMUM_CAPACITY{8U};

    using alloc_traits = std::allocator_traits<Allocator>;
    static_assert( std::is_same_v<typename alloc_traits::value_type, SPair<const Key, Type>>, "Invalid value type" );

public:
    using allocator_type = Allocator;
    using key_type = Key;
    using mapped_type = Type;
    using value_type = SPair<const Key, Type>;
    using size_type = typename alloc_traits::size_type;
    using hasher_type = Hash;
    using key_compare = Equals;
    using difference_type = typename alloc_traits::difference_type;

private:
    using node_type = SDenseMapNode<Key, Type>;
    using sparse_container_type = CVector<size_type, typename alloc_traits::template rebind_alloc<size_type>>;
    using packed_container_type = CVector<node_type, typename alloc_traits::template rebind_alloc<node_type>>;

public:
    using iterator = CDenseMapIterator<typename packed_container_type::iterator>;
    using const_iterator = CDenseMapIterator<typename packed_container_type::const_iterator>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<iterator>;

    using local_iterator = CDenseMapLocalIterator<typename packed_container_type::iterator>;
    using const_local_iterator = CDenseMapLocalIterator<typename packed_container_type::const_iterator>;
    using reverse_local_iterator = std::reverse_iterator<iterator>;
    using const_reverse_local_iterator = std::reverse_iterator<const_iterator>;

    struct SInsertReturnType
    {
        iterator position;// Points to the inserted element or the existing one
        bool inserted;    // True if insertion succeeded
        node_type node;   // Holds the node if insertion failed, empty if iter succeeded
    };

    /**
     * @brief Default constructor. Initializes the map with default settings.
     */
    constexpr CDenseMap()
        : CDenseMap{key_compare{}}
    {
    }

    /**
     * @brief Constructs the map with a specified allocator.
     * @param alloc Allocator used for memory allocation within the map.
     */
    constexpr explicit CDenseMap( const allocator_type& alloc )
        : CDenseMap{key_compare{}, alloc}
    {
    }

    /**
     * @brief Constructs the map with a specified key comparator and allocator.
     * @param comp Comparison function to compare keys.
     * @param alloc Allocator used for memory allocation within the map. Defaults to `allocator_type()`.
     */
    constexpr explicit CDenseMap( const key_compare& comp, const allocator_type& alloc = allocator_type() )
        : CDenseMap{g_MINIMUM_CAPACITY, comp, alloc}
    {
    }

    /**
     * @brief Constructs the map with an initial bucket count and allocator.
     * @param cnt Initial number of buckets.
     * @param allocator Allocator used for memory allocation within the map.
     */
    constexpr CDenseMap( size_type cnt, const allocator_type& allocator )
        : CDenseMap{cnt, hasher_type{}, allocator}
    {
    }

    /**
     * @brief Constructs the map with an initial bucket count, key comparator, and allocator.
     * @param cnt Initial number of buckets.
     * @param comp Comparison function to compare keys.
     * @param allocator Allocator used for memory allocation within the map.
     */
    constexpr CDenseMap( size_type cnt, const key_compare& comp, const allocator_type& allocator )
        : CDenseMap{cnt, hasher_type{}, comp, allocator}
    {
    }

    /**
     * @brief Constructs the map with an initial bucket count, hash function, and allocator.
     * @param cnt Initial number of buckets.
     * @param hash Hash function for generating hash values from keys.
     * @param allocator Allocator used for memory allocation within the map.
     */
    constexpr CDenseMap( size_type cnt, const hasher_type& hash, const allocator_type& allocator )
        : CDenseMap{cnt, hash, key_compare{}, allocator}
    {
    }

    /**
     * @brief Constructs the map with an initial bucket count, hash function, key comparator, and allocator.
     * @param cnt Initial number of buckets.
     * @param hash Hash function for generating hash values from keys.
     * @param compare Comparison function to compare keys.
     * @param allocator Allocator used for memory allocation within the map.
     */
    constexpr explicit CDenseMap( const size_type cnt, const hasher_type& hash, const key_compare& compare, const allocator_type& allocator )
        : m_sparse{allocator, hash}
        , m_packed{allocator, compare}
    {
        Rehash( cnt );
    }

    /**
     * @brief Constructs the map using a range of key-value pairs.
     * @tparam InputIt Input iterator type.
     * @param first Iterator to the beginning of the range.
     * @param last Iterator to the end of the range.
     * @param comp Comparison function to compare keys. Defaults to `key_compare{}`.
     * @param alloc Allocator used for memory allocation within the map. Defaults to `allocator_type{}`.
     */
    template<typename InputIt>
    constexpr CDenseMap( InputIt first, InputIt last, const key_compare& comp = key_compare{}, const allocator_type& alloc = allocator_type{} )
        : CDenseMap{first, last, hasher_type{}, comp, alloc}
    {
    }

    /**
     * @brief Constructs the map using a range of key-value pairs with specified hash function, key comparator, and allocator.
     * @tparam InputIt Input iterator type.
     * @param first Iterator to the beginning of the range.
     * @param last Iterator to the end of the range.
     * @param hash Hash function for generating hash values from keys.
     * @param comp Comparison function to compare keys.
     * @param alloc Allocator used for memory allocation within the map.
     */
    template<typename InputIt>
    constexpr CDenseMap( InputIt first, InputIt last, const hasher_type& hash, const key_compare& comp, const allocator_type& alloc )
        : m_sparse{alloc, hash}
        , m_packed{alloc, comp}
    {
        insert( first, last );
    }

    /**
     * @brief Copy constructor with allocator. Creates a copy of another map, using a specific allocator.
     * @param other The map to copy from.
     * @param allocator Allocator used for memory allocation within the map.
     */
    constexpr CDenseMap( const CDenseMap& other, const allocator_type& allocator )
        : m_sparse{std::piecewise_construct, std::forward_as_tuple( other.m_sparse.first(), allocator ), std::forward_as_tuple( other.m_sparse.second() )}
        , m_packed{std::piecewise_construct, std::forward_as_tuple( other.m_packed.first(), allocator ), std::forward_as_tuple( other.m_packed.second() )}
        , m_threshold{other.m_threshold}
    {
    }

    /**
     * @brief Move constructor with allocator. Transfers resources from another map, using a specific allocator.
     * @param other The map to move from.
     * @param allocator Allocator used for memory allocation within the map.
     */
    constexpr CDenseMap( CDenseMap&& other, const allocator_type& allocator )
        : m_sparse{std::piecewise_construct, std::forward_as_tuple( std::move( other.m_sparse.first() ) ), allocator, std::forward_as_tuple( std::move( other.m_sparse.second() ) )}
        , m_packed{std::piecewise_construct, std::forward_as_tuple( std::move( other.m_packed.first() ) ), allocator, std::forward_as_tuple( std::move( other.m_packed.second() ) )}
        , m_threshold{other.m_threshold}
    {
    }

    /**
     * @brief Constructs the map from an initializer list of key-value pairs and a specific allocator.
     * @param init Initializer list containing key-value pairs to insert.
     * @param alloc Allocator used for memory allocation within the map.
     */
    constexpr CDenseMap( std::initializer_list<value_type> init, const allocator_type& alloc )
        : CDenseMap{init, hasher_type{}, key_compare{}, alloc}
    {
    }

    /**
     * @brief Constructs the map from an initializer list of key-value pairs with a specific hash function.
     * @param init Initializer list containing key-value pairs to insert.
     * @param hasher Hash function for generating hash values from keys.
     */
    constexpr CDenseMap( std::initializer_list<value_type> init, const hasher_type& hasher )
        : CDenseMap{init, hasher, key_compare{}, allocator_type{}}
    {
    }

    /**
     * @brief Constructs the map from an initializer list of key-value pairs with a specific key comparator.
     * @param init Initializer list containing key-value pairs to insert.
     * @param comp Comparison function to compare keys.
     */
    constexpr CDenseMap( std::initializer_list<value_type> init, const key_compare& comp = key_compare{} )
        : CDenseMap{init, hasher_type{}, comp, allocator_type{}}
    {
    }

    /**
     * @brief Constructs the map from an initializer list of key-value pairs with a specified hash function, key comparator, and allocator.
     * @param init Initializer list containing key-value pairs to insert.
     * @param hasher Hash function for generating hash values from keys.
     * @param comp Comparison function to compare keys.
     * @param alloc Allocator used for memory allocation within the map.
     */
    constexpr CDenseMap( std::initializer_list<value_type> init, const hasher_type& hasher, const key_compare& comp, const allocator_type& alloc )
        : m_sparse{alloc, hasher}
        , m_packed{alloc, comp}
    {
        Rehash( init.size() );
        insert( init );
    }

    /**
     * @brief Constructs the map using a range of key-value pairs with a specified allocator.
     *
     * This constructor initializes the dense map by taking a range defined by two iterators.
     *
     * @tparam InputIt Type of the input iterator.
     * @param first Iterator to the beginning of the range.
     * @param last Iterator to the end of the range.
     * @param alloc Allocator used for memory allocation within the map.
     */
    template<typename InputIt>
    constexpr CDenseMap( InputIt first, InputIt last, const allocator_type& alloc )
        : CDenseMap( first, last, key_compare{}, alloc )
    {
    }

    /**
     * @brief Constructs the map from a compatible range using a specified allocator.
     *
     * This constructor initializes the dense map from a range compatible with the specified value type.
     *
     * @tparam R The type of the range, which must be compatible with the value type of the map.
     * @param from_range Tag to indicate the use of a range.
     * @param range The range of key-value pairs to initialize the map with.
     * @param alloc Allocator used for memory allocation within the map.
     */
    template<ContainerCompatibleRange<value_type> R>
    constexpr CDenseMap( std::from_range_t /*from_range*/, R&& range, const allocator_type& alloc )
        : CDenseMap( std::from_range, std::forward<R>( range ), key_compare{}, alloc )
    {
    }

    /**
     * @brief Constructs the map from a compatible range with specified key comparator and allocator.
     *
     * This constructor initializes the dense map from a range compatible with the specified value type
     * and allows for customization of the key comparison function.
     *
     * @tparam R The type of the range, which must be compatible with the value type of the map.
     * @param from_range Tag to indicate the use of a range.
     * @param range The range of key-value pairs to initialize the map with.
     * @param comp Comparison function to compare keys. Defaults to `key_compare{}`.
     * @param alloc Allocator used for memory allocation within the map. Defaults to `allocator_type{}`.
     */
    template<ContainerCompatibleRange<value_type> R>
    constexpr CDenseMap( std::from_range_t /*from_range*/, R&& range, const key_compare& comp = key_compare{}, const allocator_type& alloc = allocator_type{} )
        : m_sparse{alloc, hasher_type{}}
        , m_packed{alloc, comp}
    {
        insert( std::begin( std::forward<R>( range ) ), std::end( std::forward<R>( range ) ) );
    }

    CDenseMap( CDenseMap& ) = default;
    CDenseMap( CDenseMap&& ) noexcept = default;
    ~CDenseMap() = default;
    auto operator=( const CDenseMap& ) -> CDenseMap& = default;
    auto operator=( CDenseMap&& ) noexcept -> CDenseMap& = default;

    /**
     * @brief Returns the associated allocator.
     * @return the associated allocator.
     */
    constexpr auto get_allocator() const noexcept -> allocator_type
    {
        return m_sparse.first().get_allocator();
    }

    /**
     * @brief Returns the associated key allocator.
     * @return the associated key comparator
     */
    constexpr auto key_comp() const noexcept -> key_type
    {
        return m_packed.second();
    }

    /**
     * \brief Accesses the element with the specified key.
     *
     * \param key The key of the element to find.
     * \return A reference to the mapped value of the specified key.
     * \throws std::out_of_range If the key is not found.
     */
    constexpr auto at( const key_type& key ) -> mapped_type&
    {
        const iterator& iter{find( key )};
        if ( iter == end() )
        {
            throw std::out_of_range( "SDenseMap::at:  key not found" );
        }
        return iter->second;
    }

    /**
     * \brief Accesses the element with the specified key.
     *
     * \param key The key of the element to find.
     * \return A const reference to the mapped value of the specified key.
     * \throws std::out_of_range If the key is not found.
     * \overload auto at( const key_type& key )
     */
    constexpr auto at( const key_type& key ) const -> const mapped_type&
    {
        const iterator& iter{find( key )};
        if ( iter == end() )
        {
            throw std::out_of_range( "SDenseMap::at:  key not found" );
        }
        return iter->second;
    }

    /**
     * \brief Accesses the element with the specified key.
     *
     * \tparam K The type of the key.
     * \param key The key of the element to find.
     * \return A reference to the mapped value of the specified key.
     * \throws std::out_of_range If the key is not found.
     * \overload auto at( const key_type& key )
     */
    template<typename K>
    constexpr auto at( const K& key ) -> mapped_type&
    {
        const iterator& iter{find( key )};
        if ( iter == end() )
        {
            throw std::out_of_range( "SDenseMap::at:  key not found" );
        }
        return iter->second;
    }

    /**
     * \brief Accesses the element with the specified key.
     *
     * \tparam K The type of the key.
     * \param key The key of the element to find.
     * \return A const reference to the mapped value of the specified key.
     * \throws std::out_of_range If the key is not found.
     * \overload auto at( const key_type& key )
     */
    template<typename K>
    constexpr auto at( const K& key ) const -> const mapped_type&
    {
        const iterator& iter{find( key )};
        if ( iter == end() )
        {
            throw std::out_of_range( "SDenseMap::at:  key not found" );
        }
        return iter->second;
    }

    constexpr auto operator[]( const key_type& key ) -> mapped_type&
    {
        return InsertOrDoNothing( key ).first->second;
    }

    constexpr auto operator[]( key_type&& key ) -> mapped_type&
    {
        return InsertOrDoNothing( std::move( key ) ).first->second;
    }

    template<typename K>
        requires ( !std::same_as<Key, K> )
    constexpr auto operator[]( K&& key ) -> mapped_type&
    {
        return InsertOrDoNothing( std::forward<K>( key ) );
    }

    constexpr auto begin() -> iterator
    {
        return iterator( m_packed.first().begin() );
    }

    constexpr auto begin() const -> const_iterator
    {
        return const_iterator( m_packed.first().cbegin() );
    }

    constexpr auto cbegin() const -> const_iterator
    {
        return const_iterator( m_packed.first().cbegin() );
    }

    constexpr auto end() -> iterator
    {
        return iterator( m_packed.first().end() );
    }

    constexpr auto end() const -> const_iterator
    {
        return const_iterator( m_packed.first().cend() );
    }

    constexpr auto cend() const -> const_iterator
    {
        return const_iterator( m_packed.first().cend() );
    }

    constexpr auto rbegin() -> reverse_iterator
    {
        return reverse_iterator( end() );
    }

    constexpr auto rbegin() const -> const_reverse_iterator
    {
        return const_reverse_iterator( cend() );
    }

    constexpr auto crbegin() const -> const_reverse_iterator
    {
        return const_reverse_iterator( cend() );
    }

    constexpr auto rend() -> reverse_iterator
    {
        return reverse_iterator( begin() );
    }

    constexpr auto rend() const -> const_reverse_iterator
    {
        return const_reverse_iterator( cbegin() );
    }

    constexpr auto crend() const -> const_reverse_iterator
    {
        return const_reverse_iterator( cbegin() );
    }

    [[nodiscard]] constexpr auto empty() const -> bool
    {
        return m_packed.first().empty();
    }

    constexpr auto size() const -> size_type
    {
        return m_packed.first().size();
    }

    constexpr auto max_size() const -> size_type
    {
        return m_packed.first().max_size();
    }

    constexpr void clear() noexcept
    {
        m_sparse.first().clear();
        m_packed.first().clear();
        Rehash( 0U );
    }

    constexpr auto insert( const value_type& value ) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing( value.first, value.second );
    }

    template<typename P>
    constexpr auto insert( P&& value ) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing( std::forward<P>( value ).first, std::forward<P>( value ).second );
    }

    constexpr auto insert( value_type&& value ) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing( std::move( value ).first, std::move( value ).second );
    }

    constexpr auto insert( const_iterator pos, const value_type& value ) -> iterator
    {
        if ( pos != end() && m_packed.second()( pos->first, value.first ) )
        {
            return pos;
        }
        return InsertOrDoNothing( value.first, value.second ).first;
    }

    template<typename P>
    constexpr auto insert( const_iterator pos, P&& value ) -> iterator
    {
        if ( pos != end() && m_packed.second()( pos->first, value.first ) )
        {
            return pos;
        }
        return InsertOrDoNothing( std::forward<P>( value ).first, std::forward<P>( value ).second ).first;
    }

    constexpr auto insert( const_iterator pos, value_type&& value ) -> iterator
    {
        if ( pos != end() && m_packed.second()( pos->first, value.first ) )
        {
            return pos;
        }
        return InsertOrDoNothing( std::move( value ).first, std::move( value ).second ).first;
    }

    template<typename InputIt>
    constexpr void insert( InputIt first, InputIt last )
    {
        for ( auto itr{first}; itr != last; ++itr )
        {
            InsertOrDoNothing( *itr );
        }
    }

    constexpr void insert( std::initializer_list<value_type> ilist )
    {
        std::ranges::for_each( ilist,
                               [this]( const value_type& value )
                               {
                                   InsertOrDoNothing( value.first, value.second );
                               } );
    }

    auto insert( node_type&& nodeHandle ) -> SInsertReturnType
    {
        const auto index{KeyToBucket( nodeHandle.first )};
        if ( auto itr{FindInBucket( nodeHandle.first, index )}; itr != end() )
        {
            return {itr, false, std::move( nodeHandle )};
        }

        m_packed.first().emplace_back( m_sparse.first()[ index ], std::move( nodeHandle ) );
        m_sparse.first()[ index ] = m_packed.first().size() - 1;
        RehashIfRequired();

        return {--end(), true, {}};
    }

    constexpr auto insert( const_iterator pos, node_type&& nodeHandle ) -> iterator
    {
        const auto& key{nodeHandle.first};
        auto bucketIndex{KeyToBucket( key )};

        if ( pos != end() && KeyToBucket( pos->first ) == bucketIndex && pos->first == key )
        {
            return pos;
        }

        if ( auto itr{FindInBucket( key, bucketIndex )}; itr != end() )
        {
            return itr;
        }

        m_packed.first().emplace_back( m_sparse.first()[ bucketIndex ], std::move( nodeHandle ) );
        m_sparse.first()[ bucketIndex ] = m_packed.first().size() - 1;

        RehashIfRequired();

        return --end();

    }

    template<ContainerCompatibleRange<value_type> R>
    constexpr void insert_range( R&& range )
    {
        std::ranges::for_each( std::forward<R>( range ),
                               [this]( const value_type& value )
                               {
                                   InsertOrDoNothing( value.first, value.second );
                               } );
    }

    template<typename M>
    constexpr auto insert_or_assign( const Key& key, M&& obj ) -> SPair<iterator, bool>
    {
        return InsertOrAssign( key, std::forward<M>( obj ) );
    }

    template<typename M>
    constexpr auto insert_or_assign( Key&& key, M&& obj ) -> SPair<iterator, bool>
    {
        return InsertOrAssign( std::move( key ), std::forward<M>( obj ) );
    }

    template<typename K, typename M>
    constexpr auto insert_or_assign( K&& key, M&& obj ) -> SPair<iterator, bool>
    {
        return InsertOrAssign( std::forward<K>( key ), std::forward<M>( obj ) );
    }

    template<typename M>
    constexpr auto insert_or_assign( const_iterator hint, const Key& key, M&& obj ) -> iterator
    {
        if ( hint != end() && m_packed.second()( hint->first, key ) )
        {
            return hint;
        }
        return InsertOrDoNothing( key, std::forward<M>( obj ) ).first;
    }

    template<typename M>
    constexpr auto insert_or_assign( const_iterator hint, Key&& key, M&& obj ) -> iterator
    {
        if ( hint != end() && m_packed.second()( hint->first, key ) )
        {
            return hint;
        }
        return InsertOrDoNothing( std::move( key ), std::forward<M>( obj ) ).first;
    }

    template<typename K, typename M>
    constexpr auto insert_or_assign( const_iterator hint, K&& key, M&& obj ) -> iterator
    {
        if ( hint != end() && m_packed.second()( hint->first, key ) )
        {
            return hint;
        }
        return InsertOrDoNothing( std::forward<K>( key ), std::forward<M>( obj ) ).first;
    }

    template<typename... Args>
    constexpr auto emplace( Args&&... args ) -> SPair<iterator, bool>
    {
        if constexpr ( sizeof...( Args ) == 0U )
        {
            return InsertOrDoNothing( key_type{} );
        }
        else if constexpr ( sizeof...( Args ) == 1U )
        {
            return InsertOrDoNothing( std::forward<Args>( args ).first..., std::forward<Args>( args ).second... );
        }
        else if constexpr ( sizeof...( Args ) == 2U )
        {
            return InsertOrDoNothing( std::forward<Args>( args )... );
        }
        else
        {
            auto& node{m_packed.first().emplace_back( m_packed.first().size(), std::forward<Args>( args )... )};
            const size_type index{KeyToBucket( node.element.first )};

            if ( auto iterator{FindInBucket( node.element.first, index )}; iterator != end() )
            {
                m_packed.first().pop_back();
                return SPair{iterator, false};
            }

            std::swap( node.next, m_sparse.first()[ index ] );
            RehashIfRequired();

            return SPair{--end(), true};
        }
    }

    template<typename... Args>
    constexpr auto emplace_hint( const_iterator hint, Args&&... args ) -> iterator
    {
        if constexpr ( sizeof...( Args ) == 0U )
        {
            if ( hint != end() && m_packed.second()( hint->first, key_type{} ) )
            {
                return hint;
            }
            return InsertOrDoNothing( key_type{} ).first;
        }
        else if constexpr ( sizeof...( Args ) == 1U )
        {
            const auto tuple{std::forward_as_tuple( args... )};
            key_type key{std::get<0>( tuple ).first};
            if ( hint != end() && m_packed.second()( hint->first, key ) )
            {
                return hint;
            }
            key_type value{std::get<0>( tuple ).second};
            return InsertOrDoNothing( key, value ).first;
        }
        else if constexpr ( sizeof...( Args ) == 2U )
        {
            return InsertOrDoNothing( std::forward<Args>( args )... ).first;
        }
        else
        {
            auto& node{m_packed.first().emplace_back( m_packed.first().size(), std::forward<Args>( args )... )};
            const size_type index{KeyToBucket( node.element.first )};

            if ( auto iter{FindInBucket( node.element.first, index )}; iter != end() )
            {
                m_packed.first().pop_back();
                return iter;
            }

            std::swap( node.next, m_sparse.first()[ index ] );
            RehashIfRequired();

            return --end();
        }
    }

    template<typename... Args>
    constexpr auto try_emplace( const Key& key, Args&&... args ) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing( key, std::forward<Args>( args )... );
    }

    template<typename... Args>
    constexpr auto try_emplace( Key&& key, Args&&... args ) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing( std::move( key ), std::forward<Args>( args )... );
    }

    template<typename K, typename... Args>
    constexpr auto try_emplace( K&& key, Args&&... args ) -> SPair<iterator, bool>
    {
        return InsertOrDoNothing( std::forward<K>( key ), std::forward<Args>( args )... );
    }

    template<typename... Args>
    constexpr auto try_emplace( const_iterator hint, const Key& key, Args&&... args ) -> iterator
    {
        if ( hint != end() && m_packed.second()( hint->first, key ) )
        {
            return hint;
        }
        return InsertOrDoNothing( key, std::forward<Args>( args )... );
    }

    template<typename... Args>
    constexpr auto try_emplace( const_iterator hint, Key&& key, Args&&... args ) -> iterator
    {
        if ( hint != end() && m_packed.second()( hint->first, key ) )
        {
            return hint;
        }
        return InsertOrDoNothing( std::move( key ), std::forward<Args>( args )... );
    }

    template<typename K, typename... Args>
    constexpr auto try_emplace( const_iterator hint, K&& key, Args&&... args ) -> iterator
    {
        if ( hint != end() && m_packed.second()( hint->first, key ) )
        {
            return hint;
        }
        return InsertOrDoNothing( std::forward<K>( key ), std::forward<Args>( args )... );
    }

    constexpr auto erase( const_iterator pos ) -> iterator
    {
        const auto diff{pos - cbegin()};
        erase( pos->first );
        return begin() + diff;
    }

    constexpr auto erase( const_iterator first, const_iterator last ) -> iterator
    {
        const auto diff{first - cbegin()};
        for ( auto from{last - cbegin()}; from != diff; --from )
        {
            erase( m_packed.first()[ from - 1U ].element.first );
        }
        return begin() + diff;
    }

    constexpr auto erase( const Key& key ) -> size_type
    {
        for ( size_type* pCurr{&m_sparse.first()[ KeyToBucket( key ) ]}; *pCurr != ( std::numeric_limits<size_type>::max )(); pCurr = &m_packed.first()[ *pCurr ].next )
        {
            if ( m_packed.second()( m_packed.first()[ *pCurr ].element.first, key ) )
            {
                const auto index{*pCurr};
                *pCurr = m_packed.first()[ *pCurr ].next;
                MoveAndPop( index );
                return 1U;
            }
        }

        return 0U;
    }

    template<typename K>
    constexpr auto erase( K&& key ) -> size_type
    {
        for ( size_type* pCurr{&m_sparse.first()[ KeyToBucket( std::forward<K>( key ) ) ]}; *pCurr != ( std::numeric_limits<size_type>::max )(); pCurr = &m_packed.first()[ *pCurr ].next )
        {
            if ( m_packed.second()( m_packed.first()[ *pCurr ].element.first, std::forward<K>( key ) ) )
            {
                const auto index{*pCurr};
                *pCurr = m_packed.first()[ *pCurr ].next;
                MoveAndPop( index );
                return 1U;
            }
        }

        return 0U;
    }

    void swap( CDenseMap& other ) noexcept
    {
        using std::swap;
        swap( m_sparse, other.m_sparse );
        swap( m_packed, other.m_packed );
        swap( m_threshold, other.m_threshold );
    }

    friend void Swap( CDenseMap& lhs, CDenseMap& rhs ) noexcept
    {
        lhs.swap( rhs );
    }

    constexpr auto extract( const_iterator position ) -> node_type
    {
        if ( position == cend() )
        {
            return {};
        }

        auto& key{position->first};
        auto bucketIndex{KeyToBucket( key )};

        node_type extractedNode{*position};
        auto lastPosition{std::prev( m_packed.first().end() )};

        if ( position != lastPosition )
        {
            *position = std::move( *lastPosition );
            m_sparse.first()[ KeyToBucket( lastPosition->first ) ] = std::distance( m_packed.first().begin(), position );
        }

        m_packed.first().pop_back();
        m_sparse.first()[ bucketIndex ] = 0;

        return extractedNode;
    }

    constexpr auto extract( const Key& key ) -> node_type
    {
        const size_type bucket_index{KeyToBucket( key )};
        const_iterator iter{FindInBucket( key, bucket_index )};

        if ( iter == cend() )
        {
            return {};
        }

        return extract( iter );
    }

    template<typename K>
        requires std::is_invocable_v<key_compare, K, Key>
    constexpr auto extract( K&& key ) -> node_type
    {
        const size_type bucketIndex{KeyToBucket( key )};
        const_iterator iter{FindInBucket( std::forward<K>( key ), bucketIndex )};

        if ( iter == cend() )
        {
            return {};
        }

        return extract( iter );
    }

    constexpr auto count( const Key& key ) const -> size_type
    {
        return find( key ) != cend();
    }

    template<typename K>
    constexpr auto count( const K& key ) const -> size_type
    {
        return find( key ) != cend();
    }

    constexpr auto find( const Key& key ) -> iterator
    {
        return FindInBucket( key, KeyToBucket( key ) );
    }

    constexpr auto find( const Key& key ) const -> const_iterator
    {
        return FindInBucket( key, KeyToBucket( key ) );
    }

    template<typename K>
    constexpr auto find( const K& key ) -> iterator
    {
        return FindInBucket( key, KeyToBucket( key ) );
    }

    template<typename K>
    constexpr auto find( const K& key ) const -> const_iterator
    {
        return FindInBucket( key, KeyToBucket( key ) );
    }

    constexpr auto contains( const Key& key ) const -> bool
    {
        return find( key ) != cend();
    }

    template<typename K>
    constexpr auto contains( const K& key ) const -> bool
    {
        return find( key ) != cend();
    }


private:
    SCompressedPair<sparse_container_type, hasher_type> m_sparse;
    SCompressedPair<packed_container_type, key_compare> m_packed;
    float m_threshold{g_DEFAULT_THRESHOLD};

    constexpr auto lbegin( const size_type index ) -> local_iterator
    {
        return {m_packed.first().begin(), m_sparse.first()[ index ]};
    }

    constexpr auto lbegin( const size_type index ) const -> const_local_iterator
    {
        return {m_packed.first().begin(), m_sparse.first()[ index ]};
    }

    constexpr auto lcbegin( const size_type index ) const -> const_local_iterator
    {
        return {m_packed.first().cbegin(), m_sparse.first()[ index ]};
    }

    constexpr auto lend( const size_type index ) -> local_iterator
    {
        return {m_packed.first().end(), m_sparse.first()[ index ]};
    }

    constexpr auto lend( const size_type index ) const -> const_local_iterator
    {
        return {m_packed.first().end(), m_sparse.first()[ index ]};
    }

    constexpr auto lcend( const size_type index ) const -> const_local_iterator
    {
        return {m_packed.first().cend(), m_sparse.first()[ index ]};
    }

    constexpr auto lrbegin( const size_type index ) -> reverse_local_iterator
    {
        return {m_packed.first().rbegin(), m_sparse.first()[ index ]};
    }

    constexpr auto lrbegin( const size_type index ) const -> const_reverse_local_iterator
    {
        return {m_packed.first().rbegin(), m_sparse.first()[ index ]};
    }

    constexpr auto lcrbegin( const size_type index ) const -> const_reverse_local_iterator
    {
        return {m_packed.first().crbegin(), m_sparse.first()[ index ]};
    }

    constexpr auto lrend( const size_type index ) -> reverse_local_iterator
    {
        return {m_packed.first().rend(), m_sparse.first()[ index ]};
    }

    constexpr auto lrend( const size_type index ) const -> const_reverse_local_iterator
    {
        return {m_packed.first().rend(), m_sparse.first()[ index ]};
    }

    constexpr auto lcrend( const size_type index ) const -> const_reverse_local_iterator
    {
        return {m_packed.first().crend(), m_sparse.first()[ index ]};
    }

    constexpr auto BucketCount() const -> size_type
    {
        return m_sparse.first().size();
    }

    constexpr auto MaxBucketCount() const -> size_type
    {
        return m_sparse.first().max_size();
    }

    constexpr auto BucketSize( const size_type index ) const -> size_type
    {
        return static_cast<size_type>(std::distance( begin( index ), lend( index ) ));
    }

    constexpr auto Bucket( const key_type& key ) const -> size_type
    {
        return KeyToBucket( key );
    }

    [[nodiscard]] constexpr auto LoadFactor() const -> float
    {
        return size() / static_cast<float>(BucketCount());
    }

    [[nodiscard]] constexpr auto MaxLoadFactor() const -> float
    {
        return m_threshold;
    }

    constexpr void MaxLoadFactor( const float value )
    {
        m_threshold = value;
        Rehash( 0U );
    }

    constexpr void reserve( const size_type cnt )
    {
        m_packed.first().reserve( cnt );
        Rehash( static_cast<size_type>(std::ceil( cnt / MaxLoadFactor() )) );
    }

    constexpr auto hash_function() const -> hasher_type
    {
        return m_sparse.second();
    }
    constexpr auto NextPowerOfTwo( size_type value ) const -> size_type
    {
        value -= value ? 1 : 0;
        for ( size_type i{1}; i < std::numeric_limits<size_type>::digits; i <<= 1 )
        {
            value |= value >> i;
        }
        return ++value;
    }

    template<typename Other>
    constexpr auto KeyToBucket( const Other& key ) const noexcept -> size_type
    {
        // bucket_count needs to bee guaranteed a power of twoo
        if constexpr ( std::is_same_v<Other, value_type> )
        {
            return static_cast<size_type>(m_sparse.second()( key.first )) & ( BucketCount() - 1 );
        }
        else
        {
            return static_cast<size_type>(m_sparse.second()( key )) & ( BucketCount() - 1 );
        }
    }

    constexpr void Rehash( const size_type count )
    {
        const size_type value{std::max<size_type>( count, std::max<size_type>( g_MINIMUM_CAPACITY, static_cast<size_type>(size() / MaxLoadFactor()) ) )};
        if ( const size_type nextPow{NextPowerOfTwo( value )}; nextPow != BucketCount() )
        {
            m_sparse.first().resize( nextPow );

            std::ranges::for_each( m_sparse.first(),
                                   []( size_type& elem )
                                   {
                                       elem = ( std::numeric_limits<size_type>::max )();
                                   } );
            for ( size_type pos{}, last{size()}; pos < last; ++pos )
            {
                const auto index{KeyToBucket( m_packed.first()[ pos ].element.first )};
                m_packed.first()[ pos ].next = std::exchange( m_sparse.first()[ index ], pos );
            }
        }
    }

    void RehashIfRequired()
    {
        if ( size() > BucketCount() * MaxLoadFactor() )
        {
            Rehash( BucketCount() * 2U );
        }
    }

    template<typename Key2>
    constexpr auto FindInBucket( const Key2& key, const std::size_t bucket ) -> iterator
    {
        for ( local_iterator iter{lbegin( bucket )}, last{lend( bucket )}; iter != last; ++iter )
        {
            if constexpr ( std::is_same_v<Key2, value_type> )
            {
                if ( m_packed.second()( iter->first, key.first ) )
                {
                    return begin() + static_cast<difference_type>(iter.index());
                }
            }
            else
            {
                if ( m_packed.second()( iter->first, key ) )
                {
                    return begin() + static_cast<difference_type>(iter.index());
                }
            }
        }
        return end();
    }

    template<typename Key2>
    constexpr auto FindInBucket( const Key2& key, const std::size_t bucket ) const -> const_iterator
    {
        for ( auto iter{lcbegin( bucket )}, last{lcend( bucket )}; iter != last; ++iter )
        {
            if ( m_packed.second()( iter->first, key ) )
            {
                return cbegin() + static_cast<difference_type>(iter.index());
            }
        }
        return cend();
    }

    template<typename Key2, typename... Args>
    constexpr auto InsertOrDoNothing( Key2&& key, Args&&... args ) -> SPair<iterator, bool>
    {
        const size_type index{KeyToBucket( key )};

        if ( iterator iter{FindInBucket( key, index )}; iter != end() )
        {
            return SPair{iter, false};
        }

        m_packed.first().emplace_back( m_sparse.first()[ index ], std::piecewise_construct, std::forward_as_tuple( std::forward<Key2>( key ) ), std::forward_as_tuple( std::forward<Args>( args )... ) );
        m_sparse.first()[ index ] = m_packed.first().size() - 1U;
        RehashIfRequired();

        return SPair{--end(), true};
    }

    template<typename Key2, typename Args>
    constexpr auto InsertOrAssign( Key2&& key, Args&& value ) -> SPair<iterator, bool>
    {
        const auto index{KeyToBucket( key )};

        if ( auto iter{FindInBucket( key, index )}; iter != end() )
        {
            iter->second = std::forward<Args>( value );
            return SPair{iter, false};
        }

        m_packed.first().emplace_back( m_sparse.first()[ index ], std::forward<Key2>( key ), std::forward<Args>( value ) );
        m_sparse.first()[ index ] = m_packed.first().size() - 1U;
        RehashIfRequired();

        return SPair{--end(), true};
    }

    constexpr void MoveAndPop( const std::size_t pos )
    {
        if ( const auto last{size() - 1U}; pos != last )
        {
            size_type* pCurr{&m_sparse.first()[ KeyToBucket( m_packed.first().back().element.first ) ]};
            m_packed.first()[ pos ] = std::move( m_packed.first().back() );
            for ( ; *pCurr != last; pCurr = &m_packed.first()[ *pCurr ].next )
            {
            }
            *pCurr = pos;
        }

        m_packed.first().pop_back();
    }
};

//todo :
// export template<typename Key, typename T, typename Compare = std::less<T>, typename Allocator = std::allocator<SPair<const Key, T>>>
// class Map
// {
//     using alloc_traits = std::allocator_traits<Allocator>;
//     static_assert( std::is_same_v<typename alloc_traits::value_type, SPair<const Key, T>>, "Invalid value type" );
// public:
//     using key_type = Key;
//     using DenseMapped_type = T;
//     using value_type = SPair<const Key, T>;
//     using size_type = typename alloc_traits::size_type;
//     using difference_type = typename alloc_traits::difference_type;
//     using key_compare = Compare;
//     using allocator_type = Allocator;
//     using reference = value_type&;
//     using const_reference = const value_type&;
//     using pointer = typename alloc_traits::pointer;
//     using const_pointer = typename alloc_traits::const_pointer;
//     using iterator = MapIterator<T>;
//     using const_iterator = const MapIterator<T>;
//     using reverse_iterator = std::reverse_iterator<iterator>;
//     using const_reverse_iterator = std::reverse_iterator<const_iterator>;
//     Map()
//         : Map( key_compare() )
//     {
//     }
//     explicit Map( const key_compare& comp, const Allocator& alloc = Allocator() );
//     explicit Map( const Allocator& alloc );
//     template<typename InputIt>
//     Map( InputIt first, InputIt last, const key_compare& comp = key_compare(), const Allocator& alloc = Allocator() );
//     template<typename InputIt>
//     Map( InputIt first, InputIt last, const Allocator& alloc )
//         : Map( first, last, key_compare(), alloc )
//     {
//     }
//     Map( const Map& other );
//     Map( const Map& other, const Allocator& alloc );
//     Map( Map&& other );
//     Map( Map&& other, const Allocator& alloc );
//     Map( std::initializer_list<value_type> init, const key_compare& comp = key_compare(), const Allocator& alloc = Allocator() );
//     Map( std::initializer_list<value_type> init, const Allocator& alloc )
//         : Map( init, key_compare(), alloc )
//     {
//     }
//     template<ContainerCompatibleRange<value_type> R>
//     Map( std::from_range_t, R&& rg, const key_compare& comp = key_compare(), const Allocator& alloc = Allocator() );
//     template<ContainerCompatibleRange<value_type> R>
//     Map( std::from_range_t, R&& rg, const Allocator& alloc )
//         : Map( std::from_range, std::forward<R>( rg ), key_compare(), alloc )
//     {
//     }
// };

}// namespace DeerContainer

export template<typename Key, typename Value, typename Allocator>
struct std::uses_allocator<DeerContainer::CDenseMap<Key, Value>, Allocator>
        : std::true_type
{
};
