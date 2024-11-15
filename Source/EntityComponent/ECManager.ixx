//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//
module;
#include <algorithm>
#include <memory>
#include <string_view>
#include <utility>

export module DeerEntity.ECManager ;
import DeerContainer.Pair;
import DeerContainer.Set;
import DeerContainer.Map;

namespace DeerEntity
{
using id_type = uint32_t;

template<typename T>
constexpr auto TypeName() noexcept -> std::string_view
{
    #if defined __GNUC__
    #define PRETTY_FUNCTION __PRETTY_FUNCTION__
    constexpr char PRETTY_FUNCTION_PREFIX{'='};
    constexpr char PRETTY_FUNCTION_SUFFIX{']'};
    #define PRETTY_FUNCTION_SUFFIX ']'
    #elif defined __clang__
    #define PRETTY_FUNCTION __FUNCSIG__
    constexpr char PRETTY_FUNCTION_PREFIX{'='};
    constexpr char PRETTY_FUNCTION_SUFFIX{']'};
    #elif defined _MSC_VER
    #define PRETTY_FUNCTION __FUNCSIG__
    constexpr char PRETTY_FUNCTION_PREFIX{'<'};
    constexpr char PRETTY_FUNCTION_SUFFIX{'>'};
    #endif
    constexpr std::string_view functionName{PRETTY_FUNCTION};
    constexpr uint64_t first{functionName.find_first_not_of( ' ', functionName.find_first_of( PRETTY_FUNCTION_PREFIX ) + 1 )};
    return functionName.substr( first, functionName.find_last_of( PRETTY_FUNCTION_SUFFIX ) - first );
}

export template<typename T>
constexpr auto TypeHash() noexcept -> std::size_t
{
    constexpr std::string_view name{TypeName<T>()};
    constexpr std::size_t prime{1099511628211ULL};
    std::size_t value{};
    for ( auto&& curr : name )
    {
        value = ( value ^ static_cast<std::size_t>(curr) ) * prime;
    }
    return value;
}

// Entity Component Manager
export template<typename Entity, class Hash = std::identity, class Compare = std::equal_to<>, class Allocator = std::allocator<Key>>
class CECManager final
{
    using base_type = DeerContainer::CDenseSet<Entity>;
    using alloc_traits = std::allocator_traits<Allocator>;
    static_assert( std::is_same_v<typename alloc_traits::value_type, Entity>, "Invalid value type" );
    // std::shared_ptr because of its type erased allocator which is useful here
    using pool_container_type = DeerContainer::CDenseMap<id_type, std::shared_ptr<base_type>, std::identity, std::equal_to<>, typename alloc_traits::template rebind_alloc<DeerContainer::SPair<const id_type, std::shared_ptr<base_type>>>>;

public:
    using allocator_type = Allocator;
    using entity_type = Entity;
    using size_type = std::size_t;
    using common_type = base_type;

    // Create a new entity
    entity_type CreateEntity()
    {
        entity_type new_entity = static_cast<entity_type>(m_nextEntityId++);
        m_entities.insert( new_entity );
        return new_entity;
    }

    // Destroy an existing entity
    void DestroyEntity( entity_type entity )
    {
        if ( m_entities.contains( entity ) )
        {
            // Remove entity from all component pools
            for ( auto& [ id, pool ] : m_pools )
            {
                pool->erase( entity );
            }
            m_entities.erase( entity );
        }
    }

    // Attach a component to an entity
    template<typename Component, typename... Args>
    void AttachComponent( entity_type entity, Args&&... component )
    {
        constexpr id_type type_id{TypeHash<Component>()};
        if ( m_pools.find( type_id ) == m_pools.cend() )
        {
            // Create a new pool for this component type
            m_pools[ type_id ] = std::make_shared<DeerContainer::CSparseSet<Entity, Allocator>>();
        }
        m_pools[ type_id ]->insert( entity, std::forward<Args>( component )... );
    }

    // Remove a component from an entity
    template<typename Component>
    void RemoveComponent( entity_type entity )
    {
        constexpr id_type type_id{TypeHash<Component>()};
        if ( auto it = m_pools.find( type_id ); it != m_pools.cend() )
        {
            it->second->erase( entity );
        }
    }

    // Check if an entity has a specific component
    template<typename Component>
    bool HasComponent( entity_type entity ) const
    {
        constexpr id_type type_id{TypeHash<Component>()};
        if ( auto it = m_pools.find( type_id ); it != m_pools.cend() )
        {
            return it->second->contains( entity );
        }
        return false;
    }

    // Retrieve a component (non-const)
    template<typename Component>
    Component& GetComponent( entity_type entity )
    {
        constexpr id_type type_id{TypeHash<Component>()};
        return m_pools.at( type_id ).get().get( entity );
    }

private:
    pool_container_type m_pools;
    DeerContainer::CSparseSet<Entity, Allocator> m_entities;
    size_type m_nextEntityId{0};
};
}// namespace DeerEntity
