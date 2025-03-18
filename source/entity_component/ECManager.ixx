//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//
module;
#include <cstddef>
#include <cstdint>
#include <memory>

export module DeerEntity:ECManager;
import DeerContainer;
using namespace DeerContainer;

namespace DeerEntity
{
using id_type = uint32_t;

// Entity Component Manager
export template <typename Entity, class Allocator = std::allocator<Entity>>
class CECManager final
{
    using base_type    = CSparseSet<Entity, Allocator>;
    using alloc_traits = std::allocator_traits<Allocator>;

    static_assert(std::is_same_v<typename alloc_traits::value_type, Entity>, "Invalid value type");

    using pool_container_type = CDenseMap<id_type,
                                          std::shared_ptr<base_type>,
                                          std::identity,
                                          std::equal_to<>,
                                          typename alloc_traits::template rebind_alloc<SPair<const id_type, std::shared_ptr<base_type>>>>;

  public:
    using entity_type = uint32_t;
    using size_type   = std::size_t;

    // Create a new entity
    constexpr auto CreateEntity() -> entity_type
    {
        entity_type new_entity{static_cast<entity_type>(m_nextEntityId++)};
        m_entities.emplace(new_entity);
        return new_entity;
    }

    // Destroy an existing entity
    constexpr void DestroyEntity(entity_type entity)
    {
        if (m_entities.contains(entity))
        {
            // Remove entity from all component pools
            for (auto& [id, pool] : m_pools)
            {
                pool->erase(entity);
            }
            m_entities.erase(entity);
        }
    }
    [[nodiscard]] constexpr auto GetEntities() const -> const CDenseSet<uint32_t>&
    {
        return m_entities;
    }

    template <typename Component, typename... Args>
    constexpr void AddComponent(entity_type entity, Args&&... args)
    {
        CDenseSet<Component>& pool{GetOrCreatePool<Component>()};
        pool.emplace(entity, std::forward<Args>(args)...);
    }

    template <typename Component>
    constexpr void RemoveComponent(entity_type entity)
    {
        if (CDenseSet<Component> & pool{GetOrCreatePool<Component>()}; pool.contains(entity))
        {
            pool->erase(entity);
        }
    }

    template <typename Component>
    [[nodiscard]] constexpr auto HasComponent(entity_type entity) const -> bool
    {
        const auto* pPool = GetOrCreatePool<Component>();
        return pPool && pPool->contains(entity);
    }

    template <typename Component>
    constexpr auto GetComponent(entity_type entity) -> Component&
    {
        return GetOrCreatePool<std::remove_const_t<Component>>().find(entity);
    }
    template <typename Component>
    constexpr auto GetComponent(entity_type entity) const -> const Component&
    {
        return GetOrCreatePool<std::remove_const_t<Component>>().find(entity);
    }

  private:
    pool_container_type                   m_pools{};
    CDenseSet<entity_type> m_entities{};
    size_type                             m_nextEntityId{0};

    template <typename Component>
    constexpr auto GetOrCreatePool(size_type index = typeid(Component).hash_code()) -> auto&
    {
        if constexpr (std::is_same_v<Component, entity_type>)
        {
            return m_entities;
        }
        else
        {
            using storage_type = CDenseSet<Component>;

            if (auto itr = m_pools.find(index); itr != m_pools.cend())
            {
                return static_cast<storage_type&>(*itr->second);

                typename pool_container_type::mapped_type cpool{};

                cpool = std::allocate_shared<storage_type>(m_entities.get_allocator(), m_entities.get_allocator());

                m_pools.emplace(index, cpool);

                return static_cast<storage_type&>(*cpool);
            }
        }

        template <typename Component>
        [[nodiscard]] constexpr auto GetOrCreatePool(size_type index = typeid(Component).hash_code()) const -> const auto*
        {
            if constexpr (std::is_same_v<Component, entity_type>)
            {
                return &m_entities;
            }
            else
            {
                if (const auto itr = m_pools.find(index); itr != m_pools.cend())
                {
                    return static_cast<const CDenseSet<Component>*>(itr->second.get());
                }

                return nullptr;
            }
        }
    }
};
} // namespace DeerEntity
