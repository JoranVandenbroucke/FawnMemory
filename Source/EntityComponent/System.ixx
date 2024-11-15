//
// Copyright (c) 2024.
// Author: Joran.
//

module;
#include <algorithm>
#include <cassert>
#include <memory>

export module DeerEntity.System;
import DeerContainer.Map;
import DeerContainer.Vector;
import DeerContainer.Pair;
import DeerEntity.ECManager;

namespace DeerEntity
{
export struct SystemContext
{
    CECManager<uint32_t, std::allocator<uint32_t>>& manager;
    double currentTime;
    double deltaTime;
    // Add more fields as needed

    SystemContext( CECManager<uint32_t, std::allocator<uint32_t>>& mgr, const double currTime, const double dt )
        : manager( mgr )
        , currentTime( currTime )
        , deltaTime( dt )
    {
    }
};

export struct ISystem
{
    virtual ~ISystem() = default;

    virtual void Initialize( SystemContext& systemContext )
    {
    }

    virtual void Update( SystemContext& systemContext )
    {
    }

    virtual void Cleanup( SystemContext& systemContext )
    {
    }
};

export class SystemManager
{
public:
    template<typename T>
    void RegisterSystem()
    {
        constexpr auto typeName = TypeHash<T>();

        assert( m_systems.find( typeName ) == m_systems.end() && "Registering system more than once." );

        auto system = std::make_shared<T>();
        m_systems.insert( {typeName, system} );
        return system;
    }

    void InitializeSystems(SystemContext& systemContext)
    {
        std::ranges::for_each(m_systems, [&systemContext ]( const DeerContainer::SPair<const unsigned long long&, ISystem*&>& pSystem )
        {
            pSystem.second->Initialize( systemContext );
        });
    }

    void UpdateSystems(SystemContext& systemContext)
    {
        std::ranges::for_each(m_systems, [&systemContext ]( const DeerContainer::SPair<const unsigned long long&, ISystem*&>& pSystem )
        {
            pSystem.second->Update( systemContext );
        });
    }

    void CleanupSystems(SystemContext& systemContext)
    {
        std::ranges::for_each(m_systems, [&systemContext ]( const DeerContainer::SPair<const unsigned long long&, ISystem*&>& pSystem )
        {
            pSystem.second->Cleanup( systemContext );
        });
    }
private:
    DeerContainer::CDenseMap<std::size_t, ISystem*> m_systems{};
};
}// namespace DeerEntity
