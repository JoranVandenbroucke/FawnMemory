module;
#include <memory>
#include <type_traits>

export module DeerEntity:Storage;
import DeerContainer;
using namespace DeerContainer;

namespace DeerEntity
{
export typename<template Type, template Entity, typename Allocator> class Storage final
    : CSparseSet<Entity, typename std::allocator_traits<Allocator>::template rebind_alloc<Entity>>
{
    using container_type =
        SVector<typename alloc_traits::pointer, typename alloc_traits::template rebind_alloc<typename alloc_traits::pointer>>;
    using underlying_type     = CSparseSet<Entity, typename std::allocator_traits<Allocator>::template rebind_alloc<Entity>>;
    using underlying_iterator = typename underlying_type::basic_iterator;
    using traits_type         = component_traits<Type>;

  private:
    container_type m_payload;
};
} // namespace DeerEntity
