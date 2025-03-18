//
// Copyright (c) 2024.
// Author: Joran Vandenbroucke.
//

module;
#include <memory>
export module DeerAllocator;
export import :Common;
export import :Utilities;
export import :AffixAllocator;
export import :AllocatorWithStats;
// export import :BitmappedBlock;
export import :Buckitizer;
// export import :CascadingAllocator;
export import :FallbackAllocator;
export import :Freelist;
export import :Mallocator;
export import :Segregator;
export import :StackAllocator;

// todo : make lambda static "[]()static{}"; C++26
export template <typename T, auto DeleteFn>
using unique_ptr_deleter = std::unique_ptr<T, decltype([](T* obj) { DeleteFn(obj); })>;
