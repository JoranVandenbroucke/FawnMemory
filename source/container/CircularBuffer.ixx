module;
#include <memory>

export module DeerContainer:CircularBuffer;
import :CompressedPair;

export template <class T, std::size_t N, class Allocator = std::allocator<T>>
class SCircularBuffer final
{
  public:
    using value_type      = T;
    using allocator_type  = Allocator;
    using size_type       = typename std::allocator_traits<allocator_type>::size_type;
    using difference_type = typename std::allocator_traits<allocator_type>::difference_type;
    using pointer         = typename std::allocator_traits<allocator_type>::pointer;
    using const_pointer   = typename std::allocator_traits<allocator_type>::const_pointer;
    using reference       = value_type&;
    using const_reference = const value_type&;
    // todo : write iterators

  private:
    pointer* m_head;
    pointer* m_tail;
};
