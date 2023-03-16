/*******************************************************************************
 *                Helper meta-functions and convenient aliases                 *
 *******************************************************************************/

#pragma once
#include <cstddef>
#include <type_traits>

namespace meta {

template <size_t Index, typename First, typename... Rest>
struct ith {
  using type = typename ith<Index - 1, Rest...>::type;
};

template <typename First, typename... Rest>
struct ith<0, First, Rest...> {
  using type = First;
};

template <size_t Index, typename Pivot, typename First, typename... Rest>
struct idx {
  constexpr static size_t value = idx<Index + 1, Pivot, Rest...>::value;
};

template <size_t Index, typename Pivot, typename... Types>
struct idx<Index, Pivot, Pivot, Types...> {
  constexpr static size_t value = Index;
};

template <typename Pivot, typename First, typename... Rest>
constexpr inline size_t idx_v = idx<0, Pivot, First, Rest...>::value;

template <size_t Index, typename... Types>
using ith_t = typename ith<Index, Types...>::type;

template <typename Pivot, typename... Types>
inline constexpr bool once = (std::is_same_v<Pivot, Types> + ...) == 1;

template <typename... Types>
inline constexpr bool distinct = (once<Types, Types...> && ...);

// get the trait of a particular type in parameter pack at runtime
template <template <typename T> typename Trait, typename First, typename... Rest>
inline bool runtime_trait(size_t index) {
  if (index == 0) {
    return Trait<First>::value;
  } else {
    if constexpr (sizeof...(Rest) > 0) {
      return runtime_trait<Trait, Rest...>(index - 1);
    } else {
      return false;
    }
  }
}
} // namespace meta
