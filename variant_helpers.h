#pragma once

#include "meta_utils.h"
#include "variant_defs.h"

namespace detail {

/*******************************************************************************
 *                               Variant storage                               *
 *******************************************************************************/

template <bool trivial_destructor, typename... Types>
union _variant_storage;

template <typename... Types>
using variant_storage = _variant_storage<trivial_dtor<Types...>, Types...>;

template <typename First, typename... Rest>
union _variant_storage<false, First, Rest...> {
  First first;
  variant_storage<Rest...> rest;

  constexpr _variant_storage() : rest() {}

  constexpr ~_variant_storage() {
    rest.~variant_storage<Rest...>();
  }
};

template <typename First, typename... Rest>
union _variant_storage<true, First, Rest...> {
  First first;
  variant_storage<Rest...> rest;

  constexpr _variant_storage() : rest() {}
};

template <bool trivial_destructor>
union _variant_storage<trivial_destructor> {
  // all special members are implicitly defined
};

/*******************************************************************************
 *                     Helper for conversion operations                        *
 *******************************************************************************/

template <typename... Types>
struct _conversion_resolver : _conversion_resolver<Types>... {
  using _conversion_resolver<Types>::f...;
};

template <typename To>
struct _conversion_resolver<To> {
  // used for list-initialization (see below)
  struct _helper_aggregate {
    To dummy;
  };

  template <typename From>
  requires requires(From&& t) {
    // The validity of this expression must be equivalent to that of `To x[] = {std::forward<From>(t)}`
    // 1) list-initialization avoids narrowing conversions
    // 2) an aggregate type wrapper allows us to use braces (otherwise `To{...}` could mean a call to
    // constructor taking initializer_list)
    _helper_aggregate{std::forward<From>(t)};
  }
  constexpr static To f(To);
};

template <typename From, typename... To>
using resolve_conversion_t =
    decltype(_conversion_resolver<To...>::template f<From>(std::forward<From>(std::declval<From>())));

/*******************************************************************************
 *                      Recursion helper for all methods                       *
 *                (used to descend through the variant_storage)                *
 *******************************************************************************/

template <size_t Index, typename T_i, typename... Rest>
struct recursion_helper {
  template <typename T>
  constexpr static void convert_construct(T&& src, variant_storage<T_i, Rest...>& dst, size_t& current_alternative,
                                          const size_t alternative_count) {
    if constexpr (std::is_same_v<resolve_conversion_t<T, T_i, Rest...>, T_i>) {
      std::construct_at(std::addressof(dst.first), std::forward<T>(src));
      current_alternative = alternative_count - Index - 1;
    } else if constexpr (Index > 0) {
      recursion_helper<Index - 1, Rest...>::convert_construct(std::forward<T>(src), dst.rest, current_alternative,
                                                              alternative_count);
    } else {
      assert(false);
    }
  }

  constexpr static void copy_construct(size_t index, variant_storage<T_i, Rest...> const& src,
                                       variant_storage<T_i, Rest...>& dst) requires(copy_ctor<T_i, Rest...>) {
    if (index == 0) {
      std::construct_at(std::addressof(dst.first), src.first);
    } else {
      if constexpr (Index > 0) {
        recursion_helper<Index - 1, Rest...>::copy_construct(index - 1, src.rest, dst.rest);
      }
    }
  }

  constexpr static void move_construct(size_t index, variant_storage<T_i, Rest...>&& src,
                                       variant_storage<T_i, Rest...>& dst) requires(move_ctor<T_i, Rest...>) {
    if (index == 0) {
      std::construct_at(std::addressof(dst.first), std::move(src.first));
    } else {
      if constexpr (Index > 0) {
        recursion_helper<Index - 1, Rest...>::move_construct(index - 1, std::move(src.rest), dst.rest);
      }
    }
  }

  template <typename T>
  constexpr static void convert_assign(T&& src, variant_storage<T_i, Rest...>& dst, size_t& current_alternative,
                                       const size_t alternative_count) {
    if constexpr (std::is_same_v<resolve_conversion_t<T, T_i, Rest...>, T_i>) {
      dst.first = std::forward<T>(src);
      current_alternative = alternative_count - Index - 1;
    } else if constexpr (Index > 0) {
      recursion_helper<Index - 1, Rest...>::convert_assign(std::forward<T>(src), dst.rest, current_alternative,
                                                           alternative_count);
    } else {
      assert(false);
    }
  }

  constexpr static void copy_assign(size_t index, variant_storage<T_i, Rest...> const& src,
                                    variant_storage<T_i, Rest...>& dst) requires(copy_assign<T_i, Rest...>) {
    if (index == 0) {
      dst.first = src.first;
    } else {
      if constexpr (Index > 0) {
        recursion_helper<Index - 1, Rest...>::copy_assign(index - 1, src.rest, dst.rest);
      }
    }
  }

  constexpr static void move_assign(size_t index, variant_storage<T_i, Rest...>&& src,
                                    variant_storage<T_i, Rest...>& dst) requires(move_assign<T_i, Rest...>) {
    if (index == 0) {
      dst.first = std::move(src.first);
    } else {
      if constexpr (Index > 0) {
        recursion_helper<Index - 1, Rest...>::move_construct(index - 1, std::move(src.rest), dst.rest);
      }
    }
  }

  template <typename T, typename... Args>
  requires meta::once<T, T_i, Rest...> && std::is_constructible_v<T, Args...> // should I remove this?
  constexpr static void inplace_type_construct(variant_storage<T_i, Rest...>& dst, Args&&... args) {
    if constexpr (std::is_same_v<T_i, T>) {
      std::construct_at(std::addressof(dst.first), std::forward<Args>(args)...);
    } else {
      if constexpr (Index > 0) {
        recursion_helper<Index - 1, Rest...>::template inplace_type_construct<T>(dst.rest,
                                                                                 std::forward<Args>(args)...);
      }
    }
  }

  template <size_t I, typename... Args>
  constexpr static void inplace_index_construct(variant_storage<T_i, Rest...>& dst, Args&&... args) {
    if constexpr (I == 0) {
      std::construct_at(std::addressof(dst.first), std::forward<Args>(args)...);
    } else {
      recursion_helper<Index - 1, Rest...>::template inplace_index_construct<I - 1>(dst.rest,
                                                                                    std::forward<Args>(args)...);
    }
  }

  constexpr static void destroy(size_t index, variant_storage<T_i, Rest...>& dst) requires(dtor<T_i, Rest...>) {
    if (index == 0) {
      dst.first.~T_i();
    } else {
      if constexpr (Index > 0) {
        recursion_helper<Index - 1, Rest...>::destroy(index - 1, dst.rest);
      }
    }
  }

  template <typename T>
  constexpr static bool holds(size_t index, variant_storage<T_i, Rest...> const& dst) noexcept {
    if (std::is_same_v<T, T_i> && index == 0) {
      return true;
    }
    if constexpr (Index > 0) {
      return recursion_helper<Index - 1, Rest...>::template holds<T>(index - 1, dst.rest);
    } else {
      return false;
    }
  }

  template <size_t I>
  constexpr static auto get_if(variant_storage<T_i, Rest...>& dst) {
    if constexpr (I == 0) {
      return std::addressof(dst.first);
    } else {
      return recursion_helper<Index - 1, Rest...>::template get_if<I - 1>(dst.rest);
    }
  }

  template <size_t I>
  constexpr static auto get_if(variant_storage<T_i, Rest...> const& dst) {
    if constexpr (I == 0) {
      return std::addressof(dst.first);
    } else {
      return recursion_helper<Index - 1, Rest...>::template get_if<I - 1>(dst.rest);
    }
  }

  constexpr static void swap_same_alternative(size_t index, variant_storage<T_i, Rest...>& src,
                                              variant_storage<T_i, Rest...>& dst) {
    if (index == 0) {
      using std::swap;
      swap(dst.first, src.first);
    } else {
      if constexpr (Index > 0) {
        recursion_helper<Index - 1, Rest...>::swap_same_alternative(index - 1, src.rest, dst.rest);
      }
    }
  }
};

template <bool trivial_dtor, typename... Alternatives>
struct variant_destructor_base {
  size_t current_alternative{variant_npos};
  detail::variant_storage<Alternatives...> alternatives;

  using recur = detail::recursion_helper<sizeof...(Alternatives) - 1, Alternatives...>;

  constexpr ~variant_destructor_base() noexcept((std::is_nothrow_destructible_v<Alternatives> && ...)) {
    if (current_alternative != variant_npos) {
      recur::destroy(this->current_alternative, this->alternatives);
    }
  }
};

template <typename... Alternatives>
struct variant_destructor_base<true, Alternatives...> {
  size_t current_alternative{variant_npos};
  detail::variant_storage<Alternatives...> alternatives;

  using recur = detail::recursion_helper<sizeof...(Alternatives) - 1, Alternatives...>;

  constexpr ~variant_destructor_base() = default;
};

} // namespace detail