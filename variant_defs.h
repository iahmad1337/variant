/*******************************************************************************
 *               Definitions not dependent on variant's insides                *
 *******************************************************************************/

#pragma once

#include <array>
#include <functional>

#include "meta_utils.h"

template <typename... Alternatives>
class variant;

struct in_place_t {
  explicit in_place_t() = default;
};
inline constexpr in_place_t in_place{};

template <class T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};
template <class T>
inline constexpr in_place_type_t<T> in_place_type{};

template <size_t I>
struct in_place_index_t {

  explicit in_place_index_t() = default;
};
template <size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

template <size_t I, typename T>
struct variant_alternative; /* undefined */

template <size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
  using type = meta::ith_t<I, Types...>;
};

template <size_t I, typename T>
struct variant_alternative<I, const T> {
  using type = std::add_const_t<typename variant_alternative<I, T>::type>;
};

template <size_t I, typename T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

template <class T>
struct variant_size; /* undefined */

template <class... Types>
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <class T>
inline constexpr size_t variant_size_v = variant_size<std::remove_cvref_t<T>>::value;

inline constexpr size_t variant_npos = -1;

class bad_variant_access : public std::exception {
public:
  bad_variant_access() noexcept = default;
  bad_variant_access(const bad_variant_access& other) noexcept = default;

  const char* what() const noexcept override {
    return "bad variant access";
  }
};

namespace detail {

template <typename... Ts>
inline constexpr bool default_ctor = (std::is_default_constructible_v<Ts> && ...);
template <typename... Ts>
inline constexpr bool copy_ctor = (std::is_copy_constructible_v<Ts> && ...);
template <typename... Ts>
inline constexpr bool trivial_copy_ctor = (std::is_trivially_copy_constructible_v<Ts> && ...) && copy_ctor<Ts...>;
template <typename... Ts>
inline constexpr bool copy_assign = (std::is_copy_assignable_v<Ts> && ...);
template <typename... Ts>
inline constexpr bool trivial_copy_assign = (std::is_trivially_copy_assignable_v<Ts> && ...) && copy_assign<Ts...>;
template <typename... Ts>
inline constexpr bool move_ctor = (std::is_move_constructible_v<Ts> && ...);
template <typename... Ts>
inline constexpr bool trivial_move_ctor = (std::is_trivially_move_constructible_v<Ts> && ...) && move_ctor<Ts...>;
template <typename... Ts>
inline constexpr bool move_assign = (std::is_move_assignable_v<Ts> && ...);
template <typename... Ts>
inline constexpr bool trivial_move_assign = (std::is_trivially_move_assignable_v<Ts> && ...) && move_assign<Ts...>;
template <typename... Ts>
inline constexpr bool dtor = (std::destructible<Ts> && ...);
template <typename... Ts>
inline constexpr bool trivial_dtor = (std::is_trivially_destructible_v<Ts> && ...) && dtor<Ts...>;

/*******************************************************************************
 *                             `visit` helpers                                 *
 *******************************************************************************/

template <size_t... Is>
struct static_invoker {
  template <typename Visitor, typename... Variants>
  constexpr static decltype(auto) visit(Visitor&& visitor, Variants&&... variants) {
    static_assert(sizeof...(Is) == sizeof...(Variants));
    // ADL should take care of `get`
    return std::invoke(std::forward<Visitor>(visitor), get<Is>(std::forward<Variants>(variants))...);
  }
};

template <typename Visitor, typename... Variants, size_t... Indices, size_t... FirstSeq>
constexpr inline auto make_jump_table(std::index_sequence<Indices...>, std::index_sequence<FirstSeq...>) {
  return std::array{&static_invoker<Indices..., FirstSeq>::template visit<Visitor, Variants...>...};
}

template <typename Visitor, typename... Variants, size_t... Indices, size_t... FirstSeq, typename... RestSeqs>
constexpr inline auto make_jump_table(std::index_sequence<Indices...>, std::index_sequence<FirstSeq...>,
                                      RestSeqs... rest_seqs) {
  return std::array{
      make_jump_table<Visitor, Variants...>(std::index_sequence<Indices..., FirstSeq>{}, rest_seqs...)...};
}

template <typename Visitor, typename... Variants>
constexpr inline auto jump_table = make_jump_table<Visitor, Variants...>(
    std::index_sequence<>{}, std::make_index_sequence<variant_size_v<Variants>>{}...);

template <typename Array>
constexpr inline auto multi_at(const Array& array, size_t first) {
  return array[first];
}

template <typename Array, typename... Rest>
constexpr inline auto multi_at(const Array& array, size_t first, Rest... rest) {
  return multi_at(array[first], rest...);
}
} // namespace detail
