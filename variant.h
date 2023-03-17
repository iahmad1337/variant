#pragma once

#include "meta_utils.h"
#include "variant_defs.h"
#include "variant_helpers.h"
#include <compare>
#include <concepts>

template <typename... Alternatives>
class variant : private detail::variant_destructor_base<detail::trivial_dtor<Alternatives...>, Alternatives...> {
  static_assert(sizeof...(Alternatives) > 0, "variant must hold at least one alternative");

  using first_t = typename meta::ith_t<0, Alternatives...>;

  using recur = detail::recursion_helper<Alternatives...>;

public:
  /*******************************************************************************
   *                                Constructors                                 *
   *******************************************************************************/

  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<first_t>) requires(detail::default_ctor<first_t>)
      : variant{in_place_index<0>} {}

  constexpr variant(variant const& other) requires(detail::trivial_copy_ctor<Alternatives...>) = default;

  constexpr variant(variant const& other) noexcept((std::is_nothrow_copy_constructible_v<Alternatives> &&
                                                    ...)) requires(!detail::trivial_copy_ctor<Alternatives...> &&
                                                                   detail::copy_ctor<Alternatives...>) {
    if (!other.valueless_by_exception()) {
      recur::copy_construct(other.current_alternative, other.alternatives, this->alternatives);
      this->current_alternative = other.current_alternative;
    }
  }

  constexpr variant(variant&& other) requires(detail::trivial_move_ctor<Alternatives...>) = default;
  constexpr variant(variant&& other) noexcept((std::is_nothrow_move_constructible_v<Alternatives> &&
                                               ...)) requires(!detail::trivial_move_ctor<Alternatives...> &&
                                                              detail::move_ctor<Alternatives...>) {
    if (!other.valueless_by_exception()) {
      recur::move_construct(other.current_alternative, std::move(other.alternatives), this->alternatives);
      this->current_alternative = other.current_alternative;
    }
  }

  template <typename T>
  requires(
      detail::resolvable_conversion<T, Alternatives...> &&
      !std::same_as<
          std::remove_cvref_t<T>,
          variant>) constexpr variant(T&& t) noexcept(std::
                                                          is_nothrow_constructible_v<
                                                              detail::resolve_conversion_t<T, Alternatives...>, T>) {
    recur::convert_construct(std::forward<T>(t), this->alternatives, this->current_alternative,
                             sizeof...(Alternatives));
  }

  template <class T, class... Args>
  requires meta::once<T, Alternatives...> && std::is_constructible_v<T, Args...>
  constexpr explicit variant(in_place_type_t<T>, Args&&... args) {
    recur::template inplace_type_construct<T>(this->alternatives, std::forward<Args>(args)...);
    this->current_alternative = meta::idx_v<T, Alternatives...>;
  }

  template <size_t I, class... Args>
  requires(I < sizeof...(Alternatives)) &&
      std::is_constructible_v<meta::ith_t<I, Alternatives...>, Args...> constexpr explicit variant(in_place_index_t<I>,
                                                                                                   Args&&... args) {
    recur::template inplace_index_construct<I>(this->alternatives, std::forward<Args>(args)...);
    this->current_alternative = I;
  }

  /*******************************************************************************
   *                                 Assignment                                  *
   *******************************************************************************/

  // Copy assign
  constexpr variant& operator=(const variant& rhs) requires detail::trivial_copy_ctor<Alternatives...> &&
      detail::trivial_copy_assign<Alternatives...> && detail::trivial_dtor<Alternatives...>
  = default;

  constexpr variant&
  operator=(const variant& rhs) requires detail::copy_ctor<Alternatives...> && detail::copy_assign<Alternatives...> &&
      (!(detail::trivial_copy_ctor<Alternatives...> && detail::trivial_copy_assign<Alternatives...> &&
         detail::trivial_dtor<Alternatives...>)) {
    if (this == &rhs) {
      return *this;
    }
    if (rhs.valueless_by_exception()) {
      if (!valueless_by_exception()) {
        reset();
      }
    } else {
      if (rhs.index() == index()) {
        recur::copy_assign(index(), rhs.alternatives, this->alternatives);
      } else {
        if (meta::runtime_trait<std::is_nothrow_copy_constructible, Alternatives...>(rhs.index()) ||
            !meta::runtime_trait<std::is_nothrow_move_constructible, Alternatives...>(rhs.index())) {
          reset();
          recur::copy_construct(rhs.index(), rhs.alternatives, this->alternatives);
        } else {
          this->operator=(variant(rhs));
        }
      }
    }
    this->current_alternative = rhs.current_alternative;
    return *this;
  }

  // Move assign
  constexpr variant& operator=(variant&& rhs) requires detail::trivial_move_ctor<Alternatives...> &&
      detail::trivial_move_assign<Alternatives...> && detail::trivial_dtor<Alternatives...>
  = default;

  constexpr variant&
  operator=(variant&& rhs) noexcept(((std::is_nothrow_move_constructible_v<Alternatives> &&
                                      std::is_nothrow_move_assignable_v<Alternatives>)&&...)) requires
      detail::move_ctor<Alternatives...> && detail::move_assign<Alternatives...> &&
      (!(detail::trivial_move_ctor<Alternatives...> && detail::trivial_move_assign<Alternatives...> &&
         detail::trivial_dtor<Alternatives...>)) {
    if (this == &rhs) {
      return *this;
    }
    if (rhs.valueless_by_exception()) {
      if (!valueless_by_exception()) {
        reset();
      }
    } else {
      if (rhs.index() == index()) {
        recur::move_assign(index(), std::move(rhs.alternatives), this->alternatives);
      } else {
        reset();
        recur::move_construct(rhs.index(), std::move(rhs.alternatives), this->alternatives);
      }
    }
    this->current_alternative = rhs.current_alternative;
    return *this;
  }

  // Conversion assign
  template <typename T>
  requires detail::resolvable_conversion<T, Alternatives...> &&(
      std::is_assignable_v<Alternatives&, std::remove_cvref_t<T>> || ...) constexpr variant&
  operator=(T&& t) noexcept((std::is_nothrow_assignable_v<detail::resolve_conversion_t<T, Alternatives...>&, T> &&
                             std::is_nothrow_constructible_v<detail::resolve_conversion_t<T, Alternatives...>, T>)) {
    using T_j = detail::resolve_conversion_t<T, Alternatives...>;
    if (holds_alternative<T_j>(*this)) {
      recur::convert_assign(std::forward<T>(t), this->alternatives, this->current_alternative, sizeof...(Alternatives));
    } else {
      if constexpr (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>) {
        this->emplace<T_j>(std::forward<T>(t));
      } else {
        this->emplace<T_j>(T_j(std::forward<T>(t)));
      }
    }
    this->current_alternative = meta::idx_v<T_j, Alternatives...>;
    return *this;
  }

  template <class T, class... Args>
  requires meta::once<T, Alternatives...> && std::is_constructible_v<T, Args...>
  constexpr T& emplace(Args&&... args) {
    return emplace<meta::idx_v<T, Alternatives...>>(std::forward<Args>(args)...);
  }

  template <size_t I, class... Args>
  requires std::is_constructible_v<meta::ith_t<I, Alternatives...>, Args...>
  constexpr variant_alternative_t<I, variant>& emplace(Args&&... args) {
    if (!valueless_by_exception()) {
      reset();
      recur::template inplace_index_construct<I>(this->alternatives, std::forward<Args>(args)...);
      this->current_alternative = I;
    }
    return get<I>(*this);
  }

  constexpr size_t index() const noexcept {
    return this->current_alternative;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return this->current_alternative == variant_npos;
  }

  constexpr void reset() noexcept {
    if (!valueless_by_exception()) {
      recur::destroy(this->current_alternative, this->alternatives);
      this->current_alternative = variant_npos;
    }
  }

  /*******************************************************************************
   *                                   Swap                                      *
   *******************************************************************************/

  constexpr void swap(variant& rhs) noexcept(
      ((std::is_nothrow_move_constructible_v<Alternatives> &&
        std::is_nothrow_swappable_v<Alternatives>)&&...)) requires((std::is_swappable_v<Alternatives> && ...) &&
                                                                   detail::move_ctor<Alternatives...>) {
    if (valueless_by_exception() && rhs.valueless_by_exception()) {
      return;
    } else if (index() == rhs.index()) {
      recur::swap_same_alternative(index(), this->alternatives, rhs.alternatives);
    } else {
      if (valueless_by_exception()) {
        recur::move_construct(rhs.index(), std::move(rhs.alternatives), this->alternatives);
        std::swap(this->current_alternative, rhs.current_alternative);
        rhs.reset();
      } else if (rhs.valueless_by_exception()) {
        recur::move_construct(index(), std::move(this->alternatives), rhs.alternatives);
        std::swap(this->current_alternative, rhs.current_alternative);
        reset();
      } else {
        variant temp{std::move(rhs)};
        rhs = std::move(*this);
        *this = std::move(temp);
      }
    }
  }

  /*******************************************************************************
   *                                 Friends                                     *
   *******************************************************************************/

  template <typename T>
  friend constexpr bool holds_alternative(variant const& v) noexcept {
    return recur::template holds<T>(v.current_alternative, v.alternatives);
  }

  /*******************************************************************************
   *                             `get_if` overloads                              *
   *******************************************************************************/

  template <size_t I>
  requires(I < sizeof...(Alternatives)) friend constexpr std::add_pointer_t<
      variant_alternative_t<I, variant<Alternatives...>>> get_if(variant<Alternatives...>* pv) noexcept {
    if (pv->index() == I) {
      return recur::template get_if<I>(pv->alternatives);
    } else {
      return nullptr;
    }
  }

  template <size_t I>
  requires(I < sizeof...(Alternatives)) friend constexpr std::add_pointer_t<
      const variant_alternative_t<I, variant<Alternatives...>>> get_if(variant<Alternatives...> const* pv) noexcept {
    if (pv->index() == I) {
      return recur::template get_if<I>(pv->alternatives);
    } else {
      return nullptr;
    }
  }

  template <class T>
  requires meta::once<T, Alternatives...>
  friend constexpr std::add_pointer_t<T> get_if(variant<Alternatives...>* pv) noexcept {
    return get_if<meta::idx_v<T, Alternatives...>>(pv);
  }

  template <class T>
  requires meta::once<T, Alternatives...>
  friend constexpr std::add_pointer_t<const T> get_if(variant<Alternatives...> const* pv) noexcept {
    return get_if<meta::idx_v<T, Alternatives...>>(pv);
  }
};

/*******************************************************************************
 *                             `get` overloads                                 *
 *******************************************************************************/

template <size_t I, typename... Alternatives>
constexpr variant_alternative_t<I, variant<Alternatives...>>& get(variant<Alternatives...>& v) {
  variant_alternative_t<I, variant<Alternatives...>>* res = get_if<I>(&v);
  if (res) {
    return *res;
  } else {
    throw bad_variant_access{};
  }
}

template <size_t I, typename... Alternatives>
constexpr variant_alternative_t<I, variant<Alternatives...>>&& get(variant<Alternatives...>&& v) {
  return std::move(get<I>(v));
}

template <size_t I, typename... Alternatives>
constexpr variant_alternative_t<I, variant<Alternatives...>> const& get(variant<Alternatives...> const& v) {
  auto res = get_if<I>(&v);
  if (res) {
    return *res;
  } else {
    throw bad_variant_access{};
  }
}

template <size_t I, typename... Alternatives>
constexpr variant_alternative_t<I, variant<Alternatives...>> const&& get(variant<Alternatives...> const&& v) {
  return std::move(get<I>(v));
}

template <typename T, typename... Alternatives>
requires meta::once<T, Alternatives...>
constexpr T& get(variant<Alternatives...>& v) {
  return get<meta::idx_v<T, Alternatives...>>(v);
}

template <typename T, typename... Alternatives>
requires meta::once<T, Alternatives...>
constexpr T&& get(variant<Alternatives...>&& v) {
  return std::move(get<meta::idx_v<T, Alternatives...>>(v));
}

template <typename T, typename... Alternatives>
requires meta::once<T, Alternatives...>
constexpr T const& get(variant<Alternatives...> const& v) {
  return get<meta::idx_v<T, Alternatives...>>(v);
}

template <typename T, typename... Alternatives>
requires meta::once<T, Alternatives...>
constexpr T const&& get(variant<Alternatives...> const&& v) {
  return std::move(get<meta::idx_v<T, Alternatives...>>(v));
}

/*******************************************************************************
 *                           `visit` implementation                            *
 *******************************************************************************/

template <class Visitor, class... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access{};
  }
  return std::invoke(detail::multi_at(detail::jump_table<Visitor, Variants...>, vars.index()...),
                     std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <class R, class Visitor, class... Variants>
constexpr R visit(Visitor&& vis, Variants&&... vars) {
  return visit(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

/*******************************************************************************
 *                             comparison operators                            *
 *******************************************************************************/

template <typename... Alternatives>
constexpr auto operator<=>(const variant<Alternatives...>& lhs, const variant<Alternatives...>& rhs) {
  if (lhs.valueless_by_exception() && rhs.valueless_by_exception()) {
    return std::strong_ordering::equal;
  } else if (lhs.valueless_by_exception()) {
    return std::strong_ordering::less;
  } else if (rhs.valueless_by_exception()) {
    return std::strong_ordering::greater;
  } else {
    return ::visit(
        [&lhs, &rhs](auto&& x, auto&& y) {
          if constexpr (std::is_same_v<std::remove_cvref_t<decltype(x)>, std::remove_cvref_t<decltype(y)>>) {
            if (lhs.index() == rhs.index()) {
              // basically imitating `x <=> y`
              if (x < y) {
                return std::strong_ordering::less;
              } else if (x == y) {
                return std::strong_ordering::equal;
              } else {
                return std::strong_ordering::greater;
              }
            }
          }
          return lhs.index() <=> rhs.index();
        },
        lhs, rhs);
  }
}

template <typename... Alternatives>
constexpr bool operator<(const variant<Alternatives...>& lhs, const variant<Alternatives...>& rhs) {
  return (lhs <=> rhs) == std::strong_ordering::less;
}

template <typename... Alternatives>
constexpr bool operator>(const variant<Alternatives...>& lhs, const variant<Alternatives...>& rhs) {
  return (lhs <=> rhs) == std::strong_ordering::greater;
}

template <typename... Alternatives>
constexpr bool operator<=(const variant<Alternatives...>& lhs, const variant<Alternatives...>& rhs) {
  auto res = (lhs <=> rhs);
  return res == std::strong_ordering::less || res == std::strong_ordering::equal;
}

template <typename... Alternatives>
constexpr bool operator>=(const variant<Alternatives...>& lhs, const variant<Alternatives...>& rhs) {
  auto res = (lhs <=> rhs);
  return res == std::strong_ordering::greater || res == std::strong_ordering::equal;
}

template <typename... Alternatives>
constexpr bool operator==(const variant<Alternatives...>& lhs, const variant<Alternatives...>& rhs) {
  return (lhs <=> rhs) == std::strong_ordering::equal;
}

template <typename... Alternatives>
constexpr bool operator!=(const variant<Alternatives...>& lhs, const variant<Alternatives...>& rhs) {
  return !(lhs == rhs);
}
