#pragma once

#include <tp/tpack.h>

#include <optional>


// disable warning about surprising (expected for us) behavior with virtual inheritance
#if defined(_MSC_VER)
	#pragma warning(push)
	#pragma warning(disable: 4250)
#endif

namespace ddv {

	template<typename... Ts>
	inline constexpr bool static_false = false;

	using void_value_t = tp::unit<void>;
	inline constexpr auto void_value = void_value_t{};

	using void_result_t = std::optional<void_value_t>;
	inline constexpr auto ok = void_result_t{std::in_place};
	inline constexpr auto none = void_result_t{std::nullopt};

	struct const_tag {};
	inline constexpr auto const_ = const_tag{};

	namespace detail {
	
		// will produce Is_1 + offs, Is_2 + offs, ...
		template<std::size_t offs, std::size_t... Is>
		constexpr auto offset_indexes(std::index_sequence<Is...>) {
			return std::index_sequence<offs + Is...>{};
		}

		template<std::size_t From, std::size_t To>
		constexpr auto bounded_index_sequence() {
			static_assert(From <= To, "Incorrect boundaries for index sequence");
			return offset_indexes<From>(std::make_index_sequence<To - From>{});
		}

		template<typename T>
		constexpr auto is_pointer_like(int)
		-> decltype(*std::declval<T>(), static_cast<bool>(std::declval<T>())) { return true; }

		template<typename T>
		constexpr bool is_pointer_like(...) { return false; }

		template<typename From, typename To>
		constexpr auto can_static_cast(int)
		-> decltype(static_cast<To>(std::declval<From>()), bool{}) { return true; }

		template<typename From, typename To>
		constexpr bool can_static_cast(...) { return false; }

		template<typename T> struct is_optional : std::false_type {};
		template<typename T> struct is_optional<std::optional<T>> : std::true_type {};

		template<typename T> struct deduce_value { using type = T; };
		template<typename T> struct deduce_value<std::optional<T>> {
			using type = T;
		};

		template<typename T> struct deduce_result { using type = std::optional<T>; };
		template<typename T> struct deduce_result<std::optional<T>> {
			using type = std::optional<T>;
		};
		template<> struct deduce_result<void> { using type = void_result_t; };

		template<typename T>
		inline constexpr bool is_value_void = std::is_void_v<T> || std::is_same_v<T, void_value_t>;

		// blocks for building visitor mux
		template<typename T>
		struct mux_wire {
			virtual void visit(T*) = 0;
		};

		// all demux wires share the same `Mux` instance
		template<typename T, typename Mux, typename Demux>
		struct demux_wire : virtual Mux {
			using Mux::visit;

			void visit(T* x) override final {
				using DemuxBackend = typename Demux::DemuxBackend;
				auto& self = static_cast<Demux&>(*this);
				static_cast<DemuxBackend&>(self).visit(x);
			}
		};

		template<typename Mux, typename Demux, typename... Ts>
		constexpr auto make_visitor(tp::tpack<Ts...>) {
			struct demux final
				: Demux, demux_wire<Ts, Mux, demux>... {
				using DemuxBackend [[maybe_unused]] = Demux;
				using Demux::Demux;
			};
			return tp::unit_v<demux>;
		}

	} // namespace detail

	template<std::size_t From, std::size_t To>
	inline constexpr auto bounded_index_sequence = detail::bounded_index_sequence<From, To>();

	template<typename T>
	inline constexpr bool is_pointer_like = detail::is_pointer_like<std::remove_cvref_t<T>>(0);

	template<typename Base, typename Derived>
	inline constexpr bool is_virtual_base_of = std::is_base_of_v<Base, Derived> && !detail::can_static_cast<Base*, Derived*>(0);

	template<typename T>
	inline constexpr bool is_optional = detail::is_optional<std::remove_cvref_t<T>>::value;

	template<typename T>
	using deduce_value_t = typename detail::deduce_value<std::remove_cvref_t<T>>::type;

	template<typename T>
	using deduce_result_t = typename detail::deduce_result<std::remove_cvref_t<T>>::type;

	// true if T is exactly void or `void_value_t`
	template<typename T>
	inline constexpr auto is_void = detail::is_value_void<deduce_value_t<T>>;

	inline constexpr auto noop = [](auto&&...) {};
	using noop_t = decltype(noop);

	template<bool Res>
	inline constexpr auto noop_bool = [](auto&&...) { return Res; };
	inline constexpr auto noop_false = noop_bool<false>;
	inline constexpr auto noop_true = noop_bool<true>;

	template<typename... Ts>
	struct mux : detail::mux_wire<Ts>... {
		using types = tp::tpack<Ts...>;
		using visited_types = tp::tpack<std::add_pointer_t<Ts>...>;
		using const_mux_type = mux<std::add_const_t<Ts>...>;

		using detail::mux_wire<Ts>::visit...;
	};

	template<typename Mux, typename Demux>
	using visitor = typename decltype(
		detail::make_visitor<Mux, Demux>(typename Mux::types{})
	)::type;

	namespace detail {

		template<typename... Ts>
		constexpr bool is_mux_interface(const mux<Ts...>*) { return true; }

		constexpr bool is_mux_interface(...) { return false; }

	} // namespace detail

	template<typename T>
	inline constexpr bool is_mux_interface = detail::is_mux_interface(std::add_pointer_t<T>{});

	// can be used with pipe operator to extract a value of given type from variant type returned by serial visitor
	template<typename T>
	inline constexpr auto cast = [](auto&& x) -> T { return std::forward<decltype(x)>(x); };

} // namespace ddv

#if defined(_MSC_VER)
	#pragma warning(pop)
#endif
