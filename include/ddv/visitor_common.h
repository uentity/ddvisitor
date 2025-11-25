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

	template<typename T> using nut_t = std::remove_cvref_t<T>;
	template<typename T> using nut = tp::unit<nut_t<T>>;

	template<typename T>
	inline constexpr auto nut_v = nut<T>{};

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

		template<typename From, typename To>
		concept can_static_cast = requires(From x) { static_cast<To>(x); };

		template<typename T>
		concept is_void = std::is_void_v<T> || std::same_as<T, void_value_t>;

		template<typename T> struct deduce_value { using type = T; };
		template<typename T> struct deduce_value<std::optional<T>> {
			using type = T;
		};

		template<typename T> struct deduce_result { using type = std::optional<T>; };
		template<typename T> struct deduce_result<std::optional<T>> {
			using type = std::optional<T>;
		};
		template<> struct deduce_result<void> { using type = void_result_t; };

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
				auto& self_demux = static_cast<Demux&>(*this);
				static_cast<DemuxBackend&>(self_demux).visit(x);
			}
		};

		template<typename Mux, typename Demux, typename... Ts>
		constexpr auto make_visitor_t(tp::tpack<Ts...>) {
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
	concept PointerLikeType = requires(T x) { *x; static_cast<bool>(x); };

	template<typename Base, typename Derived>
	concept VirtualBaseOf = std::is_base_of_v<Base, Derived> && !detail::can_static_cast<Base*, Derived*>;

	template<typename T>
	concept OptionalType = std::same_as<nut_t<T>, std::optional<typename nut_t<T>::value_type>>;

	template<typename T>
	using deduce_value_t = detail::deduce_value<std::remove_cvref_t<T>>::type;

	template<typename T>
	using deduce_result_t = detail::deduce_result<std::remove_cvref_t<T>>::type;

	// true if T is exactly void or `void_value_t`
	template<typename T>
	concept VoidType = detail::is_void<deduce_value_t<T>>;

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
		detail::make_visitor_t<Mux, Demux>(typename Mux::types{})
	)::type;

	template<typename T>
	concept MuxType = std::same_as<nut_t<T>, tp::make<mux, typename nut_t<T>::types>>;

	// can be used with pipe operator to extract a value of given type from variant type returned by serial visitor
	template<typename T>
	inline constexpr auto cast = []<typename X>(X&& x) -> T { return std::forward<X>(x); };

	// compiles faster that std::invoke_result_t
	template<typename F, typename... Args>
	using call_result_t = decltype(std::declval<F>()(std::declval<Args>()...));

} // namespace ddv

#if defined(_MSC_VER)
	#pragma warning(pop)
#endif
