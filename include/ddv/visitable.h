#pragma once

#include <ddv/serial_visitor.h>
#include <tp/tpack.h>


// VISITOR_SUPPORT_DECL(final) will make overrides final
#define VISITOR_SUPPORT_DECL(...) \
void accept(mux_type& v) override __VA_ARGS__; \
void accept(const_mux_type& v) const override __VA_ARGS__;

#define VISITOR_SUPPORT(...) \
auto accept(mux_type& v) -> void override __VA_ARGS__ { v.visit(this); } \
auto accept(const_mux_type& v) const -> void override __VA_ARGS__ { v.visit(this); }

#define VISITOR_SUPPORT_IMPL(T) \
void T::accept(mux_type& v) { v.visit(this); } \
void T::accept(const_mux_type& v) const { v.visit(this); }

namespace ddv {

	/////////////////////////////////////////////////////////////////////////
	// `serial` wrapper that can store demux result calculated in void `T->accept()` call
	// intended to be used with visitable types and `serial::apply()`
	template<typename Mux, typename Serial, typename... Args>
		requires SerialVisitorType<Serial>
	struct visitable_demux : private Serial {
		// [NOTE] can't just inherit ctors like `using Serial::Serial;`
		// copy/move ctors aren't inherited => initializing `visitable_demux` with instance of `serial` would fail
		template<typename... Fs>
			requires std::constructible_from<Serial, Fs...>
		visitable_demux(std::tuple<Args...> args, Fs&&... fs)
			: Serial(std::forward<Fs>(fs)...), args_{std::move(args)}
		{}

		template<typename... Fs>
			requires (sizeof...(Args) == 0)
		visitable_demux(Fs&&... fs) : visitable_demux(std::tuple<>{}, std::forward<Fs>(fs)...) {}

		template<typename T>
		constexpr auto visit(T&& value) {
			const auto do_visit = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
				return Serial::visit(std::forward<T>(value), std::get<Is>(std::move(args_))...);
			};

			constexpr auto Is = std::index_sequence_for<Args...>{};
			if constexpr (std::is_void_v<decltype(do_visit(Is))>)
				do_visit(Is);
			else
				res_ = do_visit(Is);
		}

		decltype(auto) operator*() noexcept { return std::move(res_); }

	private:
		std::tuple<Args...> args_;

		using demux_outcome = decltype(std::declval<Serial>().visit(
			std::declval<tp::make<std::variant, typename Mux::visited_types>>(), std::declval<Args>()...
		));
		// result_type =
		// 1. void_value_t, if Serial result is void for all visited types from Mux
		// 2. invoke result type of Serial::visit() otherwise
		using result_type = std::conditional_t<std::is_void_v<demux_outcome>, void_value_t, demux_outcome>;
		result_type res_;

	public:
		static constexpr bool is_result_void = std::is_same_v<result_type, void_value_t>;
	};

	/////////////////////////////////////////////////////////////////////////
	// make visitor with given multiplexer interface and `serial` demultiplexer
	template<typename Mux, typename... Args, typename... Fs>
	constexpr auto make_visitable_visitor(std::tuple<Args...>&& args, Fs&&... fs) noexcept {
		using Serial = decltype(serial{std::declval<Fs>()...});
		using Demux = visitable_demux<Mux, Serial, Args...>;
		return visitor<Mux, Demux>(std::move(args), std::forward<Fs>(fs)...);
	}

	/////////////////////////////////////////////////////////////////////////
	// Implements double dispatch visitor API
	// Ancestor of types hierarchy must inherit from it to gain visitor support
	template<typename Ancestor, typename Mux>
	struct visitable {
		using mux_type = Mux;
		using const_mux_type = typename Mux::const_mux_type;

		virtual auto accept(mux_type&) -> void = 0;
		virtual auto accept(const_mux_type&) const -> void = 0;

		template<typename F, typename... Xs>
			requires SerialVisitorType<F>
		auto visit(F&& f, Xs&&... xs) {
			auto v = make_visitable_visitor<mux_type>(std::forward_as_tuple(std::forward<Xs>(xs)...), std::forward<F>(f));
			this->accept(v);
			if constexpr (!decltype(v)::is_result_void)
				return *v;
		}

		template<typename F, typename... Fs>
		auto visit(F&& f, Fs&&... fs) {
			return visit(serial{std::forward<F>(f), std::forward<Fs>(fs)...});
		}

		template<typename F, typename... Xs>
			requires SerialVisitorType<F>
		auto visit(F&& f, Xs&&... xs) const {
			auto v = make_visitable_visitor<const_mux_type>(std::forward_as_tuple(std::forward<Xs>(xs)...), std::forward<F>(f));
			this->accept(v);
			if constexpr (!decltype(v)::is_result_void)
				return *v;
		}

		template<typename F, typename... Fs>
		auto visit(F&& f, Fs&&... fs) const {
			return visit(serial{std::forward<F>(f), std::forward<Fs>(fs)...});
		}

		// produces callable that accepts pointer to Ancestor and calls `f` iff argument actually points to any of `Ts...`
		// [NOTE] const/non-const Ts makes difference because type filter works by comparing type IDs
		template<typename... Ts, typename F>
		static constexpr auto make_filter(F&& f) {
			return make_filter(std::forward<F>(f), strip(tp::tpack_v<Ts...>));
		}

		// same as above but for capturing multiple specializations of template type `U<T>`
		template<template<typename...> typename U, typename T, typename... Ts, typename F>
		static constexpr auto make_filter(F&& f) {
			// if `T` is tpack - treat it as list of template args to generate specializations of `U`
			if constexpr (tp::is_tpack_v<T>)
				return make_filter(
					std::forward<F>(f),
					tp::transform(T{}, []<typename X>(tp::unit<X>) { return tp::unit_v<U<X>>; })
				);
			else
				return make_filter(std::forward<F>(f), tp::tpack_v<U<T>, U<Ts>...>);
		}

		// generates specializations `const U<T>...`
		// code duplication is unfortunate, but it's a tradeoff to prevent extra indirection layers
		template<template<typename...> typename U, typename T, typename... Ts, typename F>
		static constexpr auto make_filter(F&& f, const_tag) {
			if constexpr (tp::is_tpack_v<T>)
				return make_filter(
					std::forward<F>(f),
					tp::transform(T{}, []<typename X>(tp::unit<X>) { return tp::unit_v<const U<X>>; })
				);
			else
				return make_filter(std::forward<F>(f), tp::tpack_v<const U<T>, const U<Ts>...>);
		}

	private:
		template<typename F, typename... Ts>
		static constexpr auto make_filter(F&& f, tp::tpack<Ts...> ts) {
			constexpr auto n = tp::size(ts);
			// if no types are explicitly requested -- infer one from F's first argument
			if constexpr (n == 0)
				return do_make_filter(std::forward<F>(f));
			// no need to wrap single cherry capture lambda with serial visitor, return it directly
			else if constexpr (n == 1)
				return do_make_filter(std::forward<F>(f), tp::head(ts));
			// otherwise build serial visitor containing lambda for each requested type
			else
				return serial{do_make_filter(std::forward<F>(f), tp::unit_v<Ts>)...};
		}

		template<typename F, typename Cherry = void>
		static constexpr auto do_make_filter(F&& f, tp::unit<Cherry> cherry = void_value) {
			if constexpr (util::can_deduce_callable<F>) {
				using Finfo = util::deduce_callable<F>;
				if constexpr (std::is_void_v<Cherry>)
					return make_cherry_picker(std::forward<F>(f), Finfo::args_v);
				else
					return make_cherry_picker(std::forward<F>(f), cherry + tail(Finfo::args_v));
			}
			else {
				static_assert(!std::is_void_v<Cherry>, "Cannot deduce type to filter from 1st argument of the callable."
					"Specify it explicitly either as `make_visitor()` template param or as type of 1st argument.");
				return make_cherry_picker(std::forward<F>(f), cherry);
			}
		}

		template<typename F, typename Cherry, typename... Ts>
		static constexpr auto make_cherry_picker(F&& f, tp::tpack<Cherry, Ts...>) {
			using cherry_t = std::conditional_t<std::is_pointer_v<Cherry>, std::remove_pointer_t<Cherry>, Cherry>;
			using cherry_ptr_t = std::add_pointer_t<cherry_t>;
			using bait_ptr_t = std::conditional_t<std::is_const_v<cherry_t>, const Ancestor*, Ancestor*>;
			static_assert(
				std::invocable<F> || (std::is_base_of_v<Ancestor, cherry_t> && std::invocable<F, cherry_ptr_t, Ts...>),
				"Callable must either accept pointer to type derived from visitable hierarchy ancestor as 1st argument "
				"or take no arguments"
			);

			return [f = std::forward<F>(f)](bait_ptr_t self, Ts... xs) mutable {
				constexpr bool f_invocable = std::invocable<F>;
				using Fret_t = std::conditional_t<
					f_invocable, std::invoke_result<F>, std::invoke_result<F, cherry_ptr_t, Ts...>
				>::type;
				using res_t = deduce_result_t<Fret_t>;

				if (typeid(*self) == typeid(cherry_t)) {
					const auto call_f = [&] {
						if constexpr (f_invocable)
							return f();
						else {
							if constexpr (VirtualBaseOf<Ancestor, cherry_t>)
								return f(dynamic_cast<cherry_ptr_t>(self), std::forward<Ts>(xs)...);
							else
								return f(static_cast<cherry_ptr_t>(self), std::forward<Ts>(xs)...);
						}
					};

					if constexpr (std::is_void_v<Fret_t>) {
						call_f();
						return make_result<res_t>(ok);
					}
					else
						return make_result<res_t>(call_f());
				}
				return make_result<res_t>(none);
			};
		}
	};

} // namespace ddv
