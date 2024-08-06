#pragma once

#include <ddv/serial_visitor.h>
#include <tp/tpack.h>


// VISITOR_SUPPORT_DECL(final) will make overrides final
#define VISITOR_SUPPORT_DECL(...) \
void accept(mux_type& v) override __VA_ARGS__; \
void accept(const_mux_type& v) const override __VA_ARGS__;

#define VISITOR_SUPPORT(...) \
auto accept(mux_type& v) -> void override __VA_ARGS__ { return v.visit(this); } \
auto accept(const_mux_type& v) const -> void override __VA_ARGS__ { return v.visit(this); }

#define VISITOR_SUPPORT_IMPL(T) \
void T::accept(mux_type& v) { return v.visit(this); } \
void T::accept(const_mux_type& v) const { return v.visit(this); }

namespace ddv {

	/////////////////////////////////////////////////////////////////////////
	// `serial` wrapper that can store demux result calculated in void `T->accept()` call
	// intended to be used with visitable types and `serial::apply()`
	template<typename Mux, typename Serial>
	struct visitable_demux : Serial {
		// [NOTE] can't just inherit ctors like `using Serial::Serial;`
		// copy/move ctors aren't inherited => initializing `visitable_demux` with instance of `serial` would fail
		template<typename... Ts>
			requires std::constructible_from<Serial, Ts...>
		visitable_demux(Ts&&... args) : Serial(std::forward<Ts>(args)...) {}

		template<typename T>
		constexpr auto visit(T&& value) {
			auto v = serial{[&](auto&& value) {
				if constexpr (std::is_void_v<decltype( Serial::visit(std::declval<T>()) )>)
					Serial::visit(std::forward<T>(value));
				else
					res_ = Serial::visit(std::forward<T>(value));
			}};
			v.visit(std::forward<T>(value));
		}

		decltype(auto) operator*() { return std::move(res_); }

	private:
		using demux_outcome = decltype(
			std::declval<Serial>().visit(std::declval< tp::make<std::variant, typename Mux::visited_types> >())
		);
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
	template<typename Mux, typename... Fs>
	constexpr auto make(Fs&&... fs) {
		using Serial = decltype( serial{std::declval<Fs>()...} );
		using Demux = visitable_demux<Mux, Serial>;
		return visitor<Mux, Demux>(std::forward<Fs>(fs)...);
	}

	template<typename T> struct TD;

	/////////////////////////////////////////////////////////////////////////
	// Implements double dispatch visitor API
	// Ancestor of types hierarchy must inherit from it to gain visitor support
	template<typename Ancestor, typename Mux>
	struct visitable {
		using mux_type = Mux;
		using const_mux_type = typename Mux::const_mux_type;

		virtual auto accept(mux_type&) -> void = 0;
		virtual auto accept(const_mux_type&) const -> void = 0;

		template<typename... Fs>
		auto visit(Fs&&... fs) {
			auto v = make<mux_type>(std::forward<Fs>(fs)...);
			this->accept(v);
			if constexpr (!decltype(v)::is_result_void)
				return *v;
		}

		template<typename... Fs>
		auto visit(Fs&&... fs) const {
			auto v = make<const_mux_type>(std::forward<Fs>(fs)...);
			this->accept(v);
			if constexpr (!decltype(v)::is_result_void)
				return *v;
		}

		// produces callable that accepts pointer to Ancestor and calls `f` iff argument actually points to any of `Ts...`
		// [NOTE] const/non-const Ts makes difference because type filter works by comparing type IDs
		template<typename... Ts, typename F>
		static constexpr auto make_visitor(F&& f) {
			return make_visitor(std::forward<F>(f), strip(tp::tpack_v<Ts...>));
		}

		// same as above but for capturing multiple specializations of template type `U<T>`
		template<template<typename...> typename U, typename T, typename... Ts, typename F>
		static constexpr auto make_visitor(F&& f) {
			// if `T` is tpack - treat it as list of template args to generate specializations of `U`
			if constexpr (tp::is_tpack_v<T>)
				return make_visitor(
					std::forward<F>(f),
					transform(T{}, []<typename X>(tp::unit<X>) { return tp::unit_v<U<X>>; })
				);
			else
				return make_visitor(std::forward<F>(f), tp::tpack_v<U<T>, U<Ts>...>);
		}

		// generates specializations `const U<T>...`
		// code duplication is unfortunate, but it's a tradeoff to prevent extra indirection layers
		template<template<typename...> typename U, typename T, typename... Ts, typename F>
		static constexpr auto make_visitor(F&& f, const_tag) {
			if constexpr (tp::is_tpack_v<T>)
				return make_visitor(
					std::forward<F>(f),
					transform(T{}, []<typename X>(tp::unit<X>) { return tp::unit_v<const U<X>>; })
				);
			else
				return make_visitor(std::forward<F>(f), tp::tpack_v<const U<T>, const U<Ts>...>);
		}

	private:
		template<typename F, typename... Ts>
		static constexpr auto make_visitor(F&& f, tp::tpack<Ts...> ts) {
			constexpr auto n = size(ts);
			// if no types are explicitly requested -- infer one from F's first argument
			if constexpr (n == 0)
				return do_make_visitor(std::forward<F>(f));
			// no need to wrap single cherry capture lambda with serial visitor, return it directly
			else if constexpr (n == 1)
				return do_make_visitor(std::forward<F>(f), head(ts));
			// otherwise build serial visitor containing lambda for each requested type
			else
				return serial{
					do_make_visitor(std::forward<F>(f), tp::unit_v<Ts>)...,
					// item that offloads value to rest of upper level visitor chain
					[] { return none; }
				};
		}

		template<typename F, typename Cherry = void>
		static constexpr auto do_make_visitor(F&& f, tp::unit<Cherry> cherry = void_value) {
			if constexpr (util::can_deduce_callable<F>) {
				using Finfo = util::deduce_callable<F>;
				if constexpr (std::is_void_v<Cherry>)
					return make_cherry_visitor(std::forward<F>(f), typename Finfo::args{});
				else
					return make_cherry_visitor(std::forward<F>(f), cherry + tail(typename Finfo::args{}));
			}
			else {
				static_assert(!std::is_void_v<Cherry>, "Cannot deduce type to filter from 1st callable argument. "
					"Specify it explicitly either as `make_visitor()` template param or as type of 1st argument.");
				return make_cherry_visitor(std::forward<F>(f), cherry);
			}
		}

		template<typename F, typename Cherry, typename... Ts>
		static constexpr auto make_cherry_visitor(F&& f, tp::tpack<Cherry, Ts...>) {
			using cherry_t = std::conditional_t<std::is_pointer_v<Cherry>, std::remove_pointer_t<Cherry>, Cherry>;
			using cherry_ptr_t = std::add_pointer_t<cherry_t>;
			using bait_ptr_t = std::conditional_t<std::is_const_v<cherry_t>, const Ancestor*, Ancestor*>;
			static_assert(
				std::is_base_of_v<Ancestor, cherry_t>
				&& (std::is_invocable_v<F> || std::is_invocable_v<F, cherry_ptr_t, Ts...>),
				"Callable must either accept pointer to type derived from visitable hierarchy ancestor as 1st argument "
				"or take no arguments"
			);

			return [f = std::forward<F>(f)](bait_ptr_t self, Ts... xs) mutable {
				using Fret_t = std::conditional_t<
					std::is_invocable_v<F>, std::invoke_result<F>, std::invoke_result<F, cherry_ptr_t, Ts...>
				>::type;
				using res_t = deduce_result_t<Fret_t>;

				if (typeid(*self) == typeid(cherry_t)) {
					const auto call_f = [&] {
						if constexpr (std::is_invocable_v<F>)
							return f();
						else {
							if constexpr (is_virtual_base_of<Ancestor, cherry_t>)
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
