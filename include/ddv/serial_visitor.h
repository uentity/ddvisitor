#pragma once

#include <ddv/visitor_common.h>
#include <ddv/deduce_callable.h>

#include <concepts>
#include <tuple>
#include <variant>


namespace ddv {

	template<typename... Fs> class serial;
	// deduction guides - required because `serial` has converting ctor
	template<typename... Fs> serial(Fs&&...) -> serial<Fs...>;

	namespace detail {

		template<typename T>
		constexpr auto is_variant(tp::unit<T>) -> std::false_type;

		template<typename... Ts>
		constexpr auto is_variant(tp::unit<std::variant<Ts...>>) -> std::true_type;

		template<typename T>
		constexpr auto is_serial_visitor(tp::unit<T>) -> std::false_type;

		template<typename... Fs>
		constexpr auto is_serial_visitor(tp::unit<serial<Fs...>>) -> std::true_type;

		template<typename U, typename T>
		constexpr bool carry_type(tp::tpack<T>) {
			return std::is_same_v<typename deduce_value<T>::type, U>;
		}

		template<typename U, typename... Ts>
		constexpr bool carry_type(tp::unit<std::variant<Ts...>>) {
			return (carry_type<U>(tp::unit_v<Ts>) || ...);
		}


	} // ddv::detail

	template<typename T>
	concept is_variant = decltype(detail::is_variant(nut_v<T>))::value;

	template<typename T>
	concept is_serial_visitor = decltype(detail::is_serial_visitor(nut_v<T>))::value;

	// checks whether type T can carry target type U where T can be optional/variant
	template<typename T, typename U>
	concept carry_type = detail::carry_type<U>(nut_v<T>);

	template<typename T>
	concept carry_void = carry_type<T, void> || carry_type<T, void_value_t>;

	// make result of type R from source value of type T
	// if R is void -> make_result<R>() is also void, otherwise it produces std::optional
	// includes specific processing if T is `void_value_t` and if T is optional
	// if T is variant, unpacks variant and converts contained value to R
	template<typename R, typename T>
	constexpr decltype(auto) make_result(T&& src) {
		if constexpr (!std::is_void_v<R>) {
			using res_t = deduce_result_t<R>;
			using src_value_t = deduce_value_t<T>;
			using tgt_value_t = deduce_value_t<res_t>;
			if constexpr (std::is_same_v<res_t, std::remove_cvref_t<T>>)
				return std::forward<T>(src);
			// case when `src` is void value (`ok` or `none`)
			else if constexpr (std::is_same_v<src_value_t, void_value_t>) {
				if constexpr (carry_void<tgt_value_t>)
					return res_t{src};
				else
					return res_t{std::nullopt};
			}
			else if constexpr (is_optional<T>) {
				if (src)
					return make_result<res_t>(*std::forward<T>(src));
				else
					return res_t{std::nullopt};
			}
			// if `src` is variant - unpack value and convert to result type
			else if constexpr (is_variant<src_value_t> && !std::is_same_v<src_value_t, tgt_value_t>)
				return std::visit(
					[]<typename X>(X&& x) { return make_result<res_t>(std::forward<X>(x)); },
					std::forward<T>(src)
				);
			else
				return res_t{std::forward<T>(src)};
		}
	}

	/////////////////////////////////////////////////////////////////////////
	// Implements a visitor that sequentially examines functors in passed order and calls first matched one
	// You can provide fallback case as last function in chain
	// [NOTE] `serial` will make a copy of callables passed by lvalue references,
	// to prevent that use `std::ref()`
	template<typename... Fs>
	class serial {
		// storage type is result of `std::make_tuple()` call => referenced Fs are copied
		using storage_t = decltype(std::make_tuple(std::declval<Fs>()...));
		template<std::size_t i> using Fi = std::tuple_element_t<i, storage_t>;
		static constexpr auto chain_length = std::tuple_size_v<storage_t>;

		template<typename T>
		using bind_lvalue_ref = std::conditional_t<
			std::is_lvalue_reference_v<T>, T, std::add_lvalue_reference_t<std::add_const_t<T>>
		>;

		// forward calls to an instance of `serial` stored by reference
		// returned value is lost because `ref::visit()` has void return type
		struct ref {
			serial& self;
			ref(serial& self_) : self(self_) {}

			template<typename T>
			constexpr auto visit(T&& value) -> void {
				self.visit(std::forward<T>(value));
			}
		};

		// checks whether `F` accepts a reference to visitor mux type as a 2nd argument, if not - return `void_value`
		// otherwise return `visitor<Mux, ref>` if `Complete` is true - for calling `F`
		// and type of 2nd argument if `Complete` is false -- for `can_visit` check
		template<typename F, bool Complete>
		static constexpr auto make_ref_visitor_type() {
			if constexpr (util::can_deduce_callable<F>) {
				using Finfo = util::deduce_callable<F>;
				if constexpr (Finfo::nargs > 1) {
					using second_arg = typename Finfo::template ith_arg<1>;
					static_assert(
						is_mux_interface<second_arg> && std::is_lvalue_reference_v<second_arg>,
						"2nd argument of matched callable must be a reference to the visitor interface (multiplexer)"
					);
					using Mux = std::remove_cvref_t<second_arg>;
					if constexpr (Complete)
						return tp::unit_v<visitor<Mux, ref>>;
					else
						return tp::unit_v<second_arg>;
				}
				else return void_value;
			}
			else return void_value;
		}

		template<typename F, bool Complete>
		using ref_visitor_type = typename decltype(make_ref_visitor_type<F, Complete>())::type;

		// calc final decision whether F matches the value of type T being visited
		template<typename F, typename T>
		static constexpr bool is_matched = std::invocable<F>
			|| std::invocable<F, T>
			|| std::invocable<F, T, ref_visitor_type<F, false>>;

		template<typename T, typename... Gs>
		static constexpr bool can_visit_impl(tp::unit<T>, std::tuple<Gs...>* gs = nullptr) {
			if constexpr (is_optional<T>)
				return can_visit_impl(tp::unit_v<decltype(*std::declval<T>())>, gs);
			else if constexpr (is_variant<T>)
				return can_visit_impl<Gs...>(nut_v<T>);
			else
				// true if T can be visited by at least one callable
				return is_void<T> || (is_matched<Gs, bind_lvalue_ref<T>> || ...);
		}

		template<typename... Gs, typename... Ts>
		static constexpr bool can_visit_impl(tp::unit<std::variant<Ts...>>) {
			// returns true only if can visit each alternative of variant type T
			return (can_visit_impl<Ts, Gs...>(tp::unit_v<Ts>) && ...);
		}

		storage_t fs_;

	public:
		// tests if value of type `F` can be visited by this serial visitor (there is at least one matching callable)
		template<typename T>
		static constexpr bool can_visit = can_visit_impl(tp::unit_v<T>, (storage_t*)nullptr);

		// converting ctor to perfectly forward callables into internal storage
		template<typename... Gs>
			requires std::constructible_from<storage_t, Gs...>
		constexpr serial(Gs&&... gs) : fs_(std::forward<Gs>(gs)...) {}

		// call operator is only enabled if passed value can be visited
		// supports optionals and variants auto-unpacking
		// non-simplified return type to pass all possible results, including `ok` and `none`
		template<typename T>
			requires can_visit<T>
		constexpr auto operator()(T&& value) {
			return do_visit<false>(std::forward<T>(value));
		}

		// public interface to `operator()` above to be used by humans
		// always enabled, produces readable error if value can't be visited, don't return void values
		// auto simplifies result type `optional<variant<void_value_t, T>>` -> `optional<T>`
		// downside: cannot distinguish between `ok` and `none` in returned value, always will be `none`
		template<typename T>
		constexpr auto visit(T&& value) {
			static_assert(
				can_visit<T>,
				"There is no callable accepting given value. "
				"You can append `noop` to the chain of callables to provide default fallback."
			);
			// pass Simplify = true flag that strips `void_value_t` from result type
			// like `optional<variant<void_value_t, T>>` -> `optional<T>`
			using res_t = decltype( do_visit<true>(std::declval<T>()) );
			if constexpr (is_void<res_t>)
				do_visit<true>(std::forward<T>(value));
			else
				return do_visit<true>(std::forward<T>(value));
		}

		// effectively calls `value.visit(*this)`
		template<typename T>
		constexpr auto apply(T&& value) {
			return unpack_and_invoke(
				std::forward<T>(value),
				[this](auto&& x) { return x.visit(*this); },
				std::true_type{}
			);
		}

	private:
		template<bool Simplify, typename T>
		constexpr auto do_visit(T&& value) {
			return unpack_and_invoke<Simplify>(
				std::forward<T>(value),
				[&](auto&& x) {
					return invoke_first_match<0, Simplify>(std::forward<decltype(x)>(x));
				},
				std::false_type{}
			);
		}

		// ---------------- helpers
		template<typename L, typename... Rs>
		static constexpr auto merge_variant(tp::unit<std::variant<Rs...>> v) {
			if constexpr (find<L>(tp::tpack_v<Rs...>) < sizeof...(Rs))
				return v;
			else
				return tp::unit_v<std::variant<L, Rs...>>;
		}

		template<typename... Ls, typename... Rs>
		static constexpr auto merge_variant(tp::unit<std::variant<Ls...>>, tp::unit<std::variant<Rs...>>) {
			return tp::make_v<std::variant>(distinct(tp::tpack_v<Ls..., Rs...>));
		}

		// calculate merged type that can carry any of `Ts`
		template<typename... Ts>
		static constexpr auto merge_types(tp::tpack<Ts...> ts) {
			static_assert(size(ts));
			return fold_left(ts, []<typename L, typename R>(tp::tpack<L, R>) {
				if constexpr (std::is_void_v<L> || std::is_same_v<L, R>)
					return tp::unit_v<R>;
				else if constexpr (std::is_void_v<R>)
					return tp::unit_v<L>;
				else if constexpr (is_variant<L>) {
					if constexpr (is_variant<R>)
						return merge_variant(tp::unit_v<L>, tp::unit_v<R>);
					else
						return merge_variant<R>(tp::unit_v<L>);
				}
				else if constexpr (is_variant<R>)
					return merge_variant<L>(tp::unit_v<R>);
				else
					return tp::unit_v<std::variant<L, R>>;
			});
		}

		// transform variant<Ts..., void_value_t, Us...> -> variant<Ts..., Us...> or passthrough
		// difference between returned ddv::ok and ddv::none will be lost
		template<typename... Ts>
		static constexpr auto simplify_merged_type(tp::unit<std::variant<Ts...>> t) {
			if constexpr (sizeof...(Ts) > 1) {
				constexpr auto ts = filter(
					tp::tpack_v<Ts...>,
					[]<typename X>(tp::unit<X>) { return !std::is_same_v<X, void_value_t>; }
				);
				if constexpr (size(ts) == 1)
					return ts;
				else
					return tp::make_v<std::variant>(ts);
			}
			else
				return t;
		}

		template<typename T>
		static constexpr auto simplify_merged_type(tp::unit<T>) -> tp::unit<T> { return {}; }

		template<bool Simplify, typename... Ts>
		static constexpr auto calc_merged_type() {
			const auto res = merge_types(tp::tpack_v<Ts...>);
			if constexpr (Simplify)
				return simplify_merged_type(res);
			else
				return res;
		}

		template<bool Simplify, typename... Ts>
		using make_merged_type = decltype(calc_merged_type<Simplify, Ts...>())::type;

		template<bool Simplify, typename R>
		using make_result_type = deduce_result_t<std::conditional_t<
			Simplify, typename decltype(simplify_merged_type(tp::unit_v<deduce_value_t<R>>))::type, R
		>>;

		// try to invoke F with every variant alternative and calculate resulting common value type
		template<bool Simplify, typename F, typename... Ts>
		static constexpr auto calc_variant_response(tp::unit<std::variant<Ts...>>) {
			return calc_merged_type<Simplify, deduce_value_t<call_result_t<F, Ts>>...>();
		}

		// recursively unpack optional/variant, optionally deref pointer-likes and then call `f` on extracted value
		template<bool Simplify = false, typename T, typename F, bool DerefPtrs>
		static constexpr auto unpack_and_invoke(T&& value, F&& f, std::bool_constant<DerefPtrs> dp) {
			if constexpr (is_void<T>)
				return;
			else if constexpr (is_optional<T> || (DerefPtrs && is_pointer_like<T>)) {
				using res_t = decltype( unpack_and_invoke(*std::declval<T>(), std::declval<F>(), dp) );
				if (value)
					return unpack_and_invoke(*std::forward<T>(value), std::forward<F>(f), dp);
				else
					return make_result<res_t>(none);
			}
			else if constexpr (is_variant<T>) {
				const auto do_invoke = [&f, dp]<typename X>(X&& x) {
					return unpack_and_invoke(std::forward<X>(x), std::forward<F>(f), dp);
				};
				// calculate type to return by visiting every value alternative
				using res_value_t = decltype(
					calc_variant_response<Simplify, decltype(do_invoke)>(nut_v<T>)
				)::type;

				// unpack variant, visit value & convert result to calculated type
				return std::visit([&]<typename V>(V&& x) {
					if constexpr (std::is_void_v<decltype( do_invoke(std::declval<V>()) )>) {
						do_invoke(std::forward<V>(x));
						return make_result<res_value_t>(ok);
					}
					else
						return make_result<res_value_t>( do_invoke(std::forward<V>(x)) );
				}, std::forward<T>(value));
			}
			else
				return f(std::forward<T>(value));
		}

		// ---------------- matched fn invoke
		template<typename T, std::size_t... Is>
		static constexpr auto find_match_idx(std::index_sequence<Is...>) {
			std::size_t res = chain_length;
			(void)((is_matched<Fi<Is>, T> ? res = Is, false : true) && ...);
			return res;
		}

		template<std::size_t From = 0, bool Simplify = false, typename T>
		constexpr auto invoke_first_match(T&& value) {
			using U = bind_lvalue_ref<T>;
			constexpr auto match_idx = find_match_idx<U>(bounded_index_sequence<From, chain_length>);
			if constexpr (match_idx < chain_length) {
				constexpr auto invoke_matched_fn = []<typename F, typename X>(F&& f, X&& x, serial* self) {
					if constexpr (std::invocable<F>)
						return f();
					else if constexpr (std::invocable<F, X>)
						return f(std::forward<X>(x));
					else {
						auto self_ref = ref_visitor_type<F, true>(*self);
						return f(std::forward<X>(x), self_ref);
					}
				};
				using ret_t = call_result_t<decltype(invoke_matched_fn), Fi<match_idx>, U, serial*>;

				if constexpr (std::is_void_v<ret_t>)
					invoke_matched_fn(std::get<match_idx>(fs_), std::forward<T>(value), this);
				else {
					using value_t = deduce_value_t<ret_t>;
					using res_t = make_result_type<Simplify, value_t>;
					// if matched visitor functor returns `optional` -- enable runtime matches processing branch
					if constexpr (is_optional<ret_t>) {
						// calculate final result type with possible next match invoke
						using next_ret_t = decltype( invoke_first_match<match_idx + 1>(std::declval<T>()) );
						constexpr bool next_match_found = !std::is_same_v<next_ret_t, std::nullopt_t>;
						if constexpr (next_match_found) {
							using next_value_t = deduce_value_t<deduce_result_t<next_ret_t>>;
							using final_res_t = deduce_result_t<make_merged_type<Simplify, value_t, next_value_t>>;

							// invoke current matched functor (pass value by reference to prevent stealing)
							if (auto r = invoke_matched_fn( std::get<match_idx>(fs_), static_cast<U>(value), this ))
								return make_result<final_res_t>(*std::move(r));
							// if it haven't processed the value, invoke next match
							else {
								if constexpr (std::is_void_v<next_ret_t>) {
									invoke_first_match<match_idx + 1>(std::forward<T>(value));
									return make_result<final_res_t>(ok);
								}
								else
									return make_result<final_res_t>( invoke_first_match<match_idx + 1>(std::forward<T>(value)) );
							}
						}
						// current match returned optional, but next match wasn't found
						else
							return make_result<res_t>( invoke_matched_fn(std::get<match_idx>(fs_), std::forward<T>(value), this) );
					}
					// otherwise we have static match - callable returned non-optional result
					else
						return make_result<res_t>( invoke_matched_fn(std::get<match_idx>(fs_), std::forward<T>(value), this) );
				}
			}
			// indicate that no more matches found
			else
				return std::nullopt;
		}
	};

	// Y = source | sink : Y(x) -> z : source.visit(x) -> y -> sink.visit(y) -> z
	template<typename Source, typename Sink>
		requires is_serial_visitor<Source> || is_serial_visitor<Sink>
	constexpr auto operator |(Source&& source, Sink&& sink) {
		return serial{
			[source = serial{std::forward<Source>(source)}, sink = serial{std::forward<Sink>(sink)}]
			<typename T>(T&& value) mutable {
				using value_t = deduce_value_t<decltype( source.visit(std::declval<T>()) )>;
				if constexpr (std::is_void_v<value_t>)
					source.visit(std::forward<T>(value));
				else
					return sink.visit(source.visit(std::forward<T>(value)));
			}
		};
	}

	// Y = source >> sink : Y(x) -> z : source.apply(x) -> y -> sink.apply(y) -> z :
	// x.visit(source) -> y -> y.visit(sink) -> z
	template<typename Source, typename Sink>
		requires is_serial_visitor<Source> || is_serial_visitor<Sink>
	constexpr auto operator >>(Source&& source, Sink&& sink) {
		return serial{
			[source = serial{std::forward<Source>(source)}, sink = serial{std::forward<Sink>(sink)}]
			<typename T>(T&& value) mutable {
				using value_t = deduce_value_t<decltype( source.apply(std::declval<T>()) )>;
				if constexpr (std::is_void_v<value_t>)
					source.apply(std::forward<T>(value));
				else
					return sink.apply(source.apply(std::forward<T>(value)));
			}
		};
	}

} // namespace ddv
