#pragma once

#include <ddv/visitor_common.h>
#include <ddv/deduce_callable.h>

#include <concepts>
#include <tuple>
#include <type_traits>
#include <variant>


namespace ddv {

	template<typename... Fs> class serial;
	// deduction guides - required because `serial` has converting ctor
	template<typename... Fs> serial(Fs&&...) -> serial<Fs...>;

	namespace detail {

		template<typename T>
		using bind_lvalue_ref = std::conditional<
			std::is_lvalue_reference_v<T>, T, std::add_lvalue_reference_t<std::add_const_t<T>>
		>;
		template<typename T>
		using bind_lvalue_ref_t = typename bind_lvalue_ref<T>::type;

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

		template<typename... Args, typename... Params>
			requires ((
				std::same_as<Args, Params>
				|| std::derived_from<std::remove_pointer_t<Args>, std::remove_pointer_t<Params>>
			) && ...)
		constexpr auto args_match_params(tp::tpack<Args...>, tp::tpack<Params...>) -> std::true_type;

		template<typename... Args, typename... Params>
		constexpr auto args_match_params(tp::tpack<Args...>, tp::tpack<Params...>) -> std::false_type;

	} // ddv::detail

	template<typename T>
	concept VariantType = decltype(detail::is_variant(nut_v<T>))::value;

	template<typename T>
	concept SerialVisitorType = decltype(detail::is_serial_visitor(nut_v<T>))::value;

	// checks whether type T can carry target type U where T can be optional/variant
	template<typename T, typename U>
	concept CarryType = detail::carry_type<U>(nut_v<T>);

	template<typename T>
	concept CarryVoid = CarryType<T, void> || CarryType<T, void_value_t>;

	template<typename Args, typename Params>
	concept ArgsMatchParams = decltype(detail::args_match_params(
		tp::transform<std::decay>(Args{}), tp::transform<std::decay>(Params{})
	))::value;

	template<typename F, typename... Args>
	concept StrictCallable = std::invocable<F, Args...>
		&& (!util::can_deduce_callable<F>
			|| ArgsMatchParams<tp::tpack<Args...>, typename util::deduce_callable<F>::args>
		);

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
				if constexpr (CarryVoid<tgt_value_t>)
					return res_t{src};
				else
					return res_t{std::nullopt};
			}
			else if constexpr (OptionalType<T>) {
				if (src)
					return make_result<res_t>(*std::forward<T>(src));
				else
					return res_t{std::nullopt};
			}
			// if `src` is variant - unpack value and convert to result type
			else if constexpr (VariantType<src_value_t> && !std::is_same_v<src_value_t, tgt_value_t>)
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
		template<typename F, bool Complete, size_t Pos>
		static consteval auto make_ref_visitor_type() {
			if constexpr (util::can_deduce_callable<F>) {
				using Finfo = util::deduce_callable<F>;
				if constexpr (Pos < Finfo::nargs) {
					using ref_arg = typename Finfo::template ith_arg<Pos>;
					if constexpr (Complete)
						return tp::unit_v<visitor<std::remove_cvref_t<ref_arg>, ref>>;
					else if constexpr (MuxType<ref_arg>) {
						static_assert(
							std::is_lvalue_reference_v<ref_arg>,
							"Recursion: last param must be an lvalue reference to the visitor interface type (ddv::mux)"
						);
						return tp::unit_v<ref_arg>;
					}
					else return void_value;
				}
				else return void_value;
			}
			else return void_value;
		}

		template<typename F, bool Complete, size_t Pos>
		using ref_visitor_type = typename decltype(make_ref_visitor_type<F, Complete, Pos>())::type;

		// calc final decision whether F matches (can be called with) the value of type T being visited
		template<typename F, typename... Ts>
		static constexpr bool is_matched = std::invocable<F>
			|| StrictCallable<F, Ts...>
			|| StrictCallable<F, Ts..., ref_visitor_type<F, false, sizeof...(Ts)>>;

		template<typename T, typename... Args, typename... Gs>
		static consteval bool can_visit_impl(tp::unit<T>, tp::tpack<Args...> args, tp::unit<std::tuple<Gs...>> gs) {
			if constexpr (OptionalType<T>)
				return can_visit_impl(tp::unit_v<decltype(*std::declval<T>())>, args, gs);
			else if constexpr (VariantType<T>)
				return can_visit_impl<Gs...>(nut_v<T>, args);
			else
				// true if T can be visited by at least one callable
				return VoidType<T> || (is_matched<Gs, detail::bind_lvalue_ref_t<T>, detail::bind_lvalue_ref_t<Args>...> || ...);
		}

		template<typename... Gs, typename... Ts>
		static consteval bool can_visit_impl(tp::unit<std::variant<Ts...>>, auto args) {
			// returns true only if can visit each alternative of variant type T
			return (can_visit_impl(tp::unit_v<Ts>, args, tp::unit_v<std::tuple<Gs...>>) && ...);
		}

		storage_t fs_;

	public:
		// tests if value of type `F` can be visited by this serial visitor (there is at least one matching callable)
		template<typename T, typename... Args>
		static constexpr bool can_visit = can_visit_impl(tp::unit_v<T>, tp::tpack_v<Args...>, tp::unit_v<storage_t>);

		// converting ctor to perfectly forward callables into internal storage
		template<typename... Gs>
			requires std::constructible_from<storage_t, Gs...>
		constexpr serial(Gs&&... gs) : fs_(std::forward<Gs>(gs)...) {}

		// call operator is only enabled if passed value can be visited
		// supports optionals and variants auto-unpacking
		// non-simplified return type to pass all possible results, including `ok` and `none`
		template<typename T, typename... Args>
			requires can_visit<T, Args...>
		constexpr auto operator()(T&& value, Args&&... args) {
			return do_visit<false>(std::forward<T>(value), std::forward<Args>(args)...);
		}

		// public interface to `operator()` above to be used by humans
		// always enabled, produces readable error if value can't be visited, don't return void values
		// auto simplifies result type `optional<variant<void_value_t, T>>` -> `optional<T>`
		// downside: cannot distinguish between `ok` and `none` in returned value, always will be `none`
		template<typename T, typename... Args>
		constexpr auto visit(T&& value, Args&&... args) {
			static_assert(
				can_visit<T, Args...>,
				"There is no callable accepting given value. "
				"You can append `noop` to the chain of callables to provide default fallback."
			);
			// pass Simplify = true flag that strips `void_value_t` from result type
			// like `optional<variant<void_value_t, T>>` -> `optional<T>`
			using res_t = decltype( do_visit<true>(std::declval<T>(), std::declval<Args>()...) );
			if constexpr (VoidType<res_t>)
				do_visit<true>(std::forward<T>(value), std::forward<Args>(args)...);
			else
				return do_visit<true>(std::forward<T>(value), std::forward<Args>(args)...);
		}

		// effectively calls `value.visit(*this)`
		template<typename T>
		constexpr auto apply(T&& value) {
			return unpack_and_invoke<false, true>(
				[this](auto&& x) { return x.visit(*this); },
				std::forward<T>(value)
			);
		}

	private:
		template<bool Simplify, typename T, typename... Args>
		constexpr auto do_visit(T&& value, Args&&... args) {
			return unpack_and_invoke<Simplify, false>(
				[&]<typename U>(U&& x) {
					return invoke_first_match<0, Simplify>(std::forward<U>(x), std::forward<Args>(args)...);
				},
				std::forward<T>(value)
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
				else if constexpr (VariantType<L>) {
					if constexpr (VariantType<R>)
						return merge_variant(tp::unit_v<L>, tp::unit_v<R>);
					else
						return merge_variant<R>(tp::unit_v<L>);
				}
				else if constexpr (VariantType<R>)
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
		template<bool Simplify = false, bool DerefPtrs, typename F, typename T>
		static constexpr auto unpack_and_invoke(F&& f, T&& value) {
			if constexpr (VoidType<T>)
				return;
			else if constexpr (OptionalType<T> || (DerefPtrs && PointerLikeType<T>)) {
				using res_t = decltype(unpack_and_invoke<false, DerefPtrs>(std::declval<F>(), *std::declval<T>()));
				if (value)
					return unpack_and_invoke<false, DerefPtrs>(std::forward<F>(f), *std::forward<T>(value));
				else
					return make_result<res_t>(none);
			}
			else if constexpr (VariantType<T>) {
				const auto do_invoke = [&f]<typename X>(X&& x) {
					return unpack_and_invoke<false, DerefPtrs>(std::forward<F>(f), std::forward<X>(x));
				};
				// calculate type to return by visiting every value alternative
				using res_value_t = decltype( calc_variant_response<Simplify, decltype(do_invoke)>(nut_v<T>) )::type;

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
		template<typename... Ts, std::size_t... Is>
		static constexpr auto find_match_idx(tp::tpack<Ts...>, std::index_sequence<Is...>) {
			std::size_t res = chain_length;
			(void)((is_matched<Fi<Is>, Ts...> ? res = Is, false : true) && ...);
			return res;
		}

		template<std::size_t From = 0, bool Simplify = false, typename... Ts>
		constexpr auto invoke_first_match(Ts&&... values) {
			constexpr auto Us = tp::transform<detail::bind_lvalue_ref>(tp::tpack_v<Ts...>);
			constexpr auto match_idx = find_match_idx(Us, bounded_index_sequence<From, chain_length>);
			if constexpr (match_idx < chain_length) {
				constexpr auto invoke_matched_fn = []<typename F, typename... Xs>(serial* self, F&& f, Xs&&... xs) {
					if constexpr (std::invocable<F>)
						return f();
					else if constexpr (std::invocable<F, Xs...>)
						return f(std::forward<Xs>(xs)...);
					else {
						auto self_ref = ref_visitor_type<F, true, sizeof...(Xs)>(*self);
						return f(std::forward<Xs>(xs)..., self_ref);
					}
				};
				using ret_t = call_result_t<decltype(invoke_matched_fn), serial*, Fi<match_idx>, detail::bind_lvalue_ref_t<Ts>...>;

				if constexpr (std::is_void_v<ret_t>)
					invoke_matched_fn(this, std::get<match_idx>(fs_), std::forward<Ts>(values)...);
				else {
					using value_t = deduce_value_t<ret_t>;
					using res_t = make_result_type<Simplify, value_t>;
					// if matched visitor functor returns `optional` -- enable runtime matches processing branch
					if constexpr (OptionalType<ret_t>) {
						// calculate final result type with possible next match invoke
						using next_ret_t = decltype(invoke_first_match<match_idx + 1>(std::declval<Ts>()...));
						constexpr bool next_match_found = !std::is_same_v<next_ret_t, std::nullopt_t>;
						if constexpr (next_match_found) {
							using next_value_t = deduce_value_t<deduce_result_t<next_ret_t>>;
							using final_res_t = deduce_result_t<make_merged_type<Simplify, value_t, next_value_t>>;

							// invoke current matched functor (pass value by reference to prevent stealing)
							if (auto r = invoke_matched_fn(this, std::get<match_idx>(fs_), static_cast<detail::bind_lvalue_ref_t<Ts>>(values)...))
								return make_result<final_res_t>(*std::move(r));
							// if it haven't processed the value, invoke next match
							else {
								if constexpr (std::is_void_v<next_ret_t>) {
									invoke_first_match<match_idx + 1>(std::forward<Ts>(values)...);
									return make_result<final_res_t>(ok);
								}
								else
									return make_result<final_res_t>(invoke_first_match<match_idx + 1>(std::forward<Ts>(values)...));
							}
						}
						// current match returned optional, but next match wasn't found
						else
							return make_result<res_t>(invoke_matched_fn(this, std::get<match_idx>(fs_), std::forward<Ts>(values)...));
					}
					// otherwise we have static match - callable returned non-optional result
					else
						return make_result<res_t>(invoke_matched_fn(this, std::get<match_idx>(fs_), std::forward<Ts>(values)...));
				}
			}
			// indicate that no more matches found
			else
				return std::nullopt;
		}
	};

	// Pipe operator with extra args applied only to source
	// Y = source | sink : Y(x) -> z : source.visit(x) -> y -> sink.visit(y) -> z
	// Y = source | sink : Y(x, args...) -> z : source.visit(x, args...) -> y -> sink.visit(y) -> z
	template<typename Source, typename Sink>
		requires SerialVisitorType<Source> || SerialVisitorType<Sink>
	constexpr auto operator |(Source&& source, Sink&& sink) {
		return serial{
			[source = serial{std::forward<Source>(source)}, sink = serial{std::forward<Sink>(sink)}]
			<typename T, typename... Ts>(T&& value, Ts&&... args) mutable {
				using value_t = deduce_value_t<decltype( source.visit(std::declval<T>(), std::declval<Ts>()...) )>;
				if constexpr (std::is_void_v<value_t>)
					source.visit(std::forward<T>(value), std::forward<Ts>(args)...);
				else
					return sink.visit(source.visit(std::forward<T>(value), std::forward<Ts>(args)...));
			}
		};
	}

	// Pipe operator with extra args applied only to both source AND sink
	// Y = source | sink : Y(x) -> z : source.visit(x) -> y -> sink.visit(y) -> z
	// Y = source | sink : Y(x, args...) -> z : source.visit(x, args...) -> y -> sink.visit(y, args...) -> z
	template<typename Source, typename Sink>
		requires SerialVisitorType<Source> || SerialVisitorType<Sink>
	constexpr auto operator ||(Source&& source, Sink&& sink) {
		return serial{
			[source = serial{std::forward<Source>(source)}, sink = serial{std::forward<Sink>(sink)}]
			<typename T, typename... Ts>(T&& value, Ts&&... args) mutable {
				using value_t = deduce_value_t<decltype(
					source.visit(std::declval<T>(), std::declval<detail::bind_lvalue_ref_t<Ts>>()...)
				)>;
				if constexpr (std::is_void_v<value_t>)
					source.visit(std::forward<T>(value), static_cast<detail::bind_lvalue_ref_t<Ts>>(args)...);
				else
					return sink.visit(
						source.visit(std::forward<T>(value), static_cast<detail::bind_lvalue_ref_t<Ts>>(args)...),
						std::forward<Ts>(args)...
					);
			}
		};
	}

	// Y = source >> sink : Y(x) -> z : source.apply(x) -> y -> sink.apply(y) -> z :
	// x.visit(source) -> y -> y.visit(sink) -> z
	template<typename Source, typename Sink>
		requires SerialVisitorType<Source> || SerialVisitorType<Sink>
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

	/////////////////////////////////////////////////////////////////////////
	// make visitor with given multiplexer interface and `serial` demultiplexer
	template<typename Mux, typename... Fs>
	constexpr auto make_serial_visitor(Fs&&... fs) noexcept {
		using Serial = decltype(serial{std::declval<Fs>()...});
		return visitor<Mux, Serial>(std::forward<Fs>(fs)...);
	}

} // namespace ddv
