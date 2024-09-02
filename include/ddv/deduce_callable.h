#pragma once

#include <tp/tpack.h>

namespace ddv::util::detail {

	template<typename F, typename = void>
	struct deduce_callable_core {};

	template<typename R, typename... Args>
	struct deduce_callable_core<R(Args...), void> {
		using type = R(Args...);
		using args = tp::tpack<Args...>;
		using result = R;
	};

	template<typename R, typename... Args>
	struct deduce_callable_core<R (*)(Args...), void> {
		using type = R(Args...);
		using args = tp::tpack<Args...>;
		using result = R;
	};

	template<typename C, typename R, typename... Args>
	struct deduce_callable_core<R (C::*)(Args...), void> {
		using type = R(Args...);
		using args = tp::tpack<Args...>;
		using result = R;
	};

	template<typename C, typename R, typename... Args>
	struct deduce_callable_core<R (C::*)(Args...) const, void> {
		using type = R(Args...);
		using args = tp::tpack<Args...>;
		using result = R;
	};

	template<typename F>
	struct deduce_callable_core<F, std::void_t<decltype(&F::operator())>> {
		using deducer = deduce_callable_core<std::remove_reference_t<decltype(&F::operator())>>;
		using type = typename deducer::type;
		using args = typename deducer::args;
		using result = typename deducer::result;
	};

	template<typename F>
	struct deduce_callable : deduce_callable_core<F> {
		using core_t = deduce_callable_core<F>;
		using type = typename core_t::type;
		using args = typename core_t::args;
		using result = typename core_t::result;

		static constexpr auto nargs = size(args{});
		static constexpr auto args_v = args{};

		using nonempty_args = std::conditional_t<(nargs > 0), args, tp::tpack<void>>;
		static constexpr auto nonempty_args_v = nonempty_args{};

		using first_arg = decltype(head(nonempty_args_v))::type;
		using last_arg = decltype(back(nonempty_args_v))::type;
		template<std::size_t i> using ith_arg = decltype(get<i>(args_v))::type;
	};

	template<typename F>
		requires requires { tp::unit_v<typename deduce_callable_core<F>::type>; }
	constexpr auto can_deduce_callable() -> std::true_type;

	template<typename F>
	constexpr auto can_deduce_callable() -> std::false_type;

} // namespace ddv::util::detail

namespace ddv::util {

	template<typename F>
	using deduce_callable = detail::deduce_callable<std::remove_reference_t<F>>;

	template<typename F>
	concept can_deduce_callable = decltype(detail::can_deduce_callable<std::remove_reference_t<F>>())::value;

} // namespace dd::util
