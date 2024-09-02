#include <ddv/serial_visitor.h>

#include <catch2/catch_test_macros.hpp>

namespace {

	enum Match {
		None = 0,
		One = 1,
		Two = 2,
		Three = 4,
		Four = 8,
		Five = 16,
		Six = 32,
		Seven = 64,
		Eight = 128,
		Nine = 256,
		Err1 = 1024,
		Err = 2048,
	};

	struct value1 {};
	struct value2 {};
	struct value3 {};
	struct neutrino {};

} // hidden namespace

TEST_CASE("[serial visitor] core", "[ddv]") {
	auto res = Match::None;
	const auto make_visitor = [&] {
		return ddv::serial{
			[&](int&) { res = Match::One; },
			[&](const int&) { res = Match::Two; return 42; },
			[&](const char*) { res = Match::Three; return 24; },

			[&](value1&) { res = Match::Four; return "Hi"; },
			[&](const value1&) { res = Match::Five; },

			// example of runtime dynamic visitor that can skip values under some conditions
			// skipped value is denoted by empty optional
			[&, i = 0](const value2&) mutable -> std::optional<int> {
				res = Match::Six;
				++i;
				if(i%2 == 1)
					return i;
				return {};
			},
			[&](const value2&) { res = Match::Seven; },

			// both of this lambdas can actually 'return void' to the calling side
			// example of void dynamic visitor
			[&, i = 0](value3) mutable {
				res = Match::Eight;
				++i;
				if(i%2 == 0)
					return ddv::ok;
				else
					return ddv::none;
			},
			[&](value3) { res = Match::Nine; },

			[&] { res = Match::Err; }
		};
	};

	auto v1 = make_visitor();
	// check that cop/move ctors correctly resolved by CTAD
	static_assert(std::is_same_v<decltype(v1), decltype(ddv::serial{v1})>);

	// must hit overload taking unsigned
	auto i = 42;
	static_assert(tp::unit_v<decltype(v1.visit(i))> == tp::unit_v<void>);
	v1.visit(i);
	CHECK(res == Match::One);
	// must hit 2nd overload for const int& (int& cannot bind to temporary)
	auto r = v1.visit(std::optional{i});
	CHECK(res == Match::Two);
	CHECK(*r == 42);
	// must hit 3rd overload for float
	r = v1.visit("Hello world");
	CHECK(res == Match::Three);
	CHECK(*r == 24);

	// must fall to generic error capture
	auto n = neutrino{};
	static_assert(tp::unit_v<decltype(v1.visit(n))> == tp::unit_v<void>);
	v1.visit(n);
	CHECK(res == Match::Err);

	// must hit value1& overload
	auto x =  value1{};
	auto s = v1.visit(x);
	CHECK(res == Match::Four);
	CHECK(*s == std::string_view{"Hi"});
	// here we pass constant value and matching lambda is void
	//v1.visit(std::variant<value1>{});
	v1.visit(static_cast<const value1&>(x));
	CHECK(res == Match::Five);
	// fallback to enclosing capture
	v1.visit(n);
	CHECK(res == Match::Err);

	// test dynamic visitor
	r = v1.visit(std::optional{value2{}});
	CHECK(res == Match::Six);
	CHECK(*r == 1);
	// on next call i == 2 => dynamic visitor should skips value
	r = v1.visit(value2{});
	CHECK(res == Match::Seven);
	CHECK(!r.has_value());

	static_assert(std::is_void_v<decltype( v1.visit(std::declval<value3>()) )>);
	v1.visit(value3{});
	CHECK(res == Match::Nine);
	v1.visit(value3{});
	CHECK(res == Match::Eight);
	v1.visit(value3{});
	CHECK(res == Match::Nine);

	auto v2 = make_visitor();
	v2.visit(neutrino{});
	CHECK(res == Match::Err);
	v2.visit("Hello");
	CHECK(res == Match::Three);
	v2.visit(x);
	CHECK(res == Match::Four);
	const auto x1 = value1{};
	v2.visit(x1);
	CHECK(res == Match::Five);

	auto pipe = ddv::serial{[](int x) { return x; }}
		| [](int x) { return x + 2; }
		| [](int x) { return x*2; };
	auto y = pipe(42);
	CHECK(*y == 88);
}


TEST_CASE("[serial visitor] variant result types", "[ddv]") {
	// test variant result types
	auto var_v = ddv::serial{
		[](int i) -> std::optional<value1> { if(i == 1) return value1{}; else return {}; },
		[](int i) -> std::optional<std::variant<int, double>> { if(i < 3) return 1.0; else return {}; },
		[](int i) -> std::optional<std::variant<int, bool>> { if(i < 4) return true; else return {}; }
	};

	auto var_r = var_v.visit(1);
	CHECK(var_r);
	CHECK(std::holds_alternative<value1>(*var_r));

	var_r = var_v.visit(2);
	CHECK(var_r);
	CHECK(std::holds_alternative<double>(*var_r));

	var_r = var_v.visit(3);
	CHECK(var_r);
	CHECK(std::holds_alternative<bool>(*var_r));

	var_r = var_v.visit(4);
	CHECK(!var_r);

	// visit variant type with static matching serial visitor where callables return different types
	// demonstrates return types merging capabilities of serial visitor
	auto stat_var_v = ddv::serial{
		[](auto x) -> std::enable_if_t<std::is_same_v<decltype(x), bool>, std::int32_t> { return 1; },
		[](auto x) -> std::enable_if_t<std::is_same_v<decltype(x), int>, std::variant<std::int64_t, double>> { return 1.0; },
		[](auto x) -> std::enable_if_t<std::is_same_v<decltype(x), double>, std::variant<std::int64_t, bool>> { return true; }
	};
	using SV = decltype(stat_var_v);
	using V = std::variant<bool, int, double>;
	static_assert(SV::can_visit<V>);
	static_assert(SV::can_visit<bool>);
	static_assert(SV::can_visit<int>);
	static_assert(SV::can_visit<double>);
	static_assert(SV::can_visit<std::variant<bool, int>>);
	static_assert(SV::can_visit<std::variant<int, double>>);
	static_assert(!SV::can_visit<std::variant<bool, int, double, value1>>);

	auto stat_var_r = stat_var_v.visit(V{true});
	static_assert(std::is_same_v<decltype(stat_var_r), std::optional<std::variant<std::int32_t, std::int64_t, double, bool>>>);
	CHECK(stat_var_r);
	CHECK(std::holds_alternative<std::int32_t>(*stat_var_r));
	CHECK(*(stat_var_v | ddv::cast<int>).visit(V{true}) == 1);

	stat_var_r = stat_var_v.visit(V{42});
	CHECK(stat_var_r);
	CHECK(std::holds_alternative<double>(*stat_var_r));
	CHECK(*(stat_var_v | ddv::cast<double>).visit(V{42}) == 1.0);

	stat_var_r = stat_var_v.visit(V{42.});
	CHECK(stat_var_r);
	CHECK(std::holds_alternative<bool>(*stat_var_r));
	CHECK(*(stat_var_v | ddv::cast<bool>).visit(V{42.}) == true);

	// test variant type simplification
	auto simple_var_v = ddv::serial{
		[](int i) { return i == 1 ? ddv::ok : ddv::none; },
		[](int i) { return std::optional<double>(i); }
	};
	// serial visitor call operator doesn't simplify result type
	auto svar_r = simple_var_v(1);
	static_assert(std::is_same_v<decltype(svar_r), std::optional<std::variant<ddv::void_value_t, double>>>);
	CHECK(svar_r);
	CHECK(std::holds_alternative<ddv::void_value_t>(*svar_r));
	// ... and serial::visit() implements simplification
	auto svar_rv = simple_var_v.visit(2);
	static_assert(std::is_same_v<decltype(svar_rv), std::optional<double>>);
	CHECK(svar_rv);
	CHECK(*svar_rv == 2.);
}

TEST_CASE("[serial visitor] strict call policy", "[ddv]") {
	auto v = ddv::serial{
		[](double) { return Match::One; },
		[](float) { return Match::Two; },
		[](long) { return Match::Three; },
		[](int) { return Match::Four; },
		[](short) { return Match::Five; },
		[](unsigned long) { return Match::Six; },
		[](unsigned) { return Match::Seven; },
		[](unsigned short) { return Match::Eight; },
		[](char) { return Match::Nine; },
	};

	CHECK(*v.visit('a') == Match::Nine);
	CHECK(*v.visit((unsigned short)42) == Match::Eight);
	CHECK(*v.visit((unsigned)42) == Match::Seven);
	CHECK(*v.visit((unsigned long)42) == Match::Six);
	CHECK(*v.visit((short)42) == Match::Five);
	CHECK(*v.visit(42) == Match::Four);
	CHECK(*v.visit((long)42) == Match::Three);
	CHECK(*v.visit((float)42) == Match::Two);
	CHECK(*v.visit(42.) == Match::One);
}
