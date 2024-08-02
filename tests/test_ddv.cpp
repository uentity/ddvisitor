#include <ddv/visitable.h>

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

	struct neutrino {};

	// non-virtual inheritance
	struct Base;
	struct Derived;
	struct GrandBase;
	struct GrandDerivedL;
	struct GrandDerivedR;

	using BaseVisitor = ddv::mux<Base, Derived, GrandBase, GrandDerivedL, GrandDerivedR>;

	struct Base : ddv::visitable<Base, BaseVisitor> {};

	struct Derived : Base {
		VISITOR_SUPPORT()
	};
	struct GrandBase : Derived {
		VISITOR_SUPPORT()
	};
	struct GrandDerivedL final : GrandBase {
		VISITOR_SUPPORT()
	};
	struct GrandDerivedR final : GrandBase {
		VISITOR_SUPPORT()
	};

	template<typename T>
	struct GrandT final : GrandBase {
		VISITOR_SUPPORT()
	};

	auto gt_int64 = GrandT<std::int64_t>{};
	const auto& gti_cref = gt_int64;
	auto gt_string = GrandT<std::string>{};
	auto gt_double = GrandT<double>{};
	auto gt_bool =   GrandT<bool>{};
	auto gt_time =   GrandT<neutrino>{};

	// virtual inheritance
	struct VBase;
	struct VDerived;
	using VTestMux = ddv::mux<VBase, VDerived>;

	struct VBase : ddv::visitable<Base, VTestMux> {};

	struct VDerived final : virtual VBase {
		VISITOR_SUPPORT()
	};

	template<typename T>
	struct VDerivedT final : virtual VBase {
		VISITOR_SUPPORT()
	};

} // hidden namespace

TEST_CASE("[visitable] core", "[ddv]") {
	static_assert(ddv::is_virtual_base_of<VBase, VDerived>);
	static_assert(!ddv::is_virtual_base_of<Derived, GrandBase>);

	// test `visitable`
	auto isv1 = Base::make_visitor<GrandT, std::int64_t, std::string>(
		[](auto) { return Match::One; }
	);
	static_assert(ddv::is_serial_visitor<decltype(isv1)>);
	auto isv2 = Base::make_visitor<tp::tpack<GrandT<std::int64_t>, GrandT<std::string>>>(
		[](auto) { return Match::One; }
	);
	static_assert(ddv::is_serial_visitor<decltype(isv2)>);
	auto isv3 = Base::make_visitor<GrandT, tp::tpack<std::int64_t, std::string>>(
		[](auto) { return Match::One; }
	);
	static_assert(ddv::is_serial_visitor<decltype(isv3)>);

	auto dv = Base::make_visitor<GrandT, double>(
		[](auto) { return Match::Two; }
	);
	using dv_t = decltype(dv);
	static_assert(!ddv::is_serial_visitor<dv_t>);

	CHECK(dv(&gt_double) == Match::Two);
	static_assert(std::is_invocable_v<dv_t, Base*>);
	static_assert(!std::is_invocable_v<dv_t>);
	static_assert(!std::is_invocable_v<dv_t, int>);
	static_assert(!std::is_invocable_v<dv_t, Base*, int>);
	static_assert(!std::is_invocable_v<dv_t, Base*, decltype(isv3)>);

	auto bv = Base::make_visitor([](GrandT<bool>*) { return Match::Three; });
	static_assert(!ddv::is_serial_visitor<decltype(bv)>);

	const auto check_gt_visitor = [&](auto gt_visitor) {
		CHECK(*gt_int64.visit(gt_visitor) == Match::One);
		CHECK(*gt_string.visit(gt_visitor) == Match::One);
		CHECK(*gt_double.visit(gt_visitor) == Match::Two);
		CHECK(*gt_bool.visit(gt_visitor) == Match::Three);
		CHECK(*gt_time.visit(gt_visitor) == Match::Four);
	};

	check_gt_visitor(ddv::serial{
		isv1, dv, bv,
		[] { return Match::Four; }
	});
	check_gt_visitor(ddv::serial{
		isv2, dv, bv,
		[] { return Match::Four; }
	});
	check_gt_visitor(ddv::serial{
		isv3, dv, bv,
		[] { return Match::Four; }
	});

	// this must compile without additional closures like `noop`
	gt_bool.visit([](Base*) {});
}

TEST_CASE("[visitable] void callables with dynamic matching") {
	auto res = Match::None;
	auto one_shot = ddv::serial{[&, flag = true]() mutable {
		if (flag) {
			res = Match::One;
			flag = false;
			return ddv::ok;
		}
		return ddv::none;
	}};
	auto vvv = ddv::serial{
		Base::make_visitor<GrandT, bool, std::int64_t>(
			// share `one_shot` between `bool` & `int64_t` type filters
			std::ref(one_shot)
		),
		Base::make_visitor<GrandT, bool>(
			[&] { res = Match::Two; }
		),
		[&] { res = Match::Err; },
		ddv::noop
	};

	// [NOTE] `visitable::visit()` will make internal copy (or move value) of passed visitor
	// by using `std::ref()` we refer to same visitor instance
	auto rvvv = std::ref(vvv);
	// fall through
	gt_string.visit(rvvv);
	CHECK(res == Match::Err);
	// fall through: non-const visitor do not match pointer to const
	gti_cref.visit(rvvv);
	CHECK(res == Match::Err);
	// one_shot first match with flag
	gt_bool.visit(rvvv);
	CHECK(res == Match::One);
	// fall through: shared one_shot is called again, but flag is false
	gt_int64.visit(rvvv);
	CHECK(res == Match::Err);
	// one_shot flag tripped -> 2nd callable triggers in vvv
	gt_bool.visit(rvvv);
	CHECK(res == Match::Two);
}

TEST_CASE("[visitable] const types support") {
	// test const types visitor
	auto const_vvv = ddv::serial{
		Base::make_visitor<GrandT, bool, std::int64_t>([&] {
			return Match::One;
		}, ddv::const_),
		[&] { return Match::Err; }
	};

	CHECK(*gt_bool.visit(const_vvv) == Match::One);
	CHECK(*gti_cref.visit(const_vvv) == Match::One);
	CHECK(*gt_string.visit(const_vvv) == Match::Err);
}

TEST_CASE("[visitable] recursive visitor") {
	// test visitor with self reference
	auto res = Match::None;
	auto self_ref_v = ddv::serial{
		Base::make_visitor([&](GrandT<std::int64_t>*) {
			res = Match::One;
		}),
		Base::make_visitor([&](GrandT<std::string>*) {
			res = Match::Two;
		}),
		// specify type of the first argument as pointer 'most common' parent class
		// when type to filter is provided as template param and self-reference visitor is required
		// [NOTE] filter for multiple types + self-reference visitor *will not work* atm
		Base::make_visitor<GrandT, double>([&](Base*, BaseVisitor& self) {
			// forward to builtin int64_t scalar handler
			gt_int64.accept(self);
			// alternatively:
			//self.visit(&gt_int64);
		}),
		Base::make_visitor<GrandT, bool>([&](Base*, BaseVisitor& self) {
			// forward to builtin int64_t scalar handler
			gt_int64.accept(self);
		}),
		[&](const GrandDerivedL*, BaseVisitor& self) {
			// forward to int64_t handler
			self.visit(&gt_int64);
		},
		[&](const GrandDerivedR*, BaseVisitor& self) {
			// forward to std::string handler
			self.visit(&gt_string);
			return 42;
		},
		[&](const GrandBase*, BaseVisitor& self) {
			// forward to double handler, which forwards to int64 handler
			self.visit(&gt_double);
		},
		[&](const Derived*, BaseVisitor& self) {
			// forward to bool handler, which forwards to int64 handler
			self.visit(&gt_bool);
			return 42.42;
		},
		ddv::noop
	};

	gt_int64.visit(self_ref_v);
	CHECK(res == Match::One);

	res = Match::None;
	self_ref_v.visit(&gt_int64);
	CHECK(res == Match::One);

	res = Match::None;
	self_ref_v.visit(&gt_string);
	CHECK(res == Match::Two);
	// double -> int64
	res = Match::None;
	self_ref_v.visit(&gt_double);
	CHECK(res == Match::One);
	// bool -> int64
	res = Match::None;
	self_ref_v.visit(&gt_bool);
	CHECK(res == Match::One);

	// Derived -> bool -> int64
	res = Match::None;
	CHECK(Derived{}.visit(self_ref_v | ddv::cast<double>) == 42.42);
	CHECK(res == Match::One);
	// GrandBase -> double -> int64
	res = Match::None;
	CHECK(!GrandBase{}.visit(self_ref_v).has_value());
	CHECK(res == Match::One);
	// GrandDerivedR -> string
	res = Match::None;
	CHECK(GrandDerivedR{}.visit(self_ref_v | ddv::cast<int>) == 42);
	CHECK(res == Match::Two);
	// GrandDerivedL -> int64
	res = Match::None;
	CHECK(!GrandDerivedL{}.visit(self_ref_v).has_value());
	CHECK(res == Match::One);
}