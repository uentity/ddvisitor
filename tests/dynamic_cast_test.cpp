/*
 * Benchmark comparing performance of `dynamic_cast` and ddvisitor
 *
 * Based on program listing for blog post "Performance comparison of three different
 * implementations of dynamic_cast" [1]
 *
 * [1]: https://blog.michael.franzl.name/2021/03/21/performance-comparison-of-three-different-implementations-of-dynamic_cast/
 *
*/

#include <cstdint>
#include <iostream>
#include <chrono>
#include <iterator>
#include <tp/tpack.h>
#include <vector>
#include <memory>
#include <cstdlib>
#include <string>
#include <functional>
#include <random>
#include <algorithm>

#include <ddv/visitable.h>

#include <catch2/catch_test_macros.hpp>

#define NAME_SUPPORT(cls_name) \
virtual auto name() const -> std::string_view override { return #cls_name; }

uint64_t max_num_ops = 0;

inline constexpr uint64_t n = 2'000'000; // number of iterations
inline constexpr uint64_t num_usecs_per_sec = 1'000'000;


namespace deep {
	struct A; struct B; struct C; struct D; struct E; struct F; struct G; struct H;
	using Mux = ddv::mux<A, B, C, D, E, F, G, H>;
	struct A : ddv::visitable<A, Mux> {
		VISITOR_SUPPORT();
		virtual auto name() const -> std::string_view { return "deep::A"; }
	};

	struct B : A { VISITOR_SUPPORT() NAME_SUPPORT(deep::B) };
	struct C : B { VISITOR_SUPPORT() NAME_SUPPORT(deep::C) };
	struct D : C { VISITOR_SUPPORT() NAME_SUPPORT(deep::D) };
	struct E : D { VISITOR_SUPPORT() NAME_SUPPORT(deep::E) };
	struct F : E { VISITOR_SUPPORT() NAME_SUPPORT(deep::F) };
	struct G : F { VISITOR_SUPPORT() NAME_SUPPORT(deep::G) };
	struct H : G { VISITOR_SUPPORT() NAME_SUPPORT(deep::H) };
}

namespace shallow {
	struct A; struct B; struct C; struct D; struct E; struct F; struct G; struct H;
	using Mux = ddv::mux<A, B, C, D, E, F, G, H>;
	struct A : ddv::visitable<A, Mux> {
		VISITOR_SUPPORT();
		virtual auto name() const -> std::string_view { return "shallow::A"; }
	};

	struct B : A { VISITOR_SUPPORT() NAME_SUPPORT(shallow::B) };
	struct C : A { VISITOR_SUPPORT() NAME_SUPPORT(shallow::C) };
	struct D : A { VISITOR_SUPPORT() NAME_SUPPORT(shallow::D) };
	struct E : A { VISITOR_SUPPORT() NAME_SUPPORT(shallow::E) };
	struct F : A { VISITOR_SUPPORT() NAME_SUPPORT(shallow::F) };
	struct G : A { VISITOR_SUPPORT() NAME_SUPPORT(shallow::G) };
	struct H : A { VISITOR_SUPPORT() NAME_SUPPORT(shallow::H) };
}

namespace balanced {
	struct A; struct B; struct C; struct D; struct E; struct F; struct G; struct H;
	using Mux = ddv::mux<A, B, C, D, E, F, G, H>;
	struct A : ddv::visitable<A, Mux> {
		VISITOR_SUPPORT();
		virtual auto name() const -> std::string_view { return "balanced::A"; }
	};

	struct B : A { VISITOR_SUPPORT() NAME_SUPPORT(balanced::B) };
	struct C : B { VISITOR_SUPPORT() NAME_SUPPORT(balanced::C) };
	struct D : B { VISITOR_SUPPORT() NAME_SUPPORT(balanced::D) };

	struct E : A { VISITOR_SUPPORT() NAME_SUPPORT(balanced::E) };
	struct F : E { VISITOR_SUPPORT() NAME_SUPPORT(balanced::F) };
	struct G : E { VISITOR_SUPPORT() NAME_SUPPORT(balanced::G) };
	struct H : E { VISITOR_SUPPORT() NAME_SUPPORT(balanced::H) };
}

// Same interface as A, but not related.
struct Z {
	void accept() {};
	std::string_view name() const { return "Z"; }
};

void draw_bar(float percent, std::string s = "-") {
	const auto cols = 60;
	uint64_t width = cols * percent * 4.0;
	if (width > cols) {
		width = cols;
		printf("|");
		while (width--) std::cout << s;
		printf("...\n");
	} else {
		printf("|");
		while (width--) std::cout << s;
		printf("|\n");
	}
}

uint64_t run(std::string_view label, std::function<uint64_t()> benchmark) {
	auto t1 = std::chrono::high_resolution_clock::now();
	auto successes = benchmark();
	auto t2 = std::chrono::high_resolution_clock::now();

	auto usecs_per_iterations = std::chrono::duration_cast<std::chrono::microseconds>(t2-t1).count();
	auto num_ops = num_usecs_per_sec / (float(usecs_per_iterations) / n);
	if (max_num_ops == 0) max_num_ops = num_ops; // the first run will be 100%
	auto percent = float(num_ops) / float(max_num_ops);
	printf(
		"%11s: %5.1f MHz (%3.0f%%) [%7lu] ",
		std::string(label).c_str(),
		num_ops / num_usecs_per_sec,
		percent * 100, successes
	);
	draw_bar(percent);
	return num_ops;
}

template<typename A>
auto generate_data(unsigned int from = 0, unsigned int width = 7) {
	using types = typename A::mux_type::types;
	static const auto ts = [] {
		auto res = std::array<std::unique_ptr<A>, tp::size(types{})>{};
		tp::for_each(types{}, [&res, i = uint64_t{}]<typename T>(tp::unit<T>) mutable {
			res[i++] = std::make_unique<T>();
		});
		return res;
	}();

	auto v = std::vector<A*>(n); // ensure contiguous memory
	for(auto& vi : v) {
		uint64_t val = from + rand() % (width + 1);
		vi = ts[val].get();
	}
	return v;
}

template<typename V>
void shuffle(V& v) {
	auto rng = std::default_random_engine{};
	std::shuffle(std::begin(v), std::end(v), rng);
}

void print_average(float num, float denom = 9.0) {
	auto avg = num / denom;
	printf("------------\n");
	printf("AVG: %5.1f MHz                  ", avg / num_usecs_per_sec);
	draw_bar(avg / max_num_ops, "=");
}

float dummy = 0;

template<typename A>
void run_benchmarks(std::vector<A*>& v) {
	float sum = 0;

	// Cache warming
	dummy += [&v] { uint64_t s = 0; for (auto& e: v) { auto *p = static_cast<A*>(e); p ? ++s : ++dummy; } return s; }();
	dummy += [&v] { uint64_t s = 0; for (auto& e: v) { auto *p = static_cast<A*>(e); p ? ++s : ++dummy; } return s; }();

	printf("Base-line: static_cast\n");
	printf("```\n");
	dummy += run("-", [&v] { uint64_t s = 0; for (auto& e: v) { auto *p = static_cast<A*>(e); p ? ++s : ++dummy; } return s; });
	printf("```\n\n");

	constexpr auto types_v = typename A::mux_type::types{} + tp::unit_v<Z>;

	printf("Implementation: `dynamic_cast`\n");
	printf("```\n");
	sum = 0;
	tp::for_each(types_v, [&]<typename T>(tp::unit<T>) {
		auto res = run(T{}.name(), [&v] {
			uint64_t s = 0;
			for (auto& e: v) {
				auto* p = dynamic_cast<T*>(e);
				p ? ++s : ++dummy;
			}
			return s;
		});
		// for dynamic_cast exclude casts A -> A since they are instant
		if constexpr (!std::is_same_v<T, A>)
			sum += res;
	});
	print_average(sum, 8.0);
	printf("```\n");

	printf("Base-line: single virtual dispatch\n");
	printf("```\n");
	dummy += run("-v", [&v] { uint64_t s = 0; for (auto& e: v) { auto p = e->name(); p.size() ? ++s : ++dummy; } return s; });
	printf("```\n\n");

	printf("Implementation: `DDV fast`\n");
	printf("```\n");
	sum = 0;
	tp::for_each(types_v, [&]<typename T>(tp::unit<T>) {
		auto res = run(T{}.name(), [&v] {
			auto s = 0;
			auto vtor = ddv::make_serial_visitor<typename A::mux_type>(
				[&](T*) { ++s; },
				[] { ++dummy; }
			);
			for (auto& e: v) {
				e->accept(vtor);
			}
			return s;
		});
		sum += res;
	});
	print_average(sum);
	printf("```\n");

	printf("Implementation: `DDV std`\n");
	printf("```\n");
	sum = 0;
	tp::for_each(types_v, [&]<typename T>(tp::unit<T>) {
		auto res = run(T{}.name(), [&v] {
			auto s = 0;
			auto vtor = ddv::serial{
				[&](T*) { ++s; },
				[] { ++dummy; }
			};
			for (auto& e: v) {
				e->visit(vtor);
			}
			return s;
		});
		sum += res;
	});
	print_average(sum);
	printf("```\n");

	printf("Implementation: `DDV type filter`\n");
	printf("```\n");
	sum = 0;
	tp::for_each(types_v, [&]<typename T>(tp::unit<T>) {
		auto res = run(T{}.name(), [&v] {
			auto s = 0;
			auto vtor = ddv::serial{
				A::template make_visitor<T>([&] { ++s; }),
				[] { ++dummy; }
			};
			for (auto& e: v) {
				e->visit(vtor);
			}
			return s;
		});
		sum += res;
	});
	print_average(sum);
	printf("```\n");
}

TEST_CASE("[ddv] dynamic_cast benchmark") {
	auto vec_deep_successful = generate_data<deep::A>(6, 0);
	auto vec_deep_fails =      generate_data<deep::A>(1, 0);
	auto vec_deep_mixed =      generate_data<deep::A>(0, 7);

	auto vec_shallow_successful = generate_data<shallow::A>(6, 0);
	auto vec_shallow_fails =      generate_data<shallow::A>(1, 0);
	auto vec_shallow_mixed =      generate_data<shallow::A>(0, 7);

	auto vec_balanced_mixed = generate_data<balanced::A>(0, 7);

	// Run the benchmark loop 3 times:
	// 1st: Warming up, discard.
	// 2nd: Objects are ordered in memory
	// 3rd: Objects are shuffled in memory
	for (unsigned int i = 0; i < 3; i++) {
		max_num_ops = 0;

		printf("\n\n\n\n\n");

		switch(i) {
			case 0:
				printf("## Run 0 (discard)\n\n");
				break;
			case 1:
				printf("## Run 1 (objects aligned)\n\n");
				break;
			case 2:
				printf("## Run 2 (objects shuffled)\n\n");

				shuffle(vec_deep_successful);
				shuffle(vec_deep_fails);
				shuffle(vec_deep_mixed);
				shuffle(vec_shallow_successful);
				shuffle(vec_shallow_fails);
				shuffle(vec_shallow_mixed);
				shuffle(vec_balanced_mixed);
		}

		printf("### Class hierarchy: deep\n\n");

		printf("#### Cast type: Mostly successful (cast from class G)\n\n");
		run_benchmarks(vec_deep_successful);

		printf("#### Cast type: Mostly failed (cast from class B)\n\n");
		run_benchmarks(vec_deep_fails);

		printf("#### Cast type: Mixed (cast from random classes)\n\n");
		run_benchmarks(vec_deep_mixed);


		printf("\n\n\n\n\n");
		printf("### Class hierarchy: shallow\n\n");

		printf("#### Cast type: Mostly successful (cast from class G)\n\n");
		run_benchmarks(vec_shallow_successful);

		printf("#### Cast type: Mostly failed (cast from class B)\n\n");
		run_benchmarks(vec_shallow_fails);

		printf("#### Cast type: Mixed (cast from random classes)\n\n");
		run_benchmarks(vec_shallow_mixed);


		printf("\n\n\n\n\n");
		printf("### Class hierarchy: balanced\n\n");

		printf("#### Cast type: Mixed (cast from random classes)\n\n");
		run_benchmarks(vec_balanced_mixed);
	}

	printf("\n\n\n\n\n");
	//std::cout << "sizeof JustKclRtti: " << sizeof(JustKclRtti) << "\n";
	//std::cout << "sizeof A: " << sizeof(A) << "\n";
	//printf("%f", dummy);
}