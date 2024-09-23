# Serial visitor
`ddv::serial` class is a general purpose tool for ordered serial matching of the list of callables against argument passed to `serial::operator()` of `serial::visit()`.

It has the following capabilities.
1. Serial visitor is constructed from the list of *any callables* (function pointers, lambdas with or without captures, functor-like class instances, etc) that are stored internally in a tuple.

2. Relation between particular argument `x` and callable `f` falls into one of three categories depending on the invoke result type:
	* *no match*: if statement `f(x)` or `f()` results in compile error meaning that `f` cannot be called with `x`,
	* *dynamic match*: `f(x)` or `f()` returns an instance of `std::optional<T>` where `T` is an arbitrary type,
	* *static match*: `f(x)` or `f()` is `void` or returns an instance *of some other (non-optional)* type `T`.

	Conclusion from the above: if `f` can be called without arguments, it will be a static or dynamic match for *any argument* `x`.

3. When `serial::operator()(x)` or `serial::visit(x)` are invoked, callables are probed *sequentially in the order of their appearance in the visitor initialization list* until a static match is found for `x`. There could be three possible outcomes.
	* *No match*: `x` resulted in no match for all callables in the list. Then, `operator()(x)` will *get disabled* and `visit(x)` will *trigger a static assertion* (compile error).
	* *Single static match*: the first found matched callable `f` is a static match. Then, `f(x)` is called and the result is delivered to the caller. Note that there can be more matches for `x` after `f` in the list, but *they will be unreachable* and will never be examined in the context of visiting `x`.
	* *Multiple matches*: one or more dynamic matches `g_1, ..., g_k` were found for `x` before the static match `f`. Combined, these callables form an *effective match chain* `M` for `x`: `M(x) = [g_1, ..., g_k, f]`. For the single static match case `M(x) = [f]`.

	The effective match chain is always built *at compile time*.

4. In the latter case of multiple matches, visiting `x` works as following.
	1. For each dynamic match `g_i` from `M(x)` a call `r = g_i(x)` is made, where returned instance of `std::optional` is stored in `r`.
	2. If `r` is initialized, it is returned to the caller and chain processing stops.
	3. Otherwise, the next function from the chain is taken: `i = i + 1`, goto step 1.
	4. if a static match `f` is reached, it always finishes the chain processing and the result of `f(x)` is returned as a result.

	The above algorithms walks through the `M(x)` *at runtime*.

5. The value returned from the `serial::operator()(x)` to the caller is either:
	* `void`, if `M(x) = [f]` and `f(x)` is void,
	* an instance of `std::optional<R>` in all other cases, which *can be uninitialized*.

	Let's see how `R` is calculated in different cases.
	* If `M(x) = [f]` and `f(x) -> T`, then `R = T`.
	* If `M(x) = [g_1, ..., g_k, f]` and `g_1(x) -> std::optional<T1>, ..., g_k(x) -> std::optional<Tk>, f(x) -> U`, then `R = std::variant<distinct(non_void(T1, ..., Tk, U))>`.
	Here `distinct()` represents hypothetical compile-time algorithm that removes duplicates from the list of passed types, and `non_void()` removes `void` entries from it.
	* As a specific case of the above, if any `T_i` from the list `T1, ..., Tk, U` is `std::variant<V1, ..., Vn>`, then it is replaced with `V1, ..., Vn` in this list, i.e. variants are replaced with the lists of their alternative types.

	The algorithm above calculates the *merged result type* that can carry the result of invocation of any callable from effective match chain and can be directly initialized like `R(f(x))` for any `f` in `M(x)`.

6. If an argument `x` to be matched is an instance of `std::optional` or `std::variant`, it is automatically unpacked and matching is evaluated against the unpacked value. Unpacking is recursive so it can extract from nested `std::optional<std::variant<...>>` types.