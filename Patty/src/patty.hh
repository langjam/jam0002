#pragma once

#include <algorithm>
#include <cassert>
#include <filesystem>
#include <functional>
#include <list>
#include <ranges>
#include <string>

#include <fmt/format.h>
#include <fmt/ostream.h>

#define R(Range) std::begin(Range), std::end(Range)
#define CR(Range) std::cbegin(Range), std::cend(Range)

namespace fs = std::filesystem;
using namespace std::string_view_literals;
using namespace fmt::literals;

struct Value;
struct Context;

struct Sequence;
struct Dynamic_Generator;
struct Circular_Generator;
struct Composed_Generator;
struct Value_Sequence;

extern fs::path program_name;
extern fs::path filename;

inline void error(auto const& message)
{
	fmt::print(stderr, "{}: error: {}\n", program_name.c_str(), message);
}

[[noreturn]]
inline void error_fatal(auto const& ...message)
{
	error(message...);
	std::exit(1);
}

Value eval(Context &ctx, Value value);
void print(Value const& value);
Value read(std::string_view &source);
void intrinsics(Context &ctx);

struct Sequence
{
	static Value take(Sequence &seq, Context &ctx, unsigned n);

	virtual Value index(Context &ctx, unsigned n) = 0;
	virtual Value take(Context &ctx, unsigned n) = 0;
	virtual Value len(Context &ctx) = 0;
	virtual Value pop(Context &ctx, unsigned n) = 0;
};

struct Value
{
	enum class Type
	{
		Nil,
		String,
		Symbol,
		Int,
		List,
		Cpp_Function,
		Sequence
	} type = Type::Nil;

	std::string sval = {};
	std::int64_t ival = 0;
	std::list<Value> list = {};
	std::function<Value(struct Context&, Value)> cpp_function = nullptr;
	std::shared_ptr<Sequence> sequence = nullptr;

	static inline Value nil() { return {}; }
	static inline Value string(std::string_view src) { return Value { Type::String, std::string(src.data(), src.size()) }; }
	static inline Value symbol(std::string_view src) { return Value { Type::Symbol, std::string(src.data(), src.size()) }; }
	static inline Value cpp(char const *name, std::function<Value(struct Context&, Value)> &&function) { auto v = Value { Type::Cpp_Function }; v.cpp_function = std::move(function); v.sval = name; return v; }
	static inline Value integer(int64_t ival) { auto v = Value { Type::Int }; v.ival = ival; return v; }

	Value& at(unsigned index) &;
	Value&& at(unsigned index) &&;

	inline auto tail() { return std::ranges::subrange(std::next(list.begin()), list.end()); }
	inline auto tail() const { return std::ranges::subrange(std::next(list.cbegin()), list.cend()); }
	inline auto init() { return std::ranges::subrange(list.begin(), std::prev(list.end())); }
	inline auto init() const { return std::ranges::subrange(list.cbegin(), std::prev(list.cend())); }

	bool coarce_bool() const;

	void operator+=(Value const& other);
	void operator-=(Value const& other);
	void operator*=(Value const& other);

	bool operator==(Value const& other) const;
	bool operator!=(Value const& other) const;

	Value take(Context&, uint64_t n);
	std::optional<uint64_t> size(Context &ctx) const;
	Value index(Context &ctx, unsigned n);

	bool is_static_expression(Context &ctx) const;
	void subst(Context &ctx);
};

struct Context
{
	std::vector<std::unordered_map<std::string, Value>> scopes;

	struct Scope_Guard
	{
		~Scope_Guard();
		Context *ctx;
	};

	struct Define_Descriptor
	{
		void operator=(decltype(Value{}.cpp_function) func);
		char const* name;
		Context *ctx;
	};

	Value* operator[](std::string const& name);
	void assign(std::string const& name, Value value);
	Define_Descriptor define(char const* val);
	Scope_Guard local_scope();
};

struct Dynamic_Generator : Sequence
{
	Value expr;
	int64_t start = 0;

	Value index(Context &ctx, unsigned n) override;
	Value take(Context &ctx, unsigned n) override;
	Value len(Context &ctx) override;
	Value pop(Context &ctx, unsigned n) override;
};

struct Circular_Generator : Sequence
{
	Value value_set;

  Value index(Context &ctx, unsigned n) override;
	Value take(Context &ctx, unsigned n) override;
	Value len(Context &ctx) override;
	Value pop(Context &ctx, unsigned n) override;
};

struct Composed_Generator : Sequence
{
	std::vector<std::shared_ptr<Sequence>> children;

	Value index(Context &ctx, unsigned n) override;
	Value take(Context &ctx, unsigned n) override;
	Value len(Context &ctx) override;
	Value pop(Context &ctx, unsigned n) override;
};

struct Value_Sequence : Sequence
{
	Value expr;

	Value index(Context &ctx, unsigned n) override;
	Value take(Context &ctx, unsigned n) override;
	Value len(Context &ctx) override;
	Value pop(Context &ctx, unsigned n) override;
};

struct Zip_Sequence : Sequence
{
	std::vector<std::shared_ptr<Sequence>> children;
	std::function<Value(Context&, Value)> zipper;

	Value index(Context &ctx, unsigned n) override;
	Value take(Context &ctx, unsigned n) override;
	Value len(Context &ctx) override;
	Value pop(Context &ctx, unsigned n) override;
};

#include "format.hh"
