#include "patty.hh"

#include <charconv>
#include <iostream>

bool Value::operator==(Value const& other) const
{
	if (type != other.type)
		return false;

	switch (type) {
	case Type::Nil: return true;
	case Type::Int: return ival == other.ival;
	case Type::Cpp_Function: return (!sval.empty() && !other.sval.empty()) && sval == other.sval;
	case Type::Symbol:
	case Type::String: return sval == other.sval;
	case Type::List: return std::equal(CR(list), CR(other.list));
	case Type::Sequence: return false;
	}

	return false;
}

bool Value::operator!=(Value const& other) const
{
	return !(*this == other);
}

bool Value::is_static_expression(Context &ctx) const
{
	switch (type) {
	case Type::List:
		if (!list.empty() && list.front().type == Type::Symbol) {
			for (auto name : { "zip-with", "tail" }) {
				if (name == list.front().sval)
					return false;
			}
		}
		return std::ranges::all_of(list, [&](auto const& val) { return val.is_static_expression(ctx); });
	case Type::Symbol:
		return sval != "n";
	default:
		return true;
	}
}

std::optional<uint64_t> Value::size(Context &ctx) const
{
	switch (type) {
	case Value::Type::List:
		return list.size();

	case Value::Type::Sequence:
		{
			auto len = sequence->len(ctx);
			return len.type == Type::Int ? std::optional<uint64_t>(len.ival) : std::nullopt;
		}

	case Value::Type::String:
		return sval.size();

	default:
		return std::nullopt;
	}
}

Value Value::index(Context &ctx, unsigned n)
{
	switch (type) {
	case Value::Type::List:
		return at(n);

	case Value::Type::Sequence:
		return sequence->index(ctx, n);

	case Value::Type::String:
		return Value::integer(sval[n]);

	default:
		return Value::nil();
	}
}

void Value::subst(Context &ctx)
{
	switch (type) {
	case Type::List:
		for (auto &el : list)
			el.subst(ctx);
		break;

	case Type::Symbol:
		if (auto v = ctx[sval]; v)
			*this = *v;
		break;

	default:
		;
	}
}

Value Value::take(Context &ctx, uint64_t n)
{
	switch (type) {
	case Type::String:
		return Value::string(std::string_view(sval).substr(0, n));

	case Type::List:
		{
			auto to_remove = int64_t(list.size()) - int64_t(n);
			if (to_remove < 0)
				return *this;
			list.erase(std::next(list.begin(), n), list.end());
			return *this;
		}

	case Type::Sequence:
		{
			assert(sequence);
			return Sequence::take(*sequence, ctx, n);
		}

	default:
		error_fatal("take only supports strings, lists and sequences");
	}
}

bool Value::coarce_bool() const
{
	switch (type) {
	case Type::Sequence:
	case Type::Symbol:
	case Type::Cpp_Function: return true;
	case Type::Nil: return false;
	case Type::Int: return ival != 0;
	case Type::List: return !list.empty();
	case Type::String: return !sval.empty();
	}

	return false;
}

Value& Value::at(unsigned index) &
{
	return *std::next(list.begin(), index);
}

Value&& Value::at(unsigned index) &&
{
	return std::move(*std::next(list.begin(), index));
}

void Value::operator+=(Value const& other)
{
	assert(type == Type::Int);
	assert(other.type == Type::Int);
	ival += other.ival;
}

void Value::operator-=(Value const& other)
{
	assert(type == Type::Int);
	assert(other.type == Type::Int);
	ival -= other.ival;
}

void Value::operator*=(Value const& other)
{
	assert(type == Type::Int);
	assert(other.type == Type::Int);
	ival *= other.ival;
}

void print(Value const& value)
{
	fmt::print("{}\n", value);
}

// TODO expose to userspace
Value read(std::string_view &source)
{
	while (!source.empty()) {
		for (; !source.empty() && std::isspace(source.front()); source.remove_prefix(1)) {}
		if (source.starts_with('#')) {
			source.remove_prefix(source.find('\n'));
		} else {
			break;
		}
	}

	if (source.empty())
		return Value::nil();

	if (source.starts_with('"')) {
		auto end = std::adjacent_find(std::cbegin(source)+1, std::cend(source), [](char prev, char curr) {
			return prev != '\\' && curr == '"';
		});

		auto str = Value::string({ std::cbegin(source) + 1, end + 1 }); // TODO add escaping like \n
		source.remove_prefix(std::distance(std::cbegin(source), end + 2));
		return str;
	}

	if (std::isdigit(source.front()) || (source.front() == '-' && std::isdigit(source[1]))) {
		auto value = Value();
		value.type = Value::Type::Int;
		auto [p, ec] = std::from_chars(&source.front(), &source.back() + 1, value.ival);
		assert(p != &source.front());
		source.remove_prefix(p - &source.front());
		return value;
	}

	static constexpr std::string_view Valid_Symbol_Char = "+-*/%$@!^&[]:;<>,.|=";
	if (std::isalpha(source.front()) || Valid_Symbol_Char.find(source.front()) != std::string_view::npos) {
		auto end = std::find_if(source.cbegin()+1, source.cend(), [](char curr) {
			return !std::isalnum(curr) && Valid_Symbol_Char.find(curr) == std::string_view::npos;
		});
		auto symbol = Value::symbol({ std::cbegin(source), end });
		source.remove_prefix(std::distance(std::cbegin(source), end));
		return symbol;
	}

	if (source.starts_with('(')) {
		Value list, elem;
		list.type = Value::Type::List;
		source.remove_prefix(1);
		while ((elem = read(source)).type != Value::Type::Nil) {
			list.list.push_back(std::move(elem));
		}
		return list;
	}

	if (source.starts_with(')')) {
		source.remove_prefix(1);
	}

	return Value::nil();
}

// TODO expose to userspace
Value eval(Context &ctx, Value value)
{
	switch (value.type) {
	case Value::Type::Sequence:
	case Value::Type::Int:
	case Value::Type::Nil:
	case Value::Type::Cpp_Function:
	case Value::Type::String:
		return value;

	case Value::Type::Symbol:
		if (auto resolved = ctx[value.sval]; resolved) {
			return *resolved;
		} else {
			error_fatal("Cannot resolve symbol {}"_format(value.sval));
		}

	case Value::Type::List:
		{
			// assert(!value.list.empty());
			if (value.list.empty())
				return Value::nil();

			auto callable = eval(ctx, value.list.front());
			switch (callable.type) {
			case Value::Type::Cpp_Function:
				value.list.pop_front();
				return callable.cpp_function(ctx, std::move(value));

			case Value::Type::List:
				{
					// Local scope
					ctx.scopes.emplace_back();

					auto const& formal = callable.list.front();
					assert(formal.type == Value::Type::List);
					auto formal_it = formal.list.begin();
					for (auto arg = std::next(value.list.begin()); arg != value.list.end(); ++arg, ++formal_it) {
						assert(formal_it->type == Value::Type::Symbol);
						ctx.assign(formal_it->sval, eval(ctx, std::move(*arg)));
					}
					assert(formal_it == formal.list.end()); // TODO not all parameters were provided
					auto result = eval(ctx, *std::next(callable.list.begin()));
					ctx.scopes.pop_back();
					return result;
				}

			default:
				return value;
			}
		}
	}

	return Value::nil();
}
