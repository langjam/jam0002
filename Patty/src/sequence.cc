#include "patty.hh"

#include <cmath>
#include <iostream>
#include <algorithm>

Value Sequence::take(Sequence &seq, Context &ctx, unsigned n)
{
	auto result = seq.take(ctx, n);
	if (result.type == Value::Type::List) {
		unsigned i = 0;
		for (auto &el : result.list) {
			if (el.type == Value::Type::Sequence) {
				auto seq = el;
				auto result2 = Sequence::take(*el.sequence, ctx, n - i + 1);
				result.list.erase(std::next(result.list.begin(), i), result.list.end());

				for (auto &el2 : result2.list)
					result.list.push_back(std::move(el2));
				return result;
			}
			++i;
		}
	}
	return result;
}

Value Dynamic_Generator::take(Context &ctx, unsigned n)
{
	Value result;
	result.type = Value::Type::List;
	for (int64_t i = 0; i < n; ++i) {
		auto local_scope_guard = ctx.local_scope();
		ctx.assign("n", Value::integer(i + start));
		result.list.push_back(eval(ctx, expr));
	}
	return result;
}

Value Dynamic_Generator::index(Context &ctx, unsigned n)
{
	auto local_scope_guard = ctx.local_scope();
	ctx.assign("n", Value::integer(n + start));
	return eval(ctx, expr);
}

Value Dynamic_Generator::len(Context &)
{
	return Value::nil();
}

Value Dynamic_Generator::pop(Context&, unsigned n)
{
	auto copy = *this;
	copy.start += n;

	Value retval;
	retval.type = Value::Type::Sequence;
	retval.sequence = std::make_shared<Dynamic_Generator>(copy);
	return retval;
}

Value Circular_Generator::take(Context &ctx, unsigned n)
{
	Value result;
	result.type = Value::Type::List;
	for (int64_t i = 0; i < n; ++i) {
		auto local_scope_guard = ctx.local_scope();
		ctx.assign("n", Value::integer(i));
		result.list.push_back(eval(ctx, value_set.at(i % value_set.list.size())));
	}
	return result;
}

Value Circular_Generator::index(Context &ctx, unsigned n)
{
	return eval(ctx, value_set.at(n % value_set.list.size()));
}

Value Circular_Generator::len(Context&)
{
	return Value::integer(value_set.list.size());
}

Value Circular_Generator::pop(Context &, unsigned n)
{
	auto copy = *this;

	if (n >= copy.value_set.list.size()) return Value::nil();
	do copy.value_set.list.pop_front(); while (n-- >= 1);

	Value retval;
	retval.type = Value::Type::Sequence;
	retval.sequence = std::make_shared<Circular_Generator>(copy);
	return retval;
}

Value Composed_Generator::take(Context &ctx, unsigned n)
{
	Value result;
	result.type = Value::Type::List;

	for (;;) {
		for (auto& gen : children) {
			if (auto circular = dynamic_cast<Circular_Generator*>(gen.get()); circular != nullptr) {
				unsigned copied = std::min(circular->value_set.list.size(), (size_t)n);

				auto it = circular->value_set.list.begin();
				for (auto i = 0u; i < copied; ++i, ++it)
					result.list.push_back(eval(ctx, *it));

				if (copied == n)
					return result;
				else
					n -= copied;
			} else {
				auto rest = gen->take(ctx, n);
				std::ranges::move(std::move(rest.list), std::back_inserter(result.list));
				return result;
			}
		}
	}
}

Value Composed_Generator::index(Context &ctx, unsigned n)
{
	for (;;) {
		for (auto& gen : children) {
			if (auto circular = dynamic_cast<Circular_Generator*>(gen.get()); circular != nullptr) {
				auto size = circular->value_set.list.size();
				if (n < size)
					return circular->index(ctx, n);
				n -= size;
			} else {
				return gen->index(ctx, n);
			}
		}
	}
}

Value Composed_Generator::len(Context &ctx)
{
	auto sum = Value::integer(0);
	for (auto const& gen : children)
		if (auto r = gen->len(ctx); r.type == Value::Type::Nil)
			return Value::nil();
		else
			sum += r;
	return sum;
}

Value Composed_Generator::pop(Context &, unsigned)
{
	assert(false && "unimplemented");
}


Value Value_Sequence::index(Context &ctx, unsigned n)
{
	return expr.index(ctx, n);
}

Value Value_Sequence::take(Context &ctx, unsigned n)
{
	return expr.take(ctx, n);
}

Value Value_Sequence::len(Context &ctx)
{
	auto sz = expr.size(ctx);
	if (sz)
		return Value::integer(*sz);
	return Value::nil();
}

Value Value_Sequence::pop(Context &, unsigned)
{
	assert(false && "unimplemented");
}

#if 0
struct Zip_Sequence : Sequence
{
	std::vector<std::shared_ptr<Sequence>> children;
	std::function<Value(Value)> zipper;
#endif

Value Zip_Sequence::index(Context &ctx, unsigned n)
{
	Value list;
	list.type = Value::Type::List;
	for (auto &seq : children) {
		list.list.push_back(seq->index(ctx, n));
	}
	return zipper(ctx, list);
}

Value Zip_Sequence::take(Context &ctx, unsigned n)
{
	Value list;
	list.type = Value::Type::List;

	std::vector<Value> takes;
	for (auto &seq : children) {
		takes.push_back(seq->take(ctx, n));
	}

	for (unsigned i = 0; i < n; ++i) {
		Value frame;
		frame.type = Value::Type::List;

		for (auto &take : takes) {
			if (take.list.size() > i)
				return list;
			frame.list.push_back(take.at(i));
		}

		list.list.push_back(zipper(ctx, frame));
	}

	return list;
}

Value Zip_Sequence::len(Context &)
{
	assert(false && "unimplemented");
}

Value Zip_Sequence::pop(Context &, unsigned)
{
	assert(false && "unimplemented");
}
