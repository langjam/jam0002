template<>
struct fmt::formatter<Value> : fmt::formatter<std::string_view>
{
	thread_local static inline bool in_list = false;

	template<typename FC>
	auto format(Value const& value, FC &fc)
	{
		switch (value.type) {
		case Value::Type::Nil:
			return fmt::format_to(fc.out(), "nil");
		case Value::Type::String:
			if (in_list)
				return fmt::format_to(fc.out(), "{}", std::quoted(value.sval));
			else
				return fmt::format_to(fc.out(), "{}", value.sval);
		case Value::Type::Symbol:
			return fmt::format_to(fc.out(), "{}", value.sval);

		case Value::Type::Int:
			return fmt::format_to(fc.out(), "{}", value.ival);

		case Value::Type::Sequence:
			return fmt::format_to(fc.out(), "(seq)");

		case Value::Type::List: {
			in_list = true;
			auto result = fmt::format_to(fc.out(), "({})", fmt::join(value.list, " "));
			in_list = false;
			return result;
		}
		case Value::Type::Cpp_Function:
			return fmt::format_to(fc.out(), "<cpp-function {}>", value.sval);
		}

		assert(false && "unreachable");
	}
};
