#include "patty.hh"

Context::Scope_Guard::~Scope_Guard()
{
	ctx->scopes.pop_back();
}

Value* Context::operator[](std::string const& name)
{
	for (auto scope = scopes.rbegin(); scope != scopes.rend(); ++scope)
		if (scope->contains(name))
			return &(*scope)[name];
	return nullptr;
}

void Context::assign(std::string const& name, Value value)
{
	scopes.back().emplace(name, std::move(value));
}

void Context::Define_Descriptor::operator=(decltype(Value{}.cpp_function) func)
{
	ctx->assign(name, Value::cpp(name, std::move(func)));
}

Context::Define_Descriptor Context::define(char const* val)
{
	return Define_Descriptor{val, this};
}

Context::Scope_Guard Context::local_scope()
{
	scopes.emplace_back();
	return Scope_Guard{this};
}
