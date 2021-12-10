#pragma once
#include <iostream>

#include "tao/pegtl.hpp"

namespace ptl = tao::pegtl;

namespace wally
{

    struct star_space
        : ptl::star<ptl::one<' ', '\t'>>
    {};
    struct one_nl
        : ptl::sor<
            ptl::string<'\n'>,
            ptl::string<'\r','\n'>
        >
    {};
    struct plus_nl
        : ptl::plus<
            ptl::sor<
                ptl::string<'\n'>,
                ptl::string<'\n'>,
                ptl::string<'\r','\n'>
            >
        >
    {};
    struct star_nl_sp
        : ptl::star<
            ptl::sor<
                ptl::string<'\n'>,
                ptl::string<'\r','\n'>,
                ptl::one<' ', '\t'>
            >
        >
    {};
    struct plus_nl_sp
        : ptl::plus<
            ptl::sor<
                ptl::string<'\n'>,
                ptl::string<'\r'>,
                ptl::string<'\r','\n'>,
                ptl::one<' ', '\t'>
            >
        >
    {};

    struct integer
        : ptl::seq<
            ptl::opt<ptl::one<'+', '-'>>,
            ptl::plus<ptl::digit>
        >
    {};

    struct decimal
        : ptl::seq<
            ptl::opt<ptl::one<'+', '-'>>,
            ptl::plus<ptl::digit>,
            ptl::one<'.'>,
            ptl::plus<ptl::digit>
        >
    {};

    struct variable
        : ptl::seq<
            ptl::lower,
            ptl::star<ptl::sor<ptl::alnum, ptl::string<'_'>>>
        >
    {};

    struct operand
        : ptl::sor<variable, decimal, integer>
    {};

    struct expression;

    // operator precedence (), /, *, +, -
    struct paren_expr
        : ptl::seq<
            ptl::string<'('>,
            star_space,
            expression,
            star_space,
            ptl::string<')'>
        >
    {};

    struct div_term
        : ptl::sor<paren_expr, operand>
    {};
    struct div_expr
        : ptl::seq< div_term, star_space, ptl::plus<ptl::string<'/'>, star_space, div_term, star_space> >
    {};

    struct mul_term
        : ptl::sor<div_expr, paren_expr, operand>
    {};
    struct mul_expr
        : ptl::seq< mul_term, star_space, ptl::plus<ptl::string<'*'>, star_space, mul_term, star_space> >
    {};

    struct plmin_term
        : ptl::sor<div_expr, mul_expr, paren_expr, operand>
    {};
    struct plmin_expr
        : ptl::seq<
            plmin_term,
            star_space,
            ptl::plus<
                ptl::sor<ptl::string<'+'>, ptl::string<'-'>>,
                star_space,
                plmin_term,
                star_space
            >
        >
    {};

    struct expression
        : ptl::sor<
            plmin_expr, mul_expr, div_expr, paren_expr, operand
        >
    {};

    // FIXME:
    struct pattern
        : ptl::seq<
            ptl::string<'/', '*'>,
            ptl::until<ptl::string<'*', '/'>, ptl::one<'*', ' '>>,
            star_space,
            ptl::opt<one_nl>
        >
    {};

    struct var_decl
        : ptl::seq<
            ptl::opt<pattern>,
            star_space,
            ptl::string<'v','a','r'>,
            star_space,
            variable,
            star_space,
            ptl::opt<
                ptl::string<'='>,
                star_space,
                expression
            >
        >
    {};

    struct var_asgn
        : ptl::seq<
            ptl::opt<pattern>,
            star_space,
            variable,
            star_space,
            ptl::string<'='>,
            star_space,
            expression
        >
    {};

    // struct func_name_decl
    //     : ptl::seq<
    //         ptl::upper,
    //         ptl::star<ptl::sor<ptl::alnum, ptl::string<'_'>>>
    //     >
    // {};

    // struct func_args_decl
    //     : ptl::seq<
    //         variable,
    //         ptl::star<
    //             star_space,
    //             ptl::one<','>,
    //             star_space,
    //             variable
    //         >
    //     >
    // {};

    // struct func_body_decl;

    // struct func_decl
    //     : ptl::seq<
    //         ptl::string<'f','u','n','c'>,
    //         star_space,
    //         func_name_decl,
    //         star_space,
    //         ptl::one<'('>,
    //         star_space,
    //         func_args_decl,
    //         star_space,
    //         ptl::one<')'>,
    //         star_nl_sp,
    //         ptl::string<'{'>,
    //         func_body_decl,
    //         star_nl_sp,
    //         ptl::one<'}'>
    //     >
    // {};

    // struct func_call_args
    //     : ptl::seq<
    //         variable,
    //         ptl::opt<
    //             star_space,
    //             pattern
    //         >,
    //         ptl::star<
    //             star_space,
    //             ptl::one<','>,
    //             star_space,
    //             variable,
    //             ptl::opt<star_space, pattern>
    //         >
    //     >
    // {};

    // struct func_call
    //     : ptl::seq<
    //         ptl::upper,
    //         ptl::star<ptl::sor<ptl::alnum, ptl::string<'_'>>>,
    //         star_space,
    //         ptl::one<'('>,
    //         star_space,
    //         func_call_args,
    //         star_space,
    //         ptl::one<')'>
    //     >
    // {};

    struct comment
        : ptl::seq<
            ptl::two<'/'>,
            //  ptl::star<ptl::any>,
            //  one_nl,
            ptl::until<ptl::eolf>
        >
    {};

    struct print_call
        : ptl::seq<
            ptl::string<'P','r','i','n','t'>,
            star_space,
            ptl::one<'('>,
            star_space,
            variable,
            star_space,
            ptl::one<')'>
        >
    {};

    struct secon_states;

    struct condition
        : ptl::seq<
            expression,
            star_space,
            ptl::sor<
                ptl::string<'>', '='>,
                ptl::string<'<', '='>,
                ptl::string<'!', '='>,
                ptl::one<'>'>,
                ptl::one<'<'>,
                ptl::two<'='>
            >,
            star_space,
            expression
        >
    {};

    struct if_cond
        : ptl::seq<
            ptl::string<'i', 'f'>,
            star_space,
            ptl::one<'('>,
            star_space,
            condition,
            star_space,
            ptl::star<
                ptl::sor< ptl::two<'|'>, ptl::two<'&'> >,
                star_space,
                condition
            >,
            star_space,
            ptl::one<')'>,
            star_nl_sp,
            ptl::one<'{'>,
            ptl::star<
                star_nl_sp,
                secon_states
            >,
            star_nl_sp,
            ptl::one<'}'>
        >
    {};

    struct while_cond
        : ptl::seq<
            ptl::string<'w', 'h', 'i', 'l', 'e'>,
            star_space,
            ptl::one<'('>,
            star_space,
            condition,
            star_space,
            ptl::star<
                ptl::sor< ptl::two<'|'>, ptl::two<'&'> >,
                star_space,
                condition
            >,
            star_space,
            ptl::one<')'>,
            star_nl_sp,
            ptl::one<'{'>,
            ptl::star<
                star_nl_sp,
                secon_states
            >,
            star_nl_sp,
            ptl::one<'}'>
        >
    {};

    // // TODO: add comment to body and test
    // struct func_body_decl
    //     : ptl::star<star_nl_sp, ptl::sor<var_asgn, var_decl, if_cond, while_cond>>
    // {};

    struct main_states
        : ptl::sor<
            ptl::seq<
                ptl::sor<
                    var_decl,
                    var_asgn,
                    // func_decl,
                    // func_call,
                    if_cond,
                    while_cond,
                    print_call
                >,
                ptl::eolf
            >,
            comment
        >
    {};

    struct secon_states
        : ptl::sor<
            // var_decl,
            var_asgn
            // ,func_call
            , print_call
            , comment
            , if_cond
            , while_cond
        >
    {};

    struct grammar
        : ptl::must<
            ptl::plus<
                star_nl_sp,
                main_states
            >,
            star_nl_sp,
            ptl::eof
        >
    {};

    template< typename Rule >
    struct action
    {};

    // template<>
    // struct action< expression >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "got expr " << in.string() << "\n";
    //    }
    // };
    // template<>
    // struct action< paren_expr >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "parsed " << in.string() << "\n";
    //    }
    // };
    // template<>
    // struct action< plmin_expr >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "parsed " << in.string() << "\n";
    //    }
    // };
    // template<>
    // struct action< mul_expr >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "parsed " << in.string() << "\n";
    //    }
    // };
    // template<>
    // struct action< div_expr >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "parsed " << in.string() << "\n";
    //    }
    // };
    // template<>
    // struct action< plmin_term >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "parsed term " << in.string() << "\n";
    //    }
    // };
    // template<>
    // struct action< mul_term >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "parsed_term " << in.string() << "\n";
    //    }
    // };
    // template<>
    // struct action< div_term >
    // {
    //    template< typename ParseInput >
    //    static void apply( const ParseInput& in, std::string& v )
    //    {
    //       std::cout << "parsed_term " << in.string() << "\n";
    //    }
    // };
    template<>
    struct action< variable >
    {
       template< typename ParseInput >
       static void apply( const ParseInput& in, std::string& v )
       {
          std::cout << "got var " << in.string() << "\n";
       }
    };
    template<>
    struct action< comment >
    {
        template< typename ParseInput >
        static void apply( const ParseInput& in, std::string& v )
        {
            std::cout << "comment "  << in.string() << "\n";
        }
    };
    template<>
    struct action< pattern >
    {
        template< typename ParseInput >
        static void start( const ParseInput& in )
        {
            std::cout << "pattern " << "\n";
        }
    };
    template<>
    struct action< var_decl >
    {
        template< typename ParseInput >
        static void apply( const ParseInput& in, std::string& v )
        {
            std::cout << "var decl "  << in.string() << "\n";
        }
    };
    template<>
    struct action< var_asgn >
    {
        template< typename ParseInput >
        static void apply( const ParseInput& in, std::string& v )
        {
            std::cout << "var asgn "  << in.string() << "\n";
        }
    };
    // template<>
    // struct action< func_decl >
    // {
    //     template< typename ParseInput >
    //     static void apply( const ParseInput& in, std::string& v )
    //     {
    //         std::cout << "func decl " << in.string() << "\n";
    //     }
    // };
    // template<>
    // struct action< func_call >
    // {
    //     template< typename ParseInput >
    //     static void apply( const ParseInput& in, std::string& v )
    //     {
    //         std::cout << "func call " << in.string() << "\n";
    //     }
    // };
    template<>
    struct action< plus_nl >
    {
        template< typename ParseInput >
        static void apply( const ParseInput& in, std::string& v )
        {
            std::cout << "." << in.string();
        }
    };
    template<>
    struct action< condition >
    {
        template< typename ParseInput >
        static void apply( const ParseInput& in, std::string& v )
        {
            std::cout << "cond " << in.string() << "\n";
        }
    };
    template<>
    struct action< if_cond >
    {
        template< typename ParseInput >
        static void apply( const ParseInput& in, std::string& v )
        {
            std::cout << "if cond " << in.string() << "\n";
        }
    };


    template< typename Rule > struct selector : std::true_type {};
    // template<> struct selector< star_space > : std::false_type {};
    // template<> struct selector< star_nl_sp > : std::false_type {};
    // template<> struct selector< ptl::one<','> > : std::false_type {};
}
