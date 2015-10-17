//
//  Copyright (c) 2015, Aleksandar Dezelin
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are met:
//
//  1. Redistributions of source code must retain the above copyright notice, this
//     list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
//  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
//  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
//  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#ifndef FLANG_OCAMLAST_H
#define FLANG_OCAMLAST_H

// Increase the maximum number of tuple items for Boost variant types
#define BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS
#define BOOST_MPL_LIMIT_LIST_SIZE 40
#define BOOST_MPL_LIMIT_VECTOR_SIZE 40

#include "ocamlids.h"

#include <boost/config/warning_disable.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/optional.hpp>
#include <boost/optional/optional_io.hpp>
#include <boost/spirit/include/support_extended_variant.hpp>
#include <boost/spirit/include/support_attributes.hpp>
#include <boost/variant/recursive_variant.hpp>

#include <vector>
#include <memory>

namespace ocaml
{
namespace ast
{

///////////////////////////////////////////////////////////////////////////
//  The AST
///////////////////////////////////////////////////////////////////////////

struct tagged
{
    int id; // Used to annotate the AST with the iterator position.
    // This id is used as a key to a map<int, Iterator>
    // (not really part of the AST.)
};

//
// Lexical
//

struct lowercase_ident
    : tagged
{
    lowercase_ident(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct capitalized_ident
    : tagged
{
    capitalized_ident(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct ident
    : tagged
{
    ident(std::string const &name = "")
        : name(name) { }

    std::string name;
};

typedef std::vector<ident> ident_list;

struct label_name
    : tagged
{
    label_name(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct label
    : tagged
{
    label(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct optlabel
    : tagged
{
    optlabel(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct integer_literal
    : tagged
{
    integer_literal(int i = 0)
        : val(i) { }

    int val;
};

struct float_literal
    : tagged
{
    float_literal(float f = 0.f)
        : val(f) { }

    float val;
};

struct char_literal
    : tagged
{
    char_literal(std::string const &c = "")
        : val(c) { }

    std::string val;
};

struct string_literal
    : tagged
{
    string_literal(std::string const &str = "")
        : val(str) { }

    std::string val;
};

//
// Names
//


//
//  BNF-like notation:
//  ========================================================================
//
//  value-name	::=	lowercase-ident
//           ∣	 ( operator-name )
//
//  operator-name	::=	prefix-symbol ∣  infix-op
//
//  infix-op	::=	infix-symbol
//          ∣  *  ∣  +  ∣  -  ∣  -.  ∣  =  ∣  !=  ∣  <  ∣  >  ∣  or  ∣  ||
//          ∣  &  ∣  &&  ∣  :=  ∣  mod  ∣  land  ∣  lor  ∣  lxor  ∣  lsl
//          ∣  lsr  ∣  asr
//
//  constr-name	::=	capitalized-ident
//
//  tag-name	::=	capitalized-ident
//
//  typeconstr-name	::=	lowercase-ident
//
//  field-name	::=	lowercase-ident
//
//  module-name	::=	capitalized-ident
//
//  modtype-name	::=	ident
//
//  class-name	::=	lowercase-ident
//
//  inst-var-name	::=	lowercase-ident
//
//  method-name	::=	lowercase-ident
//
//  value-path	::=	[ module-path . ]  value-name
//
//  constr	::=	[ module-path . ]  constr-name
//
//  typeconstr	::=	[ extended-module-path . ]  typeconstr-name
//
//  field	::=	[ module-path . ]  field-name
//
//  modtype-path	::=	[ extended-module-path . ]  modtype-name
//
//  class-path	::=	[ module-path . ]  class-name
//
//  classtype-path	::=	[ extended-module-path . ]  class-name
//
//  module-path	::=	module-name  { . module-name }
//
//  extended-module-path	::=	extended-module-name  { . extended-module-name }
//
//  extended-module-name	::=	module-name  { ( extended-module-path ) }
//

struct method_name
    : tagged
{
    lowercase_ident name;
};

struct inst_var_name
    : tagged
{
    lowercase_ident name;
};

struct class_name
    : tagged
{
    lowercase_ident name;
};

struct modtype_name
    : tagged
{
    modtype_name(ident const& name = ident())
      : name(name) {}

    ident name;
};

struct module_name
    : tagged
{
    module_name(capitalized_ident const& name = capitalized_ident())
        : name(name) {}

    capitalized_ident name;
};

struct field_name
    : tagged
{
    lowercase_ident name;
};

struct typeconstr_name
    : tagged
{
    lowercase_ident name;
};

struct tag_name
    : tagged
{
    capitalized_ident name;
};

struct constr_name
    : tagged
{
    constr_name(ast::capitalized_ident const& name = ast::capitalized_ident())
        : name(name) {}

    capitalized_ident name;
};

struct operation
    : tagged
{
    operation(std::size_t tokenId)
        : op((ocaml::lexer::Tokens)tokenId) {}

    operation(ocaml::lexer::Tokens op = ocaml::lexer::Tokens::Unknown)
        : op(op) {}

    ocaml::lexer::Tokens op;
};

struct infix_symbol
    : tagged
{
    infix_symbol(std::string const &symbol = "")
        : symbol(symbol) { }

    std::string symbol;
};

struct infix_op
    : tagged
      , boost::spirit::extended_variant<
        infix_symbol,
        operation
    >
{
    infix_op()
        : base_type() { }

    infix_op(infix_symbol const &val)
        : base_type(val) { }

    infix_op(operation const &val)
        : base_type(val) { }
};

struct prefix_symbol
    : tagged
{
    prefix_symbol(std::string const &symbol = "")
        : symbol(symbol) { }

    std::string symbol;
};

struct operator_name
    : tagged
      , boost::spirit::extended_variant<
        prefix_symbol,
        infix_op
    >
{
    operator_name()
        : base_type() { }

    operator_name(prefix_symbol const &val)
        : base_type(val) { }

    operator_name(infix_op const &val)
        : base_type(val) { }
};

struct value_name
    : tagged
      , boost::spirit::extended_variant<
        lowercase_ident,
        operator_name
    >
{
    value_name()
        : base_type() { }

    value_name(lowercase_ident const &val)
        : base_type(val) { }

    value_name(operator_name const &val)
        : base_type(val) { }
};

struct extended_module_path;

typedef std::vector<extended_module_path> extended_module_path_list;

struct extended_module_name
    : tagged
{
    extended_module_name(module_name const& name = module_name())
        : name(name) {}

    module_name name;
    boost::optional<extended_module_path_list> paths;
};

typedef std::vector<extended_module_name> extended_module_name_list;

struct extended_module_path
    : tagged
{
    extended_module_path(extended_module_name const& name = extended_module_name())
        : name(name) {}

    extended_module_name name;
    boost::optional<extended_module_name_list> other;
};

typedef std::vector<module_name> module_name_list;

struct module_path
    : tagged
{
    module_path(module_name const& name = module_name())
        : name(name) {}

    module_path(module_name const& name,
                boost::optional<module_name_list> const& other)
        : name(name)
    {
        if (other.is_initialized())
            if (other.get().size() > 0)
                this->other = other;
    }

    module_name name;
    boost::optional<module_name_list> other;
};

struct classtype_path
    : tagged
{
    classtype_path(
        boost::optional<extended_module_path> const& path =
            boost::optional<extended_module_path>())
        : path(path) {}

    boost::optional<extended_module_path> path;
    class_name name;
};

struct class_path
    : tagged
{
    class_path(
        boost::optional<module_path> const path =
            boost::optional<module_path>())
        : path(path) {}

    boost::optional<module_path> path;
    class_name name;
};

struct modtype_path
    : tagged
{
    modtype_path(
        boost::optional<extended_module_path> const& path =
            boost::optional<extended_module_path>())
        : path(path) {}

    modtype_path(modtype_name const& name)
    	: name(name) {}

    boost::optional<extended_module_path> path;
    modtype_name name;
};

struct field
    : tagged
{
    field(
        boost::optional<module_path> const& path =
            boost::optional<module_path>())
        : path(path) {}

    boost::optional<module_path> path;
    field_name name;
};

struct typeconstr
    : tagged
{
    typeconstr(
        boost::optional<extended_module_path> const& path =
            boost::optional<extended_module_path>())
        : path(path) {}

    boost::optional<extended_module_path> path;
    typeconstr_name name;
};

struct constr
    : tagged
{
    constr(
        boost::optional<module_path> const& path =
            boost::optional<module_path>())
        : path(path) {}

    constr(constr_name const& name)
        : name(name) {}

    constr(
        boost::optional<module_path> const& path, constr_name const& name)
        : path(path), name(name) {}

    boost::optional<module_path> path;
    constr_name name;
};

struct value_path
    : tagged
{
    value_path(
        boost::optional<module_path> const& path =
            boost::optional<module_path>())
        : path(path) {}

    boost::optional<module_path> path;
    value_name name;
};

//
// Type expressions
//


//
//  BNF-like notation:
//  ========================================================================
//
//  typexpr	::=	' ident
//      	∣	 _
//       	∣	 ( typexpr )
//      	∣	 [[?]label-name:]  typexpr ->  typexpr
//      	∣	 typexpr  { * typexpr }+
//       	∣	 typeconstr
//      	∣	 typexpr  typeconstr
//      	∣	 ( typexpr  { , typexpr } )  typeconstr
//      	∣	 typexpr as '  ident
//      	∣	 polymorphic-variant-type
//      	∣	 < [..] >
//      	∣	 < method-type  { ; method-type }  [; ∣  ; ..] >
//      	∣	 # class-path
//      	∣	 typexpr #  class-path
//      	∣	 ( typexpr  { , typexpr } ) #  class-path
//
//  poly-typexpr	::=	typexpr
//      	∣	 { ' ident }+ .  typexpr
//
//  method-type	::=	method-name :  poly-typexpr
//
//  polymorphic-variant-type	::=	[ tag-spec-first  { | tag-spec } ]
//      	∣	 [> [ tag-spec ]  { | tag-spec } ]
//      	∣	 [< [|] tag-spec-full  { | tag-spec-full }  [ > { `tag-name }+ ] ]
//
//  tag-spec-first	::=	`tag-name  [ of typexpr ]
//      	∣	 [ typexpr ] |  tag-spec
//
//  tag-spec	::=	`tag-name  [ of typexpr ]
// 	        ∣	 typexpr
//
//  tag-spec-full	::=	`tag-name  [ of [&] typexpr  { & typexpr } ]
//      	∣	 typexpr
//

//    Refactored type expressions grammar:
//    =======================================
//
//    non_rr_types_typexpr ::=
//        ''' ident
//        | '_'
//        | '(' typexpr ')'
//        | typeconstr
//        | '(' typexpr { , typexpr } ')' typeconstr
//        | polymorphic-variant-type
//        | '<' ['..'] '>'
//        | '<' method-type {';' method-type} [';' ∣ ';..'] '>'
//        | '#' class-path
//        | '(' typexpr  {',' typexpr} ')' '#' class-path
//
//    abbreviation_types_typexpr ::= non_rr_types_typexpr abbreviation_types_typexpr'
//    abbreviation_types_typexpr'::= ('#' class-path) abbreviation_types_typexpr' | epsilon
//
//    aliased_types_typexpr ::= abbreviation_types_typexpr aliased_types_typexpr'
//    aliased_types_typexpr'::= ('as' ''' ident) aliased_types_typexpr' | epsilon
//
//    constructed_types_typexpr ::= aliased_types_typexpr constructed_types_typexpr'
//    constructed_types_typexpr'::= typeconstr constructed_types_typexpr' | epsilon
//
//    tuple_types_typexpr ::= constructed_types_typexpr tuple_types_typexpr'
//    tuple_types_typexpr'::= +('*' typexpr) tuple_types_typexpr' | epsilon
//
//    function_types_typexpr ::= tuple_types_typexpr function_types_typexpr'
//    function_types_typexpr'::= ('->' typexpr) function_types_typexpr' | epsilon
//
//    typexpr ::=
//        function_types_typexpr
//        | tuple_types_typexpr
//        | constructed_types_typexpr
//        | aliased_types_typexpr
//        | abbreviation_types_typexpr
//        | non_rr_types_typexpr

struct function_types_typexpr;
struct tuple_types_typexpr;
struct constructed_types_typexpr;
struct aliased_types_typexpr;
struct abbreviation_types_typexpr;
struct non_rr_types_typexpr;

struct typexpr
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<function_types_typexpr>,
        boost::recursive_wrapper<tuple_types_typexpr>,
        boost::recursive_wrapper<constructed_types_typexpr>,
        boost::recursive_wrapper<aliased_types_typexpr>,
        boost::recursive_wrapper<abbreviation_types_typexpr>,
        boost::recursive_wrapper<non_rr_types_typexpr>
    >
{
    typexpr()
        : base_type() { }

    typexpr(function_types_typexpr const &val)
        : base_type(val) { }

    typexpr(tuple_types_typexpr const &val)
        : base_type(val) { }

    typexpr(constructed_types_typexpr const &val)
        : base_type(val) { }

    typexpr(aliased_types_typexpr const &val)
        : base_type(val) { }

    typexpr(abbreviation_types_typexpr const &val)
        : base_type(val) { }

    typexpr(non_rr_types_typexpr const &val)
        : base_type(val) { }
};

typedef std::vector<typexpr> typexpr_list;

struct function_types_typexpr_label
    : tagged
      , boost::spirit::extended_variant<
          label_name,
          optlabel
      >
{
    function_types_typexpr_label()
        : base_type() { }

    function_types_typexpr_label(label_name const& val)
        : base_type(val) { }

    function_types_typexpr_label(optlabel const& val)
        : base_type(val) { }
};

struct function_types_typexpr_rr
    : tagged
{
    function_types_typexpr_rr() { }

    function_types_typexpr_rr(typexpr *expr,
        function_types_typexpr_rr *function_expr_rr)
        : expr(expr), function_expr_rr(function_expr_rr)
    { }

    std::shared_ptr<typexpr> expr;
    std::shared_ptr<function_types_typexpr_rr> function_expr_rr;
};

struct function_types_typexpr
    : tagged
{
    function_types_typexpr() { }

    function_types_typexpr(boost::optional<function_types_typexpr_label> const& label,
        tuple_types_typexpr const& tuple_expr,
        function_types_typexpr_rr const& function_expr_rr)
        : label(label), tuple_expr(tuple_expr), function_expr_rr(function_expr_rr)
    { }

    boost::optional<function_types_typexpr_label> label;
    boost::recursive_wrapper<tuple_types_typexpr> tuple_expr;
    boost::recursive_wrapper<function_types_typexpr_rr> function_expr_rr;
};

struct tuple_types_typexpr_rr
    : tagged
{
    tuple_types_typexpr_rr() { }

    tuple_types_typexpr_rr(typexpr_list const& list,
        tuple_types_typexpr_rr *tuple_expr_rr)
        : exprList(list), tuple_expr_rr(tuple_expr_rr) { }

    typexpr_list exprList;
    std::shared_ptr<tuple_types_typexpr_rr> tuple_expr_rr;
};

struct tuple_types_typexpr
    : tagged
{
    tuple_types_typexpr() { }

    tuple_types_typexpr(constructed_types_typexpr const& constructed_expr,
        tuple_types_typexpr_rr const& tuple_expr_rr)
        : constructed_expr(constructed_expr), tuple_expr_rr(tuple_expr_rr) { }

    boost::recursive_wrapper<constructed_types_typexpr> constructed_expr;
    boost::recursive_wrapper<tuple_types_typexpr_rr> tuple_expr_rr;
};

struct constructed_types_typexpr_rr
    : tagged
{
    constructed_types_typexpr_rr() { }

    constructed_types_typexpr_rr(typeconstr const& constr,
        constructed_types_typexpr_rr *constructed_expr_rr_)
        : constr(constr), constructed_expr_rr(constructed_expr_rr_)
    { }

    typeconstr constr;
    std::shared_ptr<constructed_types_typexpr_rr> constructed_expr_rr;
};

struct constructed_types_typexpr
    : tagged
{
    constructed_types_typexpr() { }

    constructed_types_typexpr(aliased_types_typexpr const& aliased_expr,
        constructed_types_typexpr_rr const& constructed_expr_rr_)
        : aliased_expr(aliased_expr), constructed_expr_rr(constructed_expr_rr_)
    { }

    boost::recursive_wrapper<aliased_types_typexpr> aliased_expr;
    boost::recursive_wrapper<constructed_types_typexpr_rr> constructed_expr_rr;
};

struct aliased_types_typexpr_rr
    : tagged
{
    aliased_types_typexpr_rr() { }

    aliased_types_typexpr_rr(ident const& ident,
        aliased_types_typexpr_rr *aliased_expr_rr)
        : _ident(ident), aliased_expr_rr(aliased_expr_rr)
    { }

    ident _ident;
    std::shared_ptr<aliased_types_typexpr_rr> aliased_expr_rr;
};

struct aliased_types_typexpr
    : tagged
{
    aliased_types_typexpr() { }

    aliased_types_typexpr(abbreviation_types_typexpr const& abbreviation_expr,
        aliased_types_typexpr_rr const& aliased_expr_rr)
        : abbreviation_expr(abbreviation_expr), aliased_expr_rr(aliased_expr_rr)
    { }

    boost::recursive_wrapper<abbreviation_types_typexpr> abbreviation_expr;
    boost::recursive_wrapper<aliased_types_typexpr_rr> aliased_expr_rr;
};

struct abbreviation_types_typexpr_rr
    : tagged
{
    abbreviation_types_typexpr_rr() { }

    abbreviation_types_typexpr_rr(class_path const& path,
        abbreviation_types_typexpr_rr *abbreviation_expr_rr)
        : path(path), abbreviation_expr_rr(abbreviation_expr_rr)
    { }

    class_path path;
    std::shared_ptr<abbreviation_types_typexpr_rr> abbreviation_expr_rr;
};

struct abbreviation_types_typexpr
    : tagged
{
    abbreviation_types_typexpr() { }

    abbreviation_types_typexpr(non_rr_types_typexpr const& non_rr_expr,
        abbreviation_types_typexpr_rr const& abbreviation_expr_rr)
        : non_rr_expr(non_rr_expr), abbreviation_expr_rr(abbreviation_expr_rr)
    { }

    boost::recursive_wrapper<non_rr_types_typexpr> non_rr_expr;
    boost::recursive_wrapper<abbreviation_types_typexpr_rr> abbreviation_expr_rr;
};

// _
struct anon_type_variable_typexpr
    : tagged
{
    // FIXME: tokens are not needed
    anon_type_variable_typexpr(ocaml::lexer::Tokens tokenId = ocaml::lexer::Tokens::Unknown)
        : var(tokenId) { }

    ocaml::lexer::Tokens var;
};

typedef std::vector<anon_type_variable_typexpr> anon_type_variable_typexpr_list;

// '(' typexpr ')'
struct parenthesized_typexpr
    : tagged
{
    typexpr expr;
};

// ( typexpr  { , typexpr } )  typeconstr
struct constructed_nary_typexpr
    : tagged
{
    typexpr expr;
    boost::optional<typexpr_list> other;
    typeconstr constr;
};

struct exact_variant_type;
struct opened_variant_type;
struct closed_variant_type;

struct polymorphic_variant_type
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<exact_variant_type>,
        boost::recursive_wrapper<opened_variant_type>,
        boost::recursive_wrapper<closed_variant_type>
    >
{
    polymorphic_variant_type()
        : base_type() { }

    polymorphic_variant_type(exact_variant_type const &val)
        : base_type(val) { }

    polymorphic_variant_type(opened_variant_type const &val)
        : base_type(val) { }

    polymorphic_variant_type(closed_variant_type const &val)
        : base_type(val) { }
};

struct tag_spec_of;
struct tag_spec_of_list;
struct tag_spec_or;

struct tag_spec_first
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<tag_spec_of>,
        boost::recursive_wrapper<tag_spec_or>
    >
{
    tag_spec_first()
        : base_type() { }

    tag_spec_first(tag_spec_of const &val)
        : base_type(val) { }

    tag_spec_first(tag_spec_or const &val)
        : base_type(val) { }
};

struct tag_spec
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<tag_spec_of>,
        typexpr
    >
{
    tag_spec()
        : base_type() { }

    tag_spec(tag_spec_of const &val)
        : base_type(val) { }

    tag_spec(typexpr const &val)
        : base_type(val) { }
};

struct tag_spec_full
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<tag_spec_of_list>,
        typexpr
    >
{
    tag_spec_full()
        : base_type() { }

    tag_spec_full(tag_spec_of_list const &val)
        : base_type(val) { }

    tag_spec_full(typexpr const &val)
        : base_type(val) { }
};

typedef std::vector<tag_spec> tag_spec_list;

struct exact_variant_type
    : tagged
{
    tag_spec_first first;
    boost::optional<tag_spec_list> other;
};

struct opened_variant_type
    : tagged
{
    boost::optional<tag_spec> first;
    boost::optional<tag_spec_list> other;
};

typedef std::vector<tag_spec_full> tag_spec_full_list;
typedef std::vector<tag_name> tag_name_list;

struct closed_variant_type
    : tagged
{
    tag_spec_full first;
    boost::optional<tag_spec_full_list> other;
    boost::optional<tag_name_list>
        tags;
};

struct tag_spec_of
    : tagged
{
    tag_name name;
    boost::optional<typexpr> expr;
};

struct tag_spec_of_list_expr_list
    : tagged
{
    typexpr expr;
    boost::optional<typexpr_list> other;
};

struct tag_spec_of_list
    : tagged
{
    tag_name name;
    boost::optional<tag_spec_of_list_expr_list> exprList;
};

struct tag_spec_or
    : tagged
{
    boost::optional<typexpr> expr;
    tag_spec tag;
};

struct explicit_polymorphic_typexpr
    : tagged
{
    boost::optional<ident_list> identList;
    typexpr expr;
};

struct polymorphic_typexpr
    : tagged
      , boost::spirit::extended_variant<
        typexpr,
        explicit_polymorphic_typexpr
    >
{
    polymorphic_typexpr()
        : base_type() { }

    polymorphic_typexpr(typexpr const &val)
        : base_type(val) { }

    polymorphic_typexpr(explicit_polymorphic_typexpr const &val)
        : base_type(val) { }
};

struct method_type
    : tagged
{
    method_name name;
    polymorphic_typexpr expr;
};

// <[..]>
struct object_row_typexpr
    : tagged
{
    object_row_typexpr(ocaml::lexer::Tokens ellipsis =
        ocaml::lexer::Tokens::Unknown)
        : ellipsis(ellipsis) { }

    ocaml::lexer::Tokens ellipsis;
};

typedef std::vector<method_type> method_type_list;

// < method-type  { ; method-type }  [; ∣  ; ..] >
struct object_typexpr
    : tagged
{
    method_type type;
    boost::optional<method_type_list> other;
    boost::optional<ocaml::lexer::Tokens> ellipsis;
};

//  #  class-path
struct octothorpe_class_path_typexpr
    : tagged
{
    class_path path;
};

struct octothorpe_list_typexpr
    : tagged
{
    typexpr expr;
    boost::optional<typexpr_list> other;
    class_path path;
};

struct non_rr_types_typexpr
    : tagged
      , boost::spirit::extended_variant<
        ident,
        anon_type_variable_typexpr,
        parenthesized_typexpr,
        typeconstr,
        constructed_nary_typexpr,
        polymorphic_variant_type,
        object_row_typexpr,
        object_typexpr,
        octothorpe_class_path_typexpr,
        octothorpe_list_typexpr
    >
{
    non_rr_types_typexpr()
        : base_type() { }

    non_rr_types_typexpr(ident const &val)
        : base_type(val) { }

    non_rr_types_typexpr(anon_type_variable_typexpr const &val)
        : base_type(val) { }

    non_rr_types_typexpr(parenthesized_typexpr const &val)
        : base_type(val) { }

    non_rr_types_typexpr(typeconstr const &val)
        : base_type(val) { }

    non_rr_types_typexpr(constructed_nary_typexpr const &val)
        : base_type(val) { }

    non_rr_types_typexpr(polymorphic_variant_type const &val)
        : base_type(val) { }

    non_rr_types_typexpr(object_row_typexpr const &val)
        : base_type(val) { }

    non_rr_types_typexpr(object_typexpr const &val)
        : base_type(val) { }

    non_rr_types_typexpr(octothorpe_class_path_typexpr const &val)
        : base_type(val) { }

    non_rr_types_typexpr(octothorpe_list_typexpr const &val)
        : base_type(val) { }
};

//
// Constants
//


//
//  BNF-like notation:
//  ========================================================================
//
//  constant	::=	integer-literal
// 	        ∣	 float-literal
// 	        ∣	 char-literal
// 	        ∣	 string-literal
// 	        ∣	 constr
// 	        ∣	 false
// 	        ∣	 true
// 	        ∣	 ()
// 	        ∣	 begin end
// 	        ∣	 []
// 	        ∣	 [||]
// 	        ∣	 `tag-name
//

struct const_false
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens false_;
};

struct const_true
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens true_;
};

struct const_unit
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens opened;
    ocaml::lexer::Tokens closed;
};

struct const_empty_record
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens begin;
    ocaml::lexer::Tokens end;
};

struct const_empty_list
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens opened;
    ocaml::lexer::Tokens closed;
};

struct const_empty_array
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens opened;
    ocaml::lexer::Tokens closed;
};

struct constant
    : tagged
      , boost::spirit::extended_variant<
        integer_literal,
        float_literal,
        char_literal,
        string_literal,
        constr,
        const_false,
        const_true,
        const_unit,
        const_empty_record,
        const_empty_list,
        const_empty_array,
        tag_name>
{
    constant()
        : base_type() { }

    constant(integer_literal const &val)
        : base_type(val) { }

    constant(float_literal const &val)
        : base_type(val) { }

    constant(char_literal const &val)
        : base_type(val) { }

    constant(string_literal const &val)
        : base_type(val) { }

    constant(constr const &val)
        : base_type(val) { }

    constant(const_false const &val)
        : base_type(val) { }

    constant(const_true const &val)
        : base_type(val) { }

    constant(const_unit const &val)
        : base_type(val) { }

    constant(const_empty_record const &val)
        : base_type(val) { }

    constant(const_empty_list const &val)
        : base_type(val) { }

    constant(const_empty_array const &val)
        : base_type(val) { }

    constant(tag_name const &val)
        : base_type(val) { }
};

//
// Patterns
//


//
//  BNF-like notation:
//  ========================================================================
//
//  pattern	::=	value-name
// 	        ∣	 _
// 	        ∣	 constant
// 	        ∣	 pattern as  value-name
// 	        ∣	 ( pattern )
// 	        ∣	 ( pattern :  typexpr )
// 	        ∣	 pattern |  pattern
// 	        ∣	 constr  pattern
// 	        ∣	 `tag-name  pattern
// 	        ∣	 #typeconstr
// 	        ∣	 pattern  { , pattern }+
// 	        ∣	 { field =  pattern  { ; field =  pattern }  [ ; ] }
// 	        ∣	 [ pattern  { ; pattern }  [ ; ] ]
// 	        ∣	 pattern ::  pattern
// 	        ∣	 [| pattern  { ; pattern }  [ ; ] |]
//

struct any_value_pattern;
struct alias_pattern;
struct parenthized_pattern;
struct or_pattern;
struct variant_pattern;
struct variant_non_empty_list_pattern;
struct variant_list_pattern;
struct polymorphic_variant_pattern;
struct polymorphic_variant_abbrev_pattern;
struct tuple_pattern;
struct record_pattern;
struct array_pattern;

struct pattern
    : tagged
      , boost::spirit::extended_variant<
        value_name,
        boost::recursive_wrapper<any_value_pattern>,
        constant,
        boost::recursive_wrapper<alias_pattern>,
        boost::recursive_wrapper<parenthized_pattern>,
        boost::recursive_wrapper<or_pattern>,
        boost::recursive_wrapper<variant_pattern>,
        boost::recursive_wrapper<polymorphic_variant_pattern>,
        boost::recursive_wrapper<polymorphic_variant_abbrev_pattern>,
        boost::recursive_wrapper<tuple_pattern>,
        boost::recursive_wrapper<record_pattern>,
        boost::recursive_wrapper<variant_list_pattern>,
        boost::recursive_wrapper<variant_non_empty_list_pattern>,
        boost::recursive_wrapper<array_pattern>
    >
{
    pattern()
        : base_type() { }

    pattern(value_name const &val)
        : base_type(val) { }

    pattern(any_value_pattern const &val)
        : base_type(val) { }

    pattern(constant const &val)
        : base_type(val) { }

    pattern(alias_pattern const &val)
        : base_type(val) { }

    pattern(parenthized_pattern const &val)
        : base_type(val) { }

    pattern(or_pattern const &val)
        : base_type(val) { }

    pattern(variant_pattern const &val)
        : base_type(val) { }

    pattern(polymorphic_variant_pattern const &val)
        : base_type(val) { }

    pattern(polymorphic_variant_abbrev_pattern const &val)
        : base_type(val) { }

    pattern(tuple_pattern const &val)
        : base_type(val) { }

    pattern(record_pattern const &val)
        : base_type(val) { }

    pattern(variant_list_pattern const &val)
        : base_type(val) { }

    pattern(variant_non_empty_list_pattern const &val)
        : base_type(val) { }

    pattern(array_pattern const &val)
        : base_type(val) { }
};

// _
struct any_value_pattern
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens anyValue;

};

// pattern as  value-name
struct alias_pattern
    : tagged
{
    pattern pattrn;
    value_name name;
};

// ( pattern :  typexpr )
struct parenthized_pattern
    : tagged
{
    pattern pattrn;
    boost::optional<typexpr> type;
};

//  pattern |  pattern
struct or_pattern
    : tagged
{
    pattern left;
    pattern right;
};

// constr  pattern
struct variant_pattern
    : tagged
{
    constr constr_;
    pattern pattrn;
};

// `tag-name  pattern
struct polymorphic_variant_pattern
    : tagged
{
    tag_name tag;
    pattern pattrn;
};

// #typeconstr
struct polymorphic_variant_abbrev_pattern
    : tagged
{
    typeconstr constr;
};

typedef std::vector<pattern> pattern_list;

// pattern  { , pattern }+
struct tuple_pattern
    : tagged
{
    pattern pattrn;
    pattern_list other;
};

struct record_field
    : tagged
{
    field name;
    pattern pattrn;
};

typedef std::vector<record_field> record_field_list;

// { field =  pattern  { ; field =  pattern }  [ ; ] }
struct record_pattern
    : tagged
{
    record_field first;
    boost::optional<record_field_list> other;
};

// [ pattern  { ; pattern }  [ ; ] ]
struct variant_list_pattern
    : tagged
{
    pattern first;
    boost::optional<pattern_list> other;
};

//  pattern :: pattern
struct variant_non_empty_list_pattern
    : tagged
{
    pattern head;
    pattern tail;
};

// [| pattern  { ; pattern }  [ ; ] |]
struct array_pattern
    : tagged
{
    pattern first;
    boost::optional<pattern_list> other;
};

//
// Expressions
//

//
//  BNF-like notation:
//  ========================================================================
//
//    expr	::=	value-path
//          ∣	 constant
//          ∣	 ( expr )
//          ∣	 begin expr end
//          ∣	 ( expr :  typexpr )
//          ∣	 expr  {, expr}+
//          ∣	 constr  expr
//          ∣	 `tag-name  expr
//          ∣	 expr ::  expr
//          ∣	 [ expr  { ; expr }  [;] ]
//          ∣	 [| expr  { ; expr }  [;] |]
//          ∣	 { field =  expr  { ; field =  expr }  [;] }
//          ∣	 { expr with  field =  expr  { ; field =  expr }  [;] }
//          ∣	 expr  { argument }+
//          ∣	 prefix-symbol  expr
//          ∣	 - expr
//          ∣	 -. expr
//          ∣	 expr  infix-op  expr
//          ∣	 expr .  field
//          ∣	 expr .  field <-  expr
//          ∣	 expr .(  expr )
//          ∣	 expr .(  expr ) <-  expr
//          ∣	 expr .[  expr ]
//          ∣	 expr .[  expr ] <-  expr
//          ∣	 if expr then  expr  [ else expr ]
//          ∣	 while expr do  expr done
//          ∣	 for value-name =  expr  ( to ∣  downto ) expr do  expr done
//          ∣	 expr ;  expr
//          ∣	 match expr with  pattern-matching
//          ∣	 function pattern-matching
//          ∣	 fun multiple-matching
//          ∣	 try expr with  pattern-matching
//          ∣	 let [rec] let-binding  { and let-binding } in  expr
//          ∣	 new class-path
//          ∣	 object class-body end
//          ∣	 expr #  method-name
//          ∣	 inst-var-name
//          ∣	 inst-var-name <-  expr
//          ∣	 ( expr :>  typexpr )
//          ∣	 ( expr :  typexpr :>  typexpr )
//          ∣	 {< [ inst-var-name =  expr  { ; inst-var-name =  expr }  [;] ] >}
//
//    argument	::=	expr
//          ∣	 ~ label-name
//          ∣	 ~ label-name :  expr
//          ∣	 ? label-name
//          ∣	 ? label-name :  expr
//
//    pattern-matching	::=	[ | ] pattern  [when expr] ->  expr  { | pattern  [when expr] ->  expr }
//
//    multiple-matching	::=	{ parameter }+  [when expr] ->  expr
//
//    let-binding	::=	pattern =  expr
//          ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr
//
//    parameter	::=	pattern
//          ∣	 ~ label-name
//          ∣	 ~ ( label-name  [: typexpr] )
//          ∣	 ~ label-name :  pattern
//          ∣	 ? label-name
//          ∣	 ? ( label-name  [: typexpr]  [= expr] )
//          ∣	 ? label-name :  pattern
//          ∣	 ? label-name : (  pattern  [: typexpr]  [= expr] )
//

struct begin_end_parenthized_expr;
struct parenthized_expr;
struct parenthized_coercion_expr;
struct tuple_expr;
struct unary_variant_expr;
struct polymorphic_variant_expr;
struct non_empty_list_variant_expr;
struct list_variant_expr;
struct array_variant_expr;
struct record_expr;
struct with_record_expr;
struct function_application_expr;
struct prefix_symbol_expr;
struct integer_negation_expr;
struct float_negation_float_expr;
struct infix_op_expr;
struct record_access_field_expr;
struct record_mutate_field_expr;
struct array_index_access_expr;
struct array_index_mutate_expr;
struct string_index_access_expr;
struct string_index_mutate_expr;
struct if_expr;
struct while_expr;
struct for_expr;
struct sequence_expr;
struct match_expr;
struct function_expr;
struct fun_expr;
struct try_expr;
struct let_expr;
struct new_expr;
struct object_expr;
struct method_invocation_expr;
struct instance_var_mutate_expr;
struct object_duplication_expr;

struct expr
    : tagged
      , boost::spirit::extended_variant<
        value_path,
        constant,
        boost::recursive_wrapper<begin_end_parenthized_expr>,
        boost::recursive_wrapper<parenthized_expr>,
        boost::recursive_wrapper<parenthized_coercion_expr>,
        boost::recursive_wrapper<tuple_expr>,
        boost::recursive_wrapper<unary_variant_expr>,
        boost::recursive_wrapper<polymorphic_variant_expr>,
        boost::recursive_wrapper<non_empty_list_variant_expr>,
        boost::recursive_wrapper<list_variant_expr>,
        boost::recursive_wrapper<array_variant_expr>,
        boost::recursive_wrapper<record_expr>,
        boost::recursive_wrapper<with_record_expr>,
        boost::recursive_wrapper<function_application_expr>,
        boost::recursive_wrapper<prefix_symbol_expr>,
        boost::recursive_wrapper<integer_negation_expr>,
        boost::recursive_wrapper<float_negation_float_expr>,
        boost::recursive_wrapper<infix_op_expr>,
        boost::recursive_wrapper<record_access_field_expr>,
        boost::recursive_wrapper<record_mutate_field_expr>,
        boost::recursive_wrapper<array_index_access_expr>,
        boost::recursive_wrapper<array_index_mutate_expr>,
        boost::recursive_wrapper<string_index_access_expr>,
        boost::recursive_wrapper<string_index_mutate_expr>,
        boost::recursive_wrapper<if_expr>,
        boost::recursive_wrapper<while_expr>,
        boost::recursive_wrapper<for_expr>,
        boost::recursive_wrapper<sequence_expr>,
        boost::recursive_wrapper<match_expr>,
        boost::recursive_wrapper<function_expr>,
        boost::recursive_wrapper<fun_expr>,
        boost::recursive_wrapper<try_expr>,
        boost::recursive_wrapper<let_expr>,
        boost::recursive_wrapper<new_expr>,
        boost::recursive_wrapper<object_expr>,
        boost::recursive_wrapper<method_invocation_expr>,
        inst_var_name,
        boost::recursive_wrapper<instance_var_mutate_expr>,
        boost::recursive_wrapper<object_duplication_expr>
    >
{
    expr()
        : base_type() { }

    expr(value_path const &val)
        : base_type(val) { }

    expr(constant const &val)
        : base_type(val) { }

    expr(begin_end_parenthized_expr const &val)
        : base_type(val) { }

    expr(parenthized_expr const &val)
        : base_type(val) { }

    expr(parenthized_coercion_expr const &val)
        : base_type(val) { }

    expr(tuple_expr const &val)
        : base_type(val) { }

    expr(unary_variant_expr const &val)
        : base_type(val) { }

    expr(polymorphic_variant_expr const &val)
        : base_type(val) { }

    expr(non_empty_list_variant_expr const &val)
        : base_type(val) { }

    expr(list_variant_expr const &val)
        : base_type(val) { }

    expr(array_variant_expr const &val)
        : base_type(val) { }

    expr(record_expr const &val)
        : base_type(val) { }

    expr(with_record_expr const &val)
        : base_type(val) { }

    expr(function_application_expr const &val)
        : base_type(val) { }

    expr(prefix_symbol_expr const &val)
        : base_type(val) { }

    expr(integer_negation_expr const &val)
        : base_type(val) { }

    expr(float_negation_float_expr const &val)
        : base_type(val) { }

    expr(infix_op_expr const &val)
        : base_type(val) { }

    expr(record_access_field_expr const &val)
        : base_type(val) { }

    expr(record_mutate_field_expr const &val)
        : base_type(val) { }

    expr(array_index_access_expr const &val)
        : base_type(val) { }

    expr(array_index_mutate_expr const &val)
        : base_type(val) { }

    expr(string_index_access_expr const &val)
        : base_type(val) { }

    expr(string_index_mutate_expr const &val)
        : base_type(val) { }

    expr(if_expr const &val)
        : base_type(val) { }

    expr(while_expr const &val)
        : base_type(val) { }

    expr(for_expr const &val)
        : base_type(val) { }

    expr(sequence_expr const &val)
        : base_type(val) { }

    expr(match_expr const &val)
        : base_type(val) { }

    expr(function_expr const &val)
        : base_type(val) { }

    expr(fun_expr const &val)
        : base_type(val) { }

    expr(try_expr const &val)
        : base_type(val) { }

    expr(let_expr const &val)
        : base_type(val) { }

    expr(new_expr const &val)
        : base_type(val) { }

    expr(object_expr const &val)
        : base_type(val) { }

    expr(method_invocation_expr const &val)
        : base_type(val) { }

    expr(inst_var_name const &val)
        : base_type(val) { }

    expr(instance_var_mutate_expr const &val)
        : base_type(val) { }

    expr(object_duplication_expr const &val)
        : base_type(val) { }
};

struct argument_label;
struct argument_optlabel;

struct argument
    : tagged
      , boost::spirit::extended_variant<
        label,
        boost::recursive_wrapper<argument_label>,
        optlabel,
        boost::recursive_wrapper<argument_optlabel>
    >
{
    argument()
        : base_type() { }

    argument(label const &val)
        : base_type(val) { }

    argument(argument_label const &val)
        : base_type(val) { }

    argument(optlabel const &val)
        : base_type(val) { }

    argument(argument_optlabel const &val)
        : base_type(val) { }

};

struct argument_label
    : tagged
{
    label name;
    expr expr_;
};

struct argument_optlabel
    : tagged
{
    optlabel name;
    expr expr_;
};

struct pattern_match
    : tagged
{
    pattern pattrn;
    boost::optional<expr> when;
    expr returnExpr;
};

typedef std::vector<pattern_match> pattern_match_list;

struct pattern_matching
    : tagged
{
    pattern_match first;
    boost::optional<pattern_match_list> other;
};

struct parameter_label_with_typexpr;
struct parameter_label_with_pattern;
struct parameter_optlabel_with_typexpr;
struct parameter_optlabel_with_pattern;
struct parameter_optlabel_with_pattern_typexpr_expr;

struct parameter
    : tagged
      , boost::spirit::extended_variant<
        pattern,
        label,
        boost::recursive_wrapper<parameter_label_with_typexpr>,
        boost::recursive_wrapper<parameter_label_with_pattern>,
        optlabel,
        boost::recursive_wrapper<parameter_optlabel_with_typexpr>,
        boost::recursive_wrapper<parameter_optlabel_with_pattern>,
        boost::recursive_wrapper<parameter_optlabel_with_pattern_typexpr_expr>
    >
{
    parameter()
        : base_type() { }

    parameter(pattern const &val)
        : base_type(val) { }

    parameter(label const &val)
        : base_type(val) { }

    parameter(parameter_label_with_typexpr const &val)
        : base_type(val) { }

    parameter(parameter_label_with_pattern const &val)
        : base_type(val) { }

    parameter(optlabel const &val)
        : base_type(val) { }

    parameter(parameter_optlabel_with_typexpr const &val)
        : base_type(val) { }

    parameter(parameter_optlabel_with_pattern const &val)
        : base_type(val) { }

    parameter(parameter_optlabel_with_pattern_typexpr_expr const &val)
        : base_type(val) { }
};

struct parameter_label_with_typexpr
    : tagged
{
    label name;
    boost::optional<typexpr> type;
};

struct parameter_label_with_pattern
    : tagged
{
    label name;
    pattern pattrn;
};

struct parameter_optlabel_with_typexpr
    : tagged
{
    optlabel name;
    boost::optional<typexpr> type;
    boost::optional<expr> expt;
};

struct parameter_optlabel_with_pattern
    : tagged
{
    optlabel name;
    pattern pattrn;
};

struct parameter_optlabel_with_pattern_typexpr_expr
    : tagged
{
    optlabel name;
    pattern pattrn;
    boost::optional<typexpr> type;
    boost::optional<expr> expt;
};

typedef std::vector<parameter> parameter_list;

struct multiple_matching
    : tagged
{
    parameter_list parameters;
    boost::optional<expr> when;
    expr returnExpr;
};

struct let_binding_pattern;
struct let_binding_value;

struct let_binding
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<let_binding_pattern>,
        boost::recursive_wrapper<let_binding_value>
    >
{
    let_binding()
        : base_type() { }

    let_binding(let_binding_pattern const &val)
        : base_type(val) { }

    let_binding(let_binding_value const &val)
        : base_type(val) { }
};

struct let_binding_pattern
    : tagged
{
    pattern pattrn;
    expr expr_;
};

struct let_binding_value
    : tagged
{
    value_name name;
    boost::optional<parameter_list> params;
    boost::optional<typexpr> type;
    boost::optional<typexpr> coercion;
    expr expr_;
};

struct begin_end_parenthized_expr
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens begin;
    expr expr_;
    ocaml::lexer::Tokens end;
};

struct parenthized_expr
    : tagged
{
    expr expr_;
    typexpr type;
};

struct parenthized_coercion_expr
    : tagged
{
    expr expr_;
    boost::optional<typexpr> type;
    typexpr coercion;
};

typedef std::vector<expr> expr_list;

struct tuple_expr
    : tagged
{
    expr first;
    expr_list other;
};

struct unary_variant_expr
    : tagged
{
    constr constr_;
    expr expr_;
};

struct polymorphic_variant_expr
    : tagged
{
    tag_name tag;
    expr expr_;
};

struct non_empty_list_variant_expr
    : tagged
{
    expr head;
    expr tail;
};

struct list_variant_expr
    : tagged
{
    expr first;
    boost::optional<expr_list> other;
};

struct array_variant_expr
    : tagged
{
    expr first;
    boost::optional<expr_list> other;
};

struct record_expr
    : tagged
{
    record_field first;
    boost::optional<record_field_list> other;
};

struct with_record_expr
    : tagged
{
    expr expr_;
    record_field with;
    boost::optional<record_field_list> withOther;
};

typedef std::vector<argument> argument_list;

struct function_application_expr
    : tagged
{
    expr expr_;
    argument_list args;
};

struct prefix_symbol_expr
    : tagged
{
    prefix_symbol symbol;
    expr expr_;
};

struct integer_negation_expr
    : tagged
{
    expr expr_;
};

struct float_negation_float_expr
    : tagged
{
    expr expr_;
};

struct infix_op_expr
    : tagged
{
    expr left;
    infix_op operation;
    expr right;
};

struct record_access_field_expr
    : tagged
{
    expr expr_;
    field field_;
};

struct record_mutate_field_expr
    : tagged
{
    expr expr_;
    field field_;
    expr mutation;
};

struct array_index_access_expr
    : tagged
{
    expr _expr;
    expr index;
};

struct array_index_mutate_expr
    : tagged
{
    expr _expr;
    expr index;
    expr mutation;
};

struct string_index_access_expr
    : tagged
{
    expr _expr;
    expr index;
};

struct string_index_mutate_expr
    : tagged
{
    expr _expr;
    expr index;
    expr mutation;
};

struct if_expr
    : tagged
{
    expr if_;
    expr then;
    boost::optional<expr> else_;
};

struct while_expr
    : tagged
{
    expr while_;
    expr do_;
};

struct for_expr
    : tagged
{
    value_name name;
    expr from;
    expr to;
    expr do_;
};

struct sequence_expr
    : tagged
{
    expr first;
    expr second;
};

struct match_expr
    : tagged
{
    expr expr_;
    pattern_matching with;
};

struct function_expr
    : tagged
{
    pattern_matching function;
};

struct fun_expr
    : tagged
{
    multiple_matching fun;
};

struct try_expr
    : tagged
{
    expr try_;
    pattern_matching with;
};

typedef std::vector<let_binding> let_binding_list;

struct let_expr
    : tagged
{
    let_binding let;
    boost::optional<let_binding_list> and_;
    expr in;
};

struct new_expr
    : tagged
{
    class_path new_;
};

struct class_body;

struct object_expr
    : tagged
{
    // FIXME: Declare class_body before this struct
    boost::recursive_wrapper<class_body> object;
};

struct method_invocation_expr
    : tagged
{
    expr expr_;
    method_name name;
};

struct instance_var_mutate_expr
    : tagged
{
    inst_var_name name;
    expr mutation;
};

// {< [ inst-var-name =  expr  { ; inst-var-name =  expr }  [;] ] >}
struct inst_var_name_expr
    : tagged
{
    inst_var_name name;
    expr expr_;
};

typedef std::vector<inst_var_name_expr> inst_var_name_expr_list;

struct object_duplication_expr
    : tagged
{
    inst_var_name_expr first;
    inst_var_name_expr_list other;
};

//
// Type and exception definitions
//

//
//  BNF-like notation:
//  ========================================================================
//
//    type-definition	::=	type typedef  { and typedef }
//
//    typedef	::=	[type-params]  typeconstr-name  type-information
//
//    type-information	::=	[type-equation]  [type-representation]  { type-constraint }
//
//    type-equation	::=	= typexpr
//
//    type-representation	::=	= [|] constr-decl  { | constr-decl }
//          ∣	 = { field-decl  { ; field-decl }  [;] }
//
//    type-params	::=	type-param
//          ∣	 ( type-param  { , type-param } )
//
//    type-param	::=	[variance] '  ident
//
//    variance	::=	+
//          ∣	 -
//
//    constr-decl	::=	(constr-name ∣  ()) [ of typexpr  { * typexpr } ]
//
//    field-decl	::=	[mutable] field-name :  poly-typexpr
//
//    type-constraint	::=	constraint ' ident =  typexpr
//
//    exception-definition	::=	exception constr-name  [ of typexpr  { * typexpr } ]
//          ∣	 exception constr-name =  constr
//

struct type_constraint
    : tagged
{
    ident ident_;
    typexpr type;
};

struct field_decl
    : tagged
{
    field_name name;
    polymorphic_typexpr type;
};

struct constr_decl
    : tagged
{
    boost::optional<constr_name> name;
    boost::optional<typexpr_list> types;
};

// + | -
struct variance
    : tagged
{
    ocaml::lexer::Tokens token;
};

struct type_param
    : tagged
{
    boost::optional<variance> var;
    ident ident_;
};

typedef std::vector<type_param> type_param_list;

struct type_params
    : tagged
      , boost::spirit::extended_variant<type_param,
        type_param_list>
{
    type_params()
        : base_type() { }

    type_params(type_param const &val)
        : base_type(val) { }

    type_params(type_param_list const &val)
        : base_type(val) { }
};

typedef std::vector<constr_decl> constr_decl_list;
typedef std::vector<field_decl> field_decl_list;

struct type_representation
    : tagged
      , boost::spirit::extended_variant<constr_decl_list, field_decl_list>
{
    type_representation()
        : base_type() { }

    type_representation(constr_decl_list const &val)
        : base_type(val) { }

    type_representation(field_decl_list const &val)
        : base_type(val) { }
};

struct type_equation
    : tagged
{
    typexpr type;
};

typedef std::vector<type_constraint> type_constraint_list;

struct type_information
    : tagged
{
    boost::optional<type_equation> equation;
    boost::optional<type_representation> representation;
    boost::optional<type_constraint_list> constraints;
};

struct typedef_
    : tagged
{
    boost::optional<type_params> params;
    typeconstr_name name;
    type_information information;
};

typedef std::vector<typedef_> typedef_list;

struct type_definition
    : tagged
{
    typedef_ typeDef;
    boost::optional<typedef_list> and_;
};

struct exception_definition_new
    : tagged
{
    constr_name name;
    boost::optional<typexpr_list> types;
};

struct exception_definition_alias
    : tagged
{
    constr_name name;
    constr constr_;
};

struct exception_definition
    : tagged
      , boost::spirit::extended_variant<
        exception_definition_new,
        exception_definition_alias
    >
{
    exception_definition()
        : base_type() { }

    exception_definition(exception_definition_new const &val)
        : base_type(val) { }

    exception_definition(exception_definition_alias const &val)
        : base_type(val) { }
};


//
// Classes
//

//
//  BNF-like notation:
//  ========================================================================
//
//    class-type	::=	[[?]label-name:]  typexpr ->  class-type
//          ∣	   class-body-type
//
//    class-body-type	::=	object [( typexpr )]  {class-field-spec} end
//          ∣	  [[ typexpr  {, typexpr} ]]  classtype-path
//
//    class-field-spec	::=	inherit class-body-type
//          ∣	  val [mutable] [virtual] inst-var-name :  typexpr
//          ∣	  val virtual mutable inst-var-name :  typexpr
//          ∣	  method [private] [virtual] method-name :  poly-typexpr
//          ∣	  method virtual private method-name :  poly-typexpr
//          ∣	  constraint typexpr =  typexpr
//
//    class-expr	::=	class-path
//          ∣	  [ typexpr  {, typexpr} ]  class-path
//          ∣	  ( class-expr )
//          ∣	  ( class-expr :  class-type )
//          ∣	  class-expr  {argument}+
//          ∣	  fun {parameter}+ ->  class-expr
//          ∣	  let [rec] let-binding  {and let-binding} in  class-expr
//          ∣	  object class-body end
//
//    class-field	::=	inherit class-expr  [as lowercase-ident]
//          ∣	  val [mutable] inst-var-name  [: typexpr] =  expr
//          ∣	  val [mutable] virtual inst-var-name :  typexpr
//          ∣	  val virtual mutable inst-var-name :  typexpr
//          ∣	  method [private] method-name  {parameter}  [: typexpr] =  expr
//          ∣	  method [private] method-name :  poly-typexpr =  expr
//          ∣	  method [private] virtual method-name :  poly-typexpr
//          ∣	  method virtual private method-name :  poly-typexpr
//          ∣	  constraint typexpr =  typexpr
//          ∣	  initializer expr
//
//    class-body	::=	  [( pattern  [: typexpr] )]  { class-field }
//
//    class-definition	::=	class class-binding  { and class-binding }
//
//    class-binding	::=	[virtual] [[ type-parameters ]]  class-name  {parameter}  [: class-type]  =  class-expr
//
//    type-parameters	::=	' ident  { , ' ident }
//
//    class-specification	::=	class class-spec  { and class-spec }
//
//    class-spec	::=	[virtual] [[ type-parameters ]]  class-name :  class-type
//
//    classtype-definition	::=	class type classtype-def  { and classtype-def }
//
//    classtype-def	::=	[virtual] [[ type-parameters ]]  class-name =  class-body-type
//

struct class_field_spec_val
    : tagged
{
    inst_var_name name;
    typexpr type;
};

struct class_field_spec_method
    : tagged
{
    method_name name;
    polymorphic_typexpr type;
};

struct class_field_spec_constraint
    : tagged
{
    typexpr first;
    typexpr second;
};

struct class_body_type;

struct class_field_spec
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<class_body_type>,
        class_field_spec_val,
        class_field_spec_method,
        class_field_spec_constraint
    >
{
    class_field_spec()
        : base_type() { }

    class_field_spec(class_body_type const &val)
        : base_type(val) { }

    class_field_spec(class_field_spec_val const &val)
        : base_type(val) { }

    class_field_spec(class_field_spec_method const &val)
        : base_type(val) { }

    class_field_spec(class_field_spec_constraint const &val)
        : base_type(val) { }
};

typedef std::vector<class_field_spec> class_field_spec_list;

struct class_body_type_object
    : tagged
{
    boost::optional<typexpr> type;
    boost::optional<class_field_spec_list> fields;
};

struct class_body_type_parametric
    : tagged
{
    boost::optional<typexpr_list> types;
    classtype_path path;
};

struct class_body_type
    : tagged
      , boost::spirit::extended_variant<
        class_body_type_object,
        class_body_type_parametric>
{
    class_body_type()
        : base_type() { }

    class_body_type(class_body_type_object const &val)
        : base_type(val) { }

    class_body_type(class_body_type_parametric const &val)
        : base_type(val) { }
};

struct class_function_type;

struct class_type
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<class_function_type>,
        class_body_type
    >
{
    class_type()
        : base_type() { }

    class_type(class_function_type const &val)
        : base_type(val) { }

    class_type(class_body_type const &val)
        : base_type(val) { }
};

struct class_function_type
    : tagged
{
    boost::optional<label_name> name;
    typexpr type;
    class_type classType;
};

struct class_expr;

struct class_field_class_expr
    : tagged
{
    boost::recursive_wrapper<class_expr> expr;
    boost::optional<lowercase_ident> ident;
};

struct class_field_val_expr
    : tagged
{
    inst_var_name name;
    boost::optional<typexpr> type;
    expr expr_;
};

struct class_field_val
    : tagged
{
    inst_var_name name;
    typexpr type;
};

struct class_field_method_typexpr_expr
    : tagged
{
    method_name name;
    boost::optional<parameter_list> params;
    boost::optional<typexpr> type;
    expr expr_;
};

struct class_field_method_expr
    : tagged
{
    method_name name;
    polymorphic_typexpr type;
    expr expr_;
};

struct class_field_method
    : tagged
{
    method_name name;
    polymorphic_typexpr type;
};

struct class_field_constraint
    : tagged
{
    typexpr first;
    typexpr second;
};

struct class_field_initializer
    : tagged
{
    expr expr_;
};

struct class_field
    : tagged
      , boost::spirit::extended_variant<
        class_field_class_expr,
        class_field_val_expr,
        class_field_val,
        class_field_method_typexpr_expr,
        class_field_method_expr,
        class_field_method,
        class_field_constraint,
        class_field_initializer
    >
{
    class_field()
        : base_type() { }

    class_field(class_field_class_expr const &val)
        : base_type(val) { }

    class_field(class_field_val_expr const &val)
        : base_type(val) { }

    class_field(class_field_val const &val)
        : base_type(val) { }

    class_field(class_field_method_typexpr_expr const &val)
        : base_type(val) { }

    class_field(class_field_method_expr const &val)
        : base_type(val) { }

    class_field(class_field_method const &val)
        : base_type(val) { }

    class_field(class_field_constraint const &val)
        : base_type(val) { }

    class_field(class_field_initializer const &val)
        : base_type(val) { }
};

struct class_expr_parameterized;
struct class_expr_class_type;
struct class_expr_arguments;
struct class_expr_function;
struct class_expr_let_binding;
struct class_expr_object;

struct class_expr
    : tagged
      , boost::spirit::extended_variant<
        class_path,
        boost::recursive_wrapper<class_expr_parameterized>,
        boost::recursive_wrapper<class_expr_class_type>,
        boost::recursive_wrapper<class_expr_arguments>,
        boost::recursive_wrapper<class_expr_function>,
        boost::recursive_wrapper<class_expr_let_binding>,
        boost::recursive_wrapper<class_expr_object>
    >
{
    class_expr()
        : base_type() { }

    class_expr(class_path const &val)
        : base_type(val) { }

    class_expr(class_expr_parameterized const &val)
        : base_type(val) { }

    class_expr(class_expr_class_type const &val)
        : base_type(val) { }

    class_expr(class_expr_arguments const &val)
        : base_type(val) { }

    class_expr(class_expr_function const &val)
        : base_type(val) { }

    class_expr(class_expr_let_binding const &val)
        : base_type(val) { }

    class_expr(class_expr_object const &val)
        : base_type(val) { }
};

struct class_expr_parameterized
    : tagged
{
    typexpr type;
    boost::optional<typexpr_list> otherTypes;
    class_path path;
};

struct class_expr_class_type
    : tagged
{
    class_expr expr;
    boost::optional<class_type> type;
};

struct class_expr_arguments
    : tagged
{
    class_expr expr;
    boost::optional<argument_list> arguments;
};

struct class_expr_function
    : tagged
{
    parameter_list parameters;
    class_expr expr;
};

struct class_expr_let_binding
    : tagged
{
    let_binding let;
    boost::optional<let_binding_list> and_;
    class_expr expr;
};

struct class_expr_object
    : tagged
{
    boost::recursive_wrapper<class_body> body;
};

typedef std::vector<class_field> class_field_list;

struct class_body
    : tagged
{
    boost::optional<parenthized_pattern> pattrn;
    class_field_list fields;
};

struct type_parameters
    : tagged
{
    ident first;
    boost::optional<ident_list> other;
};

struct class_binding
    : tagged
{
    boost::optional<type_parameters> typeParams;
    class_name className;
    boost::optional<parameter_list> parameters;
    boost::optional<class_type> classType;
    class_expr classExpr;
};

typedef std::vector<class_binding> class_binding_list;

struct class_definition
    : tagged
{
    class_binding binding;
    boost::optional<class_binding_list> and_;
};

struct class_spec
    : tagged
{
    boost::optional<type_parameters> params;
    class_name name;
    class_type type;
};

typedef std::vector<class_spec> class_spec_list;

struct class_specification
    : tagged
{
    class_spec spec;
    boost::optional<class_spec_list> and_;
};

struct classtype_def
    : tagged
{
    boost::optional<type_parameters> params;
    class_name name;
    class_body_type bodyType;
};

typedef std::vector<classtype_def> classtype_def_list;

struct classtype_definition
    : tagged
{
    classtype_def def;
    boost::optional<classtype_def_list> and_;
};

//
// Module types (module specifications)
//

//
//  BNF-like notation:
//  ========================================================================
//
//    module-type	::=	modtype-path
//          ∣	 sig { specification  [;;] } end
//          ∣	 functor ( module-name :  module-type ) ->  module-type
//          ∣	 module-type with  mod-constraint  { and mod-constraint }
//          ∣	 ( module-type )
//
//    mod-constraint	::=	type [type-params]  typeconstr  type-equation
//          ∣	 module module-path =  extended-module-path
//
//    specification	::=	val value-name :  typexpr
//          ∣	 external value-name :  typexpr =  external-declaration
//          ∣	 type-definition
//          ∣	 exception constr-decl
//          ∣	 class-specification
//          ∣	 classtype-definition
//          ∣	 module module-name :  module-type
//          ∣	 module module-name  { ( module-name :  module-type ) } :  module-type
//          ∣	 module type modtype-name
//          ∣	 module type modtype-name =  module-type
//          ∣	 open module-path
//          ∣	 include module-type
//

struct specification;
struct module_type_functor;
struct module_type_with_constraint;

struct module_type
    : tagged
      , boost::spirit::extended_variant<
        modtype_path,
        boost::recursive_wrapper<specification>,
        boost::recursive_wrapper<module_type_functor>,
        boost::recursive_wrapper<module_type_with_constraint>
    >
{
    module_type()
        : base_type() { }

    module_type(modtype_path const &val)
        : base_type(val) { }

    module_type(specification const &val)
        : base_type(val) { }

    module_type(module_type_functor const &val)
        : base_type(val) { }

    module_type(module_type_with_constraint const &val)
        : base_type(val) { }
};

struct mod_constraint_type
    : tagged
{
    boost::optional<type_params> type;
    typeconstr constr;
    type_equation equation;
};

struct mod_constraint_module
    : tagged
{
    module_path path;
    extended_module_path extendedPath;
};

struct mod_constraint
    : tagged
      , boost::spirit::extended_variant<
        mod_constraint_type,
        mod_constraint_module
    >
{
    mod_constraint()
        : base_type() { }

    mod_constraint(mod_constraint_type const &val)
        : base_type(val) { }

    mod_constraint(mod_constraint_module const &val)
        : base_type(val) { }
};

struct module_type_functor
    : tagged
{
    module_name name;
    module_type type;
    module_type returnType;
};

typedef std::vector<mod_constraint> mod_constraint_list;

struct module_type_with_constraint
    : tagged
{
    module_type type;
    mod_constraint with;
    boost::optional<mod_constraint_list> and_;
};

struct specification_val
    : tagged
{
    value_name name;
    typexpr type;
};

struct external_declaration;

struct specification_external
    : tagged
{
    value_name name;
    typexpr type;
    boost::recursive_wrapper<external_declaration> external;
};

struct specification_exception
    : tagged
{
    constr_decl constr;
};

struct specification_module
    : tagged
{
    module_name name;
    module_type type;
};

typedef std::vector<specification_module> specification_module_list;

struct specification_module_parameterized
    : tagged
{
    module_name name;
    boost::optional<specification_module_list> other;
    module_type type;
};

struct specification_module_type_name
    : tagged
{
    module_name name;
};

struct specification_module_type_name_type
    : tagged
{
    module_name name;
    module_type type;
};

struct specification_open
    : tagged
{
    module_path path;
};

struct specification_include
    : tagged
{
    module_type type;
};

struct specification
    : tagged
      , boost::spirit::extended_variant<
        specification_val,
        specification_external,
        type_definition,
        specification_exception,
        class_specification,
        classtype_definition,
        specification_module,
        specification_module_parameterized,
        specification_module_type_name,
        specification_module_type_name_type,
        specification_open,
        specification_include
    >
{
    specification()
        : base_type() { }

    specification(specification_val const &val)
        : base_type(val) { }

    specification(specification_external const &val)
        : base_type(val) { }

    specification(type_definition const &val)
        : base_type(val) { }

    specification(specification_exception const &val)
        : base_type(val) { }

    specification(class_specification const &val)
        : base_type(val) { }

    specification(classtype_definition const &val)
        : base_type(val) { }

    specification(specification_module const &val)
        : base_type(val) { }

    specification(specification_module_parameterized const &val)
        : base_type(val) { }

    specification(specification_module_type_name const &val)
        : base_type(val) { }

    specification(specification_module_type_name_type const &val)
        : base_type(val) { }

    specification(specification_open const &val)
        : base_type(val) { }

    specification(specification_include const &val)
        : base_type(val) { }
};

//
// Module expressions (module implementations)
//

//
//  BNF-like notation:
//  ========================================================================
//
//    module-expr	::=	module-path
//          ∣	 struct [ module-items ] end
//          ∣	 functor ( module-name :  module-type ) ->  module-expr
//          ∣	 module-expr (  module-expr )
//          ∣	 ( module-expr )
//          ∣	 ( module-expr :  module-type )
//
//    module-items	::=	[;;] ( definition ∣  expr )  { [;;] definition ∣  ;; expr }  [;;]
//
//    definition	::=	let [rec] let-binding  { and let-binding }
//          ∣	 external value-name :  typexpr =  external-declaration
//          ∣	 type-definition
//          ∣	 exception-definition
//          ∣	 class-definition
//          ∣	 classtype-definition
//          ∣	 module module-name  { ( module-name :  module-type ) }  [ : module-type ]  =  module-expr
//          ∣	 module type modtype-name =  module-type
//          ∣	 open module-path
//          ∣	 include module-expr
//

struct definition_let_binding
    : tagged
{
    let_binding let;
    boost::optional<let_binding_list> and_;
};

struct definition_external
    : tagged
{
    value_name name;
    typexpr type;
    boost::recursive_wrapper<external_declaration> external;
};

struct definition_module_decl
    : tagged
{
    module_name name;
    module_type type;
};

struct module_expr;
typedef std::vector<definition_module_decl> definition_module_decl_list;

struct definition_module_parameterized
    : tagged
{
    module_name name;
    boost::optional<definition_module_decl_list> params;
    boost::optional<module_type> type;
    boost::recursive_wrapper<module_expr> expr;
};

struct definition_module_type
    : tagged
{
    modtype_name name;
    module_type type;
};

struct definition_open
    : tagged
{
    module_path path;
};

struct definition_include
    : tagged
{
    boost::recursive_wrapper<module_expr> expr;
};

struct definition
    : tagged
      , boost::spirit::extended_variant<
        definition_let_binding,
        definition_external,
        type_definition,
        exception_definition,
        class_definition,
        classtype_definition,
        definition_module_parameterized,
        definition_module_type,
        definition_open,
        definition_include
    >
{
    definition()
        : base_type() { }

    definition(definition_let_binding const &val)
        : base_type(val) { }

    definition(definition_external const &val)
        : base_type(val) { }

    definition(type_definition const &val)
        : base_type(val) { }

    definition(exception_definition const &val)
        : base_type(val) { }

    definition(class_definition const &val)
        : base_type(val) { }

    definition(classtype_definition const &val)
        : base_type(val) { }

    definition(definition_module_parameterized const &val)
        : base_type(val) { }

    definition(definition_module_type const &val)
        : base_type(val) { }

    definition(definition_open const &val)
        : base_type(val) { }

    definition(definition_include const &val)
        : base_type(val) { }
};

struct module_items_def_or_expr
    : tagged
      , boost::spirit::extended_variant<
        definition,
        expr
    >
{
    module_items_def_or_expr()
        : base_type() { }

    module_items_def_or_expr(definition const &val)
        : base_type(val) { }

    module_items_def_or_expr(expr const &val)
        : base_type(val) { }
};

typedef std::vector<module_items_def_or_expr> module_items_def_or_expr_list;

struct module_items
    : tagged
{
    module_items_def_or_expr first;
    boost::optional<module_items_def_or_expr_list> other;
};

struct module_expr_struct
    : tagged
{
    boost::optional<module_items> items;
};

struct module_expr_functor;
struct module_expr_type;
struct module_expr_brackets;
struct module_expr_brackets2x;

struct module_expr
    : tagged
      , boost::spirit::extended_variant<
        module_path,
        module_expr_struct,
        boost::recursive_wrapper<module_expr_functor>,
        boost::recursive_wrapper<module_expr_brackets2x>,
        boost::recursive_wrapper<module_expr_brackets>,
        boost::recursive_wrapper<module_expr_type>
    >
{
    module_expr()
        : base_type() { }

    module_expr(module_path const &val)
        : base_type(val) { }

    module_expr(module_expr_struct const &val)
        : base_type(val) { }

    module_expr(module_expr_functor const &val)
        : base_type(val) { }

    module_expr(module_expr_brackets2x const &val)
        : base_type(val) { }

    module_expr(module_expr_brackets const &val)
        : base_type(val) { }

    module_expr(module_expr_type const &val)
        : base_type(val) { }
};

struct module_expr_functor
    : tagged
{
    module_name name;
    module_type type;
    module_expr expr;
};

struct module_expr_brackets2x
    : tagged
{
    module_expr first;
    module_expr second;
};

struct module_expr_brackets
    : tagged
{
    module_expr expr;
};

struct module_expr_type
    : tagged
{
    module_expr expr;
    module_type type;
};

//
// Compilation units
//

//
//  BNF-like notation:
//  ========================================================================
//
//    unit-interface	::=	 { specification  [;;] }
//
//    unit-implementation	::=	 [ module-items ]
//

typedef std::vector<specification> specification_list;

struct unit_interface
    : tagged
{
    boost::optional<specification_list> specs;
};

struct unit_implementation
    : tagged
{
    boost::optional<module_items> items;
};

//
// Primitives
//

//
//  BNF-like notation:
//  ========================================================================
//
//  definition	::=	 ...
//          ∣	 external value-name :  typexpr =  external-declaration
//
//  external-declaration	::=	 string-literal  [ string-literal  [ string-literal ] ]
//

struct external_declaration : tagged
{
    string_literal first;
    boost::optional<string_literal> second;
    boost::optional<string_literal> third;
};

//
// Print functions for debugging
//

struct debug_output_visitor : public boost::static_visitor<>
{
    debug_output_visitor(std::ostream& out)
        : out(out)
    {
    }

    template<typename T>
    void operator()(T & field) const
    {
        out << field;
    }

    std::ostream& out;
};

//
// Lexical
//

inline std::ostream& operator<<(std::ostream& out, capitalized_ident const& ident)
{
    out << ident.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, lowercase_ident const& ident)
{
    out << ident.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, ident const& ident)
{
    out << ident.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, ident_list const& list)
{
    for(ident const& ident_ : list)
        out << ident_;

    return out;
}

inline std::ostream& operator<<(std::ostream& out, label_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, label const& label)
{
    out << label.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, optlabel const& label)
{
    out << label.name;
    return out;
}

//
// Names
//

inline std::ostream& operator<<(std::ostream& out, modtype_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, module_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, module_name_list const& list)
{
    for(module_name const& name : list)
        out << name;

    return out;
}

inline std::ostream& operator<<(std::ostream& out, module_path const& path)
{
    out << path.name << path.other;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, operation const& op)
{
    out << op.op;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, infix_symbol const& symbol)
{
    out << symbol.symbol;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, prefix_symbol const& symbol)
{
    out << symbol.symbol;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, infix_op const& op)
{
    boost::apply_visitor(debug_output_visitor(out), op);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, operator_name const& name)
{
    boost::apply_visitor(debug_output_visitor(out), name);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, value_name const& name)
{
    boost::apply_visitor(debug_output_visitor(out), name);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, method_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, value_path const& path)
{
    out << path.path << path.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, constr_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_name_list const& list)
{
    for(tag_name const& name : list)
        out << name.name;

    return out;
}

inline std::ostream& operator<<(std::ostream& out, constr const& constr_)
{
    out << constr_.path << constr_.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, typeconstr_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, typeconstr const& constr_)
{
    out << constr_.path << constr_.name;
    return out;
}

std::ostream& operator<<(std::ostream& out, extended_module_name const& name);
std::ostream& operator<<(std::ostream& out, extended_module_path const& path);

inline std::ostream& operator<<(std::ostream& out, extended_module_name_list const& list)
{
    for(extended_module_name const& name: list)
        out << name;

    return out;
}

inline std::ostream& operator<<(std::ostream& out, extended_module_name const& name)
{
    out << name.name << name.paths;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, extended_module_path_list const& list)
{
    for(extended_module_path const& path : list)
        out << path;

    return out;
}

inline std::ostream& operator<<(std::ostream& out, extended_module_path const& path)
{
    out << path.name << path.other;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, field_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, field const& field_)
{
    out << field_.path << field_.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, modtype_path const& path)
{
    out << path.path << path.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, class_name const& name)
{
    out << name.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, class_path const& path)
{
    out << path.path << path.name;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, classtype_path const& path)
{
    out << path.path << path.name;
    return out;
}

//
// Type expressions
//

inline std::ostream& operator<<(std::ostream& out, anon_type_variable_typexpr const& expr)
{
    out << expr.var;
    return out;
}

std::ostream& operator<<(std::ostream& out, typexpr const& expr);
std::ostream& operator<<(std::ostream& out, typexpr_list const& list);

inline std::ostream& operator<<(std::ostream& out, parenthesized_typexpr const& expr)
{
    out << expr.expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, constructed_nary_typexpr const& expr)
{
    out << expr.expr << expr.other << expr.constr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, object_row_typexpr const& expr)
{
    out << expr.ellipsis;
    return out;
}

std::ostream& operator<<(std::ostream& out, method_type const& type);
std::ostream& operator<<(std::ostream& out, method_type_list const& list);

inline std::ostream& operator<<(std::ostream& out, object_typexpr const& expr)
{
    out << expr.type << expr.other << expr.ellipsis;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, octothorpe_class_path_typexpr const& expr)
{
    out << expr.path;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, octothorpe_list_typexpr const& expr)
{
    out << expr.expr << expr.other << expr.path;
    return out;
}

std::ostream& operator<<(std::ostream& out, polymorphic_variant_type const& type);
std::ostream& operator<<(std::ostream& out, typexpr const& expr);
std::ostream& operator<<(std::ostream& out, typexpr_list const& list);

inline std::ostream& operator<<(std::ostream& out, explicit_polymorphic_typexpr const& expr)
{
    out << expr.identList << expr.expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, polymorphic_typexpr const& expr)
{
    boost::apply_visitor(debug_output_visitor(out), expr);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, method_type const& type)
{
    out << type.name << type.expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, method_type_list const& list)
{
    for(method_type const& type : list)
        out << type;

    return out;
}

std::ostream& operator<<(std::ostream& out, tag_spec_first const& spec);
std::ostream& operator<<(std::ostream& out, tag_spec_list const& list);

inline std::ostream& operator<<(std::ostream& out, exact_variant_type const& type)
{
    out << type.first << type.other;
    return out;
}

std::ostream& operator<<(std::ostream& out, tag_spec const& spec);

inline std::ostream& operator<<(std::ostream& out, opened_variant_type const& type)
{
    out << type.first << type.other;
    return out;
}

std::ostream& operator<<(std::ostream& out, tag_spec_full const& spec);
std::ostream& operator<<(std::ostream& out, tag_spec_full_list const& list);

inline std::ostream& operator<<(std::ostream& out, closed_variant_type const& type)
{
    out << type.first << type.other << type.tags;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, polymorphic_variant_type const& type)
{
    boost::apply_visitor(debug_output_visitor(out), type);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_of const& spec)
{
    out << spec.name << spec.expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_of_list_expr_list const& exprList)
{
    out << exprList.expr << exprList.other;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_of_list const& spec)
{
    out << spec.name << spec.exprList;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_or const& spec)
{
    out << spec.expr << spec.tag;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_first const& spec)
{
    boost::apply_visitor(debug_output_visitor(out), spec);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec const& spec)
{
    boost::apply_visitor(debug_output_visitor(out), spec);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_list const& list)
{
    for(tag_spec const& spec : list)
        out << spec;

    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_full const& spec)
{
    boost::apply_visitor(debug_output_visitor(out), spec);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tag_spec_full_list const& list)
{
    for(tag_spec_full const& spec : list)
        out << spec;

    return out;
}

std::ostream& operator<<(std::ostream& out, std::shared_ptr<typexpr> const& expr);
std::ostream& operator<<(std::ostream& out, function_types_typexpr_rr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<function_types_typexpr_rr> const& expr)
{
    out << expr.get();
    return out;
}

inline std::ostream& operator<<(std::ostream& out,
    std::shared_ptr<function_types_typexpr_rr> const& expr)
{
    if (!expr)
        return out;

    out << *expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, function_types_typexpr_rr const& expr)
{
    out << expr.expr << expr.function_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, function_types_typexpr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<function_types_typexpr> const& expr)
{
    out << expr.get();
    return out;
}

std::ostream& operator<<(std::ostream& out, boost::recursive_wrapper<tuple_types_typexpr> const& expr);

inline std::ostream& operator<<(std::ostream& out, function_types_typexpr const& expr)
{
    out << expr.tuple_expr << expr.function_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, tuple_types_typexpr_rr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<tuple_types_typexpr_rr> const& expr)
{
    out << expr.get();
    return out;
}

inline std::ostream& operator<<(std::ostream& out, std::shared_ptr<tuple_types_typexpr_rr> const& expr)
{
    if (!expr)
        return out;

    out << *expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, tuple_types_typexpr_rr const& expr)
{
    out << expr.exprList << expr.tuple_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, boost::recursive_wrapper<constructed_types_typexpr> const& expr);

inline std::ostream& operator<<(std::ostream& out, tuple_types_typexpr const& expr)
{
    out << expr.constructed_expr << expr.tuple_expr_rr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<tuple_types_typexpr> const& expr)
{
    out << expr.get();
    return out;
}

std::ostream& operator<<(std::ostream& out, constructed_types_typexpr_rr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<constructed_types_typexpr_rr> const& expr)
{
    out << expr.get();
    return out;
}

inline std::ostream& operator<<(std::ostream& out, std::shared_ptr<constructed_types_typexpr_rr> const& expr)
{
    if (!expr)
        return out;

    out << *expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, constructed_types_typexpr_rr const& expr)
{
    out << expr.constr << expr.constructed_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, constructed_types_typexpr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<constructed_types_typexpr> const& expr)
{
    out << expr.get();
    return out;
}

std::ostream& operator<<(std::ostream& out, boost::recursive_wrapper<aliased_types_typexpr> const& expr);

inline std::ostream& operator<<(std::ostream& out, constructed_types_typexpr const& expr)
{
    out << expr.aliased_expr << expr.constructed_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, aliased_types_typexpr_rr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<aliased_types_typexpr_rr> const& expr)
{
    out << expr.get();
    return out;
}

inline std::ostream& operator<<(std::ostream& out, std::shared_ptr<aliased_types_typexpr_rr> const& expr)
{
    if (!expr)
        return out;

    out << *expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, aliased_types_typexpr_rr const& expr)
{
    out << expr._ident << expr.aliased_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, aliased_types_typexpr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<aliased_types_typexpr> const& expr)
{
    out << expr.get();
    return out;
}

std::ostream& operator<<(std::ostream& out, boost::recursive_wrapper<abbreviation_types_typexpr> const& expr);

inline std::ostream& operator<<(std::ostream& out, aliased_types_typexpr const& expr)
{
    out << expr.abbreviation_expr << expr.aliased_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, abbreviation_types_typexpr_rr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<abbreviation_types_typexpr_rr> const& expr)
{
    out << expr.get();
    return out;
}

inline std::ostream& operator<<(std::ostream& out, std::shared_ptr<abbreviation_types_typexpr_rr> const& expr)
{
    if (!expr)
        return out;

    out << *expr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, abbreviation_types_typexpr_rr const& expr)
{
    out << expr.path << expr.abbreviation_expr_rr;
    return out;
}

std::ostream& operator<<(std::ostream& out, abbreviation_types_typexpr const& expr);

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<abbreviation_types_typexpr> const& expr)
{
    out << expr.get();
    return out;
}

std::ostream& operator<<(std::ostream& out, boost::recursive_wrapper<non_rr_types_typexpr> const& expr);

inline std::ostream& operator<<(std::ostream& out, abbreviation_types_typexpr const& expr)
{
    out << expr.non_rr_expr << expr.abbreviation_expr_rr;
    return out;
}

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<non_rr_types_typexpr> const& expr)
{
    boost::apply_visitor(debug_output_visitor(out), expr.get());
    return out;
}

inline std::ostream& operator<<(std::ostream& out, non_rr_types_typexpr const& expr)
{
    boost::apply_visitor(debug_output_visitor(out), expr);
    return out;
}

inline std::ostream& operator<<(std::ostream& out,
    std::shared_ptr<typexpr> const& expr)
{
    if (!expr)
        return out;

    boost::apply_visitor(debug_output_visitor(out), *expr);
    return out;
}

inline std::ostream& operator<<(std::ostream& out,
    boost::recursive_wrapper<typexpr> const& expr)
{
    boost::apply_visitor(debug_output_visitor(out), expr.get());
    return out;
}

inline std::ostream& operator<<(std::ostream& out, typexpr const& expr)
{
    boost::apply_visitor(debug_output_visitor(out), expr);
    return out;
}

inline std::ostream& operator<<(std::ostream& out, typexpr_list const& list)
{
    for(typexpr const& expr : list)
        out << expr;

    return out;
}

//
// Constants
//

inline std::ostream& operator<<(std::ostream& out, const_false const& constant)
{
    out << constant.false_;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, const_true const& constant)
{
    out << constant.true_;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, const_unit const& constant)
{
    out << constant.opened << constant.closed;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, const_empty_record const& constant)
{
    out << constant.begin << constant.end;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, const_empty_list const& constant)
{
    out << constant.opened << constant.closed;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, const_empty_array const& constant)
{
    out << constant.opened << constant.closed;
    return out;
}

inline std::ostream& operator<<(std::ostream& out, constant const& constant)
{
    boost::apply_visitor(debug_output_visitor(out), constant);
    return out;
}

} // namespace ast
} // namespace ocaml

//
// Structure adaptations
//

//
// Lexical
//

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::lowercase_ident,
    (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::capitalized_ident,
    (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::ident,
    (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::label_name,
    (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::label,
    (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::optlabel,
    (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::integer_literal,
    (int, val)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::float_literal,
    (float, val)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::char_literal,
    (std::string, val)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::string_literal,
    (std::string, val)
)

//
// Names
//

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::infix_symbol,
    (std::string, symbol)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::prefix_symbol,
    (std::string, symbol)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::operation,
    (ocaml::lexer::Tokens, op)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::constr_name,
    (ocaml::ast::capitalized_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::tag_name,
    (ocaml::ast::capitalized_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::typeconstr_name,
    (ocaml::ast::lowercase_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::field_name,
    (ocaml::ast::lowercase_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::module_name,
    (ocaml::ast::capitalized_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::class_name,
    (ocaml::ast::lowercase_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::inst_var_name,
    (ocaml::ast::lowercase_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::method_name,
    (ocaml::ast::lowercase_ident, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::value_path,
    (boost::optional<ocaml::ast::module_path>, path)
    (ocaml::ast::value_name, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::constr,
    (boost::optional<ocaml::ast::module_path>, path)
    (ocaml::ast::constr_name, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::typeconstr,
    (boost::optional<ocaml::ast::extended_module_path>, path)
    (ocaml::ast::typeconstr_name, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::field,
    (boost::optional<ocaml::ast::module_path>, path)
    (ocaml::ast::field_name, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::modtype_path,
    (boost::optional<ocaml::ast::extended_module_path>, path)
    (ocaml::ast::modtype_name, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::class_path,
    (boost::optional<ocaml::ast::module_path>, path)
    (ocaml::ast::class_name, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::classtype_path,
    (boost::optional<ocaml::ast::extended_module_path>, path)
    (ocaml::ast::class_name, name)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::module_path,
    (ocaml::ast::module_name, name)
    (boost::optional<ocaml::ast::module_name_list>, other)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::extended_module_path,
    (ocaml::ast::extended_module_name, name)
    (boost::optional<ocaml::ast::extended_module_name_list>, other)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::extended_module_name,
    (ocaml::ast::module_name, name)
    (boost::optional<ocaml::ast::extended_module_path_list>, paths)
)

//
// Type expressions
//

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::method_type,
    (ocaml::ast::method_name, name)
    (ocaml::ast::polymorphic_typexpr, expr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::anon_type_variable_typexpr,
    (ocaml::lexer::Tokens, var)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::parenthesized_typexpr,
    (ocaml::ast::typexpr, expr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::constructed_nary_typexpr,
    (ocaml::ast::typexpr, expr)
    (boost::optional<ocaml::ast::typexpr_list>, other)
    (ocaml::ast::typeconstr, constr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::tag_spec_of_list_expr_list,
    (ocaml::ast::typexpr, expr)
    (boost::optional<ocaml::ast::typexpr_list>, other)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::tag_spec_of_list,
    (ocaml::ast::tag_name, name)
    (boost::optional<ocaml::ast::tag_spec_of_list_expr_list>, exprList)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::tag_spec_of,
    (ocaml::ast::tag_name, name)
    (boost::optional<ocaml::ast::typexpr>, expr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::tag_spec_or,
    (boost::optional<ocaml::ast::typexpr>, expr)
    (ocaml::ast::tag_spec, tag)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::exact_variant_type,
    (ocaml::ast::tag_spec_first, first)
    (boost::optional<ocaml::ast::tag_spec_list>, other)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::opened_variant_type,
    (boost::optional<ocaml::ast::tag_spec>, first)
    (boost::optional<ocaml::ast::tag_spec_list>, other)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::closed_variant_type,
    (ocaml::ast::tag_spec_full, first)
    (boost::optional<ocaml::ast::tag_spec_full_list>, other)
    (boost::optional<ocaml::ast::tag_name_list>, tags)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::explicit_polymorphic_typexpr,
    (boost::optional<ocaml::ast::ident_list>, identList)
    (ocaml::ast::typexpr, expr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::object_row_typexpr,
    (ocaml::lexer::Tokens, ellipsis)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::object_typexpr,
    (ocaml::ast::method_type, type)
    (boost::optional<ocaml::ast::method_type_list>, other)
    (boost::optional<ocaml::lexer::Tokens>, ellipsis)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::octothorpe_class_path_typexpr,
    (ocaml::ast::class_path, path)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::octothorpe_list_typexpr,
    (ocaml::ast::typexpr, expr)
    (boost::optional<ocaml::ast::typexpr_list>, other)
    (ocaml::ast::class_path, path)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::function_types_typexpr_rr,
    (std::shared_ptr<ocaml::ast::typexpr>, expr)
    (std::shared_ptr<ocaml::ast::function_types_typexpr_rr>, function_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::function_types_typexpr,
    (boost::optional<ocaml::ast::function_types_typexpr_label>, label)
    (boost::recursive_wrapper<ocaml::ast::tuple_types_typexpr>, tuple_expr)
    (boost::recursive_wrapper<ocaml::ast::function_types_typexpr_rr>, function_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::tuple_types_typexpr_rr,
    (ocaml::ast::typexpr_list, exprList)
    (std::shared_ptr<ocaml::ast::tuple_types_typexpr_rr>, tuple_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::tuple_types_typexpr,
    (boost::recursive_wrapper<ocaml::ast::constructed_types_typexpr>, constructed_expr)
    (boost::recursive_wrapper<ocaml::ast::tuple_types_typexpr_rr>, tuple_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::constructed_types_typexpr_rr,
    (ocaml::ast::typeconstr, constr)
    (std::shared_ptr<ocaml::ast::constructed_types_typexpr_rr>, constructed_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::constructed_types_typexpr,
    (boost::recursive_wrapper<ocaml::ast::aliased_types_typexpr>, aliased_expr)
    (boost::recursive_wrapper<ocaml::ast::constructed_types_typexpr_rr>, constructed_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::aliased_types_typexpr_rr,
    (ocaml::ast::ident, _ident)
    (std::shared_ptr<ocaml::ast::aliased_types_typexpr_rr>, aliased_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::aliased_types_typexpr,
    (boost::recursive_wrapper<ocaml::ast::abbreviation_types_typexpr>, abbreviation_expr)
    (boost::recursive_wrapper<ocaml::ast::aliased_types_typexpr_rr>, aliased_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::abbreviation_types_typexpr_rr,
    (ocaml::ast::class_path, path)
    (std::shared_ptr<ocaml::ast::abbreviation_types_typexpr_rr>, abbreviation_expr_rr)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::abbreviation_types_typexpr,
    (boost::recursive_wrapper<ocaml::ast::non_rr_types_typexpr>, non_rr_expr)
    (boost::recursive_wrapper<ocaml::ast::abbreviation_types_typexpr_rr>, abbreviation_expr_rr)
)

//
// Constants
//

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::const_false,
    (ocaml::lexer::Tokens, false_)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::const_true,
    (ocaml::lexer::Tokens, true_)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::const_unit,
    (ocaml::lexer::Tokens, opened)
    (ocaml::lexer::Tokens, closed)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::const_empty_record,
    (ocaml::lexer::Tokens, begin)
    (ocaml::lexer::Tokens, end)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::const_empty_list,
    (ocaml::lexer::Tokens, opened)
    (ocaml::lexer::Tokens, closed)
)

BOOST_FUSION_ADAPT_STRUCT(
    ocaml::ast::const_empty_array,
    (ocaml::lexer::Tokens, opened)
    (ocaml::lexer::Tokens, closed)
)

#if defined(_MSC_VER)
namespace boost
{
namespace spirit
{

// Visual Studio 2015 needs this

// The hold[] directive uses swap() to implement the rollback / commit semantics 
// for the attribute. For this reason the attribute type needs to to be usable with 
// boost::swap(needs to either define a proper overload for 
// swap(attribute_type&, attribute_type&) or expose a member function 
// attribute_type::swap(attribute_type&).
static void swap(ocaml::ast::extended_module_name &a, ocaml::ast::extended_module_name &b)
{
	boost::swap(a.name, b.name);
	boost::swap(a.paths, b.paths);
}

} // namespace spirit
} // namespace boost
#endif // _MSC_VER

#endif //FLANG_OCAMLAST_H
