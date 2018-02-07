//! Generates remote C bindings for a given Flipper module.
//!
//! This mod makes use of the `handlebars` rust crate. There's
//! a `templates/c.hbs` file which describes the layout of a
//! C module. To populate the template, we create serializable
//! representations of all the parameters and functions in the
//! Flipper module and pass that data to the handlebars
//! renderer.

use std::io::Write;
use std::io::Cursor;
use failure::Error;

use handlebars::{
    Handlebars,
    Helper,
    RenderContext,
    RenderError,
};

use bindings::generators::GeneratorError;
use bindings::{
    Parameter,
    Function,
    Module,
};

// For some reason the compiler gives "unused imports" warnings for these items
// even though they're used and we can't compile without them. Suppress warnings.
#[allow(unused_imports)] use std::rc::Rc;
#[allow(unused_imports)] use bindings::Type;

/// A serializable representation of a Parameter in C.
/// This is used by handlebars to populate the C template.
#[derive(Debug, Serialize, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct CParameter {
    name: String,
    typ: String,
}

/// A serializable representation of a Function in C.
/// This is used by handlebars to populate the C template.
#[derive(Debug, Serialize, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct CFunction {
    name: String,
    params: Vec<CParameter>,
    ret: String,
    ret_fmr: String,
}

/// A serializable representation of a Module in C.
/// This is used by handlebars to populate the C template.
#[derive(Debug, Serialize, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct CModule {
    name: String,
    description: String,
    funcs: Vec<CFunction>,
}

impl From<Parameter> for CParameter {
    fn from(param: Parameter) -> Self {
        CParameter {
            name: param.name,
            typ: param.typ.name(),
        }
    }
}

impl From<Function> for CFunction {
    fn from(func: Function) -> Self {
        let params: Vec<CParameter> = func.parameters.into_iter().map(|param| param.into()).collect();

        let ret = func.ret.name();
        let ret_size = func.ret.size();

        let lf_type = match ret_size {
            0 => "lf_void_t".to_owned(),
            1 => "lf_uint8_t".to_owned(),
            2 => "lf_uint16_t".to_owned(),
            4 => "lf_uint32_t".to_owned(),
            8 => "lf_uint64_t".to_owned(),
            _ => "lf_void_t".to_owned(),
        };

        CFunction {
            name: func.name,
            params,
            ret,
            ret_fmr: lf_type,
        }
    }
}

impl From<Module> for CModule {
    fn from(module: Module) -> Self {
        CModule {
            name: module.name,
            description: module.description,
            funcs: module.functions.into_iter().map(|s| s.into()).collect(),
        }
    }
}

/// C does not allow trailing commas in struct definitions, so we need
/// a custom handlebars helper which takes the appropriate struct contents
/// and appends them to the template, omitting the last comma of each entry.
fn struct_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let values = h.param(0).map(|p| p.value())
        .and_then(|value| value.as_array())
        .ok_or(RenderError::new("struct_expansion requires an array as the first argument"))?;

    let module_name = h.param(1).map(|p| p.value())
        .and_then(|value| value.as_str())
        .ok_or(RenderError::new("struct_expansion requires a string as the second argument"))?;

    let mut funcs = String::new();
    for (i, value) in values.iter().enumerate() {
        if i > 0 { funcs.push_str(",\n    "); }
        funcs.push_str(module_name);
        funcs.push('_');
        funcs.push_str(value.as_object()
            .and_then(|obj| obj.get("name"))
            .and_then(|name| name.as_str())
            .ok_or(RenderError::new("struct_expansion failed to print 'name'"))?
        );
    }

    rc.writer.write(funcs.into_bytes().as_ref())?;

    Ok(())
}

/// C does not allow trailing commas in parameter lists, so we need
/// a custom handlebars helper which takes the appropriate arguments
/// and appends them to the template, omitting the last comma of each entry.
fn param_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let values = h.param(0).map(|p| p.value())
        .and_then(|value| value.as_array())
        .ok_or(RenderError::new("param_expansion requires an array as the first argument"))?;

    let mut params = String::new();
    for (i, value) in values.iter().enumerate() {
        if i > 0 { params.push_str(", "); }
        params.push_str(value.as_object()
            .and_then(|obj| obj.get("typ"))
            .and_then(|typ| typ.as_str())
            .ok_or(RenderError::new("param_expansion failed to print 'typ'"))?
        );
        params.push(' ');
        params.push_str(value.as_object()
            .and_then(|obj| obj.get("name"))
            .and_then(|name| name.as_str())
            .ok_or(RenderError::new("param_expansion failed to print 'name'"))?
        );
    }

    rc.writer.write(params.into_bytes().as_ref())?;

    Ok(())
}

/// C does not allow trailing commas in parameter lists, so we need
/// a custom handlebars helper. However, fmr parameters need to be wrapped
/// in a `lf_infer( )` macro, so this helper does that as well.
fn fmr_expansion_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let values = h.param(0).map(|p| p.value())
        .and_then(|value| value.as_array())
        .ok_or(RenderError::new("fmr_expansion requires an array as the first argument"))?;

    let mut lf_args = String::new();
    for (i, value) in values.iter().enumerate() {
        if i > 0 { lf_args.push_str(", "); }
        lf_args.push_str("lf_infer(");
        lf_args.push_str(value.as_object()
            .and_then(|obj| obj.get("name"))
            .and_then(|name| name.as_str())
            .ok_or(RenderError::new("fmr_expansion failed to print 'name'"))?
        );
        lf_args.push(')');
    }

    rc.writer.write(lf_args.into_bytes().as_ref())?;

    Ok(())
}

/// Configures handlebars, serializes the given `CModule`, and writes it
/// to the given output.
pub fn generate_module<W: Write>(module: Module, out: &mut W) -> Result<(), Error> {
    let mut reg = Handlebars::new();
    reg.register_helper("param_expansion", Box::new(param_helper));
    reg.register_helper("struct_expansion", Box::new(struct_helper));
    reg.register_helper("fmr_expansion", Box::new(fmr_expansion_helper));

    let template_bytes: &[u8] = include_bytes!("./templates/c.hbs");
    let mut template = Cursor::new(template_bytes);
    reg.register_template_source("c", &mut template)
        .map_err(|_| GeneratorError::CRenderError("missing or malformed template file 'c.hbs'".to_owned()))?;

    let module: CModule = module.into();

    let _ = write!(out, "{}", reg.render("c", &module)
        .map_err(|e| GeneratorError::CRenderError(e.desc))?
    );

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_from_parameter() {

        // Mocked dwarf data //

        // Base types
        let b0 = Rc::new(Type::Base { name: "short unsigned int".to_owned(), size: 2 });
        let b1 = Rc::new(Type::Base { name: "unsigned int".to_owned(), size: 4 });
        let b2 = Rc::new(Type::Base { name: "char".to_owned(), size: 1 });

        // Alias types
        let a1 = Rc::new(Type::Alias { name: "uint16_t".to_owned(), typ: b0.clone() });
        let a2 = Rc::new(Type::Alias { name: "uint32_t".to_owned(), typ: b1.clone() });

        // Reference types
        let r0 = Rc::new(Type::Reference { typ: b2.clone() });

        // Parameters
        let p0 = Parameter { name: "first".to_owned(), typ: r0.clone() };
        let p1 = Parameter { name: "second".to_owned(), typ: a1.clone() };
        let p2 = Parameter { name: "third".to_owned(), typ: a2.clone() };

        // Convert dwarf representation to generator representation.
        let cp0: CParameter = p0.into();
        let cp1: CParameter = p1.into();
        let cp2: CParameter = p2.into();

        // Expected CParameters //

        let expected_cp0 = CParameter {
            name: "first".to_owned(),
            typ: "char*".to_owned(),
        };

        let expected_cp1 = CParameter {
            name: "second".to_owned(),
            typ: "uint16_t".to_owned(),
        };

        let expected_cp2 = CParameter {
            name: "third".to_owned(),
            typ: "uint32_t".to_owned(),
        };

        // Compare
        assert_eq!(cp0, expected_cp0);
        assert_eq!(cp1, expected_cp1);
        assert_eq!(cp2, expected_cp2);
    }
}