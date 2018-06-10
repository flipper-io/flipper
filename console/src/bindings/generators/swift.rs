//! Generates remote Swift bindings for a given Flipper module.
//!
//! This mod makes use of the `handlebars` rust crate. There's
//! a `templates/swift.hbs` file which describes the layout of a
//! Swift module. To populate the template, we create serializable
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

/// A serializable representation of a Parameter in Swift.
/// This is used by handlebars to populate the Swift template.
#[derive(Debug, Serialize, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct SwiftParameter {
    name: String,
    typ: String,
}

/// A serializable representation of a Function in Swift.
/// This is used by handlebars to populate the Swift template.
#[derive(Debug, Serialize, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct SwiftFunction {
    name: String,
    params: Vec<SwiftParameter>,
    ret: String,
}

/// A serializable representation of a Module in C.
/// This is used by handlebars to populate the C template.
#[derive(Debug, Serialize, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct SwiftModule {
    name: String,
    description: String,
    funcs: Vec<SwiftFunction>,
}

fn convert_to_swift(type_name: &str) -> String {
  match type_name {
    "uint8_t" => "UInt8",
    "uint16_t" => "UInt16",
    "uint32_t" => "UInt32",
    "uint64_t" => "UInt64",
    "int8_t" => "Int8",
    "int16_t" => "Int16",
    "int32_t" => "Int32",
    "int64_t" => "Int64",
    "char" => "Character",
    "bool" => "Bool",
    "int" => "Int",
    "unsigned int" => "UInt",
    "short" => "Int16",
    "unsigned short int" => "UInt16",
    "long int" => "Int64",
    _ => panic!("Unknown type {}", type_name)
  }.to_owned()
}

fn swift_name(typ: Rc<Type>) -> String {
  match *typ {
    Type::Base { ref name, .. } => convert_to_swift(name),
    Type::Alias { ref name, .. } => convert_to_swift(name),
    Type::Reference { ref typ, .. } => {
        let mut name = convert_to_swift(&typ.name()).clone();
        "UnsafePointer<".to_owned() + &name + ">"
    },
    _ => "Void".to_owned(),
  }
}

impl From<Parameter> for SwiftParameter {
    fn from(param: Parameter) -> Self {
        SwiftParameter {
            name: param.name,
            typ: swift_name(param.typ),
        }
    }
}

impl From<Function> for SwiftFunction {
    fn from(func: Function) -> Self {
        let params: Vec<SwiftParameter> = func.parameters.into_iter().map(|param| param.into()).collect();

        SwiftFunction {
            name: func.name,
            params: params,
            ret: swift_name(func.ret),
        }
    }
}

impl From<Module> for SwiftModule {
    fn from(module: Module) -> Self {
        SwiftModule {
            name: module.name,
            description: module.description,
            funcs: module.functions.into_iter().map(|s| s.into()).collect(),
        }
    }
}

/// C does not allow trailing commas in parameter lists, so we need
/// a custom handlebars helper which takes the appropriate arguments
/// and appends them to the template, omitting the last comma of each entry.
fn param_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let values = h.param(0).map(|p| p.value())
        .and_then(|value| value.as_array())
        .ok_or(RenderError::new("param_expansion requires an array as the first argument"))?;

    let mut params: Vec<String> = Vec::new();
    for value in values {
      let obj = value.as_object().unwrap();
      let name = obj.get("name")
        .and_then(|name| name.as_str())
        .ok_or(RenderError::new("param_expansion failed to print 'name'"))?;
      let typ = obj.get("typ")
        .and_then(|name| name.as_str())
        .ok_or(RenderError::new("param_expansion failed to print 'typ'"))?;
      params.push(name.to_owned() + ": " + typ);
    }

    rc.writer.write(params.join(", ").into_bytes().as_ref())?;

    Ok(())
}

/// Swift does not allow trailing commas in parameter lists, so we need
/// a custom handlebars helper.
fn args_expansion_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let values = h.param(0).map(|p| p.value())
        .and_then(|value| value.as_array())
        .ok_or(RenderError::new("args_expansion requires an array as the first argument"))?;

    let mut lf_args: Vec<&str> = Vec::new();
    for value in values {
      let name = value.as_object()
        .and_then(|obj| obj.get("name"))
        .and_then(|name| name.as_str())
        .ok_or(RenderError::new("args_expansion failed to print 'name'"))?;
      lf_args.push(name);
    }

    rc.writer.write(lf_args.join(", ").into_bytes().as_ref())?;

    Ok(())
}

/// Configures handlebars, serializes the given `SwiftModule`, and writes it
/// to the given output.
pub fn generate_module<W: Write>(module: Module, out: &mut W) -> Result<(), Error> {
    let mut reg = Handlebars::new();
    reg.register_helper("param_expansion", Box::new(param_helper));
    reg.register_helper("args_expansion", Box::new(args_expansion_helper));

    let template_bytes: &[u8] = include_bytes!("./templates/swift.hbs");
    let mut template = Cursor::new(template_bytes);
    reg.register_template_source("swift", &mut template)
        .map_err(|_| GeneratorError::SwiftRenderError("missing or malformed template file 'swift.hbs'".to_owned()))?;

    let module: SwiftModule = module.into();

    let _ = write!(out, "{}", reg.render("swift", &module)
        .map_err(|e| GeneratorError::SwiftRenderError(e.desc))?
    );

    Ok(())
}