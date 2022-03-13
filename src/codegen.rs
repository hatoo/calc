use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{IntType, PointerType, VoidType},
    values::IntValue,
    AddressSpace,
};

use crate::parser::{Expr, Operator, WithDecl, AST};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    ty_void: VoidType<'ctx>,
    ty_i32: IntType<'ctx>,
    ty_i8_ptr: PointerType<'ctx>,
    ty_i8_ptr_ptr: PointerType<'ctx>,
    i32_zero: IntValue<'ctx>,
    name_map: HashMap<String, IntValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            ty_void: context.void_type(),
            ty_i32: context.i32_type(),
            ty_i8_ptr: context.i8_type().ptr_type(AddressSpace::Generic),
            ty_i8_ptr_ptr: context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .ptr_type(AddressSpace::Generic),
            i32_zero: context.i32_type().const_int(0, true),
            name_map: Default::default(),
        }
    }

    pub fn run(&mut self, ast: &AST) {
        let ty_main_fn = self
            .ty_i32
            .fn_type(&[self.ty_i32.into(), self.ty_i8_ptr_ptr.into()], false);
        let main_fn = self
            .module
            .add_function("main", ty_main_fn, Some(Linkage::External));

        let bb = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(bb);

        let expr = match ast {
            AST::WithDecl(WithDecl { vars, expr }) => {
                let ty_read_fn = self.ty_i32.fn_type(&[self.ty_i8_ptr.into()], false);
                let read_fn =
                    self.module
                        .add_function("calc_read", ty_read_fn, Some(Linkage::External));

                for var in vars {
                    let str_text = self.context.const_string(var.as_bytes(), true);

                    let str = self.module.add_global(
                        str_text.get_type(),
                        None,
                        format!("{}.str", var).as_str(),
                    );

                    str.set_constant(true);
                    str.set_linkage(Linkage::Private);

                    str.set_initializer(&str_text);

                    let ptr = unsafe {
                        self.builder.build_in_bounds_gep(
                            str.as_pointer_value(),
                            &[self.i32_zero.into(), self.i32_zero.into()],
                            "ptr",
                        )
                    };

                    let call = self
                        .builder
                        .build_call(read_fn, &[ptr.into()], "call")
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_int_value();

                    assert!(self.name_map.insert(var.clone(), call).is_none());
                }

                expr
            }
            AST::Expr(expr) => expr,
        };

        let value = self.expr(&expr);

        let ty_write_fn = self.ty_void.fn_type(&[self.ty_i32.into()], false);
        let write_fn = self
            .module
            .add_function("calc_write", ty_write_fn, Some(Linkage::External));
        self.builder
            .build_call(write_fn, &[value.into()], "call_write");

        self.builder
            .build_return(Some(&self.ty_i32.const_int(0, false)));
    }

    fn expr(&self, expr: &Expr) -> IntValue<'ctx> {
        match expr {
            Expr::Ident(ident) => self.name_map.get(ident).unwrap().clone(),
            Expr::Number(n) => self.ty_i32.const_int(*n as _, true),
            Expr::BinaryOp { op, left, right } => {
                let left = self.expr(left);
                let right = self.expr(right);

                match op {
                    Operator::Plus => self.builder.build_int_nsw_add(left, right, "add"),
                    Operator::Minus => self.builder.build_int_nsw_sub(left, right, "sub"),
                    Operator::Mul => self.builder.build_int_nsw_mul(left, right, "mul"),
                    Operator::Div => self.builder.build_int_signed_div(left, right, "div"),
                }
            }
        }
    }
}
