use crate::hir::{HIRDeclaration, HIRExpr, HIRModule, HIRStmt};

pub fn print_hir_modules(modules: &[HIRModule]) {
    for m in modules {
        println!("Module:");
        for decl in &m.decls {
            print_hir_decl(decl, 1);
        }
    }
}

fn indent(level: usize) -> String {
    "  ".repeat(level)
}

fn print_hir_decl(decl: &HIRDeclaration, level: usize) {
    match decl {
        HIRDeclaration::Var { name, ty, init, .. } => {
            println!("{}Var: {} : {:?}", indent(level), name, ty);
            if let Some(e) = init {
                print_hir_expr(e, level + 1);
            }
        }
        HIRDeclaration::Func {
            name,
            ret_ty,
            params,
            body,
            ..
        } => {
            println!(
                "{}Func: {}({:?}) -> {:?}",
                indent(level),
                name,
                params.iter().map(|p| &p.name).collect::<Vec<_>>(),
                ret_ty
            );
            print_hir_stmt(body, level + 1);
        }
        HIRDeclaration::Class { name, fields, .. } => {
            println!("{}Class: {}", indent(level), name);
            for (ty, field_name) in fields {
                println!("{}Field: {} : {:?}", indent(level + 1), field_name, ty);
            }
        }
        HIRDeclaration::Enum { name, variants, .. } => {
            println!("{}Enum: {:?}", indent(level), name);
            for v in variants {
                println!("{}Variant: {}", indent(level + 1), v.name);
                if let Some(val) = &v.value {
                    print_hir_expr(val, level + 2);
                }
            }
        }
        HIRDeclaration::Include { name, as_name } => {
            println!(
                "{}Include: {}{}",
                indent(level),
                name,
                as_name
                    .as_ref()
                    .map(|s| format!(" as {}", s))
                    .unwrap_or_default()
            );
        }
    }
}

fn print_hir_stmt(stmt: &HIRStmt, level: usize) {
    match stmt {
        HIRStmt::Expr { expr, ty, .. } => {
            println!("{}Expr ({:?}):", indent(level), ty);
            print_hir_expr(expr, level + 1);
        }
        HIRStmt::Block { stmts, .. } => {
            println!("{}Block:", indent(level));
            for s in stmts {
                print_hir_stmt(s, level + 1);
            }
        }
        HIRStmt::VarDecl { name, ty, init, .. } => {
            println!("{}VarDecl: {} : {:?}", indent(level), name, ty);
            if let Some(e) = init {
                print_hir_expr(e, level + 1);
            }
        }
        HIRStmt::If {
            cond_then_ladder,
            else_branch,
            ..
        } => {
            println!("{}If:", indent(level));
            for (c, s) in cond_then_ladder {
                println!("{}Condition:", indent(level + 1));
                print_hir_expr(c, level + 2);
                println!("{}Then:", indent(level + 1));
                print_hir_stmt(s, level + 2);
            }
            if let Some(e) = else_branch {
                println!("{}Else:", indent(level + 1));
                print_hir_stmt(e, level + 2);
            }
        }
        HIRStmt::Switch {
            subject: expr,
            cases,
            ..
        } => {
            println!("{}Switch:", indent(level));
            print_hir_expr(expr, level + 1);
            for (constant, body) in cases {
                println!("{}Case: {:?}", indent(level + 1), constant);
                for stmt in body {
                    print_hir_stmt(stmt, level + 2);
                }
            }
        }
        HIRStmt::While { cond, body, .. } => {
            println!("{}While:", indent(level));
            print_hir_expr(cond, level + 1);
            print_hir_stmt(body, level + 1);
        }
        HIRStmt::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            println!("{}For:", indent(level));
            if let Some(i) = init {
                println!("{}Init:", indent(level + 1));
                print_hir_stmt(i, level + 2);
            }
            if let Some(c) = cond {
                println!("{}Condition:", indent(level + 1));
                print_hir_expr(c, level + 2);
            }
            if let Some(s) = step {
                println!("{}Step:", indent(level + 1));
                print_hir_expr(s, level + 2);
            }
            println!("{}Body:", indent(level + 1));
            print_hir_stmt(body, level + 2);
        }
        HIRStmt::Return { expr, .. } => {
            println!("{}Return:", indent(level));
            if let Some(e) = expr {
                print_hir_expr(e, level + 1);
            }
        }
        HIRStmt::Break { .. } => println!("{}Break", indent(level)),
        HIRStmt::Continue { .. } => println!("{}Continue", indent(level)),
    }
}

fn print_hir_expr(expr: &HIRExpr, level: usize) {
    match expr {
        HIRExpr::Ident { name, ty, .. } => println!("{}Ident: {} ({:?})", indent(level), name, ty),
        HIRExpr::Constant { value, ty, .. } => {
            println!("{}Constant: {:?} ({:?})", indent(level), value, ty)
        }
        HIRExpr::Assign { lhs, rhs, ty, .. } => {
            println!("{}Assign ({:?}):", indent(level), ty);
            print_hir_expr(lhs, level + 1);
            print_hir_expr(rhs, level + 1);
        }
        HIRExpr::Binary {
            op, lhs, rhs, ty, ..
        } => {
            println!("{}Binary ({:?}): {:?}", indent(level), ty, op);
            print_hir_expr(lhs, level + 1);
            print_hir_expr(rhs, level + 1);
        }
        HIRExpr::Unary {
            op, expr: e, ty, ..
        } => {
            println!("{}Unary ({:?}): {:?}", indent(level), ty, op);
            print_hir_expr(e, level + 1);
        }
        HIRExpr::Call { func, args, ty, .. } => {
            println!("{}Call ({:?}):", indent(level), ty);
            println!("{}{}", func, indent(level));
            for a in args {
                print_hir_expr(a, level + 1);
            }
        }
        HIRExpr::Member {
            base, field, ty, ..
        } => {
            println!("{}Member ({:?}):", indent(level), ty);
            print_hir_expr(base, level + 1);
            print_hir_expr(field, level + 1);
        }
        HIRExpr::Index {
            base, index, ty, ..
        } => {
            println!("{}Index ({:?}):", indent(level), ty);
            print_hir_expr(base, level + 1);
            print_hir_expr(index, level + 1);
        }
        HIRExpr::Conditional {
            cond,
            then_expr,
            else_expr,
            ty,
            ..
        } => {
            println!("{}Conditional ({:?}):", indent(level), ty);
            println!("{}Condition:", indent(level + 1));
            print_hir_expr(cond, level + 2);
            println!("{}Then:", indent(level + 1));
            print_hir_expr(then_expr, level + 2);
            println!("{}Else:", indent(level + 1));
            print_hir_expr(else_expr, level + 2);
        }
        HIRExpr::Cast { expr: e, ty, .. } => {
            println!("{}Cast ({:?}):", indent(level), ty);
            print_hir_expr(e, level + 1);
        }
    }
}
