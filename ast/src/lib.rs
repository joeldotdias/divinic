pub mod ast;
pub mod span;

#[cfg(test)]
mod tests {
    use super::ast::*;
    use super::span::DUMMY_SPAN;

    #[test]
    fn test_expression_builders() {
        // x
        let x = Expression::ident("x", 1, DUMMY_SPAN);
        if let Expression::Ident { name, .. } = &x {
            assert_eq!(name, "x");
        } else {
            panic!("expected ident");
        }

        // 42
        let forty_two = Expression::int(42, 2, DUMMY_SPAN);
        assert!(forty_two.is_const());

        // x = 42
        let assign = Expression::assign(x.clone(), forty_two.clone(), 3, DUMMY_SPAN);
        if let Expression::Assign { lhs, rhs, .. } = &assign {
            assert_eq!(**lhs, x);
            assert_eq!(**rhs, forty_two);
        } else {
            panic!("expected assign");
        }

        // x + 42
        let add = Expression::bin(x.clone(), BinaryOp::Add, forty_two.clone(), 4, DUMMY_SPAN);
        if let Expression::Binary { op, .. } = &add {
            assert_eq!(*op, BinaryOp::Add);
        } else {
            panic!("expected binary add");
        }

        // f(x, 42)
        let call = Expression::call(
            Expression::ident("f", 5, DUMMY_SPAN),
            vec![x.clone(), forty_two.clone()],
            6,
            DUMMY_SPAN,
        );
        if let Expression::Call { args, .. } = &call {
            assert_eq!(args.len(), 2);
        } else {
            panic!("expected call");
        }
    }
}

