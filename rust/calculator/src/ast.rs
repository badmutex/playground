
#[derive(Debug, Clone)]
pub enum Op { ADD,  SUB,  MUL,  DIV,  POW }

#[derive(Debug, Clone)]
pub enum Expr {
    Num(i32),
    Op(Op, Box<Expr>, Box<Expr>)
}

impl Expr {
    pub fn eval(self) -> i32 {
        match self {
            Expr::Num(i) => i,
            Expr::Op(op, l, r) => {
                let l = l.eval();
                let r = r.eval();
                match op {
                    Op::ADD => l + r,
                    Op::SUB => l - r,
                    Op::MUL => l * r,
                    Op::DIV => l / r,
                    Op::POW => l.pow(r as u32),
                }
            }
        }
    }
}
