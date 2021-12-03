pub enum Pat {
    PatVar { name: u32 },
    PatConstr { name: String, args: Vec<Pat> },
}

pub enum Ast {
    Var { name: String },
    Match { patterns: Vec<(Pat, Ast)> },
    Constr { name: String, args: Vec<Ast> },
}

fn main() {
    println!("Hello, world!");
}
