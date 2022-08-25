use std::collections::HashMap;
use std::env;
use std::fs::read_to_string;
use std::process;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("{}", "отсутсвует название файла");
        process::exit(1);
    }

    let programm = read_file(&args[1]);

    let mut lexer = Lexer::new(&programm);
    let mut parser = Parser::new(lexer);

    let mut prog = parser.parse();
    let mut compiler = Compiler {
        programm: Vec::new(),
        pc: 0,
    };
    compiler.compile(prog);
    let mut prog2 = compiler.programm;
    let mut vm = VM {
        var: &mut HashMap::new(),
        stack: Vec::new(),
        pc: 0,
    };
    vm.run(prog2);
}

fn read_file(file_name: &str) -> String {
    read_to_string(file_name).expect("Cannot read file")
}

#[derive(Debug)]
enum Token {
    NUM(u32),
    ID(char),
    IF,
    ELSE,
    WHILE,
    DO,
    LBRA,
    RBRA,
    LPAR,
    RPAR,
    PLUS,
    MINUS,
    LESS,
    EQUAL,
    SEMICOLON,
    EOF,
    ERROR,
}

#[derive(Debug)]
enum Node {
    VAR(char),
    CONST(u32),
    ADD(Box<Node>, Box<Node>),
    SUB(Box<Node>, Box<Node>),
    LT(Box<Node>, Box<Node>),
    SET(Box<Node>, Box<Node>),
    IF1(Box<Node>, Box<Node>),
    IF2(Box<Node>, Box<Node>, Box<Node>),
    WHILE(Box<Node>, Box<Node>),
    DO(Box<Node>, Box<Node>),
    EMPTY,
    SEQ(Box<Node>, Box<Node>),
    EXPR(Box<Node>),
    PROG(Box<Node>),
    ERROR,
}

struct Lexer<'a> {
    programm: &'a str,
    ch: char,
    current_position: usize,
    current_token: Token,
}

impl Lexer<'_> {
    fn new<'a>(programm: &'a str) -> Lexer<'a> {
        return Lexer {
            programm,
            ch: programm.chars().nth(0).expect("Programm is empty"),
            current_position: 0,
            current_token: Token::EOF,
        };
    }

    fn error<'a>(&self, msg: &'a str) {
        eprintln!("{}", msg);
        process::exit(1);
    }

    fn getc<'a>(&mut self) {
        self.current_position = self.current_position + 1;
        self.ch = self
            .programm
            .chars()
            .nth(self.current_position)
            .unwrap_or('\0');
    }

    fn next_token(&mut self) {
        while self.ch.is_whitespace() {
            self.getc();
        }

        let mut token = Token::EOF;
        token = match self.ch {
            '\0' => Token::EOF,
            '{' => {
                self.getc();
                Token::LBRA
            }
            '}' => {
                self.getc();
                Token::RBRA
            }
            '(' => {
                self.getc();
                Token::LPAR
            }
            ')' => {
                self.getc();
                Token::RPAR
            }
            '=' => {
                self.getc();
                Token::EQUAL
            }
            '+' => {
                self.getc();
                Token::PLUS
            }
            '-' => {
                self.getc();
                Token::MINUS
            }
            '<' => {
                self.getc();
                Token::LESS
            }
            ';' => {
                self.getc();
                Token::SEMICOLON
            }
            ch if ch.is_ascii_digit() => {
                let mut number = 0;
                while self.ch.is_ascii_digit() {
                    number = number * 10 + self.ch.to_digit(10).unwrap();
                    self.getc();
                }
                Token::NUM(number)
            }
            ch if ch.is_alphabetic() => {
                let mut ident = Vec::new();
                while self.ch.is_alphabetic() {
                    ident.push(self.ch);
                    self.getc();
                }
                if ident.len() == 1 {
                    Token::ID(ident[0])
                } else {
                    let ident = ident.into_iter().collect::<String>();
                    match ident.as_str() {
                        "if" => Token::IF,
                        "else" => Token::ELSE,
                        "do" => Token::DO,
                        "while" => Token::WHILE,
                        _ => Token::ERROR,
                    }
                }
            }
            _ => {
                self.getc();
                Token::ERROR
            }
        };
        self.current_token = token;
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl Parser<'_> {
    fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }

    fn error(&self, msg: &str) {
        eprintln!("{}", msg);
        process::exit(1);
    }

    fn term(&mut self) -> Node {
        match self.lexer.current_token {
            Token::ID(a) => {
                self.lexer.next_token();
                Node::VAR(a)
            }
            Token::NUM(a) => {
                self.lexer.next_token();
                Node::CONST(a)
            }
            _ => self.paren_expr(),
        }
    }

    fn summa(&mut self) -> Node {
        let mut n = self.term();
        loop {
            match self.lexer.current_token {
                Token::PLUS => {
                    self.lexer.next_token();
                    n = Node::ADD(Box::from(n), Box::from(self.term()));
                }
                Token::MINUS => {
                    self.lexer.next_token();
                    n = Node::SUB(Box::from(n), Box::from(self.term()));
                }
                _ => {
                    n = n;
                    break;
                }
            }
        }

        return n;
    }

    fn test(&mut self) -> Node {
        let mut n = self.summa();
        match self.lexer.current_token {
            Token::LESS => {
                self.lexer.next_token();
                n = Node::LT(Box::from(n), Box::from(self.summa()));
            }
            _ => n = n,
        }

        return n;
    }

    fn expr(&mut self) -> Node {
        match self.lexer.current_token {
            Token::ID(_) => (),
            _ => {
                return self.test();
            }
        }

        let mut n = self.test();
        match n {
            Node::VAR(a) => match self.lexer.current_token {
                Token::EQUAL => {
                    self.lexer.next_token();
                    n = Node::SET(Box::from(n), Box::from(self.expr()))
                }
                _ => (),
            },
            _ => (),
        }

        return n;
    }

    fn paren_expr(&mut self) -> Node {
        match self.lexer.current_token {
            Token::LPAR => (),
            _ => self.error("'(' expected"),
        }

        self.lexer.next_token();
        let n = self.expr();
        match self.lexer.current_token {
            Token::RPAR => (),
            _ => self.error("')' expected"),
        }

        self.lexer.next_token();
        return n;
    }

    fn statement(&mut self) -> Node {
        match self.lexer.current_token {
            Token::IF => {
                self.lexer.next_token();
                let nop1 = self.paren_expr();
                let nop2 = self.statement();
                match self.lexer.current_token {
                    Token::ELSE => {
                        self.lexer.next_token();
                        let nop3 = self.statement();
                        Node::IF2(Box::from(nop1), Box::from(nop2), Box::from(nop3))
                    }
                    _ => Node::IF1(Box::from(nop1), Box::from(nop2)),
                }
            }
            Token::WHILE => {
                self.lexer.next_token();
                let n = Node::WHILE(Box::from(self.paren_expr()), Box::from(self.statement()));
                n
            }
            Token::DO => {
                self.lexer.next_token();
                let nop1 = self.statement();
                match self.lexer.current_token {
                    Token::WHILE => (),
                    _ => self.error("'while' expected"),
                }
                self.lexer.next_token();
                let nop2 = self.paren_expr();
                match self.lexer.current_token {
                    Token::SEMICOLON => (),
                    _ => self.error("';' expected"),
                }
                Node::DO(Box::from(nop1), Box::from(nop2))
            }
            Token::SEMICOLON => {
                self.lexer.next_token();
                Node::EMPTY
            }
            Token::LBRA => {
                self.lexer.next_token();
                let mut n = Node::EMPTY;
                loop {
                    match self.lexer.current_token {
                        Token::RBRA => break,
                        _ => n = Node::SEQ(Box::from(n), Box::from(self.statement())),
                    }
                }
                self.lexer.next_token();
                return n;
            }
            _ => {
                let n = Node::EXPR(Box::from(self.expr()));
                match self.lexer.current_token {
                    Token::SEMICOLON => (),
                    _ => self.error("';' expected"),
                }
                self.lexer.next_token();
                n
            }
        }
    }

    fn parse(&mut self) -> Node {
        self.lexer.next_token();
        let node = Node::PROG(Box::from(self.statement()));
        match self.lexer.current_token {
            Token::EOF => node,
            _ => {
                self.error("Invalid statement");
                node
            }
        }
    }
}

#[derive(Debug)]
enum Command {
    IFETCH(char),
    ISTORE(char),
    IPUSH(u32),
    IPOP,
    IADD,
    ISUB,
    ILT,
    JZ(usize),
    JNZ(usize),
    JMP(usize),
    HALT,
}

struct VM<'a> {
    var: &'a mut HashMap<char, u32>,
    stack: Vec<u32>,
    pc: usize,
}

impl VM<'_> {
    fn run(&mut self, programm: Vec<Command>) {
        loop {
            println!("cur_cmd: {:?}", &programm[self.pc]);
            println!("Programm = {:?}", &programm);
            let op = &programm[self.pc];
            // self.debug(op, &self.stack);
            match op {
                Command::IFETCH(a) => {
                    self.stack.push(*self.var.get(&a).unwrap());
                    self.pc = self.pc + 1;
                }
                Command::ISTORE(a) => {
                    self.var.insert(*a, self.stack.pop().unwrap());
                    self.pc = self.pc + 1;
                }
                Command::IPUSH(a) => {
                    self.stack.push(*a);
                    self.pc = self.pc + 1;
                }
                Command::IPOP => {
                    self.pc = self.pc + 1;
                }
                Command::IADD => {
                    let op1 = self.stack.pop().unwrap();
                    let op2 = self.stack.pop().unwrap();
                    self.stack.push(op1 + op2);
                    self.pc = self.pc + 1;
                }
                Command::ISUB => {
                    let op1 = self.stack.pop().unwrap();
                    let op2 = self.stack.pop().unwrap();
                    self.stack.push(op1 - op2);
                    self.pc = self.pc + 1;
                }
                Command::ILT => {
                    let op2 = self.stack.pop().unwrap();
                    let op1 = self.stack[self.stack.len() - 1];
                    self.stack.push(if op1 < op2 { 1 } else { 0 });
                    self.pc = self.pc + 1;
                }
                Command::JZ(a) => {
                    println!("Stack = {:?}", self.stack);
                    if self.stack.pop().unwrap() == 0 {
                        self.pc = *a
                    } else {
                        self.pc = self.pc + 1;
                    }
                }
                Command::JNZ(a) => {
                    if self.stack.pop().unwrap() != 0 {
                        self.pc = *a
                    } else {
                        self.pc = self.pc + 1;
                    }
                }
                Command::JMP(a) => self.pc = *a,
                Command::HALT => break,
            }
            // println!("Stack after command: {:?}", &self.stack);
        }
        println!("Stack: {:?}\nVars: {:?}", self.stack, self.var);
    }

    fn debug(&self, c: &Command, s: &Vec<u32>) {
        println!("Current command: {:?}\nCurrent stack: {:?}", c, s);
    }
}

struct Compiler {
    programm: Vec<Command>,
    pc: usize,
}

impl Compiler {
    fn gen(&mut self, command: Command) {
        self.programm.push(command);
        self.pc = self.pc + 1;
    }

    fn get_ident_node_value(&self, n: Node) -> char {
        match n {
            Node::VAR(a) => a,
            _ => '`',
        }
    }

    fn compile(&mut self, node: Node) {
        match node {
            Node::VAR(a) => self.gen(Command::IFETCH(a)),
            Node::CONST(a) => self.gen(Command::IPUSH(a)),
            Node::ADD(a, b) => {
                self.compile(*a);
                self.compile(*b);
                self.gen(Command::IADD);
            }
            Node::SUB(a, b) => {
                self.compile(*a);
                self.compile(*b);
                self.gen(Command::ISUB);
            }
            Node::LT(a, b) => {
                self.compile(*a);
                self.compile(*b);
                self.gen(Command::ILT);
            }
            Node::SET(name, value) => {
                self.compile(*value);
                self.gen(Command::ISTORE(self.get_ident_node_value(*name)))
            }
            Node::IF1(expr, statement) => {
                self.compile(*expr);
                let pc_jz = self.pc;
                self.gen(Command::JZ(0));
                self.compile(*statement);
                match self.programm[pc_jz] {
                    Command::JZ(_) => {
                        self.programm[pc_jz] = Command::JZ(self.pc);
                    }
                    _ => {
                        panic!("{}", "Incorrect address");
                    }
                }
            }
            Node::IF2(op1, op2, op3) => {
                self.compile(*op1);
                let pc_jz = self.pc;
                self.gen(Command::JZ(0));
                self.compile(*op2);
                match self.programm[pc_jz] {
                    Command::JZ(_) => {
                        self.programm[pc_jz] = Command::JZ(self.pc);
                    }
                    _ => {
                        panic!(
                            "Incorrect address, command on address {:?} = {:?}, programm = {:?}",
                            pc_jz, self.programm[pc_jz], self.programm
                        );
                    }
                }
                let pc_jmp = self.pc;
                self.gen(Command::JMP(0));
                self.compile(*op3);
                match self.programm[pc_jmp] {
                    Command::JMP(_) => {
                        self.programm[pc_jmp] = Command::JMP(self.pc);
                    }
                    _ => {
                        panic!(
                            "Incorrect address, command on address {:?} = {:?}, programm = {:?}",
                            pc_jmp, self.programm[pc_jmp], self.programm
                        );
                    }
                }
            }
            Node::WHILE(op1, op2) => {
                let addr1 = self.pc;
                self.compile(*op1);
                let addr2 = self.pc;
                self.gen(Command::JZ(0));
                self.compile(*op2);
                self.gen(Command::JMP(addr1));
                match self.programm[addr2] {
                    Command::JZ(_) => {
                        self.programm[addr2] = Command::JZ(self.pc);
                    }
                    _ => {
                        panic!(
                            "Incorrect address, command on address {:?} = {:?}, with programm lenght = {:?}\nProgramm = {:?}",
                            addr2, self.programm[addr2], self.programm.len(), self.programm
                        );
                    }
                }
            }
            Node::DO(op1, op2) => {
                let addr = self.pc;
                self.compile(*op1);
                self.compile(*op2);
                self.gen(Command::JNZ(addr));
            }
            Node::SEQ(op1, op2) => {
                self.compile(*op1);
                self.compile(*op2);
            }
            Node::PROG(op1) => {
                self.compile(*op1);
                self.gen(Command::HALT);
            }
            Node::EMPTY => (),
            Node::EXPR(op1) => {
                self.compile(*op1);
                self.gen(Command::IPOP);
            }
            Node::ERROR => todo!(),
        }
    }
}
