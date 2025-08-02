# 13. Grammar Reference

## Grammar Notation

This grammar is written in Extended Backus-Naur Form (EBNF) with the following conventions:

- `|` denotes alternation (OR)
- `()` denotes grouping
- `[]` denotes optional elements
- `{}` denotes zero or more repetitions
- `{}+` denotes one or more repetitions
- `"text"` denotes literal text

## Lexical Grammar

### Tokens

```ebnf
Token = Keyword | Identifier | IntegerLiteral | StringLiteral | Operator

Keyword = "let" | "fun" | "do" | "end" | "return" | "if" | "else" | "while" 
        | "true" | "false" | "new" | "struct" | "as" | "type" | "extends" | "sizeof"

Identifier = Letter { Letter | Digit | "_" }
Letter = "a"..."z" | "A"..."Z" | "_"
Digit = "0"..."9"

IntegerLiteral = DecimalLiteral | HexadecimalLiteral
DecimalLiteral = Digit { Digit }
HexadecimalLiteral = "0x" HexDigit { HexDigit }
HexDigit = Digit | "a"..."f" | "A"..."F"

StringLiteral = "\"" { StringChar } "\""
StringChar = any character except "\"" and newline

Operator = "+" | "-" | "*" | "%" | "==" | "<=" | ">=" | "<" | ">" | "=" 
         | "<-" | "(" | ")" | "[" | "]" | "{" | "}" | ":" | ";" | "." | ","

Comment = "//" { any character except newline }
Whitespace = " " | "\t" | "\n" | "\r"
```

## Syntactic Grammar

### Module

```ebnf
Module = { Declaration }

Declaration = FunctionDeclaration | TypeDeclaration | GlobalDeclaration
```

### Declarations

```ebnf
FunctionDeclaration = "fun" Identifier "(" [ ParameterList ] ")" [ ":" Type ] 
                     "do" Block "end"

ParameterList = Parameter { "," Parameter }
Parameter = Identifier ":" Type

TypeDeclaration = "type" Identifier [ TypeParameterList ] "=" StructType

TypeParameterList = "(" TypeParameter { "," TypeParameter } ")"
TypeParameter = Identifier ":" "type"

GlobalDeclaration = "let" Identifier [ ":" Type ] "=" Expression ";"

StructType = "struct" "{" { StructField | MethodDeclaration } "}"
StructField = Identifier ":" Type ","
MethodDeclaration = FunctionDeclaration
```

### Types

```ebnf
Type = PrimitiveType | PointerType | IdentifierType | FunctionType

PrimitiveType = "int" | "bool" | "byte" | "type"

PointerType = "[*]" Type

IdentifierType = Identifier [ TypeArgumentList ]
TypeArgumentList = "(" Type { "," Type } ")"

FunctionType = "fun" "(" [ TypeList ] ")" [ ":" Type ]
TypeList = Type { "," Type }
```

### Statements

```ebnf
Block = { Statement } [ Expression ]

Statement = LetStatement | ReturnStatement | ExpressionStatement 
          | IfStatement | WhileStatement | AssignmentStatement | PushStatement

LetStatement = "let" Identifier [ ":" Type ] "=" Expression ";"

ReturnStatement = "return" [ Expression ] ";"

ExpressionStatement = Expression ";"

IfStatement = "if" "(" Expression ")" "do" Block "end"
            | "if" "(" Expression ")" "do" Block "else" "do" Block "end"

WhileStatement = "while" "(" Expression ")" "do" Block "end"

AssignmentStatement = LValue "=" Expression ";"

PushStatement = LValue "<-" Expression ";"

LValue = Identifier | FieldAccess | IndexAccess

FieldAccess = Expression "." Identifier

IndexAccess = Expression "[" Expression "]"
```

### Expressions

```ebnf
Expression = ConditionalExpression

ConditionalExpression = LogicalOrExpression 
                      | "if" "(" Expression ")" "do" Block 
                        [ "else" "do" Block ] "end"

LogicalOrExpression = LogicalAndExpression { "||" LogicalAndExpression }

LogicalAndExpression = EqualityExpression { "&&" EqualityExpression }

EqualityExpression = RelationalExpression { ( "==" | "!=" ) RelationalExpression }

RelationalExpression = AdditiveExpression { ( "<" | ">" | "<=" | ">=" ) AdditiveExpression }

AdditiveExpression = MultiplicativeExpression { ( "+" | "-" ) MultiplicativeExpression }

MultiplicativeExpression = CastExpression { ( "*" | "/" | "%" ) CastExpression }

CastExpression = UnaryExpression [ "as" Type ]

UnaryExpression = PrimaryExpression
                | "sizeof" Type
                | "type" Type

PrimaryExpression = Literal
                  | Identifier
                  | FieldAccess
                  | IndexAccess
                  | FunctionCall
                  | NewExpression
                  | "(" Expression ")"
                  | BlockExpression

Literal = IntegerLiteral | StringLiteral | BooleanLiteral

BooleanLiteral = "true" | "false"

FunctionCall = Expression "(" [ ArgumentList ] ")"
ArgumentList = Expression { "," Expression }

NewExpression = "new" Type "{" [ FieldInitializerList ] "}"
FieldInitializerList = FieldInitializer { "," FieldInitializer }
FieldInitializer = "." Identifier "=" Expression

BlockExpression = "do" Block "end"
```

## Operator Precedence

From highest to lowest precedence:

1. Primary expressions: literals, identifiers, parentheses, field access, indexing, function calls
2. Unary operators: `sizeof`, `type`
3. Multiplicative: `*`, `/`, `%`
4. Additive: `+`, `-`
5. Relational: `<`, `>`, `<=`, `>=`
6. Equality: `==`, `!=`
7. Logical AND: `&&` (planned)
8. Logical OR: `||` (planned)
9. Type cast: `as`
10. Assignment: `=`, `<-`

## Associativity Rules

- Binary operators are left-associative
- Assignment operators are right-associative
- Comparison operators are non-associative (cannot be chained)

## Grammar Examples

### Function Declaration

```ebnf
"fun" "add" "(" "a" ":" "int" "," "b" ":" "int" ")" ":" "int" "do"
    "return" "a" "+" "b" ";"
"end"
```

### Type Declaration

```ebnf
"type" "Point" "=" "struct" "{"
    "x" ":" "int" ","
    "y" ":" "int" ","
    
    "fun" "distance" "(" "self" ":" "Point" ")" ":" "int" "do"
        "return" "self" "." "x" "*" "self" "." "x" "+" 
                 "self" "." "y" "*" "self" "." "y" ";"
    "end"
"}"
```

### Generic Type Declaration

```ebnf
"type" "Pair" "(" "A" ":" "type" "," "B" ":" "type" ")" "=" "struct" "{"
    "first" ":" "A" ","
    "second" ":" "B"
"}"
```

### Variable Declaration

```ebnf
"let" "x" ":" "int" "=" "42" ";"
"let" "name" "=" "\"Alice\"" ";"
```

### Control Flow

```ebnf
"if" "(" "x" ">" "0" ")" "do"
    "positive" "=" "true" ";"
"else" "do"
    "positive" "=" "false" ";"
"end"

"while" "(" "i" "<" "10" ")" "do"
    "sum" "=" "sum" "+" "i" ";"
    "i" "=" "i" "+" "1" ";"
"end"
```

### Expressions

```ebnf
# Arithmetic
"a" "+" "b" "*" "c"  # Parsed as: a + (b * c)

# Function call
"add" "(" "10" "," "20" ")"

# Field access
"point" "." "x"

# Method call
"point" "." "distance" "(" ")"

# New expression
"new" "Point" "{" "." "x" "=" "10" "," "." "y" "=" "20" "}"

# Type cast
"value" "as" "byte"

# Size expression
"sizeof" "int"
```

## Ambiguity Resolution

### Statement vs Expression

The grammar resolves statement/expression ambiguity using context:

```orbit
// Statement context
x = 10;

// Expression context  
let y = x + 5;
```

### Type vs Expression

Type contexts are distinguished from expression contexts:

```orbit
// Type context
let x: int = 42;
fun f(param: Point) do ... end

// Expression context
let type_val = type int;  // type as expression
```

### Generic Instantiation

Generic type instantiation is parsed in type contexts:

```orbit
// Type context - generic instantiation
let pair: Pair(int, [*]byte) = ...;

// Expression context - function call
let result = get_first(int, [*]byte, 42, "hello");
```

## Error Recovery

The parser implements error recovery strategies:

1. **Synchronization Points**: Statements and declarations
2. **Token Insertion**: Missing semicolons, parentheses
3. **Token Deletion**: Extra tokens
4. **Panic Mode**: Skip to next synchronization point

## Grammar Restrictions

### Context-Sensitive Rules

Some rules are context-sensitive and enforced during semantic analysis:

1. **Type Checking**: Expression types must match contexts
2. **Scope Rules**: Identifiers must be declared before use
3. **Return Statements**: Must match function return type
4. **Generic Constraints**: Type parameters must be used consistently

### Semantic Constraints

1. **Function Parameters**: Cannot have duplicate parameter names
2. **Struct Fields**: Cannot have duplicate field names
3. **Type Parameters**: Cannot have duplicate type parameter names
4. **Variable Declarations**: Cannot redeclare variables in same scope

## Future Grammar Extensions

Planned grammar additions:

### Pattern Matching

```ebnf
MatchExpression = "match" Expression "{" { MatchArm } "}"
MatchArm = Pattern "=>" Expression ","
Pattern = LiteralPattern | IdentifierPattern | StructPattern
```

### Error Handling

```ebnf
TryExpression = "try" Expression [ CatchClause ]
CatchClause = "catch" "(" Identifier ":" Type ")" Block
```

### Modules

```ebnf
ModuleDeclaration = "mod" Identifier "{" { Declaration } "}"
ImportDeclaration = "import" ModulePath [ "as" Identifier ] ";"
```

This grammar reference provides the formal specification for parsing Orbit source code and serves as the authoritative definition of the language syntax.