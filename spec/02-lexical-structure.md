# 2. Lexical Structure

## Source Code Format

Orbit source code is written in UTF-8 encoded text files with the `.ob` extension. The language is case-sensitive and uses ASCII characters for keywords and operators.

## Tokens

The lexical structure of Orbit consists of the following token types:

### Keywords

Orbit has the following reserved keywords:

- `let` - Variable declarations
- `fun` - Function definitions
- `do` - Block start
- `end` - Block end
- `return` - Function return
- `if` - Conditional statement
- `else` - Conditional alternative
- `while` - Loop statement
- `true` - Boolean literal
- `false` - Boolean literal
- `new` - Object instantiation
- `struct` - Struct definition
- `as` - Type casting
- `type` - Type declaration
- `extends` - Type extension
- `sizeof` - Size operator

### Identifiers

Identifiers are used for variable names, function names, and type names. They must:

- Start with a letter (a-z, A-Z) or underscore (_)
- Contain only letters, digits (0-9), and underscores
- Not be a reserved keyword

Examples:
```orbit
x
myVariable
_internal
Point
calculateSum
```

### Literals

#### Integer Literals

Orbit supports both decimal and hexadecimal integer literals:

```orbit
42          // decimal
0x2A        // hexadecimal (same as 42)
0xFF        // hexadecimal (255)
```

#### Boolean Literals

```orbit
true
false
```

#### String Literals

String literals are enclosed in double quotes:

```orbit
"Hello, World!"
"Orbit Programming Language"
""          // empty string
```

Strings are represented as `[*]byte` (pointer to byte array) internally.

### Operators

#### Arithmetic Operators

- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `%` - Modulo

#### Comparison Operators

- `==` - Equality
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal
- `>=` - Greater than or equal

#### Assignment Operators

- `=` - Assignment
- `<-` - Push (for vectors)

#### Delimiters

- `(` `)` - Parentheses
- `[` `]` - Brackets
- `{` `}` - Braces
- `:` - Colon
- `;` - Semicolon
- `.` - Dot
- `,` - Comma

### Comments

Orbit supports single-line comments using `//`:

```orbit
// This is a comment
let x = 42; // This is also a comment
```

Multi-line comments are not currently supported.

### Whitespace

Whitespace characters (spaces, tabs, newlines) are used to separate tokens but are otherwise ignored. The language is not whitespace-sensitive.

## Token Examples

```orbit
// Keywords and identifiers
fun main() do
    let x = 42;
    return x;
end

// Literals
let name = "Alice";
let age = 30;
let isActive = true;

// Operators and delimiters
if (x > 0) do
    vec <- x * 2;
end
```

## Lexical Analysis Rules

1. **Longest Match**: The lexer applies the longest match rule when tokenizing
2. **Keyword Priority**: Keywords take precedence over identifiers
3. **Case Sensitivity**: All tokens are case-sensitive
4. **Unicode Support**: Source files must be UTF-8 encoded
5. **Line Comments**: `//` starts a comment that extends to the end of the line

## Reserved for Future Use

The following tokens are reserved for future language features:

- Additional keywords may be added in future versions
- Unicode identifiers may be supported in the future
- Additional numeric literal formats (floats, scientific notation) may be added