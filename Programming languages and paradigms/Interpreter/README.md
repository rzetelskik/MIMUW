# Holang

## Description
Holang is a Go-like, imperative, statically typed language implemented in Haskell. Most of its syntax takes from Go, although there are some modifications.

In the below definitions, the following syntax is used:
```
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```

### Program
Program is simply a collection of declarations and function definitions. It requires a ```main``` function which does not return any value and takes no arguments. Every statement, excluding blocks, has to terminate with a semicolon.

### Types
There are three basic types: ```int```, ```bool```, ```string``` and function literals of type ```func({type}) [type]```. Additionally, every type has its pointer counterpart. Holang is statically typed and, in some cases, it allows for type inferrence. Integer literals are simply 

Default values of uninitialised variables:
* ```int``` -> ```0```
* ```bool``` -> ```false```
* ```string``` -> ```""```
* ```func``` -> forbidden, function literals have no default value
* ```*int | *bool | *string | *func``` -> ```nil```

### Arithmetic operators
Arithmetic operators apply to ```int``` values only and yield values of the same type.
```
+   sum
-   difference
*   product
/   quotient
%   remainder
-x  negation
```

### Comparison operators
Comparison operators compare two operands and yield a ```bool``` value.
```
==  equal
!=  not equal
<   less
<=  less or equal
>   greater
>=  greater or equal
```

### Logical operators
Logical operators apply to ```bool``` values and yield the result of the same type.
```
&&  conditional and
||  conditional or
!   not
```

### Address operators
For an addressable operand x of type T, the address operation &x generates a pointer of type *T to x.
```go
&x;
&1; // runtime error
```

For an operand x of pointer type *T, the pointer indirection *x denotes the variable of type T pointed to by x. If x is ```nil```, an attempt to evaluate *x will cause a runtime error.
```go
*ptr;
*ptrf(x);
*nil; // runtime error
```

### print
Print is a built-in instruction used for writing to standard output.
```
print([expression])
```
It accepts expressions of types ```string```, ```int```, ```bool``` and their corresponding pointer types.

### Variables
All variables have to be declared. The syntax of declaring and assigning to variables is identical to Go's, but the ```:=``` operator has been removed. Why? Robert Griesemer mentioned it while answering the question ["What would be one thing you take out from Go?"](https://www.youtube.com/watch?v=p9VUCp98ay4&t=1227s), describing it as "convenient, but problematic". Hence, unlike Go, Holang does not allow for "redeclaring" variables.

```go
var x int; // Variable declaration.
x = 1;     // Variable assignment.
var py, pz *int = nil, nil; // Multiple variables of the same type can be declared and initialised in one line.
var s = "Holang allows for type inferrence";
``` 

### if...else...
If statements require brace brackets, and do not require parentheses.
```go
if true {
    ...
}
```
```go
if false {
    ...
} else {
    ...
}
```
Unlike Go, Holang doesn't allow for declaring scoped variables before the condition.

### for
Like if, for doesn't use parentheses either, and it requires braces. For is the only loop statement in Holang, but it has alternate forms.
```go
for true {
    ...
}
```
```go
for var i = 0; i < n; i++; {
    ...
}
```

### Functions
A function has an identifier, a list of arguments and an optional return type.
```go
func identifier({arguments}) [type] where arguments :: = {identfier} [&] type
```
Every type can be returned and accepted as argument. A function may also not return any type. All arguments are passed by value.

#### Function literals
A function literal represents an anonymous function.
```go
func(x, y int) int { return x + y; };
```
A function literal can be assigned to a variable or invoked directly.
```go
var f func(int) *int = func(x int) *int { return &x; };
var x int = func(x int) int { return 2*x; }(1);
```

Function literals are closures: they may refer to variables defined in a surrounding function. Those variables are then shared between the surrounding function and the function literal.

#### Nested functions
Only function literals can be nested inside function definitions.
```go
func f() {
    var g = func() {};

    func h() {}; // parse error
}
```

A nested function has access to all identifiers visible in its surrounding function. Nested functions are not accessible outside the surrounding function's scope.


To better understand the language's mechanisms, see ```examples``` directory.

### Functionalities
| Id | Functionality | Sum | Implemented |
| :-: | - | :-: | :-: |
| 01 | Three types (int, bool, string) | | + |
| 02 | Literals, arithmetic, comparisons | | + |
| 03 | Variables, assignment | | + |
| 04 | print | | + |
| 05 | for, if * | | + |
| 06 | Functions or procedures, recurssion | 15 | + |
| 07 | Passing by value, by reference ** | | + |
| 09 | Shadowing and static binding | | + |
| 10 | Runtime error handling | | + |
| 11 | Functions returning any value | 20 | + |
| 12 | Static typing | 24 | + |
| 13 | Nested functions with static binding | 26 | + |
| 17 | Higher-order functions, anonymous functions, closures | 30 | + |

\* While is replaced with for.
\*\* Pass-by-reference semantics is replaced with pointers' implementation.

