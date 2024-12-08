# Pandora

Pandora is a simple language (with only 15 keywords) that has simple syntax and can run simple programs.

Note: Pandora is still in development, so there may be some bugs and missing features.

## Table of contents

- [Installation](#installation)
- [Usage](#usage)
- [Features](#features)
- [Syntax](#syntax)
  - [Variables](#variables)
  - [Functions](#functions)
  - [Control flow](#control-flow)
    - [when-alt (if-else)](#when-alt-if-else)
    - [during (while)](#during-while)
    - [for](#for)
  - [Comments](#comments)
  - [Importing modules](#importing-modules)
  - [Standard library](#standard-library)
- [Examples](#examples)
- [Keywords](#keywords)

## Installation

You need to have Rust installed on your machine.

After that, you can run the following command to install Pandora:

```bash
cargo install pandora-interpreter@[version]
```

Replace `[version]` with the version you want to install (you can find the latest version on the releases page).

## Usage

You can run a Pandora program by using the following command:

```bash
unbox [file.box]
```

Replace `[file.box]` with the path to your Pandora file.

If you want some help, you can use the following command:

```bash
unbox --help # or unbox -h for short
```

## Features

- Simple syntax
- Strongly typed
- No implicit type conversion

## Syntax

### Variables

Variables are declared using the `set` keyword.

```pandora
set x: int = 5;
set y: float = 3.14;
set z: str; // we can assign a value later (but the type is mandatory)
set w: [int] = [1, 2, 3]; // array of integers
set v: [str; 3] = ["hello"; 3]; // array of strings
set u: [[int; 2]; 3] = [[1; 2]; 3]; // 2D array of integers
```

If you want to modify the value of a variable, you need to have the `mut` keyword (just like Rust).

```pandora
set mut x: int = 5;
x = 10;
```

You can also shadow a variable.

```pandora
set x: int = 5;
set x: float = 3.14; // this is allowed
```

If you want to cast a variable to another type, you can use the `as` keyword (Pandora doesn't have implicit type conversion because we don't want you to shoot yourself in the foot).

```pandora
set x: int = 5;
set y: str = x as str;
set z: float = x as str as float; // or just x as float
```

### Functions

Functions are declared using the `fun` keyword.

```pandora
fun add(x: int, y: int) -> int {
    yeet x + y;
}
```

The return type is mandatory, unless you don't want to return anything.

```pandora
fun print_hello() {
    println("Hello, world!");
}
```

### Control flow

#### when-alt (if-else)

```pandora
when x > 5 { // condition must be a boolean expression
    println("x is greater than 5");
} alt when x == 5 {
    println("x is equal to 5");
} alt {
    println("x is less than 5");
}
```

#### during (while)

```pandora
set mut x: int = 0;
during true {
    x += 1;

    when x == 10 {
        br; // break the loop
    }

    when x == 5 {
        skip; // continue the loop
    }

    println("keep going");
}
```

#### for

```pandora
set arr: [int] = [1, 2, 3, 4, 5];
for e in arr {
    println(e as str); // you need to manually cast types, and println only accepts strings
}
```

### Comments

```pandora
// this is a single-line comment
/*
    this is a
    multi-line comment
    /* with nested comments */
*/
```

### Importing modules

```pandora
add std; // import the standard library

std.println("Hello, world!"); // use the imported module (for std, you don't need to use the module name)
```

You can also import your own modules.

```pandora
add my_module; // your module must be in the same directory as the main file and has .boxx extension (my_module.boxx)

my_module.my_function();
```

### Standard library

List of standard library functions:

| Library | Function | Parameters | Description |
| --- | --- | --- | --- |
| std | print | str | Print a string to the console |
| std | println | str | Print a string to the console (with a newline) |
| std | input |  | Get a string input from the user |
| std | lower | str | Convert a string to lowercase |
| std | upper | str | Convert a string to uppercase |
| std | strlen | str | Get the length of a string |
| std | arrlen | [T] | Get the length of an array |
| std | delay | int | Delay the program for a certain amount of time (in milliseconds) |
| math | sqrt | float | Get the square root of a number |
| math | pow | float, float | Get the power of a number |
| math | abs | float | Get the absolute value of a number |
| math | gcd | int, int | Get the greatest common divisor of two numbers |
| math | ceil | float | Get the smallest integer greater than or equal to a number |
| math | floor | float | Get the largest integer less than or equal to a number |
| math | round | float | Get the nearest integer to a number |
| math | sin | float | Get the sine of a number |
| math | cos | float | Get the cosine of a number |
| math | tan | float | Get the tangent of a number |
| math | log | float, float | Get the logarithm of a number with a base |
| math | ln | float | Get the natural logarithm of a number |

## Examples

### Hello, world!

```pandora
fun main() {
    println("Hello, world!");
}

main();
```

### Fibonacci sequence

```pandora
fun fibonacci(n: int) -> int {
    when n == 0 {
        yeet 0;
    } alt when n == 1 {
        yeet 1;
    } alt {
        yeet fibonacci(n - 1) + fibonacci(n - 2);
    }
}

fun main() {
    set n: int = 10;
    set result: int = fibonacci(n);

    println(result as str);
}

main();
```

### Factorial

```pandora
fun factorial(n: int) -> int {
    when n == 0 {
        yeet 1;
    } alt {
        yeet n * factorial(n - 1);
    }
}

fun main() {
    set n: int = 5;
    set result: int = factorial(n);

    println(result as str);
}

main();
```

### Bubble sort

```pandora
fun bubble_sort(arr: [int]) -> [int] {
    set mut n: int = arrlen(arr);

    during true {
        set mut swapped: bool = false;

        set mut i: int = 0;
        during i < n - 1 {
            when arr[i] > arr[i + 1] {
                set mut temp: int = arr[i];
                arr[i] = arr[i + 1];
                arr[i + 1] = temp;

                swapped = true;
            }

            i += 1;
        }

        when !swapped {
            br;
        }

        n -= 1;
    }

    yeet arr;
}

fun main() {
    set arr: [int] = [64, 34, 25, 12, 22, 11, 90];
    set result: [int] = bubble_sort(arr);

    for e in result {
        println(e as str);
    }
}

main();
```

## Keywords

This may change in the future.

- `set`
- `mut`
- `fun`
- `yeet`
- `br`
- `skip`
- `when`
- `alt`
- `during`
- `for`
- `add`
- `as`
- `true`
- `false`
- `in`
