# Pandora

Pandora is a simple language (with only 15 keywords) that has simple syntax and can run simple programs.

Note: Pandora is still in development, so there may be some bugs and missing features.

## Table of contents

- [Installation](#installation)
  - [Method 1: Using Cargo](#method-1-using-cargo)
  - [Method 2: Manual installation](#method-2-manual-installation)
    - [Linux users](#linux-users)
    - [Windows users](#windows-users)
- [Usage](#usage)
- [Features](#features)
- [Syntax](#syntax)
  - [Types](#types)
    - [Integers](#integers)
    - [Floats](#floats)
    - [Booleans](#booleans)
    - [Characters](#characters)
    - [Strings](#strings)
    - [Arrays](#arrays)
    - [Tuples (coming soon)](#tuples-coming-soon)
  - [Variables](#variables)
  - [Functions](#functions)
  - [Control flow](#control-flow)
    - [when-alt (if-else)](#when-alt-if-else)
    - [during (while)](#during-while)
    - [for](#for)
  - [Comments](#comments)
  - [Importing modules](#importing-modules)
  - [Standard library](#standard-library)
- [Chaos mode](#chaos-mode)
- [Examples](#examples)
- [Keywords](#keywords)

## Installation

### Method 1: Using Cargo

You need to have Rust installed on your machine.

After that, you can run the following command to install Pandora:

```bash
cargo install pandora-interpreter
```

After the installation is complete, you can run the following command to check if Pandora is installed:

```bash
unbox --version
```

If you see the version of Pandora, then you have successfully installed it.

Note: If you cannot run the `unbox` command, you need to add the Cargo bin directory to your PATH. For more information, you can check the [Cargo documentation](https://www.rust-lang.org/tools/install).

If you want to update Pandora, you can run the following command:

```bash
cargo install pandora-interpreter
```

### Method 2: Manual installation

First, you need to download the latest release from the [releases page](https://github.com/dungtl2003/pandora/releases). You can choose the appropriate version for your operating system. Then you can follow the instructions below.

#### Linux users

After downloading the latest release, you need to make the file executable:

```bash
chmod +x unbox
```

Then, move the file to the `/home/[username]/.local/bin` directory: (replace `[username]` with your username)

```bash
mv unbox /home/[username]/.local/bin/unbox
```

Restart your terminal, and you can run the following command to check if Pandora is installed:

```bash
unbox --version
```

If you see the version of Pandora, then you have successfully installed it.

Note: If you cannot run the `unbox` command, it is likely that you are missing some dependencies. You can check the error message to see what dependencies you are missing.

If you want to update Pandora, you can download the latest release and replace the old file with the new one.

#### Windows users

After downloading the latest release, you need to move the file to the directory where you want to store the executable file, for example, `C:\Program Files\Pandora`.

Then, you need to add the directory to the PATH environment variable. You can follow the instructions [here](https://www.architectryan.com/2018/03/17/add-to-the-path-on-windows-10/).

After that, you can run the following command to check if Pandora is installed:

```bash
unbox --version
```

If you see the version of Pandora, then you have successfully installed it.

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

Some command options:

- `--version`, `-v`: Print the version of Pandora
- `--verbose`: Print the interpretation process (useful for debugging)
- `-h`, `--help`: Print the help message
- `--wreck`: Activate chaos mode 💀 (if you use this mode, the file's extension must be `.unbx`). More information about chaos mode can be found [here](#chaos-mode)
- `--latest`: Check for the latest version of Pandora
- `--explain [error_code]`: Explain an error code

## Features

Panodra has the following features:

- Simple syntax (only 15 keywords)
- Strongly typed (so you can always know what type a variable is)
- No implicit type conversion (so you don't shoot yourself in the foot)
- Very descriptive error messages (with total of 72 different error codes)

## Syntax

### Types

#### Integers

```pandora
5; // integer

// b, o, and h are prefixes for binary, octal, and hexadecimal numbers respectively (you can also use uppercase letters)
0b101; // binary
0o10; // octal
0h10; // hexadecimal

// you can use underscores for better readability
1_000_000;

set x: int = 5;
```

#### Floats

```pandora
3.14; // float

// you can use underscores for better readability
1_000.0100_001; // 1000.0100001 (the first letter after the dot must be a digit)

// you can also use scientific notation
1e3; // 1000
1.10_01e-30_5; // 1.1001e-305 (the dot must before the e)

set x: float = 3.14;
```

#### Booleans

```pandora
true; // true
false; // false

set x: bool = true;
```

#### Characters

```pandora
'a'; // character
'\"'; // double quote (you can remove the backslash in this case)
'\'': // single quote

set x: char = 'a';
```

#### Strings

```pandora
"hello, world!\n"; // string

r##"hello,\r\n#world!"##; // raw string (just like Rust)

"hello \
world"; // multiline string (the backslash must be the last character)

set x: str = "hello, world!";
```

#### Arrays

```pandora
// elements must have the same type
[1, 2, 3]; // array of integers
["hello", "world"]; // array of strings
[[1, 2], [3, 4]]; // 2D array of integers

set x: [int] = [1, 2, 3];
```

#### Tuples (coming soon)

This will be implemented in the future. For now, we only have `()` for empty tuples (also known as unit type), and you can't use it yet (it's just a placeholder for case when functions don't return anything). 

```pandora
(); // empty tuple 
```

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

Pandora also supports raw identifiers (just like Rust).

```pandora
set r#true: int = 5;
println(r#true as str);
```

You can also use emojis as variable names (just for fun).

```pandora
set 🍕: int = 5;
println(🍕 as str);
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
add my_module; // your module must be in the same directory as the main file and has .boxx extension (my_module.boxx). If you use chaos mode, the extension must be .unbxx

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

## Chaos mode

Chaos mode is a mode that allows you to run Pandora programs with some tweaks. All keywords will be replaced with gen-z slang. To enable this mode, firstly, in the command line, you need to add the `--wreck` flag. Secondly, the file extension must be `.unbx` (instead of `.box`), and the library file extension must be `.unbxx` (instead of `.boxx`). Below is the list of keywords that will be replaced:

| Pandora | Chaos mode |
| --- | --- |
| true | yass |
| false | nope |
| set | vibe |
| mut | chill |
| when | fr |
| alt | nah |
| fun | doit |
| br | bruhstop |
| skip | keepitup |
| for | onloop |
| in | among |
| during | staylit |
| as | flexin |
| const | deadass |
| add | snatch |
| yeet | bounce |

Example:

```pandora
vibe x: int = 5;

fr x > 5 {
    println("x is greater than 5");
} nah fr x == 5 {
    println("x is equal to 5");
} nah {
    println("x is less than 5");
}
```

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
