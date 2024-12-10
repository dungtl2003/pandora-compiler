# E0748
A raw string isn't correctly terminated because the trailing `#` count doesn't
match its leading `#` count.

Erroneous code example:

```compile_fail,E0748
let dolphins = r##"Dolphins!"#; // error!
```

To terminate a raw string, you have to have the same number of `#` at the end
as at the beginning. Example:

```
let dolphins = r#"Dolphins!"#; // One `#` at the beginning, one at the end so
                               // all good!
```

# E0758
A multi-line (doc-)comment is unterminated.

Erroneous code example:

```compile_fail,E0758
/* I am not terminated!
```

The same goes for doc comments:

```compile_fail,E0758
/*! I am not terminated!
```

You need to end your multi-line comment with `*/` in order to fix this error:

```
/* I am terminated! */
/*! I am also terminated! */
```

# E0762
A character literal wasn't ended with a quote.

Erroneous code example:

```compile_fail,E0762
static C: char = '●; // error!
```

To fix this error, add the missing quote:

```
static C: char = '●'; // ok!
```
