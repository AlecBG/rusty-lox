# Rusty Lox

Working through the [Crafting Interpreters](https://craftinginterpreters.com/) book, writing the interpreter for the language Lox in Rust because

1. I want to understand better how interpreters and compilers work.
2. I want to get better at rust.

## Tree-walk interpreter

I have fully implemented all the main features laid out in the first part of the book. There are some differences from the book's implementation, especially in the interpreter side. Instead of using the visitor pattern it used, it seemed a much better fit for rust's match used on enums. The implementation of the environment is completely different to the book's.

One could improve on what is written here quite a bit, some simple examples
1. Adding lists and dictionaries of some form.
2. Adding static type checking to the resolver
3. Adding more I/O (networking, talking to the file system, accepting from stdin, writing to stderr)
4. Improve the error messages, showing the line of code with the mistake.
5. Much more!

However, I feel like I wouldn't be learning all to much with any of these bits of work and can see pretty well how that would be done. If I complete the next section that compiles to byte code and uses a VM, them maybe I'll add it then.

Performance testing by generating Fibonnaci numbers, I find that my implementation is 100 times slower than python and 10 times slower than the jlox implementation in the book. This is mainly untested code that I do not plan to maintain that performs next to no I/O, so feel free to use this for your production servers.
