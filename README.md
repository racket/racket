# Racket Programming Language

![License: MIT/Apache/LGPL](https://img.shields.io/badge/license-MIT%2FApache%2FLGPL-blue)
![Made with Scheme](https://img.shields.io/badge/Made%20with-Scheme-lightgrey?logo=racket)

<img alt="Racket Logo" src="https://racket-lang.org/img/racket-logo.svg" width="200" height="auto" />

**Racket** is a general-purpose programming language in the Lisp/Scheme family and an ecosystem for language-oriented programming.

This repository contains the source code for the core Racket system and some related packages.  
Other parts of the Racket distribution are maintained in separate repositories under the [Racket GitHub organization](https://github.com/racket).

---

## Table of Contents 

- [What is Racket?](#what-is-racket)
- [Install](#install)
- [Documentation](#documentation)
- [Example Code](#example-code)
- [Contributing](#contributing)
- [Community](#community)
- [License](#license)

---

## What is Racket?

Racket is a mature, functional-first language with support for multiple paradigms and a focus on language creation.

### Key Features

- Supports functional, imperative, object-oriented, and logic programming
- Language-oriented: create new syntaxes and DSLs with powerful macros
- Comes with DrRacket, a beginner-friendly IDE
- Used in education, research, and production
- Includes a package ecosystem, module system, and REPL

## Install

Prebuilt binaries for major platforms are available at:
- 👉  [https://download.racket-lang.org](https://download.racket-lang.org)
> **Note**: Prebuilt packages are available for most operating systems.

## Documentation

- [Official Documentation](https://docs.racket-lang.org)
- [Tutorials and Guides](https://docs.racket-lang.org/#tutorials)
- [Racket Package Index (pkgs.racket-lang.org)](https://pkgs.racket-lang.org)
- [How to Design Programs (HTDP)](https://htdp.org/)

## Example Code

```racket
#lang racket

;; Fibonacci sequence using recursion
(define (fib n)
  (cond
    [(zero? n) 0]
    [(= n 1) 1]
    [else (+ (fib (- n 1))
             (fib (- n 2)))]))

(fib 10) ; => 55

```

For more Racket examples, visit the [Official Racket Examples page](https://docs.racket-lang.org/quick/index.html)

## Contributing

We welcome contributions of all kinds, including code, documentation, and issue reports.

- Fork this repository and open a pull request.
- Follow the instructions in the [Build Guide](build.md#contributing).
- Look for [good first issues](https://github.com/racket/racket/labels/good%20first%20issue).

By contributing, you agree to license your work under the following licenses:
- [MIT License](racket/src/LICENSE-MIT.txt)
- [Apache License 2.0](racket/src/LICENSE-APACHE.txt)
- [GNU LGPLv3 License](racket/src/LICENSE-LGPL.txt)

See the [LICENSE.txt](LICENSE.txt) file and the LICENSE files in `racket/src/` for more information.

Please read our [Friendly Environment Policy](https://racket-lang.org/friendly.html) to ensure respectful and inclusive communication.

### Building from Source

To build Racket from this repository:
- This repository is intended for Racket development or contributions.
- Follow the instructions in the [Build Guide](https://github.com/racket/racket/blob/master/build.md)
  
```bash
git clone https://github.com/racket/racket
cd racket
make
```

> **Note**: For detailed instructions, see the [Build Guide](build.md).
> - ※ To build the released version of Racket from source tarballs, visit [https://download.racket-lang.org](https://download.racket-lang.org).

## Community

Stay connected with the Racket community:

- [Discourse Forum](https://racket.discourse.group/)
- [Discourse Chat](https://racket.discourse.group/chat)
- [Discord Server](https://discord.gg/6Zq8sH5)—the '#internals' channel is used for discussions about the Racket implementation

## License

This project is distributed under the following licenses:

- [MIT License](racket/src/LICENSE-MIT.txt)
- [Apache License 2.0](racket/src/LICENSE-APACHE.txt)
- [GNU LGPLv3 License](racket/src/LICENSE-LGPL.txt)

See the [LICENSE.txt](LICENSE.txt) file for general information.
