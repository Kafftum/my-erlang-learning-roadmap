# Roadmap to Proficiency in Erlang

A roadmap of concepts that are important for becoming proficient in Erlang made by chatGPT.

## 1. Foundations of Functional Programming

Before Erlang itself, it helps to understand the functional paradigm.

- ### Concepts:

  - Immutable data

  - First-class functions

  - Recursion instead of loops

  - Pattern matching

  - Higher-order functions (map, fold, filter)

## 2. Erlang Basics (Syntax & Core Language)

Learn the language constructs and how to write simple programs.

- ### Concepts:

  - Erlang shell (REPL) & compilation (erl, c/1)

  - Variables (single-assignment, immutability)

  - Atoms, tuples, lists, strings, maps

  - Pattern matching in assignments

  - Guards

  - Modules & functions

  - List comprehensions

- ### Free Resources:

  - [Official Erlang Documentation – Getting Started](https://www.erlang.org/doc/getting_started/intro.html)

  - [Learn You Some Erlang – Starting Out (for real)](https://learnyousomeerlang.com/starting-out-for-real)

## 3. Concurrency Foundations

Erlang’s killer feature is its lightweight process model.

- ### Concepts:

  - Processes (spawn/1, spawn/3)

  - Mailboxes & message passing (! operator, receive)

  - Process IDs (PIDs)

  - Linking processes (link/1)

  - Monitoring processes (monitor/1)

- ### Free Resources:

  - [Erlang Documentation – Concurrency](https://www.erlang.org/doc/getting_started/conc_prog.html)

  - [Learn You Some Erlang – The Hitchhiker’s Guide to Concurrency](https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency)

## 4. Error Handling & Fault Tolerance

This is where Erlang’s “let it crash” philosophy shines.

- ### Concepts:

  - try … catch and throw

  - Process exit signals

  - Linking & supervision basics

  - Hot code swapping

  - The “let it crash” model

- ### Free Resources:

  - [Learn You Some Erlang – Errors and Processes](https://learnyousomeerlang.com/errors-and-processes)

  - [Erlang Docs – Error Handling](https://www.erlang.org/doc/reference_manual/errors.html)

## 5. OTP (Open Telecom Platform)

OTP is the standard library/framework that makes Erlang production-ready.

- ### Concepts:

  - Behaviours (gen_server, gen_statem, gen_event)

  - Supervisors & supervision trees

  - Applications (.app files, starting/stopping apps)

  - Releases (rebar3, relx)

- ### Free Resources:

  - [Learn You Some Erlang – What is OTP?](https://learnyousomeerlang.com/what-is-otp)

  - [Learn You Some Erlang – Building an application with OTP](https://learnyousomeerlang.com/building-applications-with-otp)

  - [Learn You Some Erlang – Building OTP applications](https://learnyousomeerlang.com/building-otp-applications)

  - [Official Erlang/OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)

  - [Erlang Central – OTP Tutorials](https://erlangcentral.org/)

## 6. Distributed Erlang

Running Erlang on multiple nodes is trivial but requires some study.

- ### Concepts:

  - Nodes (-sname, -name)

  - Distributed message passing

  - ``rpc:call/4``

  - Global process registration

  - Cookies (node authentication)

- ### Free Resources:

  - [Learn You Some Erlang – Distributed Programming](https://learnyousomeerlang.com/distributed-otp-applications)

  - [Erlang Docs – Distributed Programming](https://www.erlang.org/doc/reference_manual/distributed.html)

## 7. Tooling & Ecosystem

To be effective you’ll need build, test, and debug skills.

- ### Concepts:

  - rebar3 (build tool, project management)

  - dialyzer (static analysis for type errors)

  - observer (visual monitoring of processes)

  - common_test & eunit (testing)

- ### Free Resources:

  - [rebar3.org](https://rebar3.org/)

  - [Dialyzer Guide](https://learnyousomeerlang.com/dialyzer)

  - Erlang Observer Tutorial (YouTube)

## 8. Advanced Concepts

Once you’re comfortable, dive into deeper areas.

- ### Concepts:

  - ETS (Erlang Term Storage)

  - Mnesia (distributed database)

  - NIFs (Native Implemented Functions)

  - Hot code upgrades in OTP apps

  - Performance tuning & BEAM internals

- ### Free Resources:

  - [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/users_guide.html)

  - [Erlang/OTP in Action (free sample chapters via Manning)](https://www.manning.com/books/erlang-and-otp-in-action)

  - [Learn You Some Erlang – ETS, Mnesia, and More](https://learnyousomeerlang.com/content#the-road-to-advanced)

## 9. Real-World Practice

Finally, you’ll want to build projects to solidify your knowledge.

- ### Ideas:

  - Chat server (gen_server + supervision tree)

  - Key-value store with ETS/Mnesia

  - Distributed job queue

  - Fault-tolerant web service using Cowboy (web server for Erlang)

- ### Resources:

  - [Awesome Erlang GitHub List](https://github.com/drobakowski/awesome-erlang)

  - [Cowboy Documentation](https://ninenines.eu/docs/en/cowboy/)

