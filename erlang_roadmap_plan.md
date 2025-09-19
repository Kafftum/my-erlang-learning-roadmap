## Phase 2: Concurrency (Weeks 4–6)

Goal: Learn Erlang’s lightweight process model and message passing.

- ### Week 4: Processes & Messaging

  - Learn ``spawn/1``, ``!``, and ``receive``.

  - Exercise: Build a simple ping-pong system with two processes.

- ### Week 5: Linking & Monitoring

  - Study linking and monitoring for fault detection.

  - Exercise: Create a supervisor-like process that restarts crashed workers.

- ### Week 6: Error Handling

  - Study try…catch, exits, and the “let it crash” philosophy.

  - Exercise: Extend your supervisor to handle worker crashes gracefully.

## Phase 3: OTP (Weeks 7–8)

Goal: Become comfortable with OTP behaviours and supervision trees.

- ### Week 7: gen_server

  - Learn gen_server basics (callbacks, state management).

  - Exercise: Build a key-value store as a gen_server.

- ### Week 8: Supervisors & Applications

  - Learn about supervision trees and ``.app`` files.

  - Exercise: Turn your key-value store into an OTP application with supervision.

## Phase 4: Distribution & Tooling (Weeks 9–10)

Goal: Run Erlang across multiple nodes, and learn productivity tools.

- ### Week 9: Distributed Erlang

  - Learn node connections, cookies, ``rpc:call/4``.

  - Exercise: Run your key-value store across two nodes.

- ### Week 10: Tooling

  - Learn rebar3, dialyzer, observer.

  - Exercise: Add unit tests with eunit. Run dialyzer on your project.

## Phase 5: Advanced Topics & Projects (Weeks 11–12)

Goal: Explore advanced features and build a real project.

- ### Week 11: ETS, Mnesia, and Performance

  - Learn how to use ETS for in-memory storage.

  - Optional: Try Mnesia for a distributed database.

  - Exercise: Add persistence to your key-value store with ETS.

- ### Week 12: Capstone Project

  - Build a real-world app (choose one):

    - A chat server

    - A distributed job queue

    - A fault-tolerant web service (using Cowboy)

  - Deploy across multiple nodes and test resilience.