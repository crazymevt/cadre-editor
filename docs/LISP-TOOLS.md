# Lisp tools — what's next

**Status:** 2026-10-15. Tools a Lisper reaches for that Cadre doesn't have
yet. Cadre already has the debugger, inspector, trace, stepper,
cross-references, macroexpansion, the HyperSpec, compiler notes, loading
systems, running tests and refactoring. Tick an item when it lands.

## Order

1. [x] **Profiler.** Profile a form, or a running app for a while, with
   `sb-sprof`; show the report as a sortable table (and a flame graph), each
   function a link to its source.
2. [ ] **Library manager.** Search the systems Quicklisp (or ocicl / Qlot)
   offers, install one, and add it to the `.asd`'s `:depends-on` in one step.
3. [ ] **Threads page.** The Lisp's threads, with Interrupt, Kill, and Debug
   (the debugger in that thread): for servers, GTK apps, background work.
4. [ ] **Test explorer.** The project's tests (parachute, FiveAM) with pass
   or fail for each; run one, or the failed ones again; jump from a failure
   to its code.
5. [ ] **Macroexpansion in place**, like Emacs's macrostep: expand a macro
   in the code, a step at a time, and fold it back.
6. [ ] **Scratch buffer** for each project: a place for experiments that is
   kept, but isn't a file in the project.
