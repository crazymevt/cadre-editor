# macOS native window-drag failure

Investigation: October 18, 2026. The evidence points to a macOS WindowServer
regression, rather than a Cadre or Lisp binding regression. The underlying
OS state cleared after signing out and back in; Cadre retains native GTK dragging.

## Observations on this machine

- macOS 27.2, build `26B5112a`. The local installation log records the switch
  from `26B5101f` on October 14 at about 9:27 PM Eastern.
- Homebrew GTK 4.24.1 was installed October 2 at about 10:18 PM Eastern;
  libadwaita is 1.10.0.
- In the current Cadre source, header mouse presses reach GTK, the window is
  movable, and `gdk_toplevel_begin_move` is called, but no movement results.
- GTK's `GdkMacosWindow::beginManualMove` calls
  `-[NSWindow performWindowDragWithEvent:]`. Starting that call directly on
  the press, instead of waiting for drag motion, did not resolve the failure.
- Matching failed-drag timestamps in WindowServer contain:
  `no client initiated window drag trait gesture to activate for mouse`.
- A standalone AppKit program, with no GTK or Lisp, also receives a left
  mouse-down event for the correct window and calls the same native API,
  without a window-move notification. This test used automated pointer input;
  Cadre's failures were also reproduced by the user's manual tests.

The OS update predates the failure but does not establish its exact trigger.
No comparison against an earlier booted macOS build has been performed.

## External evidence

[Warp issue #16508](https://github.com/warpdotdev/warp/issues/16508) reports the
same native-call failure on the exact build `26B5112a`, with an A/B test where
manual movement succeeds. Comments connect the WindowServer error above to
remote/virtual input state and report that logging out clears it. This is
also confirmed locally by the sign-out test below.

[Apple Developer Forums thread 834488](https://developer.apple.com/forums/thread/834488)
also describes native title-bar dragging failing over remote input after a
macOS 27 update. Apple's response asks for a system diagnostic; it does not
provide a fixed native drag API. This related report is not proof that a
remote session triggered the local failure.

[Apple's API documentation](https://developer.apple.com/documentation/appkit/nswindow/performdrag(with:))
says native dragging is handed to WindowServer and supports system behavior
such as Spaces transitions. Directly setting the frame is therefore a
workaround with behavioral tradeoffs, not an equivalent native drag.

## Confirmed native recovery

On October 18, the user signed out of macOS and back in, then launched
`CADRE_NATIVE_WINDOW_DRAG=1 make run`. That diagnostic option bypassed the
manual handler without changing preferences. The user confirmed that native
window dragging worked again.

This supports a transient WindowServer session failure. It does not identify
the exact trigger or demonstrate that the OS defect cannot recur. The manual
handler, its setting, and the diagnostic environment option were removed after
this confirmation. Ordinary `make run` now uses GTK's native window dragging,
including native tiling and Spaces behavior.

## Minimal reproducer

The native test is kept in `scripts/macos-drag-probe.m`. It does not read or
write Cadre settings. On macOS, build and run it from the checkout:

```sh
clang -fobjc-arc -framework Cocoa scripts/macos-drag-probe.m -o /tmp/cadre-drag-probe
/tmp/cadre-drag-probe
```

Drag the custom strip, then the ordinary title bar. The custom strip calls
the documented native API using the actual mouse-down event. Terminal output
records delivery and any `NSWindowDidMoveNotification` events (`MOVED`). A
returned API call without `MOVED` shows that the handoff did not move the
window. Test with both local pointer input and any remote input in use.
