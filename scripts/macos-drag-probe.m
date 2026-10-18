// Minimal native drag reproducer: no GTK, Lisp, or Cadre settings.
// Build and use as described in docs/MACOS-WINDOW-DRAG.md.
#import <Cocoa/Cocoa.h>
#include <stdio.h>
static void record(NSString *line) {
  fprintf(stderr, "%s\n", [line UTF8String]);
  fflush(stderr);
}
@interface DragView : NSView
@end
@implementation DragView
- (void)drawRect:(NSRect)rect {
  [[NSColor windowBackgroundColor] setFill]; NSRectFill(self.bounds);
  NSRect band=NSMakeRect(0,self.bounds.size.height-70,self.bounds.size.width,70);
  [[NSColor controlBackgroundColor] setFill]; NSRectFill(band);
  NSDictionary *a=@{NSFontAttributeName:[NSFont systemFontOfSize:16],NSForegroundColorAttributeName:[NSColor labelColor]};
  [@"CUSTOM DRAG AREA — native AppKit call, no GTK" drawAtPoint:NSMakePoint(18,self.bounds.size.height-42) withAttributes:a];
  [@"Compare dragging this strip with the normal title bar above." drawAtPoint:NSMakePoint(18,120) withAttributes:a];
}
- (void)mouseDown:(NSEvent *)event {
  NSPoint point=[self convertPoint:event.locationInWindow fromView:nil];
  if(point.y>self.bounds.size.height-70) {
    record([NSString stringWithFormat:@"CUSTOM mouseDown type=%ld window=%ld eventWindow=%ld",(long)event.type,(long)self.window.windowNumber,(long)event.windowNumber]);
    [self.window performWindowDragWithEvent:event];
    record(@"CUSTOM native drag call returned");
  }
}
@end
@interface ProbeDelegate : NSObject <NSApplicationDelegate>
@property(strong) NSWindow *window;
@end
@implementation ProbeDelegate
- (void)applicationDidFinishLaunching:(NSNotification *)notification {
  self.window=[[NSWindow alloc] initWithContentRect:NSMakeRect(160,150,650,300)
    styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable
    backing:NSBackingStoreBuffered defer:NO];
  self.window.title=@"AppKit drag probe — no GTK or Lisp";
  self.window.contentView=[[DragView alloc] initWithFrame:NSMakeRect(0,0,650,300)];
  [self.window makeKeyAndOrderFront:nil];
  [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowDidMoveNotification object:self.window queue:nil usingBlock:^(NSNotification *note) {
    record([NSString stringWithFormat:@"MOVED %.1f %.1f",self.window.frame.origin.x,self.window.frame.origin.y]);
  }];
  record(@"READY");
  [NSApp activateIgnoringOtherApps:YES];
}
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)sender { return YES; }
@end
int main(void) {
  @autoreleasepool {
    NSApplication *app=[NSApplication sharedApplication];
    ProbeDelegate *delegate=[ProbeDelegate new]; app.delegate=delegate;
    [app setActivationPolicy:NSApplicationActivationPolicyRegular]; [app run];
  }
  return 0;
}
