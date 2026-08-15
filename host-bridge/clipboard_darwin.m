#import <AppKit/AppKit.h>
#import <Foundation/Foundation.h>

#include <stdlib.h>
#include <string.h>

static NSString *const HostTextMIME = @"text/plain;charset=utf-8";

static NSArray<NSDictionary<NSString *, NSString *> *> *host_image_types(void) {
  return @[
    @{ @"mime": @"image/png", @"type": @"public.png" },
    @{ @"mime": @"image/jpeg", @"type": @"public.jpeg" },
    @{ @"mime": @"image/tiff", @"type": @"public.tiff" },
    @{ @"mime": @"image/gif", @"type": @"com.compuserve.gif" },
    @{ @"mime": @"image/bmp", @"type": @"com.microsoft.bmp" },
    @{ @"mime": @"image/webp", @"type": @"org.webmproject.webp" },
  ];
}

static NSString *image_pasteboard_type(NSString *mime) {
  for (NSDictionary *entry in host_image_types()) {
    if ([entry[@"mime"] isEqualToString:mime]) {
      return entry[@"type"];
    }
  }
  return nil;
}

static int set_error(char **error, NSString *message) {
  if (error != NULL) {
    const char *encoded = [message UTF8String];
    *error = strdup(encoded == NULL ? "macOS clipboard operation failed" : encoded);
  }
  return 0;
}

static int copy_data(NSData *data, void **bytes, size_t *length, char **error) {
  if (data == nil) {
    return set_error(error, @"clipboard does not contain the requested type");
  }
  *length = [data length];
  if (*length == 0) {
    *bytes = NULL;
    return 1;
  }
  *bytes = malloc(*length);
  if (*bytes == NULL) {
    return set_error(error, @"could not allocate clipboard result");
  }
  memcpy(*bytes, [data bytes], *length);
  return 1;
}

static NSData *convert_image(NSPasteboard *pasteboard, NSString *mime) {
  NSImage *image = [[[NSImage alloc] initWithPasteboard:pasteboard] autorelease];
  if (image == nil) {
    return nil;
  }
  NSData *tiff = [image TIFFRepresentation];
  if ([mime isEqualToString:@"image/tiff"]) {
    return tiff;
  }
  if ([mime isEqualToString:@"image/webp"]) {
    return nil;
  }
  NSBitmapImageRep *representation = [NSBitmapImageRep imageRepWithData:tiff];
  if (representation == nil) {
    return nil;
  }
  NSBitmapImageFileType outputType;
  if ([mime isEqualToString:@"image/png"]) {
    outputType = NSBitmapImageFileTypePNG;
  } else if ([mime isEqualToString:@"image/jpeg"]) {
    outputType = NSBitmapImageFileTypeJPEG;
  } else if ([mime isEqualToString:@"image/gif"]) {
    outputType = NSBitmapImageFileTypeGIF;
  } else if ([mime isEqualToString:@"image/bmp"]) {
    outputType = NSBitmapImageFileTypeBMP;
  } else {
    return nil;
  }
  return [representation representationUsingType:outputType
                                      properties:[NSDictionary dictionary]];
}

int host_clipboard_write(const char *mime_bytes, const void *bytes, size_t length,
                         char **error) {
  @autoreleasepool {
    NSString *mime = [NSString stringWithUTF8String:mime_bytes];
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
    [pasteboard clearContents];
    if ([mime isEqualToString:HostTextMIME]) {
      NSString *text = [[[NSString alloc] initWithBytes:bytes
                                                 length:length
                                               encoding:NSUTF8StringEncoding] autorelease];
      if (text == nil || ![pasteboard setString:text forType:NSPasteboardTypeString]) {
        return set_error(error, @"could not write UTF-8 text to the clipboard");
      }
      return 1;
    }
    NSString *pasteboardType = image_pasteboard_type(mime);
    if (pasteboardType == nil) {
      return set_error(error, @"unsupported clipboard MIME type");
    }
    NSData *data = [NSData dataWithBytes:bytes length:length];
    if (![pasteboard setData:data forType:pasteboardType]) {
      return set_error(error, @"could not write image data to the clipboard");
    }
    return 1;
  }
}

int host_clipboard_read(const char *mime_bytes, void **bytes, size_t *length,
                        char **error) {
  @autoreleasepool {
    NSString *mime = [NSString stringWithUTF8String:mime_bytes];
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
    if ([mime isEqualToString:HostTextMIME]) {
      NSString *text = [pasteboard stringForType:NSPasteboardTypeString];
      return copy_data([text dataUsingEncoding:NSUTF8StringEncoding], bytes, length, error);
    }
    NSString *pasteboardType = image_pasteboard_type(mime);
    if (pasteboardType == nil) {
      return set_error(error, @"unsupported clipboard MIME type");
    }
    NSData *data = [pasteboard dataForType:pasteboardType];
    if (data == nil) {
      data = convert_image(pasteboard, mime);
    }
    return copy_data(data, bytes, length, error);
  }
}

int host_clipboard_types(void **bytes, size_t *length, char **error) {
  @autoreleasepool {
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
    NSMutableArray<NSString *> *mimes = [NSMutableArray array];
    if ([pasteboard stringForType:NSPasteboardTypeString] != nil) {
      [mimes addObject:HostTextMIME];
    }
    BOOL hasImage = NO;
    for (NSDictionary *entry in host_image_types()) {
      if ([pasteboard dataForType:entry[@"type"]] != nil) {
        [mimes addObject:entry[@"mime"]];
        hasImage = YES;
      }
    }
    NSImage *image = [[[NSImage alloc] initWithPasteboard:pasteboard] autorelease];
    if (image != nil) {
      hasImage = YES;
    }
    if (hasImage && ![mimes containsObject:@"image/png"]) {
      [mimes addObject:@"image/png"];
    }
    NSString *encoded = [mimes componentsJoinedByString:@"\n"];
    return copy_data([encoded dataUsingEncoding:NSUTF8StringEncoding], bytes, length, error);
  }
}

int host_clipboard_files(void **bytes, size_t *length, char **error) {
  @autoreleasepool {
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
    NSArray *urls = [pasteboard
        readObjectsForClasses:@[[NSURL class]]
                      options:@{NSPasteboardURLReadingFileURLsOnlyKey : @YES}];
    NSMutableData *encoded = [NSMutableData data];
    for (NSURL *url in urls) {
      if (![url isFileURL]) {
        continue;
      }
      const char *path = [[[url path] stringByStandardizingPath] fileSystemRepresentation];
      if (path == NULL || path[0] == '\0') {
        continue;
      }
      [encoded appendBytes:path length:strlen(path)];
      const char separator = '\0';
      [encoded appendBytes:&separator length:1];
    }
    return copy_data(encoded, bytes, length, error);
  }
}
