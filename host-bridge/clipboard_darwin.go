//go:build darwin && cgo

package bridge

/*
#cgo LDFLAGS: -framework AppKit -framework Foundation
#include <stddef.h>
#include <stdlib.h>

int host_clipboard_write(const char *mime, const void *bytes, size_t length, char **error);
int host_clipboard_read(const char *mime, void **bytes, size_t *length, char **error);
int host_clipboard_types(void **bytes, size_t *length, char **error);
int host_clipboard_files(void **bytes, size_t *length, char **error);
*/
import "C"

import (
	"bytes"
	"errors"
	"runtime"
	"strings"
	"unsafe"
)

func nativeClipboardWrite(mimeType string, contents []byte) error {
	cMIME := C.CString(mimeType)
	defer C.free(unsafe.Pointer(cMIME))
	var contentsPointer unsafe.Pointer
	if len(contents) != 0 {
		contentsPointer = unsafe.Pointer(&contents[0])
	}
	var cError *C.char
	result := C.host_clipboard_write(
		cMIME,
		contentsPointer,
		C.size_t(len(contents)),
		&cError,
	)
	runtime.KeepAlive(contents)
	return nativeClipboardError(result, cError)
}

func nativeClipboardRead(mimeType string) ([]byte, error) {
	cMIME := C.CString(mimeType)
	defer C.free(unsafe.Pointer(cMIME))
	var contents unsafe.Pointer
	var length C.size_t
	var cError *C.char
	result := C.host_clipboard_read(cMIME, &contents, &length, &cError)
	return nativeClipboardResult(result, contents, length, cError)
}

func nativeClipboardTypes() ([]string, error) {
	var contents unsafe.Pointer
	var length C.size_t
	var cError *C.char
	result := C.host_clipboard_types(&contents, &length, &cError)
	encoded, err := nativeClipboardResult(result, contents, length, cError)
	if err != nil {
		return nil, err
	}
	if len(encoded) == 0 {
		return nil, nil
	}
	return strings.Split(strings.TrimSuffix(string(encoded), "\n"), "\n"), nil
}

func nativeClipboardFiles() ([]string, error) {
	var contents unsafe.Pointer
	var length C.size_t
	var cError *C.char
	result := C.host_clipboard_files(&contents, &length, &cError)
	encoded, err := nativeClipboardResult(result, contents, length, cError)
	if err != nil {
		return nil, err
	}
	if len(encoded) == 0 {
		return nil, nil
	}
	parts := bytes.Split(bytes.TrimSuffix(encoded, []byte{0}), []byte{0})
	paths := make([]string, 0, len(parts))
	for _, part := range parts {
		if len(part) != 0 {
			paths = append(paths, string(part))
		}
	}
	return paths, nil
}

func nativeClipboardResult(
	result C.int,
	contents unsafe.Pointer,
	length C.size_t,
	cError *C.char,
) ([]byte, error) {
	if contents != nil {
		defer C.free(contents)
	}
	if err := nativeClipboardError(result, cError); err != nil {
		return nil, err
	}
	if uint64(length) > uint64(maxClipboardBytes) {
		return nil, errors.New("host clipboard exceeds size limit")
	}
	return C.GoBytes(contents, C.int(length)), nil
}

func nativeClipboardError(result C.int, cError *C.char) error {
	if cError != nil {
		defer C.free(unsafe.Pointer(cError))
	}
	if result != 0 {
		return nil
	}
	if cError == nil {
		return errors.New("macOS clipboard operation failed")
	}
	return errors.New(C.GoString(cError))
}
