//go:build darwin && !cgo

package bridge

import "errors"

func newPlatformHostActions() (hostActions, error) {
	return nil, errors.New("hostd requires cgo to access the macOS clipboard through AppKit")
}
