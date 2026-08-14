//go:build !darwin

package bridge

import "errors"

func newPlatformHostActions() (hostActions, error) {
	return nil, errors.New("the host bridge server is supported only on macOS")
}
