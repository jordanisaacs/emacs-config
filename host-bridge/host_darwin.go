//go:build darwin && cgo

package bridge

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"
)

const defaultNotificationHelper = "terminal-notifier"

type macHostActions struct {
	clipboard sync.Mutex
}

func newPlatformHostActions() (hostActions, error) {
	return &macHostActions{}, nil
}

func (*macHostActions) Notify(ctx context.Context, notification notificationRequest) error {
	helper := os.Getenv("HOSTD_NOTIFICATION_HELPER")
	if helper == "" {
		helper = defaultNotificationHelper
	}
	return runHostCommand(ctx, nil, helper, terminalNotifierArguments(notification)...)
}

func (*macHostActions) OpenURL(ctx context.Context, target string) error {
	return runHostCommand(ctx, nil, "/usr/bin/open", target)
}

func (host *macHostActions) WriteClipboard(ctx context.Context, mimeType string, contents []byte) error {
	host.clipboard.Lock()
	defer host.clipboard.Unlock()
	if err := ctx.Err(); err != nil {
		return err
	}
	err := nativeClipboardWrite(mimeType, contents)
	if contextErr := ctx.Err(); contextErr != nil {
		return contextErr
	}
	return err
}

func (host *macHostActions) ReadClipboard(ctx context.Context, mimeType string) ([]byte, error) {
	host.clipboard.Lock()
	defer host.clipboard.Unlock()
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	contents, err := nativeClipboardRead(mimeType)
	if contextErr := ctx.Err(); contextErr != nil {
		return nil, contextErr
	}
	return contents, err
}

func (host *macHostActions) ClipboardTypes(ctx context.Context) ([]string, error) {
	host.clipboard.Lock()
	defer host.clipboard.Unlock()
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	types, err := nativeClipboardTypes()
	if contextErr := ctx.Err(); contextErr != nil {
		return nil, contextErr
	}
	return types, err
}

func (host *macHostActions) ClipboardFiles(ctx context.Context) ([]string, error) {
	host.clipboard.Lock()
	defer host.clipboard.Unlock()
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	paths, err := nativeClipboardFiles()
	if contextErr := ctx.Err(); contextErr != nil {
		return nil, contextErr
	}
	return paths, err
}

func runHostCommand(ctx context.Context, stdin io.Reader, program string, args ...string) error {
	command := exec.CommandContext(ctx, program, args...)
	command.Stdin = stdin
	var stderr bytes.Buffer
	command.Stderr = &stderr
	if err := command.Run(); err != nil {
		return commandError(program, err, stderr.Bytes())
	}
	return nil
}

func commandError(program string, err error, stderr []byte) error {
	detail := string(bytes.TrimSpace(stderr))
	if detail == "" {
		return fmt.Errorf("%s failed: %w", program, err)
	}
	if len(detail) > 4096 {
		detail = detail[:4096]
	}
	return fmt.Errorf("%s failed: %w: %s", program, err, detail)
}
