package bridge

import (
	"archive/tar"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path"
	"path/filepath"
	"strings"
	"time"
)

const (
	clipboardArchiveMIME             = "application/x-tar"
	maxClipboardFileTransferDuration = 5 * time.Minute
)

var errClipboardHasNoFiles = errors.New("clipboard has no files")

type clipboardArchiveEntry struct {
	source     string
	name       string
	info       os.FileInfo
	linkTarget string
}

func prepareClipboardArchive(ctx context.Context, sources []string) ([]clipboardArchiveEntry, error) {
	entries := make([]clipboardArchiveEntry, 0)
	usedRoots := make(map[string]bool)
	var totalBytes int64

	for _, source := range sources {
		if err := ctx.Err(); err != nil {
			return nil, err
		}
		if !filepath.IsAbs(source) {
			return nil, fmt.Errorf("clipboard file path is not absolute")
		}
		source = filepath.Clean(source)
		info, err := os.Lstat(source)
		if err != nil {
			return nil, fmt.Errorf("inspect copied file: %w", err)
		}
		rootName := uniqueClipboardRoot(filepath.Base(source), usedRoots)

		addEntry := func(current string, info os.FileInfo) error {
			if err := ctx.Err(); err != nil {
				return err
			}
			if len(entries) >= maxClipboardFileEntries {
				return fmt.Errorf("copied files exceed %d-entry limit", maxClipboardFileEntries)
			}
			relative, err := filepath.Rel(source, current)
			if err != nil {
				return err
			}
			archiveName := rootName
			if relative != "." {
				archiveName = path.Join(rootName, filepath.ToSlash(relative))
			}
			entry := clipboardArchiveEntry{source: current, name: archiveName, info: info}
			switch {
			case info.Mode().IsRegular():
				if info.Size() < 0 || totalBytes > maxClipboardFileBytes-info.Size() {
					return fmt.Errorf("copied files exceed %d MiB limit", maxClipboardFileBytes/(1024*1024))
				}
				totalBytes += info.Size()
			case info.IsDir():
			case info.Mode()&os.ModeSymlink != 0:
				entry.linkTarget, err = os.Readlink(current)
				if err != nil {
					return fmt.Errorf("read copied symlink: %w", err)
				}
				if err := validateArchiveSymlink(archiveName, filepath.ToSlash(entry.linkTarget)); err != nil {
					return err
				}
			default:
				return fmt.Errorf("copied item %q is not a regular file, directory, or symlink", info.Name())
			}
			entries = append(entries, entry)
			return nil
		}

		if info.IsDir() {
			err = filepath.Walk(source, func(current string, info os.FileInfo, walkErr error) error {
				if walkErr != nil {
					return walkErr
				}
				return addEntry(current, info)
			})
		} else {
			err = addEntry(source, info)
		}
		if err != nil {
			return nil, fmt.Errorf("prepare copied item %q: %w", info.Name(), err)
		}
	}
	if len(entries) == 0 {
		return nil, errClipboardHasNoFiles
	}
	return entries, nil
}

func uniqueClipboardRoot(name string, used map[string]bool) string {
	if !used[name] {
		used[name] = true
		return name
	}
	for suffix := 2; ; suffix++ {
		candidate := fmt.Sprintf("%s-%d", name, suffix)
		if !used[candidate] {
			used[candidate] = true
			return candidate
		}
	}
}

func writeClipboardArchive(ctx context.Context, writer io.Writer, entries []clipboardArchiveEntry) error {
	archive := tar.NewWriter(writer)
	for _, entry := range entries {
		if err := ctx.Err(); err != nil {
			return err
		}
		header, err := tar.FileInfoHeader(entry.info, filepath.ToSlash(entry.linkTarget))
		if err != nil {
			return err
		}
		header.Name = entry.name
		header.Uid = 0
		header.Gid = 0
		header.Uname = ""
		header.Gname = ""
		if entry.info.IsDir() {
			header.Name += "/"
		}
		if err := archive.WriteHeader(header); err != nil {
			return err
		}
		if !entry.info.Mode().IsRegular() {
			continue
		}
		file, err := os.Open(entry.source)
		if err != nil {
			return fmt.Errorf("open copied file: %w", err)
		}
		_, copyErr := io.CopyN(archive, &contextReader{ctx: ctx, reader: file}, entry.info.Size())
		closeErr := file.Close()
		if copyErr != nil {
			return fmt.Errorf("read copied file: %w", copyErr)
		}
		if closeErr != nil {
			return closeErr
		}
	}
	return archive.Close()
}

type contextReader struct {
	ctx    context.Context
	reader io.Reader
}

func (reader *contextReader) Read(buffer []byte) (int, error) {
	if err := reader.ctx.Err(); err != nil {
		return 0, err
	}
	return reader.reader.Read(buffer)
}

type pendingSymlink struct {
	name   string
	target string
}

type pendingDirectoryMode struct {
	name string
	mode os.FileMode
}

func materializeClipboardArchive(reader io.Reader) (paths []string, err error) {
	directory, err := os.MkdirTemp("", "wl-paste-")
	if err != nil {
		return nil, fmt.Errorf("create clipboard temporary directory: %w", err)
	}
	keep := false
	defer func() {
		if !keep {
			_ = os.RemoveAll(directory)
		}
	}()

	archive := tar.NewReader(reader)
	roots := make([]string, 0)
	seenRoots := make(map[string]bool)
	symlinks := make([]pendingSymlink, 0)
	directoryModes := make([]pendingDirectoryMode, 0)
	var totalBytes int64
	entryCount := 0

	for {
		header, nextErr := archive.Next()
		if errors.Is(nextErr, io.EOF) {
			break
		}
		if nextErr != nil {
			return nil, fmt.Errorf("read clipboard archive: %w", nextErr)
		}
		entryCount++
		if entryCount > maxClipboardFileEntries {
			return nil, fmt.Errorf("clipboard archive exceeds %d-entry limit", maxClipboardFileEntries)
		}
		name, err := validateArchiveName(header.Name)
		if err != nil {
			return nil, err
		}
		root := strings.SplitN(name, "/", 2)[0]
		if !seenRoots[root] {
			seenRoots[root] = true
			roots = append(roots, root)
		}
		target := filepath.Join(directory, filepath.FromSlash(name))
		mode := os.FileMode(header.Mode) & os.ModePerm

		switch header.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(target, 0o700); err != nil {
				return nil, fmt.Errorf("create copied directory: %w", err)
			}
			directoryModes = append(directoryModes, pendingDirectoryMode{name: target, mode: mode})
		case tar.TypeReg, tar.TypeRegA:
			if header.Size < 0 || totalBytes > maxClipboardFileBytes-header.Size {
				return nil, fmt.Errorf("clipboard archive exceeds %d MiB limit", maxClipboardFileBytes/(1024*1024))
			}
			totalBytes += header.Size
			if err := os.MkdirAll(filepath.Dir(target), 0o700); err != nil {
				return nil, fmt.Errorf("create copied file parent: %w", err)
			}
			file, err := os.OpenFile(target, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o600)
			if err != nil {
				return nil, fmt.Errorf("create copied file: %w", err)
			}
			_, copyErr := io.CopyN(file, archive, header.Size)
			chmodErr := file.Chmod(mode)
			closeErr := file.Close()
			if copyErr != nil {
				return nil, fmt.Errorf("write copied file: %w", copyErr)
			}
			if chmodErr != nil {
				return nil, chmodErr
			}
			if closeErr != nil {
				return nil, closeErr
			}
		case tar.TypeSymlink:
			linkTarget := filepath.ToSlash(header.Linkname)
			if err := validateArchiveSymlink(name, linkTarget); err != nil {
				return nil, err
			}
			symlinks = append(symlinks, pendingSymlink{name: target, target: filepath.FromSlash(linkTarget)})
		default:
			return nil, fmt.Errorf("clipboard archive contains unsupported entry type")
		}
	}

	if len(roots) == 0 {
		return nil, errors.New("clipboard archive is empty")
	}
	for _, link := range symlinks {
		if err := os.MkdirAll(filepath.Dir(link.name), 0o700); err != nil {
			return nil, fmt.Errorf("create copied symlink parent: %w", err)
		}
		if _, err := os.Lstat(link.name); !errors.Is(err, os.ErrNotExist) {
			if err == nil {
				return nil, fmt.Errorf("clipboard archive contains duplicate path")
			}
			return nil, err
		}
		if err := os.Symlink(link.target, link.name); err != nil {
			return nil, fmt.Errorf("create copied symlink: %w", err)
		}
	}
	for index := len(directoryModes) - 1; index >= 0; index-- {
		if err := os.Chmod(directoryModes[index].name, directoryModes[index].mode); err != nil {
			return nil, err
		}
	}

	paths = make([]string, 0, len(roots))
	for _, root := range roots {
		paths = append(paths, filepath.Join(directory, filepath.FromSlash(root)))
	}
	keep = true
	return paths, nil
}

func validateArchiveName(raw string) (string, error) {
	if raw == "" || strings.ContainsRune(raw, '\x00') || strings.HasPrefix(raw, "/") {
		return "", errors.New("clipboard archive contains unsafe path")
	}
	raw = strings.TrimSuffix(raw, "/")
	clean := path.Clean(raw)
	if clean != raw || clean == "." || clean == ".." || strings.HasPrefix(clean, "../") {
		return "", errors.New("clipboard archive contains unsafe path")
	}
	return clean, nil
}

func validateArchiveSymlink(name, target string) error {
	if target == "" || strings.ContainsRune(target, '\x00') || path.IsAbs(target) {
		return errors.New("copied symlink points outside the transferred files")
	}
	resolved := path.Clean(path.Join(path.Dir(name), target))
	if resolved == ".." || strings.HasPrefix(resolved, "../") {
		return errors.New("copied symlink points outside the transferred files")
	}
	return nil
}

func materializeClipboardImage(mimeType string, contents []byte) (path string, err error) {
	extension, ok := clipboardImageExtensions[mimeType]
	if !ok {
		return "", fmt.Errorf("clipboard content is not a supported image")
	}
	directory, err := os.MkdirTemp("", "wl-paste-")
	if err != nil {
		return "", fmt.Errorf("create clipboard temporary directory: %w", err)
	}
	keep := false
	defer func() {
		if !keep {
			_ = os.RemoveAll(directory)
		}
	}()
	path = filepath.Join(directory, "clipboard"+extension)
	if err := os.WriteFile(path, contents, 0o600); err != nil {
		return "", fmt.Errorf("write clipboard image: %w", err)
	}
	keep = true
	return path, nil
}

var clipboardImageExtensions = map[string]string{
	"image/png":  ".png",
	"image/jpeg": ".jpg",
	"image/tiff": ".tiff",
	"image/gif":  ".gif",
	"image/bmp":  ".bmp",
	"image/webp": ".webp",
}
