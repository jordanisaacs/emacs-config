package bridge

import (
	"archive/tar"
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestClipboardArchiveRoundTrip(t *testing.T) {
	sourceRoot := t.TempDir()
	folder := filepath.Join(sourceRoot, "copied-folder")
	if err := os.MkdirAll(filepath.Join(folder, "nested"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(folder, "nested", "hello.txt"), []byte("hello\n"), 0o640); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink("nested/hello.txt", filepath.Join(folder, "hello-link")); err != nil {
		t.Fatal(err)
	}
	standalone := filepath.Join(sourceRoot, "standalone.bin")
	if err := os.WriteFile(standalone, []byte{0, 1, 2, 3}, 0o600); err != nil {
		t.Fatal(err)
	}

	entries, err := prepareClipboardArchive(context.Background(), []string{folder, standalone})
	if err != nil {
		t.Fatal(err)
	}
	var encoded bytes.Buffer
	if err := writeClipboardArchive(context.Background(), &encoded, entries); err != nil {
		t.Fatal(err)
	}
	paths, err := materializeClipboardArchive(&encoded)
	if err != nil {
		t.Fatal(err)
	}
	if len(paths) != 2 {
		t.Fatalf("paths = %q", paths)
	}
	t.Cleanup(func() { _ = os.RemoveAll(filepath.Dir(paths[0])) })

	contents, err := os.ReadFile(filepath.Join(paths[0], "nested", "hello.txt"))
	if err != nil || string(contents) != "hello\n" {
		t.Fatalf("nested file = %q, %v", contents, err)
	}
	link, err := os.Readlink(filepath.Join(paths[0], "hello-link"))
	if err != nil || link != "nested/hello.txt" {
		t.Fatalf("symlink = %q, %v", link, err)
	}
	contents, err = os.ReadFile(paths[1])
	if err != nil || !bytes.Equal(contents, []byte{0, 1, 2, 3}) {
		t.Fatalf("standalone file = %v, %v", contents, err)
	}
}

func TestMaterializeClipboardArchiveRejectsTraversal(t *testing.T) {
	var encoded bytes.Buffer
	archive := tar.NewWriter(&encoded)
	if err := archive.WriteHeader(&tar.Header{
		Name:     "../../outside",
		Mode:     0o600,
		Size:     4,
		Typeflag: tar.TypeReg,
	}); err != nil {
		t.Fatal(err)
	}
	if _, err := archive.Write([]byte("nope")); err != nil {
		t.Fatal(err)
	}
	if err := archive.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := materializeClipboardArchive(&encoded); err == nil || !strings.Contains(err.Error(), "unsafe path") {
		t.Fatalf("error = %v", err)
	}
}

func TestMaterializeClipboardImageUsesPrivateTemporaryFile(t *testing.T) {
	path, err := materializeClipboardImage("image/png", []byte("png"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(filepath.Dir(path)) })
	if filepath.Ext(path) != ".png" {
		t.Fatalf("path = %q", path)
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm() != 0o600 {
		t.Fatalf("mode = %o", info.Mode().Perm())
	}
}
