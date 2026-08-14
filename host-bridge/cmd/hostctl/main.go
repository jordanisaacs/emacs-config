package main

import (
	"os"

	bridge "snowytrees.dev/emacs-host-bridge"
)

func main() {
	os.Exit(bridge.RunHostctl(os.Args[0], os.Args[1:], os.Stdin, os.Stdout, os.Stderr))
}
