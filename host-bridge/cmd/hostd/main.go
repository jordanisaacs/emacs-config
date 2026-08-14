package main

import (
	"os"

	bridge "snowytrees.dev/emacs-host-bridge"
)

func main() {
	os.Exit(bridge.RunHostd(os.Args[1:], os.Stdout, os.Stderr))
}
