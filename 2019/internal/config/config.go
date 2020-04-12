package config

import "os"

var Render = "1" == os.Getenv("AOC_RENDER")
