package main

import "core:fmt"
import "core:os/os2"

import tinyvg "../"

main :: proc() {
    if len(os2.args) < 2 {
        fmt.println("Provide a path to a tvg file")
        return
    }

    tvg, err := tinyvg.load_from_file(os2.args[1])
    if err != nil {
        fmt.println(err)
    }

    fmt.println(tvg)
}
