package tinyvg

import "core:bytes"
import "core:io"

Reader :: struct {
    using r: bytes.Reader
}

read_data :: #force_inline proc(r: ^Reader, $T: typeid) -> (res: T, err: io.Error) {
    b: [size_of(T)]byte

    bytes.reader_read(r, b[:]) or_return
    return (^T)(&b[0])^, nil
}
