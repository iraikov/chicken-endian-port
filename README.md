# chicken-endian-port

An I/O port that supports different endian formats with efficient vector operations.

## Description

**endian-port** provides procedures for reading and writing binary
data in different endian formats (MSB/LSB) through a specialized port
abstraction. The library supports both individual scalar values and
efficient bulk vector operations for high-performance data processing.

This library builds upon the
[endian-blob](https://github.com/iraikov/chicken-endian-blob)
library's efficient C-based endian conversion routines.


## Requirements

- [endian-blob](https://github.com/iraikov/chicken-endian-blob)
- [iset](https://wiki.call-cc.org/eggref/5/iset)

## API

### Port Management

#### `(make-endian-port fileno filename byte-order)`
**procedure**

Creates an endian port record with the given file descriptor, filename, and byte order.

#### `(endian-port? obj)`
**procedure**

Returns `#t` if `obj` is an endian port, `#f` otherwise.

#### `(open-endian-port mode filename [write-mode])`
**procedure**

Opens an endian port to the specified file.

- **mode**: `'read` or `'write`
- **filename**: path to the file
- **write-mode**: (optional) `'truncate` (default) or `'append` for write mode

Returns an endian port object, or signals an error if the file cannot be opened.

```scheme
;; Open for reading
(open-endian-port 'read "data.bin")

;; Open for writing (truncate existing file)
(open-endian-port 'write "data.bin")
(open-endian-port 'write "data.bin" 'truncate)

;; Open for writing (append to existing file)
(open-endian-port 'write "log.bin" 'append)
```

#### `(port->endian-port port)`
**procedure**

Creates an endian port from an existing Scheme port. The default endianness is MSB.

#### `(close-endian-port eport)`
**procedure**

Closes the endian port.

### Port Properties

#### `(endian-port-fileno eport)`
**procedure**

Returns the file descriptor of the endian port.

#### `(endian-port-filename eport)`
**procedure**

Returns the filename associated with the endian port.

#### `(endian-port-byte-order eport)`
**procedure**

Returns the current byte order (MSB or LSB) of the endian port.

### Endianness Control

#### `(set-bigendian! eport)`
**procedure**

Sets the endianness of the endian port to MSB (big-endian).

#### `(set-littlendian! eport)`
**procedure**

Sets the endianness of the endian port to LSB (little-endian).

### Position Control

#### `(setpos eport pos [whence])`
**procedure**

Sets the file position of the endian port.

- **pos**: position offset
- **whence**: (optional) `seek/set`, `seek/cur`, or `seek/end` (default: `seek/set`)

#### `(pos eport)`
**procedure**

Returns the current file position of the endian port.

#### `(eof? eport)`
**procedure**

Returns `#t` if the endian port is at end-of-file, `#f` otherwise.

### Scalar Reading Operations

#### `(read-int1 eport [byte-order])`
#### `(read-int2 eport [byte-order])`
#### `(read-int4 eport [byte-order])`
**procedures**

Read signed integers of 1, 2, or 4 bytes respectively. Returns the integer value or `#f` on failure.

#### `(read-uint1 eport [byte-order])`
#### `(read-uint2 eport [byte-order])`
#### `(read-uint4 eport [byte-order])`
**procedures**

Read unsigned integers of 1, 2, or 4 bytes respectively. Returns the integer value or `#f` on failure.

#### `(read-ieee-float32 eport [byte-order])`
#### `(read-ieee-float64 eport [byte-order])`
**procedures**

Read IEEE 754 single or double precision floating-point numbers. Returns the float value or `#f` on failure.

#### `(read-bit-vector eport size [byte-order])`
**procedure**

Reads a bit vector of the specified size (in bits). Returns an iset bit-vector.

#### `(read-byte-vector eport size [byte-order])`
**procedure**

Reads an unsigned byte vector of the specified size. Returns a blob.

### Scalar Writing Operations

#### `(write-int1 eport value [byte-order])`
#### `(write-int2 eport value [byte-order])`
#### `(write-int4 eport value [byte-order])`
**procedures**

Write signed integers of 1, 2, or 4 bytes respectively. Returns the number of bytes written.

#### `(write-uint1 eport value [byte-order])`
#### `(write-uint2 eport value [byte-order])`
#### `(write-uint4 eport value [byte-order])`
**procedures**

Write unsigned integers of 1, 2, or 4 bytes respectively. Returns the number of bytes written.

#### `(write-ieee-float32 eport value [byte-order])`
#### `(write-ieee-float64 eport value [byte-order])`
**procedures**

Write IEEE 754 single or double precision floating-point numbers. Returns the number of bytes written.

#### `(write-bit-vector eport bit-vector [byte-order])`
**procedure**

Writes the given bit vector. Returns the number of bytes written.

#### `(write-byte-vector eport byte-vector [byte-order])`
**procedure**

Writes the given byte vector. Returns the number of bytes written.

### Vector Reading Operations

Vector-based I/O operations.

#### `(read-int1-vector eport count [byte-order])`
#### `(read-int2-vector eport count [byte-order])`
#### `(read-int4-vector eport count [byte-order])`
**procedures**

Read vectors of signed integers. Returns `s8vector`, `s16vector`, or `s32vector` respectively, or `#f` on failure.

- **count**: number of elements to read

```scheme
;; Read 1000 16-bit signed integers
(let ([data (read-int2-vector port 1000)])
  (if data
      (printf "Read ~a elements~%" (s16vector-length data))
      (printf "Read failed~%")))
```

#### `(read-uint1-vector eport count [byte-order])`
#### `(read-uint2-vector eport count [byte-order])`
#### `(read-uint4-vector eport count [byte-order])`
**procedures**

Read vectors of unsigned integers. Returns `u8vector`, `u16vector`, or `u32vector` respectively, or `#f` on failure.

#### `(read-ieee-float32-vector eport count [byte-order])`
#### `(read-ieee-float64-vector eport count [byte-order])`
**procedures**

Read vectors of IEEE 754 floating-point numbers. Returns `f32vector` or `f64vector` respectively, or `#f` on failure.

### Vector Writing Operations

#### `(write-int1-vector eport vector [byte-order])`
#### `(write-int2-vector eport vector [byte-order])`
#### `(write-int4-vector eport vector [byte-order])`
**procedures**

Write vectors of signed integers. Accepts `s8vector`, `s16vector`, or `s32vector` respectively. Returns the number of bytes written.

#### `(write-uint1-vector eport vector [byte-order])`
#### `(write-uint2-vector eport vector [byte-order])`
#### `(write-uint4-vector eport vector [byte-order])`
**procedures**

Write vectors of unsigned integers. Accepts `u8vector`, `u16vector`, or `u32vector` respectively. Returns the number of bytes written.

#### `(write-ieee-float32-vector eport vector [byte-order])`
#### `(write-ieee-float64-vector eport vector [byte-order])`
**procedures**

Write vectors of IEEE 754 floating-point numbers. Accepts `f32vector` or `f64vector` respectively. Returns the number of bytes written.

```scheme
;; Write a vector of sensor readings
(let ([readings (f32vector 3.14 2.71 1.41 1.73)])
  (write-ieee-float32-vector port readings)
  (printf "Wrote ~a float values~%" (f32vector-length readings)))
```

## Constants

#### `MSB`
Big-endian byte order constant.

#### `LSB`
Little-endian byte order constant.

## Examples

### Basic Usage

```scheme
(import endian-port srfi-4)

;; Write some data
(let ([port (open-endian-port 'write "test.dat")])
  (set-littlendian! port)
  (write-uint4 port 0x12345678)
  (write-ieee-float32 port 3.14159)
  (close-endian-port port))

;; Read it back
(let ([port (open-endian-port 'read "test.dat")])
  (set-littlendian! port)
  (let ([int-val (read-uint4 port)]
        [float-val (read-ieee-float32 port)])
    (printf "Integer: ~x, Float: ~a~%" int-val float-val))
  (close-endian-port port))
```

### Vector Operations

```scheme
(import endian-port srfi-4)

;; Generate test data
(define sensor-data (make-f32vector 10000))
(do ([i 0 (+ i 1)])
    [(= i 10000)]
  (f32vector-set! sensor-data i (* i 0.001)))

;; Write efficiently using vector operations
(let ([port (open-endian-port 'write "sensors.dat")])
  (set-bigendian! port)
  (let ([bytes-written (write-ieee-float32-vector port sensor-data)])
    (printf "Wrote ~a bytes (~a samples)~%" 
            bytes-written (f32vector-length sensor-data)))
  (close-endian-port port))

;; Read back efficiently
(let ([port (open-endian-port 'read "sensors.dat")])
  (set-bigendian! port)
  (let ([data (read-ieee-float32-vector port 10000)])
    (printf "Read ~a samples~%" (f32vector-length data))
    ;; Process data...
    (let loop ([i 0] [sum 0.0])
      (if (< i (f32vector-length data))
          (loop (+ i 1) (+ sum (f32vector-ref data i)))
          (printf "Average: ~a~%" (/ sum (f32vector-length data))))))
  (close-endian-port port))
```

### Endianness Conversion

```scheme
;; Convert a file from big-endian to little-endian
(define (convert-endianness input-file output-file)
  (let ([in-port (open-endian-port 'read input-file)]
        [out-port (open-endian-port 'write output-file)])
    
    (set-bigendian! in-port)     ; Source is big-endian
    (set-littlendian! out-port)  ; Target is little-endian
    
    ;; Process in chunks
    (let loop ([chunk (read-uint4-vector in-port 1000)])
      (when chunk
        (write-uint4-vector out-port chunk)
        (loop (read-uint4-vector in-port 1000))))
    
    (close-endian-port in-port)
    (close-endian-port out-port)))
```

### Append Mode for Logging

```scheme
;; Log sensor readings incrementally
(define (log-reading timestamp value)
  (let ([port (open-endian-port 'write "sensor.log" 'append)])
    (write-uint4 port timestamp)
    (write-ieee-float32 port value)
    (close-endian-port port)))

;; Batch logging for efficiency
(define (log-readings timestamps values)
  (let ([port (open-endian-port 'write "sensor.log" 'append)])
    (write-uint4-vector port timestamps)
    (write-ieee-float32-vector port values)
    (close-endian-port port)))
```



## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

## Authors

- Ivan Raikov (original implementation)
- Shawn Rutledge (Chicken 4 port)

## See Also

- [endian-blob](https://github.com/iraikov/chicken-endian-blob) - Endian-aware blob manipulation
