;;
;; An I/O port that supports different endian formats.
;; Extended with vector reading and writing capabilities.
;;
;; Copyright 2005-2008, 2012-2026 Ivan Raikov, Shawn Rutledge
;; Ported to Chicken 4 by Shawn Rutledge s@ecloud.org
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(module endian-port
	(make-endian-port
	 endian-port?
	 endian-port-fileno
	 endian-port-filename
	 endian-port-byte-order
	 close-endian-port
	 open-endian-port
	 port->endian-port
	 set-bigendian!
	 set-littlendian!
	 setpos
	 pos
	 eof?
	 read-int1
	 read-int2
	 read-int4
	 read-uint1
	 read-uint2
	 read-uint4
	 read-ieee-float32
	 read-ieee-float64
	 read-bit-vector
	 read-byte-vector
	 write-int1
	 write-int2
	 write-int4
	 write-uint1
	 write-uint2
	 write-uint4
	 write-ieee-float32
	 write-ieee-float64
	 write-bit-vector
	 write-byte-vector
	 read-int1-vector
	 read-int2-vector
	 read-int4-vector
	 read-uint1-vector
	 read-uint2-vector
	 read-uint4-vector
	 read-ieee-float32-vector
	 read-ieee-float64-vector
	 write-int1-vector
	 write-int2-vector
	 write-int4-vector
	 write-uint1-vector
	 write-uint2-vector
	 write-uint4-vector
	 write-ieee-float32-vector
	 write-ieee-float64-vector
	 MSB LSB)

        (import  scheme (chicken base) (chicken port) (chicken bitwise)
                 (chicken file posix) (chicken bytevector) (chicken number-vector)
                 iset byte-sequence endian-sequence)

;------------------------------------
;  Endian port data structures
;

; Structure: endian-port
;
;  * fileno:  file handle corresponding to the port
;  * filename:  file name corresponding to the port
;  * byte-order: can be MSB or LSB (constants defined in module endian-sequence)
;

(define-record endian-port fileno filename byte-order)


;------------------------------------
;  Endian port routines
;


; Procedure:
; close-endian-port:: ENDIAN-PORT -> UNDEFINED
;
; Closes the endian port.
;
(define (close-endian-port eport)
  (file-close (endian-port-fileno eport)))

; Procedure:
; open-endian-port MODE FILENAME [WRITE-MODE] -> ENDIAN-PORT
;
; Opens an endian port to the specified file. Mode can be one of 'read
; or 'write. For write mode, the optional WRITE-MODE parameter can be
; 'truncate (default) to overwrite existing files, or 'append to add
; to the end of existing files. The file is created if it doesn't exist.
; The default endianness of the newly created endian port is MSB.
;
(define (open-endian-port mode filename . rest)
  (let-optionals rest ((write-mode 'truncate))
    (cond ((eq? mode 'read)
	   (let ((fd (file-open filename (bitwise-ior open/read open/binary))))
	     (if (< fd 0)
	         (error 'endian-port  "unable to open file: " filename)
	         (make-endian-port fd filename MSB))))
	  (else
	   (let ((flags (cond ((eq? write-mode 'append)
			       (bitwise-ior open/write open/append open/creat open/binary))
			      ((eq? write-mode 'truncate)
			       (bitwise-ior open/write open/trunc open/creat open/binary))
			      (else
			       (error 'endian-port "invalid write-mode, must be 'append or 'truncate: " write-mode)))))
	     (let ((fd (file-open filename flags)))
	       (if (< fd 0)
	           (error 'endian-port  "unable to open file: " filename)
	           (make-endian-port fd filename MSB))))))))

; Procedure:
; port->endian-port:: PORT -> ENDIAN-PORT
;
; Creates an endian port to the file specified by the given port. The
; default endianness of the newly created endian port is MSB.
;
(define (port->endian-port port)
  (make-endian-port (port->fileno port) (port-name port) MSB))


; Procedure:
; set-bigendian!:: EPORT -> UNSPECIFIED
;
; Sets the endianness of the given endian port to MSB.
;
(define (set-bigendian! eport)
  (endian-port-byte-order-set! eport MSB))

; Procedure:
; set-littlendian!:: EPORT -> UNSPECIFIED
;
; Sets the endianness of the given endian port to LSB.
;
(define (set-littlendian! eport)
  (endian-port-byte-order-set! eport LSB))


; Procedure:
; setpos:: EPORT INTEGER [WHENCE] -> UNSPECIFIED
;
; Sets the file position of the given endian port to the specified
; position. The optional argument WHENCE is one of seek/set, seek/cur,
; seek/end. The default is seek/set (current position).
;
(define (setpos eport pos . rest)
  (let-optionals rest ((whence #f))
		 (cond ((not whence)
			(set-file-position! (endian-port-fileno eport) pos seek/set))
		       (else (set-file-position! (endian-port-fileno eport) pos whence)))))


; Procedure:
;  pos:: EPORT  -> INTEGER
;
; Returns the current file position of the given endian port, relative
; to the beginning of the file.
;
(define (pos eport)
  (file-position  (endian-port-fileno eport)))


; Procedure:
;  eof?:: EPORT  -> BOOLEAN
;
; Returns true if the current file position of the given endian port
; is at the end of the file, false otherwise.
;
(define (eof? eport)
  (zero? (- (file-size  (endian-port-fileno eport))
	    (file-position  (endian-port-fileno eport)))))


;------------------------------------
;  Low-level I/O helpers
;

; Procedure:
; read-raw-bytes:: EPORT * COUNT -> BYTEVECTOR | #f
;
; Reads COUNT bytes from the endian port and returns a bytevector, or
; #f if the read reaches the end of the file before all requested
; bytes are read.
;
(define (read-raw-bytes eport count)
  (let ((ret (file-read (endian-port-fileno eport) count (make-bytevector count))))
    (and (= (cadr ret) count) (car ret))))

; Procedure:
; write-raw-bytes:: EPORT * BYTEVECTOR [* COUNT] -> INTEGER
;
; Writes the first COUNT bytes of the bytevector (all of them by
; default) to the endian port and returns the number of bytes written.
;
(define (write-raw-bytes eport bv . rest)
  (let-optionals rest ((count (bytevector-length bv)))
    (file-write (endian-port-fileno eport) bv count)))

; Procedure:
; bytes->endian-sequence:: BYTEVECTOR * BYTE-ORDER -> ENDIAN-SEQUENCE
;
; Views raw bytes read from a file as an endian sequence in the given
; byte order.
;
(define (bytes->endian-sequence bv byte-order)
  (byte-sequence->endian-sequence (bytevector->byte-sequence bv) byte-order))

; Procedure:
; read-endian:: EPORT * SIZE * DECODE * BYTE-ORDER -> VALUE | #f
;
; Reads SIZE bytes and decodes them with DECODE, a procedure from an
; endian sequence to a value. Returns #f on a short read.
;
(define (read-endian eport size decode byte-order)
  (let ((bv (read-raw-bytes eport size)))
    (and bv (decode (bytes->endian-sequence bv byte-order)))))

; Procedure:
; write-endian-sequence:: EPORT * ENDIAN-SEQUENCE -> INTEGER
;
; Writes the bytes of the endian sequence and returns the number of
; bytes written.
;
(define (write-endian-sequence eport es)
  (write-raw-bytes eport
		   (byte-sequence->bytevector (endian-sequence->byte-sequence es))
		   (endian-sequence-length es)))


;------------------------------------
;  Scalar Reading Operations
;

; Procedure:
; read-uint1:: EPORT [* BYTE-ORDER] -> UINTEGER | #f
;
; Reads an unsigned integer of size 1 byte. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-uint1 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 1 endian-sequence->uint1 byte-order)))


; Procedure:
; read-uint2:: EPORT [* BYTE-ORDER] -> UINTEGER | #f
;
; Reads an unsigned integer of size 2 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-uint2 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 2 endian-sequence->uint2 byte-order)))


; Procedure:
; read-uint4:: EPORT [* BYTE-ORDER] -> UINTEGER | #f
;
; Reads an unsigned integer of size 4 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-uint4 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 4 endian-sequence->uint4 byte-order)))

; Procedure:
; read-int1:: EPORT [* BYTE-ORDER] -> INTEGER | #f
;
; Reads a signed integer of size 1 byte. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-int1 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 1 endian-sequence->sint1 byte-order)))


; Procedure:
; read-int2:: EPORT [* BYTE-ORDER] -> INTEGER | #f
;
; Reads a signed integer of size 2 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-int2 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 2 endian-sequence->sint2 byte-order)))


; Procedure:
; read-int4:: EPORT [* BYTE-ORDER] -> INTEGER | #f
;
; Reads a signed integer of size 4 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-int4 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 4 endian-sequence->sint4 byte-order)))

; Procedure:
; read-ieee-float32:: EPORT [* BYTE-ORDER] -> REAL | #f
;
; Reads an IEEE 754 single precision floating-point number. Optional
; argument BYTE-ORDER is one of MSB or LSB. If byte order is not
; specified, then use the byte order setting of the given endian port.
;
(define (read-ieee-float32 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 4 endian-sequence->ieee_float32 byte-order)))

; Procedure:
; read-ieee-float64:: EPORT [* BYTE-ORDER] -> REAL | #f
;
; Reads an IEEE 754 double precision floating-point number. Optional
; argument BYTE-ORDER is one of MSB or LSB. If byte order is not
; specified, then use the byte order setting of the given endian port.
;
(define (read-ieee-float64 eport . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian eport 8 endian-sequence->ieee_float64 byte-order)))


;------------------------------------
;  Bit and byte vectors
;
; A bit vector of SIZE bits is stored in ceiling(SIZE/8) bytes. Each
; byte holds a contiguous chunk of up to 8 bits, with bit vector index
; LO+m stored in bit m of the byte (bit 0 being the least significant).
;
; * LSB order: byte k holds the chunk starting at LO = 8k, so the
;   chunks appear from the lowest index upward and the last byte may
;   be partial.
;
; * MSB order: byte k holds the chunk starting at
;   LO = max(0, SIZE - 8(k+1)), so the chunks appear from the highest
;   index downward and the last byte holds the remaining low bits.
;

; Procedure:
; bit-chunk:: SIZE * K * BYTE-ORDER -> (values LO WIDTH)
;
; Returns the starting index and the number of bits of the chunk of
; the bit vector stored in byte K.
;
(define (bit-chunk size k byte-order)
  (if (eq? byte-order MSB)
      (let ((lo (max 0 (- size (* 8 (+ k 1))))))
	(values lo (- size (* 8 k) lo)))
      (let ((lo (* 8 k)))
	(values lo (min 8 (- size lo))))))


; Procedure:
; read-bit-vector:: PORT * SIZE (in bits) [* BYTE-ORDER] -> BIT-VECTOR | #f
;
; Reads a bit vector of the specified size (in bits) and returns an
; iset bit vector (see module iset). Optional argument BYTE-ORDER is
; one of MSB or LSB. If byte order is not specified, then use the
; byte order setting of the given endian port. Returns #f if the end
; of the file is reached first.
;
(define (read-bit-vector eport size . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (let* ((nb (quotient (+ size 7) 8))
	   (bytes (read-raw-bytes eport nb)))
      (and bytes
	   (let ((bv (make-bit-vector size)))
	     (do ((k 0 (+ k 1))) ((= k nb) bv)
	       (let-values (((lo width) (bit-chunk size k byte-order)))
		 (let ((byte (bytevector-u8-ref bytes k)))
		   (do ((m 0 (+ m 1))) ((= m width))
		     (bit-vector-set! bv (+ lo m) (bit->boolean byte m)))))))))))


; Procedure:
; write-bit-vector:: PORT * BIT-VECTOR [* BIT-ORDER [* SIZE]] -> UINTEGER
;
; Writes the given bit vector and returns the number of bytes
; written. The argument must be a bit vector as defined in the iset
; module. Optional argument BIT-ORDER is one of MSB or LSB.  If
; bit order is not specified, then use the byte order setting of the
; given endian port. The layout is the inverse of read-bit-vector.
;
; An iset bit vector does not record its size: bit-vector-length is
; one more than the index of its highest set bit. Optional argument
; SIZE (in bits) gives the intended size, and defaults to
; bit-vector-length. Since the MSB layout depends on the size, SIZE
; should match the size later passed to read-bit-vector.
;
(define (write-bit-vector eport bv . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport))
		       (size       (bit-vector-length bv)))
    (let* ((nb    (quotient (+ size 7) 8))
	   (bytes (make-bytevector nb 0)))
      (do ((k 0 (+ k 1))) ((= k nb))
	(let-values (((lo width) (bit-chunk size k byte-order)))
	  (do ((m 0 (+ m 1))
	       (byte 0 (if (bit-vector-ref bv (+ lo m))
			   (bitwise-ior byte (arithmetic-shift 1 m))
			   byte)))
	      ((= m width) (bytevector-u8-set! bytes k byte)))))
      (write-raw-bytes eport bytes))))


; Procedure:
; reverse-bytevector:: BYTEVECTOR -> BYTEVECTOR
;
; Returns a fresh bytevector with the bytes of the argument in reverse
; order.
;
(define (reverse-bytevector bv)
  (let* ((n (bytevector-length bv))
	 (r (make-bytevector n)))
    (do ((i 0 (+ i 1))) ((= i n) r)
      (bytevector-u8-set! r i (bytevector-u8-ref bv (- n i 1))))))


; Procedure:
; read-byte-vector:: PORT * SIZE [* BYTE-ORDER]  -> BYTEVECTOR | #f
;
; Reads an unsigned byte vector of the specified size and returns a
; bytevector. Optional argument BYTE-ORDER is one of MSB or LSB. If
; byte order is not specified, then use the byte order setting of the
; given endian port. In LSB order the bytes are returned in reverse
; file order. Returns #f if the end of the file is reached first.
;
(define (read-byte-vector eport size . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (let ((bytes (read-raw-bytes eport size)))
      (and bytes
	   (if (eq? byte-order MSB) bytes (reverse-bytevector bytes))))))


; Procedure:
; write-byte-vector:: PORT * BYTE-VECTOR [* BYTE-ORDER] -> UINTEGER
;
; Writes the given unsigned byte vector and returns the number of
; bytes written. The argument may be a bytevector (u8vector), a byte
; sequence or an endian sequence; the byte order recorded in an endian
; sequence is not used. Optional argument BYTE-ORDER is one of MSB or
; LSB. If byte order is not specified, then use the byte order setting
; of the given endian port. In LSB order the bytes are written in
; reverse order.
;
(define (write-byte-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (let ((bytes (cond ((bytevector? vect) vect)
		       ((byte-sequence? vect) (byte-sequence->u8vector vect))
		       ((endian-sequence? vect)
			(byte-sequence->u8vector (endian-sequence->byte-sequence vect)))
		       (else (error 'write-byte-vector
				    "argument must be a bytevector, byte sequence or endian sequence"
				    vect)))))
      (write-raw-bytes eport (if (eq? byte-order MSB) bytes (reverse-bytevector bytes))))))


;------------------------------------
;  Vector Reading Operations
;

; Procedure:
; read-endian-vector:: EPORT * COUNT * ELEMENT-SIZE * DECODE * EMPTY * BYTE-ORDER -> VECTOR | #f
;
; Reads COUNT elements of ELEMENT-SIZE bytes each and decodes them
; with DECODE, a procedure from an endian sequence to a homogeneous
; vector. EMPTY is the empty vector returned when COUNT is zero.
;
(define (read-endian-vector eport count element-size decode empty byte-order)
  (if (zero? count)
      empty
      (read-endian eport (* count element-size) decode byte-order)))

; Procedure:

; read-uint1-vector:: EPORT * COUNT [* BYTE-ORDER] -> U8VECTOR | #f
;
; Reads a vector of unsigned integers of size 1 byte each. COUNT specifies
; the number of elements to read. Returns a u8vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-uint1-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 1 endian-sequence->u8vector (u8vector) byte-order)))

; Procedure:
; read-uint2-vector:: EPORT * COUNT [* BYTE-ORDER] -> U16VECTOR | #f
;
; Reads a vector of unsigned integers of size 2 bytes each. COUNT specifies
; the number of elements to read. Returns a u16vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-uint2-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 2 endian-sequence->u16vector (u16vector) byte-order)))

; Procedure:
; read-uint4-vector:: EPORT * COUNT [* BYTE-ORDER] -> U32VECTOR | #f
;
; Reads a vector of unsigned integers of size 4 bytes each. COUNT specifies
; the number of elements to read. Returns a u32vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-uint4-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 4 endian-sequence->u32vector (u32vector) byte-order)))

; Procedure:
; read-int1-vector:: EPORT * COUNT [* BYTE-ORDER] -> S8VECTOR | #f
;
; Reads a vector of signed integers of size 1 byte each. COUNT specifies
; the number of elements to read. Returns a s8vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-int1-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 1 endian-sequence->s8vector (s8vector) byte-order)))

; Procedure:
; read-int2-vector:: EPORT * COUNT [* BYTE-ORDER] -> S16VECTOR | #f
;
; Reads a vector of signed integers of size 2 bytes each. COUNT specifies
; the number of elements to read. Returns a s16vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-int2-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 2 endian-sequence->s16vector (s16vector) byte-order)))

; Procedure:
; read-int4-vector:: EPORT * COUNT [* BYTE-ORDER] -> S32VECTOR | #f
;
; Reads a vector of signed integers of size 4 bytes each. COUNT specifies
; the number of elements to read. Returns a s32vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-int4-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 4 endian-sequence->s32vector (s32vector) byte-order)))

; Procedure:
; read-ieee-float32-vector:: EPORT * COUNT [* BYTE-ORDER] -> F32VECTOR | #f
;
; Reads a vector of IEEE 754 single precision floating-point numbers.
; COUNT specifies the number of elements to read. Returns a f32vector
; or #f if reading fails. Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-ieee-float32-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 4 endian-sequence->f32vector (f32vector) byte-order)))

; Procedure:
; read-ieee-float64-vector:: EPORT * COUNT [* BYTE-ORDER] -> F64VECTOR | #f
;
; Reads a vector of IEEE 754 double precision floating-point numbers.
; COUNT specifies the number of elements to read. Returns a f64vector
; or #f if reading fails. Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-ieee-float64-vector eport count . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (read-endian-vector eport count 8 endian-sequence->f64vector (f64vector) byte-order)))


;------------------------------------
;  Scalar Writing Operations
;

; Procedure:
; write-uint1:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an unsigned integer of size 1 byte. Returns the number of
; bytes written (always 1). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-uint1 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (uint1->endian-sequence word byte-order))))

; Procedure:
; write-uint2:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an unsigned integer of size 2 bytes. Returns the number of
; bytes written (always 2). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-uint2 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (uint2->endian-sequence word byte-order))))

; Procedure:
; write-uint4:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an unsigned integer of size 4 bytes. Returns the number of
; bytes written (always 4). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-uint4 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (uint4->endian-sequence word byte-order))))

; Procedure:
; write-int1:: EPORT * WORD [* BYTE-ORDER] -> INTEGER
;
; Writes a signed integer of size 1 byte. Returns the number of
; bytes written (always 1). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-int1 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (sint1->endian-sequence word byte-order))))

; Procedure:
; write-int2:: EPORT * WORD [* BYTE-ORDER] -> INTEGER
;
; Writes a signed integer of size 2 bytes. Returns the number of
; bytes written (always 2). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-int2 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (sint2->endian-sequence word byte-order))))

; Procedure:
; write-int4:: EPORT * WORD [* BYTE-ORDER] -> INTEGER
;
; Writes a signed integer of size 4 bytes. Returns the number of
; bytes written (always 4). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-int4 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (sint4->endian-sequence word byte-order))))

; Procedure:
; write-ieee-float32:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an IEEE 754 single precision floating-point number. Returns
; the number of bytes written (always 4). Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use
; the byte order setting of the given endian port.
;
(define (write-ieee-float32 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (ieee_float32->endian-sequence word byte-order))))

; Procedure:
; write-ieee-float64:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an IEEE 754 double precision floating-point number. Returns
; the number of bytes written (always 8). Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use
; the byte order setting of the given endian port.
;
(define (write-ieee-float64 eport word . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (ieee_float64->endian-sequence word byte-order))))


;------------------------------------
;  Vector Writing Operations
;

; Procedure:
; write-uint1-vector:: EPORT * U8VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of unsigned integers of size 1 byte each. Returns the
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-uint1-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (u8vector->endian-sequence vect byte-order))))

; Procedure:
; write-uint2-vector:: EPORT * U16VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of unsigned integers of size 2 bytes each. Returns the
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-uint2-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (u16vector->endian-sequence vect byte-order))))

; Procedure:
; write-uint4-vector:: EPORT * U32VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of unsigned integers of size 4 bytes each. Returns the
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-uint4-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (u32vector->endian-sequence vect byte-order))))

; Procedure:
; write-int1-vector:: EPORT * S8VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of signed integers of size 1 byte each. Returns the
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-int1-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (s8vector->endian-sequence vect byte-order))))

; Procedure:
; write-int2-vector:: EPORT * S16VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of signed integers of size 2 bytes each. Returns the
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-int2-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (s16vector->endian-sequence vect byte-order))))

; Procedure:
; write-int4-vector:: EPORT * S32VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of signed integers of size 4 bytes each. Returns the
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-int4-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (s32vector->endian-sequence vect byte-order))))

; Procedure:
; write-ieee-float32-vector:: EPORT * F32VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of IEEE 754 single precision floating-point numbers.
; Returns the total number of bytes written. Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-ieee-float32-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (f32vector->endian-sequence vect byte-order))))

; Procedure:
; write-ieee-float64-vector:: EPORT * F64VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of IEEE 754 double precision floating-point numbers.
; Returns the total number of bytes written. Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-ieee-float64-vector eport vect . rest)
  (let-optionals rest ((byte-order (endian-port-byte-order eport)))
    (write-endian-sequence eport (f64vector->endian-sequence vect byte-order))))

) ;; end of module
