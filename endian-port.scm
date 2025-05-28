;;
;; An I/O port that supports different endian formats.
;; Extended with vector reading and writing capabilities.
;;
;; Copyright 2005-2008, 2012-2025 Ivan Raikov, Shawn Rutledge
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
	 write-ieee-float64-vector)

        (import  scheme (chicken base) (chicken blob) (chicken port) (chicken bitwise)
                 (chicken file posix) iset srfi-4 endian-blob byte-blob)

;------------------------------------
;  Endian port data structures
;

; Structure: endian-port
;
;  * fileno:  file handle corresponding to the port
;  * filename:  file name corresponding to the port
;  * byte-order: can be MSB or LSB (type defined in unit endian-blob)
;

(define-record endian-port fileno filename byte-order)


;------------------------------------
;  Constants re-used from endian-blob
;
;(define MSB MSB)
;(define LSB LSB)

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


; Procedure:
; read-uint1:: EPORT [* BYTE-ORDER] -> UINTEGER
;
; Reads an unsigned integer of size 1 byte. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-uint1 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (make-blob 1)]
			[ret (file-read (endian-port-fileno eport) 1 buf)])
			(and (= (cadr ret) 1) (endian-blob->uint1 (byte-blob->endian-blob
					(blob->byte-blob (car ret)) byte-order) ) ))))


; Procedure:
; read-uint2:: EPORT [* BYTE-ORDER] -> UINTEGER.
;
; Reads an unsigned integer of size 2 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-uint2 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (make-blob 2)]
			[ret (file-read (endian-port-fileno eport) 2 buf)])
			(and (= (cadr ret) 2) (endian-blob->uint2 (byte-blob->endian-blob
					(blob->byte-blob (car ret)) byte-order) ) ))))


; Procedure:
; read-uint4:: EPORT [* BYTE-ORDER] -> UINTEGER
;
; Reads an unsigned integer of size 4 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-uint4 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (make-blob 4)]
			[ret (file-read (endian-port-fileno eport) 4 buf)])
			(and (= (cadr ret) 4) (endian-blob->uint4 (byte-blob->endian-blob
					(blob->byte-blob (car ret)) byte-order) ) ))))

; Procedure:
; read-int1:: EPORT [* BYTE-ORDER] -> INTEGER
;
; Reads a signed integer of size 1 byte. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-int1 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (make-blob 1)]
			[ret (file-read (endian-port-fileno eport) 1 buf)])
			(and (= (cadr ret) 1) (endian-blob->sint1 (byte-blob->endian-blob
					(blob->byte-blob (car ret)) byte-order) ) ))))


; Procedure:
; read-int2:: EPORT [* BYTE-ORDER] -> INTEGER.
;
; Reads a signed integer of size 2 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-int2 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (make-blob 2)]
			[ret (file-read (endian-port-fileno eport) 2 buf)])
			(and (= (cadr ret) 2) (endian-blob->sint2 (byte-blob->endian-blob
					(blob->byte-blob (car ret)) byte-order) ) ))))


; Procedure:
; read-int4:: EPORT [* BYTE-ORDER] -> INTEGER
;
; Reads a signed integer of size 4 bytes. Optional argument
; BYTE-ORDER is one of MSB or LSB. If byte order is not specified,
; then use the byte order setting of the given endian port.
;
(define (read-int4 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (make-blob 4)]
			[ret (file-read (endian-port-fileno eport) 4 buf)])
			(and (= (cadr ret) 4) (endian-blob->sint4 (byte-blob->endian-blob
					(blob->byte-blob (car ret)) byte-order) ) ))))

; Procedure:
;
; read-ieee-float32:: EPORT [* BYTE-ORDER] -> REAL
;
; Reads an IEEE 754 single precision floating-point number. Optional
; argument BYTE-ORDER is one of MSB or LSB. If byte order is not
; specified, then use the byte order setting of the given endian port.
;
(define (read-ieee-float32 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (make-blob 4)]
			[ret (file-read (endian-port-fileno eport) 4 buf)])
			(and (= (cadr ret) 4)
			     (endian-blob->ieee_float32
			      (byte-blob->endian-blob
			       (blob->byte-blob (car ret)) byte-order) ) ))))

; Procedure:
;
; read-ieee-float32:: EPORT [* BYTE-ORDER] -> REAL
;
; Reads an IEEE 754 double precision floating-point number. Optional
; argument BYTE-ORDER is one of MSB or LSB. If byte order is not
; specified, then use the byte order setting of the given endian port.
;
(define (read-ieee-float64 eport . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		 (let* ( [buf (make-blob 8)]
			 [ret (file-read (endian-port-fileno eport) 8 buf)] )
		   (and (= (cadr ret) 8)
			(endian-blob->ieee_float64
			 (byte-blob->endian-blob
			  (blob->byte-blob (car ret)) byte-order) ) ))))


; Procedure:
; set-bv!
;
; Helper function for read-bit-vector below
; *  set len bits in vector bv starting at position pos,
;    according to the bits set in vector b
;
; *  len may not be larger than 8
;
(define (set-bv! bv b pos len)
   (let loop ((i (- len 1))  (j 0))
     (begin
       (bit-vector-set! bv (- pos j)  (bit-vector-ref b i))
       (if (= i 0)  bv  (loop (- i 1) (+ j 1))))))


; Procedure:
; read-bit-vector:: PORT * SIZE (in bits) [* BYTE-ORDER] -> BIT-VECTOR
;
; Reads a bit vector of the specified size (in bits) and returns an
; iset bit vector (see module iset). Optional argument BYTE-ORDER is
; one of MSB or LSB. If byte order is not specified, then use the
; byte order setting of the given endian port.
;
(define (read-bit-vector eport size . rest)
  (let-optionals
   rest ((byte-order #f))
   (let ((nb  (inexact->exact (ceiling (/ size 8))))
	 (bv  (make-bit-vector size))
	 (byte-order  (if byte-order byte-order (endian-port-byte-order eport))))
     (cond
      ((eq? byte-order MSB)
       ;; if big engian, we start with the most significant bit
       (let loop ((b    (integer->bit-vector (read-int1 eport)))
		  (bi   (- nb 1))
		  (pos  (- size 1))
		  (rem  size))
	 (begin
	   (set-bv! bv b pos (min rem 8))
	   (if (= bi 0)   bv
	       (loop (integer->bit-vector (read-int1 eport))
		     (- bi 1)
		     (- pos 8)
		     (- rem 8))))))
      (else
       ;; if little endian, we start with the least significant bit
       (let loop ((b    (integer->bit-vector (read-int1 eport)))
		  (bi   (- nb 1))
		  (pos  7)
		  (rem  size))
	 (begin
	   (set-bv! bv b pos (min rem 8))
	   (if (= bi 0)   bv
	       (loop (integer->bit-vector (read-int1 eport))
		     (- bi 1)
		     (+ pos 8)
		     (- rem 8))))))))))

; Procedure:
; read-byte-vector:: PORT * SIZE [* BYTE-ORDER]  -> BYTE-VECTOR
;
; Reads an unsigned byte vector of the specified size and returns a Scheme byte
; vector. Optional argument BYTE-ORDER is one of MSB or LSB. If
; byte order is not specified, then use the byte order setting of the
; given endian port.
;
(define (read-byte-vector  eport size . rest)
  (let-optionals rest ((byte-order #f))
		 (let ((byte-order (if byte-order byte-order
				       (endian-port-byte-order eport))))
		   (let loop ((size size) (data '()))
		     (if (> size 0)
			 (let ((b  (read-uint1 eport)))
			   (loop (- size 1) (cons b data)))
			 (if (eq? byte-order MSB)
			     (u8vector->blob (list->u8vector (reverse data)))
			     (u8vector->blob (list->u8vector data))))))))


;------------------------------------
;  Helper function for bulk I/O
;

; Procedure:
; read-raw-bytes:: EPORT * COUNT -> BLOB | #f
;
; Reads COUNT bytes from the endian port and returns a blob, or #f if
; the read fails or reaches EOF before reading all requested bytes.
;
(define (read-raw-bytes eport count)
  (let* ([buf (make-blob count)]
         [ret (file-read (endian-port-fileno eport) count buf)])
    (and (= (cadr ret) count) (car ret))))

; Procedure:
; write-raw-bytes:: EPORT * BLOB -> INTEGER
;
; Writes the blob to the endian port and returns the number of bytes written.
;
(define (write-raw-bytes eport blob)
  (file-write (endian-port-fileno eport) blob))


;------------------------------------
;  Vector Reading Operations
;

; Procedure:
; read-uint1-vector:: EPORT * COUNT [* BYTE-ORDER] -> U8VECTOR | #f
;
; Reads a vector of unsigned integers of size 1 byte each. COUNT specifies
; the number of elements to read. Returns a u8vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-uint1-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport count)])
      (and raw-data
           (endian-blob->u8vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))

; Procedure:
; read-uint2-vector:: EPORT * COUNT [* BYTE-ORDER] -> U16VECTOR | #f
;
; Reads a vector of unsigned integers of size 2 bytes each. COUNT specifies
; the number of elements to read. Returns a u16vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-uint2-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport (* count 2))])
      (and raw-data
           (endian-blob->u16vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))

; Procedure:
; read-uint4-vector:: EPORT * COUNT [* BYTE-ORDER] -> U32VECTOR | #f
;
; Reads a vector of unsigned integers of size 4 bytes each. COUNT specifies
; the number of elements to read. Returns a u32vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-uint4-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport (* count 4))])
      (and raw-data
           (endian-blob->u32vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))

; Procedure:
; read-int1-vector:: EPORT * COUNT [* BYTE-ORDER] -> S8VECTOR | #f
;
; Reads a vector of signed integers of size 1 byte each. COUNT specifies
; the number of elements to read. Returns a s8vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-int1-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport count)])
      (and raw-data
           (endian-blob->s8vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))

; Procedure:
; read-int2-vector:: EPORT * COUNT [* BYTE-ORDER] -> S16VECTOR | #f
;
; Reads a vector of signed integers of size 2 bytes each. COUNT specifies
; the number of elements to read. Returns a s16vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-int2-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport (* count 2))])
      (and raw-data
           (endian-blob->s16vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))

; Procedure:
; read-int4-vector:: EPORT * COUNT [* BYTE-ORDER] -> S32VECTOR | #f
;
; Reads a vector of signed integers of size 4 bytes each. COUNT specifies
; the number of elements to read. Returns a s32vector or #f if reading fails.
; Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-int4-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport (* count 4))])
      (and raw-data
           (endian-blob->s32vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))

; Procedure:
; read-ieee-float32-vector:: EPORT * COUNT [* BYTE-ORDER] -> F32VECTOR | #f
;
; Reads a vector of IEEE 754 single precision floating-point numbers.
; COUNT specifies the number of elements to read. Returns a f32vector
; or #f if reading fails. Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-ieee-float32-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport (* count 4))])
      (and raw-data
           (endian-blob->f32vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))

; Procedure:
; read-ieee-float64-vector:: EPORT * COUNT [* BYTE-ORDER] -> F64VECTOR | #f
;
; Reads a vector of IEEE 754 double precision floating-point numbers.
; COUNT specifies the number of elements to read. Returns a f64vector
; or #f if reading fails. Optional argument BYTE-ORDER is one of MSB or LSB.
;
(define (read-ieee-float64-vector eport count . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let ([raw-data (read-raw-bytes eport (* count 8))])
      (and raw-data
           (endian-blob->f64vector
            (byte-blob->endian-blob 
             (blob->byte-blob raw-data) byte-order))))))


; Procedure:
; write-uint1:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an unsigned integer of size 1 byte. Returns the number of
; bytes written (always 1). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-uint1 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (uint1->endian-blob word byte-order)))])
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; write-uint2:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an unsigned integer of size 2 bytes. Returns the number of
; bytes written (always 2). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-uint2 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (uint2->endian-blob word byte-order)))])
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; write-uint4:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an unsigned integer of size 4 bytes. Returns the number of
; bytes written (always 4). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-uint4 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (uint4->endian-blob word byte-order)))])
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; Procedure:
; write-int1:: EPORT * WORD [* BYTE-ORDER] -> INTEGER
;
; Writes a signed integer of size 1 byte. Returns the number of
; bytes written (always 1). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-int1 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (sint1->endian-blob word byte-order)))])
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; write-int2:: EPORT * WORD [* BYTE-ORDER] -> INTEGER
;
; Writes a signed integer of size 2 bytes. Returns the number of
; bytes written (always 2). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-int2 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (sint2->endian-blob word byte-order)))])
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; write-int4:: EPORT * WORD [* BYTE-ORDER] -> INTEGER
;
; Writes a signed integer of size 4 bytes. Returns the number of
; bytes written (always 4). Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-int4 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (sint4->endian-blob word byte-order)))])
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; write-ieee-float32:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an IEEE 754 single precision floating-point number. Returns
; the number of bytes written (always 4). Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use
; the byte order setting of the given endian port.
;
(define (write-ieee-float32 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (ieee_float32->endian-blob word byte-order)))])
;(let ([testval (endian-blob->ieee_float32 (byte-blob->endian-blob (blob->byte-blob buf) byte-order)) ])
;	(unless (eq? testval word) (printf "trying to write float ~a, but after conversions it turns out as ~a~%" word testval) ) )
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; write-ieee-float64:: EPORT * WORD [* BYTE-ORDER] -> UINTEGER
;
; Writes an IEEE 754 double precision floating-point number. Returns
; the number of bytes written (always 8). Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use
; the byte order setting of the given endian port.
;
(define (write-ieee-float64 eport word . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
		(let* (	[buf (u8vector->blob (endian-blob->u8vector (ieee_float64->endian-blob word byte-order)))])
;(printf "trying to write float ~a, but after conversions it turns out as ~a~%" word
;	(endian-blob->ieee_float64 (byte-blob->endian-blob (blob->byte-blob buf) byte-order) )
			(file-write (endian-port-fileno eport) buf))))

; Procedure:
; write-byte-vector:: PORT * BYTE-VECTOR [* BYTE-ORDER] -> UINTEGER
;
; Writes the given unsigned byte vector and returns the number of bytes
; written. The argument must be a byte vector object. Optional
; argument BYTE-ORDER is one of MSB or LSB. If byte order is not
; specified, then use the byte order setting of the given endian port.
;
(define (write-byte-vector  eport vect . rest)
  (let-optionals rest ((byte-order #f))
	(let* ([u8vect (cond [(blob? vect) (blob->u8vector vect)]
						[(u8vector? vect) vect]
						[(endian-blob? vect) (endian-blob->u8vector vect)])]
		   [len (u8vector-length u8vect)]
	       [byte-order (if byte-order byte-order
				       (endian-port-byte-order eport))])
		   (if (eq? byte-order MSB)
		       (let loop ((i 0) (bytes 0))
				 (if (< i len)
				     (loop (+ i 1) (+ bytes (write-uint1 eport (u8vector-ref u8vect i))))
				     bytes))
		       (let loop ((i (- len 1)) (bytes 0))
				 (if (positive? i)
				     (loop (- i 1) (+ bytes (write-uint1 eport (u8vector-ref u8vect i))))
				     bytes))))))

;
; Procedure:
; write-bit-vector:: PORT * BIT-VECTOR [* BIT-ORDER] -> UINTEGER
;
; Writes the given bit vector and returns the number of bytes
; written. The argument must be a bit vector as defined in the iset
; module. Optional argument BIT-ORDER is one of MSB or LSB.  If
; bit order is not specified, then use the byte order setting of the
; given endian port.
;
; Note that here the "byte order" type is interpreted as bit order:
; i.e. the bits are ordered in MSB or LSB order, not the bytes that
; comprise the bit vector.
;
(define (write-bit-vector eport bv . rest)
  (let-optionals  rest ((byte-order #f))
		 (let ((len (bit-vector-length bv))
		       (byte-order (if byte-order byte-order
				       (endian-port-byte-order eport))))
		   (if (eq? byte-order LSB)
		       (let loop ((i 0) (bytes 0))
			 (if (< i len)
			     (let ((byte  (bitwise-ior (if (bit-vector-ref i bv)       #b10000000 0)
						       (if (bit-vector-ref (+ i 1) bv) #b01000000 0)
						       (if (bit-vector-ref (+ i 2) bv) #b00100000 0)
						       (if (bit-vector-ref (+ i 3) bv) #b00010000 0)
						       (if (bit-vector-ref (+ i 4) bv) #b00001000 0)
						       (if (bit-vector-ref (+ i 5) bv) #b00000100 0)
						       (if (bit-vector-ref (+ i 6) bv) #b00000010 0)
						       (if (bit-vector-ref (+ i 7) bv) #b00000001 0))))
			       (loop (+ i 8) (+ bytes (write-uint1 eport byte))))
			       bytes))
		       (let loop ((i (- len 1)) (bytes 0))
			 (if (positive? i)
			     (let ((byte  (bitwise-ior (if (bit-vector-ref i bv)       #b00000001 0)
						       (if (bit-vector-ref (- i 1) bv) #b00000010 0)
						       (if (bit-vector-ref (- i 2) bv) #b00000100 0)
						       (if (bit-vector-ref (- i 3) bv) #b00001000 0)
						       (if (bit-vector-ref (- i 4) bv) #b00010000 0)
						       (if (bit-vector-ref (- i 5) bv) #b00100000 0)
						       (if (bit-vector-ref (- i 6) bv) #b01000000 0)
						       (if (bit-vector-ref (- i 7) bv) #b10000000 0))))
			       (loop (- i 8) (+ bytes (write-uint1 eport byte)))
			       bytes)))))))

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
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (u8vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

; Procedure:
; write-uint2-vector:: EPORT * U16VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of unsigned integers of size 2 bytes each. Returns the 
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-uint2-vector eport vect . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (u16vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

; Procedure:
; write-uint4-vector:: EPORT * U32VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of unsigned integers of size 4 bytes each. Returns the 
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-uint4-vector eport vect . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (u32vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

; Procedure:
; write-int1-vector:: EPORT * S8VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of signed integers of size 1 byte each. Returns the 
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-int1-vector eport vect . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (s8vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

; Procedure:
; write-int2-vector:: EPORT * S16VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of signed integers of size 2 bytes each. Returns the 
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-int2-vector eport vect . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (s16vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

; Procedure:
; write-int4-vector:: EPORT * S32VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of signed integers of size 4 bytes each. Returns the 
; total number of bytes written. Optional argument BYTE-ORDER is one of
; MSB or LSB. If byte order is not specified, then use the byte order
; setting of the given endian port.
;
(define (write-int4-vector eport vect . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (s32vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

; Procedure:
; write-ieee-float32-vector:: EPORT * F32VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of IEEE 754 single precision floating-point numbers.
; Returns the total number of bytes written. Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-ieee-float32-vector eport vect . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (f32vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

; Procedure:
; write-ieee-float64-vector:: EPORT * F64VECTOR [* BYTE-ORDER] -> INTEGER
;
; Writes a vector of IEEE 754 double precision floating-point numbers.
; Returns the total number of bytes written. Optional argument BYTE-ORDER
; is one of MSB or LSB. If byte order is not specified, then use the byte
; order setting of the given endian port.
;
(define (write-ieee-float64-vector eport vect . rest)
  (let-optionals rest ([byte-order (endian-port-byte-order eport)])
    (let* ([endian-blob (f64vector->endian-blob vect byte-order)]
           [byte-blob (endian-blob->byte-blob endian-blob)]
           [raw-blob (byte-blob->blob byte-blob)])
      (write-raw-bytes eport raw-blob))))

) ;; end of module
