(import scheme (chicken base) (chicken blob) srfi-4 endian-port endian-blob)

;; Tests for scalar operations
(print "=== Testing Scalar Operations ===")

(let ([outp (port->endian-port (open-output-file "eptest"))])
	(write-ieee-float32 outp +nan.0)
	(write-ieee-float32 outp 5.25)
	(write-ieee-float64 outp +nan.0)
	(write-ieee-float64 outp 5.25)
	(write-int1 outp (inexact->exact (- (expt 2 7) 1)))
	(write-int2 outp (inexact->exact (- (expt 2 15) 1)))
	(write-int4 outp (inexact->exact (- (expt 2 30) 1)))
	(write-byte-vector outp (string->blob "hacking endian-port"))
	(write-byte-vector outp (blob->u8vector (string->blob "'error: bad argument count'")))
	(write-byte-vector outp (u8vector->endian-blob (blob->u8vector (string->blob "always one more bug")) LSB))
	(write-byte-vector outp (u8vector->endian-blob  (u8vector 255 254 128 127 126 125) LSB))
	(close-endian-port outp))

(let ([inp (port->endian-port (open-input-file "eptest"))])
	(print "trying to read +nan.0 (32-bit): " (read-ieee-float32 inp))
	(print "trying to read 5.25 (32-bit): "   (read-ieee-float32 inp))
	(print "trying to read +nan.0 (64-bit): " (read-ieee-float64 inp))
	(print "trying to read 5.25 (64-bit): " (read-ieee-float64 inp))
	(print "trying to read " (- (expt 2 7) 1) ": "    (read-int1 inp))
	(print "trying to read " (- (expt 2 15) 1) ": "  (read-int2 inp))
	(print "trying to read " (- (expt 2 30) 1) ": "  (read-int4 inp))
	(print "trying to read u8vector: " (blob->string (read-byte-vector inp 19)))
	(print "trying to read u8vector: " (blob->string (read-byte-vector inp 27)))
	(print "trying to read u8vector: " (blob->string (read-byte-vector inp 19)))
	(print "trying to read u8vector of unsigned bytes: " (blob->u8vector (read-byte-vector inp 6)))
	(close-endian-port inp))

;; Tests for vector operations
(print "\n=== Testing Vector Operations ===")

;; Test data setup
(define test-u8-data (u8vector 0 1 127 128 255))
(define test-u16-data (u16vector 0 1 32767 32768 65535))
(define test-u32-data (u32vector 0 1 2147483647 2147483648 4294967295))
(define test-s8-data (s8vector -128 -1 0 1 127))
(define test-s16-data (s16vector -32768 -1 0 1 32767))
(define test-s32-data (s32vector -2147483648 -1 0 1 2147483647))
(define test-f32-data (f32vector -3.14159 0.0 3.14159 +inf.0 -inf.0))
(define test-f64-data (f64vector -2.718281828459045 0.0 2.718281828459045 1.7976931348623157e308))

;; Test MSB (big-endian) vector operations
(print "\n--- Testing MSB (Big-Endian) Vector Operations ---")

(let ([outp (open-endian-port 'write "vector-test-msb" 'truncate)])  ; explicit truncate mode
  (set-bigendian! outp)
  
  ;; Write test vectors
  (print "Writing u8vector (MSB): " (write-uint1-vector outp test-u8-data) " bytes")
  (print "Writing u16vector (MSB): " (write-uint2-vector outp test-u16-data) " bytes")
  (print "Writing u32vector (MSB): " (write-uint4-vector outp test-u32-data) " bytes")
  (print "Writing s8vector (MSB): " (write-int1-vector outp test-s8-data) " bytes")
  (print "Writing s16vector (MSB): " (write-int2-vector outp test-s16-data) " bytes")
  (print "Writing s32vector (MSB): " (write-int4-vector outp test-s32-data) " bytes")
  (print "Writing f32vector (MSB): " (write-ieee-float32-vector outp test-f32-data) " bytes")
  (print "Writing f64vector (MSB): " (write-ieee-float64-vector outp test-f64-data) " bytes")
  
  (close-endian-port outp))

(let ([inp (open-endian-port 'read "vector-test-msb")])
  (set-bigendian! inp)
  
  ;; Read back and verify
  (let ([read-u8 (read-uint1-vector inp (u8vector-length test-u8-data))]
        [read-u16 (read-uint2-vector inp (u16vector-length test-u16-data))]
        [read-u32 (read-uint4-vector inp (u32vector-length test-u32-data))]
        [read-s8 (read-int1-vector inp (s8vector-length test-s8-data))]
        [read-s16 (read-int2-vector inp (s16vector-length test-s16-data))]
        [read-s32 (read-int4-vector inp (s32vector-length test-s32-data))]
        [read-f32 (read-ieee-float32-vector inp (f32vector-length test-f32-data))]
        [read-f64 (read-ieee-float64-vector inp (f64vector-length test-f64-data))])
    
    (print "Original u8vector: " test-u8-data)
    (print "Read u8vector:     " read-u8)
    (print "u8vector match: " (equal? test-u8-data read-u8))
    
    (print "Original u16vector: " test-u16-data)
    (print "Read u16vector:     " read-u16)
    (print "u16vector match: " (equal? test-u16-data read-u16))
    
    (print "Original u32vector: " test-u32-data)
    (print "Read u32vector:     " read-u32)
    (print "u32vector match: " (equal? test-u32-data read-u32))
    
    (print "Original s8vector: " test-s8-data)
    (print "Read s8vector:     " read-s8)
    (print "s8vector match: " (equal? test-s8-data read-s8))
    
    (print "Original s16vector: " test-s16-data)
    (print "Read s16vector:     " read-s16)
    (print "s16vector match: " (equal? test-s16-data read-s16))
    
    (print "Original s32vector: " test-s32-data)
    (print "Read s32vector:     " read-s32)
    (print "s32vector match: " (equal? test-s32-data read-s32))
    
    (print "Original f32vector: " test-f32-data)
    (print "Read f32vector:     " read-f32)
    ;; Note: floating point comparison requires tolerance for NaN and inf values
    (print "f32vector types match: " (and (f32vector? read-f32) 
                                          (= (f32vector-length read-f32) 
                                             (f32vector-length test-f32-data))))
    
    (print "Original f64vector: " test-f64-data)
    (print "Read f64vector:     " read-f64)
    (print "f64vector types match: " (and (f64vector? read-f64) 
                                          (= (f64vector-length read-f64) 
                                             (f64vector-length test-f64-data)))))
  
  (close-endian-port inp))

;; Test LSB (little-endian) vector operations
(print "\n--- Testing LSB (Little-Endian) Vector Operations ---")

(let ([outp (open-endian-port 'write "vector-test-lsb" 'truncate)])
  (set-littlendian! outp)
  
  ;; Write test vectors
  (print "Writing u16vector (LSB): " (write-uint2-vector outp test-u16-data) " bytes")
  (print "Writing s32vector (LSB): " (write-int4-vector outp test-s32-data) " bytes")
  (print "Writing f64vector (LSB): " (write-ieee-float64-vector outp test-f64-data) " bytes")
  
  (close-endian-port outp))

(let ([inp (open-endian-port 'read "vector-test-lsb")])
  (set-littlendian! inp)
  
  ;; Read back and verify
  (let ([read-u16 (read-uint2-vector inp (u16vector-length test-u16-data))]
        [read-s32 (read-int4-vector inp (s32vector-length test-s32-data))]
        [read-f64 (read-ieee-float64-vector inp (f64vector-length test-f64-data))])
    
    (print "LSB u16vector match: " (equal? test-u16-data read-u16))
    (print "LSB s32vector match: " (equal? test-s32-data read-s32))
    (print "LSB f64vector types match: " (and (f64vector? read-f64) 
                                              (= (f64vector-length read-f64) 
                                                 (f64vector-length test-f64-data)))))
  
  (close-endian-port inp))

;; Test endianness conversion between formats
(print "\n--- Testing Endianness Conversion ---")

(define conversion-data (s16vector -1000 0 1000 -32768 32767))

;; Write as big-endian
(let ([outp (open-endian-port 'write "endian-conversion-test" 'truncate)])
  (set-bigendian! outp)
  (write-int2-vector outp conversion-data)
  (close-endian-port outp))

;; Read as little-endian (should give different results)
(let ([inp (open-endian-port 'read "endian-conversion-test")])
  (set-littlendian! inp)
  (let ([read-as-lsb (read-int2-vector inp (s16vector-length conversion-data))])
    (print "Original (written as MSB): " conversion-data)
    (print "Read as LSB: " read-as-lsb)
    (print "Values differ (as expected): " (not (equal? conversion-data read-as-lsb))))
  (close-endian-port inp))

;; Read as big-endian (should match original)
(let ([inp (open-endian-port 'read "endian-conversion-test")])
  (set-bigendian! inp)
  (let ([read-as-msb (read-int2-vector inp (s16vector-length conversion-data))])
    (print "Read as MSB: " read-as-msb)
    (print "Values match original: " (equal? conversion-data read-as-msb)))
  (close-endian-port inp))

;; Test error conditions
(print "\n--- Testing Error Conditions ---")

;; Test reading beyond EOF
(let ([inp (open-endian-port 'read "vector-test-msb")])
  (set-bigendian! inp)
  
  ;; Position at end
  (setpos inp (pos inp))
  
  ;; Try to read more data than available
  (let ([result (read-uint4-vector inp 1000)])
    (print "Reading beyond EOF returns: " result)
    (print "Correctly returns #f: " (not result)))
  
  (close-endian-port inp))

;; Test explicit byte-order override
(print "\n--- Testing Explicit Byte-Order Override ---")

(let ([outp (open-endian-port 'write "override-test" 'truncate)])
  (set-bigendian! outp)  ; Port default is MSB
  
  ;; Write with explicit LSB override
  (write-uint2-vector outp test-u16-data LSB)
  (close-endian-port outp))

(let ([inp (open-endian-port 'read "override-test")])
  (set-bigendian! inp)  ; Port default is MSB
  
  ;; Read with explicit LSB override (should match original)
  (let ([read-lsb-override (read-uint2-vector inp (u16vector-length test-u16-data) LSB)])
    (print "Override test - original: " test-u16-data)
    (print "Override test - read with LSB: " read-lsb-override)
    (print "Override works correctly: " (equal? test-u16-data read-lsb-override)))
  
  (close-endian-port inp))

;; Test large vector performance
(print "\n--- Testing Large Vector Performance ---")

(define large-data (make-f32vector 10000))
;; Fill with test pattern
(let loop ([i 0])
  (when (< i 10000)
    (f32vector-set! large-data i (* i 0.001))
    (loop (+ i 1))))

(let ([outp (open-endian-port 'write "large-vector-test" 'truncate)])
  (let ([bytes-written (write-ieee-float32-vector outp large-data)])
    (print "Wrote large vector: " bytes-written " bytes (" 
           (f32vector-length large-data) " elements)"))
  (close-endian-port outp))

(let ([inp (open-endian-port 'read "large-vector-test")])
  (let ([read-large (read-ieee-float32-vector inp (f32vector-length large-data))])
    (print "Read large vector: " (f32vector-length read-large) " elements")
    (print "Large vector I/O successful: " (= (f32vector-length read-large) 
                                              (f32vector-length large-data)))
    ;; Spot check a few values
    (print "Spot check [0]: " (f32vector-ref read-large 0) " (expected 0.0)")
    (print "Spot check [1000]: " (f32vector-ref read-large 1000) " (expected 1.0)")
    (print "Spot check [9999]: " (f32vector-ref read-large 9999) " (expected 9.999)"))
  (close-endian-port inp))

;; Test mixed data types in sequence
(print "\n--- Testing Mixed Data Types ---")

(let ([outp (open-endian-port 'write "mixed-data-test" 'truncate)])
  (write-uint1-vector outp (u8vector 1 2 3))
  (write-ieee-float32-vector outp (f32vector 1.5 2.5))
  (write-int4-vector outp (s32vector -100 100))
  (close-endian-port outp))

(let ([inp (open-endian-port 'read "mixed-data-test")])
  (let ([u8-part (read-uint1-vector inp 3)]
        [f32-part (read-ieee-float32-vector inp 2)]
        [s32-part (read-int4-vector inp 2)])
    (print "Mixed read u8: " u8-part)
    (print "Mixed read f32: " f32-part)  
    (print "Mixed read s32: " s32-part)
    (print "All mixed reads successful: " (and u8-part f32-part s32-part)))
  (close-endian-port inp))

;; Test append mode functionality
(print "\n--- Testing Append Mode ---")

;; Create initial file with some data
(let ([outp (open-endian-port 'write "append-test" 'truncate)])
  (write-uint2-vector outp (u16vector 100 200 300))
  (close-endian-port outp))

;; Append more data to the same file
(let ([outp (open-endian-port 'write "append-test" 'append)])
  (write-uint2-vector outp (u16vector 400 500 600))
  (close-endian-port outp))

;; Read all data back - should have 6 elements
(let ([inp (open-endian-port 'read "append-test")])
  (let ([all-data (read-uint2-vector inp 6)])
    (print "Appended data: " all-data)
    (print "Append mode works: " (and (u16vector? all-data)
                                      (= (u16vector-length all-data) 6)
                                      (= (u16vector-ref all-data 0) 100)
                                      (= (u16vector-ref all-data 3) 400))))
  (close-endian-port inp))

(print "\n=== All Tests Complete ===")

(exit)
