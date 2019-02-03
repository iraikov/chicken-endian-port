(import scheme (chicken base) (chicken blob) srfi-4 endian-port endian-blob)

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

(exit)
