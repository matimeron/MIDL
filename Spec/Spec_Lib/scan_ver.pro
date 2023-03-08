Function Scan_ver, scan, length = len

;+
; NAME:
;		SCAN_VER
; VERSION:
;		7.12
; PURPOSE:
;		Verifies that the input is a valid scan
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_VER ( SCAN [, LENGTH = LEN])
; INPUTS:
;	SCAN
;		A scan, i.e. a [3,n] array.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	LENGTH
;		Optional output, see below.
; OUTPUTS:
;		Returns 1 if SCAN is a valid scan ([3,n] array), 3 if it is a "partial
;		scan" ([2,n] array), 0 otherwise.
; OPTIONAL OUTPUT PARAMETERS:
;	LENGTH
;		Returns the length, i.e. the number of points in the scan.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls ISNUM and ARREQ from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2002 by Mati Meron.
;		Modified 5-NOV-2009 by Mati Meron.  Added "partial scan" option.  
;-

	on_error, 1

	siz = size(scan)
	res = (Arreq(siz([0,1]),[2,3]) + 3*Arreq(siz([0,1]),[2,2]))*Isnum(scan)
	
	if res then len = siz[2] else len = 0

	return, res
end