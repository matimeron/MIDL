Function Scan_offset, scan, xoff = xof, yoff = yof, partial = prt, _extra= _e

;+
; NAME:
;		SCAN_OFFSET
; VERSION:
;		8.15
; PURPOSE:
;		Offsets the X and/or Y coordinate of the scan by a provided amount
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_OFFSET( SCAN [, X_OFF = XOF] [Y_OFF = YOF]
; INPUTS:
;	SCAN
;		Either a single valid scan, i.e. a [3,n] array, or a single scan number.
;		In the second case the scan will be read from the currently open SPEC
;		file.  Note that in the second case the appropriate data columns (see
;		SCAN_READ) need to be provided.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	X_OFF
;		An offset value for the X (first column) values of the scan.
;	Y_OFF
;		An offset value for the Y (second column) values of the scan.
; 	/PARTIAL
; 		Switch.   If set, "partial scans", i.e. [2,n] arrays are accepted.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ.  Only
;		active when a scan number, not explicit scan, is given.
; OUTPUTS:
;		Returns an output in a scan format (i.e. a [3,n] array), where the 3
;		columns (in order) are:
;			0	:	X values, offset from the original by X_OFF if given.
;			1	:	Y values, offset from the original by Y_OFF if given.
;			2	:	Y error values, unchanged.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		If a scan number list is provided, SCAN_READ requires COLUMNS input.
;		See there for details.
; PROCEDURE:
;		Straightforward.  Calls SCAN_LIST_VER and SCAN_READ (only when in the
;		list mode).  Uses FPU_FIX and HOW_MANY from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2008 by Mati Meron.
;		Modified 5-APR-2012 by Mati Meron.  Added keyword PARTIAL.
;-

	on_error, 1

	nsc = Scan_list_ver(scan,flag=lfl,partial=prt,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!' $
	else if nsc gt 1 then message, 'Only a single scan allowed!'

	if lfl then res = Scan_read(scan,_extra=_e) else res = scan
	wha = How_many(fir=xof,sec=yof,whi=whi)
	if whi then res[0,*] = res[0,*] + xof
	if whi/2 then res[1,*] = res[1,*] + yof

	return, FPU_fix(res)
end