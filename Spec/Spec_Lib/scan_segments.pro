Pro Scan_segments, scan, lo = lo, hi = hi, colors = col, _extra = _e

;+
; NAME:
;		SCAN_SEGMENTS
; VERSION:
;		8.16
; PURPOSE:
;		Segmented display of scan data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		SCAN_SEGMENTS, SCAN, LO = LO, HI =HI, [, keywords]
; INPUTS:
;	SCAN
;		Single valid scan, i.e. a [3,n] array.  Partial scan (a [2,n] array) is
;		acceptable too, if the keyword PARTIAL is used.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	LO
; 		Numeric vector, list of the low boundaries of the segments.
; 	HI
; 		Numeric vector, list of the high boundaries of the segments.
;	COLORS
;		A vector of display color codes.  Optional (default is [blue,green,red])
;		if given must have at least 3 entries.
;	_EXTRA
;		A formal key, used to transfer keywords to imbedded routines.  Not to be
;		used directly.
;		Note:	All the SCAN_SHOW keywords can be used.
; OUTPUTS:
; 		Screen output only.  Displays the scan data (through SCAN_SHOW) using 
; 		the first two colors from the color list to mark the segments (segment
; 		#i is the region between LO[i] and HI[i].  Overlap regions (if present)
; 		are marked using the third color from the list.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, uses SCAN_SHOW.  Calls DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 1-AUG-2012 by Mati Meron.
;-

	on_error, 1
	eps = 1e-6

	n = n_elements(lo)
	if n_elements(hi) ne n then message, 'LO-HI discrepancy!'
	dcol = [!pcol.blue,!pcol.green,!pcol.red]
	wcol = Default(col,dcol)
	if n_elements(wcol) lt 3 then message, 'At least 3 colors needed!'

	Scan_show, scan, _extra = _e
	x = reform(scan[0,*])
	y = reform(scan[1,*])
	weps = max(abs(x))*eps
	for i = 0, n-1 do begin
		loc = where(x ge lo[i] - weps and x le hi[i] + weps, nloc)
		if nloc gt 0 then oplot, x[loc], y[loc], thi = 2, col = wcol[i mod 2]
	endfor
	for i = 1, n-1 do begin
		loc = where(x ge lo[i] - weps and x le hi[i-1] + weps, nloc)
		if nloc gt 0 then oplot, x[loc], y[loc], thi = 2, col = wcol[2]
	endfor

	return
end	