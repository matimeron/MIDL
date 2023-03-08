Function Edge_mm_old, arr, wid, edge_truncate = edt

;+
; NAME:
;		EDGE_MM
; VERSION:
;		8.214
; PURPOSE:
;		Edge enhancement.
; CATEGORY:
;		Array function.
; CALLING SEQUENCE:
;		Result = EDGE_MM( ARR, WID [keywords])
; INPUTS:
;	ARR
;		Array, numeric, no more than six dimensions.
;	WID
;		Scalar, or vector with one entry for each dimension of ARR, the width of
;		the enhancement kernel.  The WIDTH entry(s) should be an odd number Or
;		numbers), for even entry(s) the next higher odd number(s) will be used.
;		Default (and minimal) value is 3.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/EDGE_TRUNCATE
;		Switch.  Same as in SMOOTH_MM and the IDL version of SMOOTH.
; OUTPUTS:
;		Returns the edge-enhanced array.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		A straightforward modification of the SMOOTH_MM routine, uses a
;		Savitzky-Golay second derivative approximation.  Calls CAST, CONVOL_MM,
;		DEFAULT, MAKE_GRID and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 10-APR-2009 by Mati Meron.
;		Modified 30-OCT-2013 by Mati Meron.  Enabled different widths in 
;		different directions. 
;-

	on_error, 1
	ndmx = 6

	siz = size(reform([arr]))
	ndm = siz[0]
	if ndm eq 0 then message, 'Input must be an array!' $
	else if ndm gt ndmx then message, 'At most six dimensions are allowed!'
	typ = Type(arr) > 4
	wwid = Default(wid,3l,/dtyp)
	case n_elements(wwid) of
		1	:	wwid = replicate(wwid,ndm)
		ndm	:
		else:	message, 'WID must be scalar, or vec with one component ' + $
				'for each input direction!'
	endcase
	m = Cast(floor(wwid/2),5,typ)
	if min(siz[1:ndm]- (2*m + 1)) lt 0 then message, 'Width exceeds array size!'
	ker = 3./ndm*(Make_grid(transpose([[-m],[m]]),2*m+1))^2
	if ndm gt 1 then begin
		for i = 0, ndm-1 do ker[i,*,*,*,*,*] = ker[i,*,*,*,*,*]/(m[i]*(m[i]+1))
		ker = 1 - total(ker,1)
	endif else ker = 1 - ker/(m[0]*(m[0]+1))

	if not keyword_set(edt) then begin
		l = lonarr(ndmx)
		l[0:ndm-1] = m
 		h = lonarr(ndmx)
		h[0:ndm-1] = siz[1:ndm] - m - 1
		res = Cast(arr,5)
		tres = Convol_mm(arr,ker)
		res(l[0]:h[0],l[1]:h[1],l[2]:h[2],l[3]:h[3],l[4]:h[4],l[5]:h[5]) = $
		tres(l[0]:h[0],l[1]:h[1],l[2]:h[2],l[3]:h[3],l[4]:h[4],l[5]:h[5])
	endif else res = Convol_mm(arr,ker,/edge_trun)

	return, Cast(res,typ,typ,/fix)
end