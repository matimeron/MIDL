Function Bsmooth_mm_old, arr, wid , edge_truncate = edt, kernel = ker

;+
; NAME:
;		BSMOOTH_MM
; VERSION:
;		8.0
; PURPOSE:
;		Binomial coefficient data smoothing.
; CATEGORY:
;		Array function.
; CALLING SEQUENCE:
;		Result = BSMOOTH_MM( ARR, WID [keywords])
; INPUTS:
;	ARR
;		Array, numeric, no more than six dimensions.
;	WID
;		The width of the smoothing window.  Can be given either as a scalar (in
;		which case it is applied to all the dimensions of the array, or as a
;		vector (in which case each entry applies to one dimension).  The WIDTH
;		entry(s) should be an odd number(s), if it is even the next higher odd
;		number(s) will be used.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/EDGE_TRUNCATE
;		Switch.  Same as in the IDL version of SMOOTH.
;	KERNEL
;		Optional output, see below.
; OUTPUTS:
;		Returns the smoothed array.
; OPTIONAL OUTPUT PARAMETERS:
;	KERNEL
;		Returns the kernel used for smoothing.  For diagnostic purposes.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Smoothes the data by convolving with a kernel of normalized binomial
;		coefficients, in each dimension.  This is the discrete equivalent of
;		gaussian smoothing.
;
;		Calls BINCOEF, CALCTYPE, CAST, CONVOL_MM and DEFAULT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-NOV-2010 by Mati Meron as a variation on SMOOTH_MM.
;-

	on_error, 1
	ndmx = 6

	siz = size(reform([arr]))
	ndm = siz[0]
	if ndm gt ndmx then message, 'At most six dimensions are allowed'
	typ = Calctype(arr,0.)
	m = Default(wid,1l,/dtyp)/2 > 0
	if n_elements(m) eq 1 then m = replicate(m,ndm) else $
	if n_elements(m) ne ndm then message, 'Improper width dimension!'

	mm = 2*m
	ker = make_array(mm+1,typ=typ>5,val=1)
	for i = 0, ndm - 1 do begin
		bc = Bincoef(1d*mm[i],lindgen(mm[i]+1))/2d^mm[i]
		case i of
			0	:	for j = 0, mm[i] do ker[j,*,*,*,*,*]= bc[j]*ker[j,*,*,*,*,*]
			1	:	for j = 0, mm[i] do ker[*,j,*,*,*,*]= bc[j]*ker[*,j,*,*,*,*]
			2	:	for j = 0, mm[i] do ker[*,*,j,*,*,*]= bc[j]*ker[*,*,j,*,*,*]
			3	:	for j = 0, mm[i] do ker[*,*,*,j,*,*]= bc[j]*ker[*,*,*,j,*,*]
			4	:	for j = 0, mm[i] do ker[*,*,*,*,j,*]= bc[j]*ker[*,*,*,*,j,*]
			5	:	for j = 0, mm[i] do ker[*,*,*,*,*,j]= bc[j]*ker[*,*,*,*,*,j]
		endcase
	endfor

	if not keyword_set(edt) then begin
		l = lonarr(ndmx)
		l[0:ndm-1] = m
 		h = lonarr(ndmx)
		h[0:ndm-1] = siz[1:ndm] - m - 1
		res = Cast(arr,typ)
		tres = Convol_mm(arr,ker)
		res(l[0]:h[0],l[1]:h[1],l[2]:h[2],l[3]:h[3],l[4]:h[4],l[5]:h[5]) = $
		tres(l[0]:h[0],l[1]:h[1],l[2]:h[2],l[3]:h[3],l[4]:h[4],l[5]:h[5])
	endif else res = Convol_mm(arr,ker,/edge_trun)

	return, Cast(res,typ,typ,/fix)
end