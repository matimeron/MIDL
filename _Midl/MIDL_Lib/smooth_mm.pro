Function Smooth_mm, arr, wid, bincoef = bnc, savgol = svg, box = box, $
	edge_truncate = edt, err_smo = ers, kernel = ker

;+
; NAME:
;		SMOOTH_MM
; VERSION:
;		8.33
; PURPOSE:
;		Data smoothing.
; CATEGORY:
;		Array function.
; CALLING SEQUENCE:
;		Result = SMOOTH_MM( ARR, WID [keywords])
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
;	/BINCOEF														|
;		Switch.  Specifies a Binomial coefficients smoothing kernel,|	At most
;		which is the discrete equivalent of a gaussian kernel.		|	one of
;		This is the default, absent any specification.				|	these 
;	/SAVGOL															|	three
;		Switch.  Specifies Savitzky-Golay smoothing kernel.			|	keywords
;	/BOX															|	may be
;		Switch.  Specifies Box smoothing kernel.					|	used.
;		
;		Note:	Due to different kernel profiles, same WID value(s) (see above) 
;				results in different "effective width".  Given WID, the 
;				effective width is, approximately:
;
;				 Binomial coefficients	-	sqrt(!pi*WID)
;				 Savitzki-Golay			-	(4/9)*WID
;				 Box					-	WID
;	/EDGE_TRUNCATE
;		Switch.  Same as in the IDL version of SMOOTH.
;	/ERR_SMO
;		Switch.  If set, the input array and the smoothing kernel are squared
;		prior to evalaution, then the square root of the result is taken.
;		Useful for error calculation purposes.
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
;		Smoothes the data by convolving with the appropriate kernel. 
;
;		Calls CALCTYPE, CAST, CONVOL_MM, DEFAULT and ONE_OF from MIDL.  
;		Indirectly calls SKERN_BNC, SKERN_BOX and SKERN_SVG, also from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-1997 by Mati Meron as M_SMOOTH.
;		Renamed 25-SEP-1999 by Mati Meron, to SMOOTH_MM.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Significantly modified 5-OCT-2014 by Mati Meron.  Combined in a single
;		routine Binary coefficient, Savitzky-Golay and Box smoothing (which 
;		were previously separate), through the keywords /BINCOEF, /SAVGOL and
;		/BOX.  Added keyword /ERR_SMO to allow for smoothing of data errors 
;		(this keyword was already available for Binary coefficient smoothing).
;		Eliminated the derivatives option which was never used.  
;-

	on_error, 1
	ndmx = 6
	kern = ['skern_bnc','skern_svg','skern_box']

	warr = reform(arr)
	siz = size(warr)
	ndm = siz[0]
	if ndm gt ndmx then message, 'At most six dimensions are allowed'
	typ = Calctype(warr,0.)
	m = Default(wid,1l,/dtyp)/2 > 0
	if n_elements(m) eq 1 then m = replicate(m,ndm) else $
	if n_elements(m) ne ndm then message, 'Improper width dimension!'

	whi = One_of(bnc,svg,box) > 0
	ker = call_function(kern[whi],m)

	if keyword_set(ers) then begin
		warr = warr^2
		ker = ker^2
		if m[-1] eq 0 then ker = reform(ker,2*m+1)
	endif

	if not keyword_set(edt) then begin
		l = lonarr(ndmx)
		l[0:ndm-1] = m
 		h = lonarr(ndmx)
		h[0:ndm-1] = siz[1:ndm] - m - 1
		res = Cast(warr,typ)
		tres = Convol_mm(warr,ker)
		res(l[0]:h[0],l[1]:h[1],l[2]:h[2],l[3]:h[3],l[4]:h[4],l[5]:h[5]) = $
		tres(l[0]:h[0],l[1]:h[1],l[2]:h[2],l[3]:h[3],l[4]:h[4],l[5]:h[5])
	endif else res = Convol_mm(warr,ker,/edge_trun)
	if keyword_set(ers) then res = sqrt(res > 0)

	return, Cast(res,typ,typ,/fix)
end