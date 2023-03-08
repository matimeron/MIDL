Function Qchain_gpar, p, q, gval = gvl, weights = wei, slope = slp, curv = crv,$
	cgrad = cgr

;+
; NAME:
;		QCHAIN_GPAR
; VERSION:
;		8.15
; PURPOSE:
;		Calculates the global parameters for a QCHAIN-type function.
; CATEGORY:
;		Mathematical Function (specialized).
; CALLING SEQUENCE:
;		Result = QCHAIN_GPAR (P, Q, GVAL = GVL [, keywords])
; INPUTS:
;	P
;		Numerical vector, mandatory.  Corresponds to the CURV values in QCHAIN
;		(see there).
;	Q
;		Numerical vector, mandatory.  Corresponds to the X input of QCHAIN.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GVAL
;		Numeric vector (at least 2 elements), containing the "grid" values.
;		Mandatory.
;	WEIGHTS
;		Numeric vector, optional weights for fitting purposes.  If given, must
;		have same length as Q.
;	/SLOPE													| At most one of
;		Switch.  If set, the global slope is calculated.	| these two may be
;	/CURV													| used.  The default
;		Switch.  If set, the global curvature is calculated.| is /CURV.
;	CGRAD
;		Optional output, see below.
; OUTPUTS:
;		Returns the global slope or the curvature (default) of a QCHAIN type 
;		function, as a numeric scalar.
; OPTIONAL OUTPUT PARAMETERS:
;	CGRAD
;		Returns the gradient of the result with respect to P, as a vector of  
;		same length as P.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Applies a linear fit to the output of QCHAIN_FUN.  Calls QCHAIN_FUN from
; 		BLINE_LIB.  Calls CALCTYPE, CAST, LINFIT_MM and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(p,q,0.)
	np = n_elements(p)
	wha = abs(One_of(slp,crv))
	y = Qchain_fun(p,q,gval=gvl,grad=grd)
	res = (Linfit_mm(q,y,wei))[wha]

	if arg_present(cgr) then begin
		cgr = make_array(np,type=typ)
		for i = 0, np-1 do cgr[i] = (Linfit_mm(q,reform(grd[*,i]),wei))[wha]
	endif

	return, Cast(res,typ,typ,/fix)
end