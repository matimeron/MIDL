Function Qchain, x, gval = gvl, curv = crv, der = der

;+
; NAME:
;		QCHAIN
; VERSION:
;		8.13
; PURPOSE:
;		Generates a function consisting of a set of quadratic arcs.
; CATEGORY:
;		Mathematical Function (specialized).
; CALLING SEQUENCE:
;		Result = QCHAIN (X, GVAL = GVL, CURV = CRV [, /DER)
; INPUTS:
;	X
;		Numerical, otherwise arbitrary.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GVAL
;		Numeric vector (at least 2 elements), containing the "grid" values.
;		Mandatory.
;	CURV
;		Numeric vector containing the curvature (i.e. second derivative) values
;		between consecutive grid values.  Mandatory.
;		
;		Note:	The length of CURV must be *one less* than the length of GVAL.
;	/DER
;		Switch.  If set, the first derivative of the result is returned.  See
;		details in OUTPUTS.
; OUTPUTS:
;		Returns a function of X with the following properties:
;			1)	The function in each interval [GVAL[i],GVAL[i+1]] is a quadratic
;				polynomial with second derivative CURV[i].
;			2)	The function and its first derivative is continuous at each 
;				internal grid point.
;			3)	At the end (i.e. first and last) grid point the function is 0.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Reasonably straightforward derivation from the definitions above.
;		Calls CAST and POLEVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2011 by Mati Meron.
;-

	on_error, 1

	gl = min(gvl,max=gh)
	if gl eq gh then message, 'Constant GVAL not acceptable!'
	s = sort(gvl)
	g = Cast(gvl[s],4)
	n = n_elements(crv)
	if n ne (n_elements(g) - 1) then message, 'Bad input lengths!'
	c = Cast(crv[s[0:-2]],4)

	dum = where(x ge gl and x le gh, ndum, ncomp = ncdum)
	if ndum gt 0 then begin
		if ncdum gt 0 then message, 'Some x values out of range, removed', /con
		wx = x[dum]
		res = 0*wx
	endif else message, 'No input or no input in range!

	gm = (g[1:-1] + g[0:-2])/2
	dg = (g[1:-1] - g[0:-2])
	lo = [0,(gl - gm)*dg]
	hi = [gh - gm]*dg
	slo = total(([0,c]*lo)[0:-2],/cum)/(gh - gl)
	shi = reverse(total(reverse(c*hi),/cum))/(gh - gl)
	a = (b = 0*c)
	derfl = keyword_set(der)
	for i = 0l, n-1 do begin
		a[i] = c[i]*g[i]^2/2+ gh*slo[i]+ gl*shi[i]
		b[i] = -(c[i]*g[i] + slo[i] + shi[i])
		if i eq n-1 then dum = where(wx ge g[i] and wx le g[i+1], ndum) $
		else dum = where(wx ge g[i] and wx lt g[i+1], ndum)
		if ndum gt 0 then begin
			if derfl then res[dum] = Poleval(wx[dum],[b[i],c[i]]) $
			else res[dum] = Poleval(wx[dum],[a[i],b[i],c[i]/2.])
		endif
	endfor

	return, res
end