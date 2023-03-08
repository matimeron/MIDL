Function Scan_trim, scan, step = stp, xstep = xstp, var_tol = vtl, $
	reverse = rev, special = spc, cut_value = cvl, netlen = len

;+
; NAME:
;		SCAN_TRIM
; VERSION:
;		8.43
; PURPOSE:
;		"Trims" scan data, eliminating some intermediate points.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_TRIM( SCAN [, keywords])
; INPUTS:
;	SCAN
;		A single valid scan, i.e. a [3,n] array ([2,n] is also acceptable, with
;		some limitations.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	STEP													|
; 		The distance (measured in number of points) between	|	One and only 
; 		consecutive data points to be kept.  Integer.		|	one of these
; 	XSTEP													|	two keywords
; 		The distance (measured in data x-units) between 	|	must be used.
; 		consecutive data points to be kept.  Float.			|
;
;		Note:	STEP can only be used when the X-coordinate of the SCAN is 
;				equispaced.  See VAR_TOL below.
;	VAR_TOL
;		Scalar, specifies the maximal relative spacing variation for the 
;		X-coordinate (defined as the ratio between maximal difference and 
;		average value of step size) below which the X-coordinate is considered
;		equispaced.  Default value is 0.01.
; 	/REVERSE
; 		By default, the first data point is always selected, then consecutive 
; 		points are chosen counting from the first, using either STEP or XSTEP. 
; 		Setting the switch REVERSE changes the direction, with the counting done
; 		from the end backwards (in which case it is the last point that is 
; 		always selected).
; 	/SPECIAL
; 		Switch.  If set, exceptional data points are also selected, in addition
; 		to those chosen using STEP or XSTEP.  The exceptional points are those 
; 		corresponding to high absolute values of second derivative.  More 
; 		exactly, points whose y-value differs from the value obtained using 
; 		linear interpolation between their neighbors, by significantly more than
; 		the statistical error of such interpolation, are chosen.
; 		
; 		Note:	SPECIAL cannot be used for [2,n] data i.e. scan data missing the
; 		data error part.  
; 	CUT_VALUE
; 		Numeric scalar, provides the threshold, in terms of statistical error,
; 		above which the difference used by SPECIAL is considered significant.
; 		Default value is 4, i.e. points differing from their linear 
; 		interpolations by more than 4 sigma are considered special.  
; 		
; 		Note:	If SPECIAL is not set, CUT_VALUE plays no role.
;	NETLEN
;		Optional output, see below.
; OUTPUTS:
;		Returns an output in a scan format ([2,n] or [3,n] array, depending on
;		the input), where some intermediate points of the original scan have
;		been dropped so as to satisfy the minimal distance or stepsize condition
;		specified by the keywords.  If special is set, the exceptional points 
;		(see description under /SPECIAL, above) are also kept.
;		
;		Note:  the returned scan will always have at least 2 points, even if
;		this necessitates violating the minimal distance/stepsize condition.
; OPTIONAL OUTPUT PARAMETERS:
;	NETLEN
;		Returns the length of the thinned scan.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The input must be a proper scan and, if /SPECIAL is set, it must include
;		data errors.
; PROCEDURE:
;		Straightforward.  Calls SCAN_VER.  Uses DEFAULT, DIF and ONE_OF, from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 30-JUN-2012 by Mati Meron.
;		Modified 1-SEP-2015 by Mati Meron.  Added keyword VAR_TOL.
;-

	on_error, 1

	case Scan_ver(scan,len=n) of
		1	:	efl = 1
		3	:	efl = 0
		else:	message, 'Not a scan!'
	endcase
	val = lonarr(n)

	if keyword_set(rev) then res = reverse(scan,2) else res = scan
	xres = reform(res[0,*])
	mind = min(abs(Dif(xres,/lin)),max=maxd)

	case One_of(stp,xstp,val=wha) of
		-1	:	message, 'Either Distance or Stepsize must be given!'
		0	:	begin
					var = Default(vtl,1e-2,/dtyp)
					if (maxd - mind) lt var*(maxd + mind) then begin
						p = round(wha) > 1
						ind = p*lindgen((n+p-1)/p)
						val[ind] = 1
					endif else message, $
					'Point spacing must be equal when using STEP!' 
				end
		1	:	begin
					wstep = 0.9999*abs(wha)
					if wstep gt mind then begin
						i = (j = 0l)
						val[j] = 1
						while j lt (n-1) do begin
							j = j + 1
							if abs(xres[j] - xres[i]) ge wstep then begin
								val[j] = 1
								i = j
							endif
						endwhile
					endif else val[*] = 1
				end
	endcase

	if keyword_set(spc) then begin
		if efl then begin
			wcvl = abs(Default(cvl,4.))
			yres = res[1,*]
			sder = Dif(Dif(yres,/bac,/lin),/for,/cli)
			sind = where(abs(sder) gt wcvl*sqrt(6)*res[2,*],nind)
			if nind gt 0 then val[sind] = 1
		endif else message, "Can't have special values without error data", /con
	endif

	if total(val,/pres) eq 1 and n gt 1 then val[n-1] = 1
	res = res[*,where(val,len)]
	if keyword_set(rev) then res = reverse(res,2)

	return, res
end