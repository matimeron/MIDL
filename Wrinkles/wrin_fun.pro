Function Wrin_fun, ni, alp, kl, eps= eps, nroot= nrt, deriv=der, square= squ, $
	previous = prv, show_roots = shr, info = inf

;+
; NAME:
;		WRIN_FUN
; VERSION:
;		7.05
; PURPOSE:
;		Calculating wrinkle "eigenfunctions".
; CATEGORY:
;		Wrinkles function.
; CALLING SEQUENCE:
;		Result = WRIN_FUN( NI, ALP, KL [, keywords])
; INPUTS:
;	NI
;		Numeric, otherwise arbitrary.  The ratio l/L (see "Wrinkle Math").
;	ALP
;		The Alpha value from "Wrinkle Math".  Given as either a single value,
;		in which case roots in the range [1,Alpha] are sought, or as a 2-element
;		vector specifying the search range.
;	KL
;		Scalar, the product KL from "Wrinkle Math".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	EPS
;		Scalar specifying the accuracy required in root calculation.  See EROOT
;		for details.  Default value is 1E-8.
;	NROOT
;		The root number to use, from the roots corresponding to a given ALP and
;		KL.  Lowest allowed number is 1.
;	DERIV
;		Specifies which derivative to use.  Only 0 (the function itself) 1 and 2
;		are currently accepted.
;	/SQUARE
;		Switch.  If set, the output is squared (intended for integration
;		purposes).
;	/PREVIOUS
;		Switch.  If set, results from the previous calculation are reused.
;	/SHOW_ROOTS
;		Switch.  Specifies screen listing of all the known roots.
;	/INFO
;		Switch.  If set, the number of roots and the last root are displayed to
;		the screen.  Not active when SHOW_ROOTS is set.
; OUTPUTS:
;		Returns the calculated value (values) of the wrinkle function.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		Block WRINSTUFF, containes previous calculation results.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Alpha must be >= 1.
; PROCEDURE:
;		Based on the results of "Wrinkle Math".  Calls WRIN_ROOT.  Calls
;		CALCTYPE, CAST, DEFAULT, FPU_FIX, ISNUM and SIGN, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-DEC-2007 by Mati Meron.
;		Modified 25-MAY-2008 by Mati Meron.  Internal changes.
;		Modified 10-JUN-2008 by Mati Meron.  Added keyword SQUARE.
;-

	common wrinstuff, palp, pkl, proots
	on_error, 1
	deps = 1e-8

	if keyword_set(prv) then begin
		if Isnum(proots) then begin
			kl = pkl
			roots = proots
		endif else message, 'There is no previous!'
	endif else begin
		typ = Calctype(alp,0.)
		weps = Default(eps,deps)
		roots = [1.,Wrin_root(Cast(alp,5),Cast(kl,5),eps=weps,stat=sts,/red)]
		if min(sts) eq 0 then message, 'Problems with the roots!'
		palp = alp
		pkl = kl
		proots = Cast(roots,typ,typ)
	endelse

	num = n_elements(roots)
	if keyword_set(shr) then begin
		print
		print, roots[1:*], form = '(f10.7,"	",f10.7)'
	endif else begin
		if keyword_set(inf) then begin
			print
			print, num-1, form = '("	",i0," nontrivial roots in table.")'
			print, roots[num-1], form = '("	last entry is ",f10.7)'
		endif
	endelse
	if (num-1) lt nrt then begin
		message,'Root # reset to highest available: '+string(num-1,'(i4)'),/cont
		wnrt = num-1
	endif else begin
		if nrt eq 0 then message, 'N = 0 not meaningful!' $
		else wnrt = nrt
	endelse

	walp = roots(wnrt)
	chip = kl*sqrt((walp+1)/2.)
	chin = kl*sqrt((walp-1)/2.)
	sgp = Sign(sin(chip))
	sgn = Sign(sin(chin))
	oni = 1 - ni

	case Default(der,0,/dtyp) of
		0	:	res = (sgn*sin(chin*ni)*sin(chip*oni) - $
						sgp*sin(chip*ni)*sin(chin*oni))/2
		1	:	res = (sgn*chin*cos(chin*ni)*sin(chip*oni) - $
						sgn*chip*sin(chin*ni)*cos(chip*oni) - $
						sgp*chip*cos(chip*ni)*sin(chin*oni) + $
						sgp*chin*sin(chip*ni)*cos(chin*oni))/2
		2	:	res = -((chip^2 + chin^2)*(sgn*sin(chin*ni)*sin(chip*oni) - $
										sgp*sin(chip*ni)*sin(chin*oni)) + $
						2*chip*chin*(sgn*cos(chin*ni)*cos(chip*oni) - $
										sgp*cos(chip*ni)*cos(chin*oni)))/2
		else:	message, 'Unsupported derivative!'
	endcase
	if keyword_set(squ) then res = res^2

	return, FPU_fix(res)
end
