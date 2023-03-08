Function Var_slit, x, sig, distribution = dist

;+
; NAME:
;		VAR_SLIT
; VERSION:
;		4.3
; PURPOSE:
;		Calculating Frauenhoffer patterns for a variable width slit.
; CATEGORY:
;		Optical calculations
; CALLING SEQUENCE:
;		Result = VAR_SLIT( X [, SIG [, DISTRIBUTION = DIST]])
; INPUTS:
;	X
;		Numeric scalar or array, representing value(s) of k*d*theta, where
;		k = 2*pi/lambda, d is the average slit width and theta is the angle.
; OPTIONAL INPUT PARAMETERS:
;	SIG
;		Sigma (i.e. square root of second moment) of the relative width
;		distribution.  Defaults to 0.
; KEYWORD PARAMETERS:
;	DISTRIBUTION
;		Accepts a character input, either "GAUSS" for gaussian distribution
;		(representing random roughness) of "UNIFORM" for uniform distribution
;		(representing angle between the slits' blades).  Only first 2 letters
;		are required.
; OUTPUTS:
;		Returns the resulting diffraction pattern, in same format as X.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Described in the "Frauenhoffer intensity pattern for a variable width
;		slit" writeup.
;		Calls DEFAULT, FPU_FIX, POLEVAL, SP_BESELJ and STRMATCH_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 1-JUN-1996 by Mati Meron.
;		Updated 20-APR-2003 by Mati Meron.
;-

	posib = ['gauss','uniform']
	coef = [[5,30,15],[5,30,9]]/60.
	dnm = Strmatch_mm(dist,posib,2) > 0
	sig = Default(sig,0.,/dtype)
	res = 0.*x
	xlim = (4*Toler(x))^(1./4)
	lit = where(abs(x) lt xlim, nlit, comp=big, ncomp=nbig)
	if nlit ne 0 then res[lit] = 1 + sig^2 - Poleval(sig^2,coef[*,dnm])*x[lit]^2
	if nbig ne 0 then begin
		sbig = sig*x[big]
		case dnm of
			0	:	res[big] = FPU_fix(exp(-sbig^2/2))
			1	:	res[big] = Sp_beselj(sqrt(3)*sbig,0)
			else:	message, "what's that!"
		endcase
		res[big] = 2/x[big]^2*(1. - cos(x[big])*res[big])
	endif

	return, FPU_fix(res)
end