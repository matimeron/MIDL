Function Voigt_prof, x, sigma = sig, gamma = gam, amp_norm = amp

;+
; NAME:
;		VOIGT_PROF
; VERSION:
;		8.0
; PURPOSE:
;		Calculates a Voigt profile.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = VOIGT_PROF( X )
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SIGMA												|
;		Scalar, the Gaussian sigma present in the 		|	Note: at least one
;		convolution.  Defaults to 0.					|	of these two values
;	GAMMA												|	must be present and
;		Scalar, the Lorentzian half width present 		|	non-zero, else an
;		in the convolution.  Defaults to 0.				|	error results.
;	/AMP_NORM
;		Switch.  If set, the output is normalized to maximal amplitude (at X=0)
;		of 1.  Default normalization is this yielding an integral of 1 upon
;		integration over [-infinity,infinity]
; OUTPUTS:
;		Returns the value(s) of the Voigt profile corresponding to the given 
;		SIGMA and GAMMA, for all input values of X.  The output format is same 
;		as this of X.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		If either SIGMA or GAMMA are zero, calculates a Lorentzian or Gaussian, 
;		respectively, else calculates using the IDL VOIGT function.  Calls 
;		CALCTYPE, CAST and HOW_MANY, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-APR-2010 by Mati Meron.
;		Modified 5-JAN-2011 by Mati Meron.  Significant internal changes.
;-

	on_error, 1

	typ = Calctype(x,0.)
	wx = Cast(x,5)
	afl  = keyword_set(amp)
	case How_many(fir=sig,sec=gam,/nozero,whi=whi) of
		0	:	message, 'At least one of SIGMA, GAMMA, must be present!'
		1	:	begin
					if whi eq 1 then begin
						res = exp(-wx^2/(2*sig^2))
						if not afl then res = res/(sqrt(2*!dpi*sig^2))
					endif else begin
						res = gam^2/(wx^2 + gam^2)
						if not afl then res = res/(!dpi*sqrt(gam^2))
					endelse
				end
		2	:	begin
					fac = 1/sqrt(2d*sig^2)
					res = voigt(fac*gam,fac*wx)
					if afl then res = res/voigt(fac*gam,0d) $
					else res = fac/sqrt(!dpi)*res
				end
	endcase

	return, Cast(res,typ,typ,/fix)
end