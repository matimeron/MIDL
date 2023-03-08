Function Pol_rootnum, coef, radius = rad, cent = cen, _extra = _e

;+
; NAME:
;		POL_ROOTNUM
; VERSION:
;		8.3
; PURPOSE:
; 		Finds the number of roots of a polynomial within a given circle in the
; 		complex plane.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = POL_ROOTNUM( COEF [, keywords])
; INPUTS:
;	COEF
;		Numeric vector containing the polynomial coefficients.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RADIUS
;		Real scalar, the radius of the circle in the complex plane.  Mandatory.
;	CENTER
;		Real or complex scalar, the center of the circle in the complex plane.
;		Defaults to (0,0).
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the number of roots within the given circle.  Roots located at 
;		the boundary count as half root each.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Complex contour integration of the polynomial's logarithmic derivative.
;		Calls CALCTYPE, ROMBERG, FLTROUND, POL_LD_CKERN and REAL_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2012 by Mati Meron.
;		Modified 5-May-2014 by Mati Meron.  Changed from EROMBERG to ROMBERG,
;		following the upgrade of ROMBERG.
;-

	on_error, 1

	typ = Calctype(coef,rad,cen,0.,def=4)
	ran = Real_mm(Cast([0,2*!dpi],typ,typ))
	cint = Romberg('Pol_ld_ckern',ran,par=coef,try=2,rad=rad,cen=cen,_extra=_e)

	return, Fltround(Real_mm(cint),dig=3)
end