Function Slit_thru, psi, hsize = hsi, vsize = vsi, slit = sli, $
	sigma = sig, radians = rad

;+
; NAME:
;		SLIT_THRU
; VERSION:
;		8.11
; PURPOSE:
;		Calculates vertical slit throughput for rotated beam.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = SLIT_THRU( PSI, HSIZE= HSI, VSIZE= VSI, SLIT= SLI [, keywords])
; INPUTS:
; 	PSI
; 		Numeric, otherwise arbitrary.  The beam rotation angle(s).  Mandatory.
; 		Assumed in degrees, unless /RADIANS is set.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	HSIZE
; 		Numeric scalar, the beam horizontal size.  Assumed FWHM unless /SIGMA
; 		is set.  Mandatory.
; 	VSIZE
; 		Numeric scalar, the beam vertical size.  Assumed FWHM unless /SIGMA
; 		is set.  Mandatory.
;	SLIT
;		Numeric scalar, the slit (full) size.
;	/SIGMA
;		Switch.  If set, HSIZE and VSIZE are taken as sigma-values.  Default
;		is FWHM.
;	/RADIANS
;		Switch.  If set, PSI values are assumed in radians.  Default is degrees.
; OUTPUTS:
;		Returns the throughput fraction value(s) for the parameters provided.
;		The form of  the output follows the form of PSI.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the reflection must be physically realizable.
; PROCEDURE:
;		Geometrical calculation, details elsewhere.  Calls ERRORF_MM and 
;		FPU_FIX, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-SEP-2011 by Mati Meron.
;-

	on_error, 1

	if keyword_set(rad) then wpsi = psi else wpsi = !dtor*psi
	if keyword_set(sig) then sca = 1. else sca = 1./sqrt(alog(256))
	xsig = sca*hsi
	ysig = sca*vsi

	res = Errorf_mm(sli/sqrt(8*((xsig*sin(wpsi))^2 + (ysig*cos(wpsi))^2)))

	return, FPU_fix(res)
end