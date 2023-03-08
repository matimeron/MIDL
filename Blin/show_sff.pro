Pro Show_sff, q, r, _extra = _e

;+
; NAME:
;		SHOW_SFF
; VERSION:
;		5.7
; PURPOSE:
;		Displays a 2D image of the spherical scattering form factor.
; CATEGORY:
;		Visualisation, x-ray.
; CALLING SEQUENCE:
;		SHOW_SFF, Q [, R] [, keywords]
; INPUTS:
;	Q
;		Numeric, arbitrary, represents the scattering Q-vector.
; OPTIONAL INPUT PARAMETERS:
;	R
;		Numeric, scalar, the radius of the scattering sphere.  Defaults to 1.
; KEYWORD PARAMETERS:
;	_EXTRA
;		A formal keyword, used to pass parameters to DISPLAY_MM.
; OUTPUTS:
;		None other than graphics output.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Strightforward, from definition.  Calls SP_FORM_FACTOR.  Also calls
;		DISPLAY_MM and MAKE_GRID from MIDL.
; MODIFICATION HISTORY:
;		Created 25-SEP-2005 by Mati Meron.
;		Modified 15-NOV-2006 by Mati Meron.  Internal changes.
;-

	qvals = Make_grid(q*[[-1.,1.],[-1.,1.]],[512,512])
	qxy = reform(qvals[0,*,*])
	qz = reform(qvals[1,*,*])
	sff = Sp_form_factor(sqrt(qxy^2 + qz^2),r)

	Display_mm, sff, qxy, qz, log = log, _extra = _e

	return
end