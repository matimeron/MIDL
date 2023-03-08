Function Phasp_foc, shape, focl

;+
; NAME:
;		FHASP_FOC
; VERSION:
;		8.2
; PURPOSE:
;		Performs a focusing transformation on a phase space region. 
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = FHASP_FOC( SHAPE, FOCL)
; INPUTS:
;	SHAPE
;		Any proper 2D shape (see SHAPE_VER for details).  The two columns are
;		assumed to represent [X,X'], in order.
;	FOCL
;		Focal length.
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the shape (which represents a boundary of a phase space region,
;		modified by the focusing.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Modifies the shape according to the standard focusing transformation.
;		Calls CAST, DEFAULT, SHAPE_CLOSE and SHAPE_VER, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2013 by Mati Meron.
;-

	on_error, 1

	if Shape_ver(shape) eq 2 then begin
		res = Shape_close(Cast(shape,4))
		if Isnum(focl) then res[1,*] = res[1,*] - 1./focl*res[0,*]
	endif else message, 'Missing or improper shape!'

	return, res
end