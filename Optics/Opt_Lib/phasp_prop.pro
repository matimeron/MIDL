Function Phasp_prop, shape, dist

;+
; NAME:
;		FHASP_PROP
; VERSION:
;		8.2
; PURPOSE:
;		Propagates a phase space region. 
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = FHASP_PROP( SHAPE, DIST)
; INPUTS:
;	SHAPE
;		Any proper 2D shape (see SHAPE_VER for details).  The two columns are
;		assumed to represent [X,X'], in order.
;	DIST
;		Propagation distance.
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the shape (which represents a boundary of a phase space region,
;		modified by the propagation.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Modifies the shape according to the standard propagation transformation.
;		Calls CAST, DEFAULT, SHAPE_CLOSE and SHAPE_VER, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2013 by Mati Meron.
;-

	on_error, 1

	if Shape_ver(shape) eq 2 then begin
		res = Shape_close(Cast(shape,4))
		res[0,*] =  res[0,*] + Default(dist,0.,/dtyp)*res[1,*]
	endif else message, 'Missing or improper shape!'

	return, res
end