Function Angle, dirf, dirs, degrees = deg

;+
; NAME:
;	ANGLE
; PURPOSE:
;	Finds the angle between two vectors.
; CATEGORY:
;	Geometry, general.
; CALLING SEQUENCE:
;	Result = ANGLE ( DIRF, DIRS [, /DEGREES] )
; INPUTS:
;    DIRF, DIRS
;       Either vectors (same length) or variables of type GELEM (see
;       explanation in the MAKE_ELEM routine).  In the second case only the
;       direction field is used.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    DEGREES
;	Switch, if set the result is given in degrees.  Default is radians.
; OUTPUTS:
;	Returns the value of the angle.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Vectors must have equal length.
; PROCEDURE:
;	Uses the function COSANGLE from XOPER_LIB.
; MODIFICATION HISTORY:
;	Created 30-JUN-1992 by Mati Meron.
;-

    on_error, 1

    if n_elements(deg) eq 0 then return, acos(Cosangle(dirf,dirs)) $
    else return, !radeg*acos(Cosangle(dirf,dirs))
end
