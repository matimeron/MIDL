Pro Move_device, dev, delts, degrees = deg, external = ext

;+
; NAME:
;	MOVE_DEVICE.
; PURPOSE:
;	Performs a set of transformations on a "device", where "device' is
;	defined as an array of geometrical elements (i.e. structures of type
;	GELEM, see routine MAKE_ELEM).  The transformations can be either 
;	rotations or translations.  More details appear in PROCEDURE, below.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = MOVE_DEVICE, DEV, DELTS [, /DEGREES ] [, EXTERNAL = ext ])
; INPUTS:
;    DEV
;	An array of GELEM structures.
;    DELTS
;	Either a numeric array or ( if EXTERNAL is set) a numeric scalar)
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    DEGREES
;	If set, all angles are measured in degrees.  Default is radians.
;    EXTERNAL
;	Structure of type GELEM, representing a line or a plane.  Optional.
; OUTPUTS:
;	None.  However DEV is modified by the call.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	There are two possible modes of operation:
;	1)  If the keyword EXTERNAL is used, all the elements of DEV are 
;	    transformed relative to EXT.  If EXT is a line, the transformation 
;	    is rotation around EXT, where DELTS provides the rotation angle. 
;	    If EXT is a plane, the transformations is a translation along the
;	    normal to the plane, by the distance DELTS.
;	2)  If EXTERNAL isn't provided, each element of DEV, in order, serves as
;	    a reference for transforming the elements that follow it.  If 
;	    DEV(i) is a line, all DEV(j) (j = i+1 ...) whose ID numbers is 
;	    divisible by the ID number of DEV(i) are rotated around this line 
;	    by an angle DELTS(i).  If DEV(i) is a plane, all DEV(j) (j = i... ) 
;	    whose ID numbers are divisible by the ID number of DEV(i) are 
;	    translated along the normal to this plane, by a distance DELTS(i).
;
;	MOVE_DEVICE uses calls to ROTATE_ELEM and TRANSLATE_ELEM in XOPER_LIB,
;	and also TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added ID number checking.
;-

    on_error, 1
    if Type(dev) ne 8 then message, 'Not a device!'

    nel= n_elements(dev) - 1
    if n_elements(ext) ne 0 then begin
	if Type(ext) ne 8 then message, 'Not a proper element!'
	if delts ne 0 then begin
	    if ext.dim eq 1 then for j = 0, nel do $
	    dev(j) = Rotate_elem(dev(j), ext, delts, degrees = deg) $
	    else if ext.dim eq 2 then for j = 0, nel do $
	    dev(j) = Translate_elem(dev(j), ext, delts)
	endif
    endif else begin
	k = where(delts(0:nel < (n_elements(delts) - 1)) ne 0, numk)
	for i = 0, numk - 1 do begin
	    l = k(i) + where(dev(k(i):*).idn mod dev(k(i)).idn eq 0, numl)
	    for j = 0, numl - 1 do begin
		if dev(k(i)).dim eq 1 and k(i) ne l(j) then dev(l(j)) = $
	    	Rotate_elem(dev(l(j)), dev(k(i)), delts(k(i)), degrees = deg) $
		else if dev(k(i)).dim eq 2 then dev(l(j)) = $
		Translate_elem(dev(l(j)), dev(k(i)), delts(k(i)))
	    endfor
	endfor
    endelse

    return
end
