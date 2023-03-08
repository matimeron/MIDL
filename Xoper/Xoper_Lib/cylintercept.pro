Function Cylintercept, lin, cyl, rad, lonvec = lv, travec = tv, backward = bc,$
    name = nam, idnum = id

;+
; NAME:
;	CYLINTERCEPT
; PURPOSE:
;	Finds the intercept of the line LIN with the cylinder CYL.  LIN is 
;	represented by a GELEM line structure (see routine MAKE_ELEM).  CYL is
;	represented by a similar structure plus the radius RAD.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = CYLINTERCEPT ( LIN, CYL, RAD, [ keywords])
; INPUTS:
;    LIN
;	A GELEM structure representing a line.
;    CYL
;	Ditto.
;    RAD
;	Scalar, the cylinder radius.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    LONVEC
;	Optional output parameter, see below.
;    TRAVEC
;	Ditto.
;    BACKWARD
;	Switch.  If set, the backwards going intercept is selected.  Default is
;	the forward intercept, i.e. the one corresponding to the positive sign
;	of the square root.
;    NAME
;	Accepts a character string that is used as the NAM field of the result.
;	Default is "Point".
;    IDNUM
;	Accepts a number that is used as the IDN field of the result.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the point where the line 
;	intercepts the cylinder.  If no intercept exists, returns an element of
;	type "Undefined".
; OPTIONAL OUTPUT PARAMETERS:
;    LONVEC
;	The name of a variable to receive the longitudinal (along the cylinder
;	axis) part of the vector connecting the intercept with the cylinder 
;	axis)
;    TRAVEC
;	Same for the transverse part.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward geometry.  Calls MAKE_ELEM and PROJECT_OPERfrom 
;	XOPER_LIB and TYPE from MIDL>
; MODIFICATION HISTORY:
;	Created 30-JULY-1992 by Mati Meron.
;-

    on_error, 1
    if Type(lin) ne 8 or Type(cyl) ne 8 then message, 'Improper elements!'
    if lin.dim ne 1 or cyl.dim ne 1 then message, 'Elements must be lines!'

    rdim = 0
    rloc = [0.,0.,0.]
    delr = cyl.loc - lin.loc
    prop = Project_oper(cyl.dir, /comp)

    a = (transpose(lin.dir)#prop#lin.dir)(0)
    b = (transpose(lin.dir)#prop#delr)(0)
    c = (transpose(delr)#prop#delr)(0) - rad^2

    disc = b^2 - a*c
    if disc ge 0 and a gt 0 then begin
	sn = 1 - 2*keyword_set(bc)
	mag = (b + sn*sqrt(disc))/a
	rloc = lin.loc + mag*lin.dir
	vrel = rloc - cyl.loc
	tv = prop#vrel
	lv = vrel - tv
    endif else rdim = -1

    return, Make_elem(rdim, rloc, [0.,0.,0.], name = nam, idnum = id)
end
