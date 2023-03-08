Function Plane_3po, pf, ps, pt, name = nam, idnum = id

;+
; NAME:
;	PLANE_3PO
; PURPOSE:
;	Generates a GELEM structure representing a plane passing through 3 
;	given points.  For description of a GELEM structure, see MAKE_ELEM.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = PLANE_3PO ( PF, PS, PT [, keywords])
; INPUTS:
;    PF
;	Either a 3 dimensional vector, or GELEM structure representing a point.
;    PS
;	Ditto.
;    PT
;	Ditto.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the line.
;	Default name is "Plane".
;    IDNUM
;	Accepts a number that is used as the IDN field of the line.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the plane passing through PF, 
;	PS, PT.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	If the three points are coplanar, an "Undefined" GELEM is returned 
;	instead of a plane.
; PROCEDURE:
;	Uses the three points to establish the direction orthogonal to the 
;	plane, then calls MAKE_PLANE in XOPER_LIB.  Also uses TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1

    if Type(pf) ne 8 then tpf = pf else $
    if pf.dim eq 0 then tpf = pf.loc else message, 'Not a point!'
    if Type(ps) ne 8 then tps = ps else $
    if ps.dim eq 0 then tps = ps.loc else message, 'Not a point!'
    if Type(pt) ne 8 then tpt = pt else $
    if pt.dim eq 0 then tpt = pt.loc else message, 'Not a point!'

    return, Make_plane(tpf, crossp(tps-tpf, tpt-tpf), name = nam, idnum = id)
end
