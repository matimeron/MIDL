Function Make_elem, dimel, locvec, dirvec, name = nam, idnum = id

;+
; NAME:
;	MAKE_ELEM
; PURPOSE:
;	Creates or modifies a geometric element (GEMEL).  GEMEL is defined as a
;	structure with the following fields:
;	    NAM: string, element's name.
;	    IDN: long integer, element's ID.
;	    DIM: integer, specifies type.  Currently used values are:
;		-1 - undefined.
;		 0 - point.
;		 1 - line.
;		 2 - plane.
;	    LOC: 3 dimensional floating vector.  Specifies location.
;	    DIR: 3 dimensional floating vector.  Specifies direction.
;	The direction field is left blank for a point.  Both location and 
;	direction are blank for "undefined".
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = MAKE_ELEM ( DIMEL [, LOCVEC, DIRVEC ] [, keywords])
; INPUTS:
;    DIMEL
;	Either a proper geometric element (structure of type GELEM) or an 
;	integer (corresponding to the DIM field).  If the second is true then
;	the parameters LOCVEC and DIRVEC must be provided.
;    LOCVEC
;	3 dimensional vector, gets translated into the LOC field of the 
;	geometric element.
;    DIRVEC
;	3 dimensional vector, gets translated into the DIR field of the 
;	geometric element.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the 
;	geometric element.  If absent, generic names ("Point", "Line" or 
;	"Plane") are used.
;    IDNUM
;	Accepts a numeric constant or variable that's used as the IDN field of
;	the geometric element.  Default value is one.  Converted to type LONG
;	on input.
; OUTPUTS:
;	Returns a structure of type GELEM.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Writes the input parameters into the fields of the output structure.
;	Also, for lines and planes, normalizes DIR and replaces LOC with its 
;	minimal equivalent (perpendicular to DIR for lines, parallel to DIR
;	for planes.  Uses TYPE from MIDL and VNORM, PROJECT_OPER, from 
;	XOPER_LIB.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM and IDN
;	field in structure GELEM.
;-

    on_error, 1

    genam = ['** Undefined! **', 'Point', 'Line', 'Plane']
    blank = {gelem, nam:genam(0), idn:1l, dim:-1, loc:fltarr(3), dir:fltarr(3)}

    if n_params() eq 1 then if Type(dimel) eq 8 then elem = dimel $
    else message, 'Input error, missing parameters!' $
    else  elem = {gelem, genam(dimel+1), 1, dimel, locvec, dirvec}
    if Type(nam) eq 7 then elem.nam = nam
    if n_elements(id) ne 0 then if Type(id) lt 7 then elem.idn = id

    if elem.dim gt 0 then begin
	norm = Vnorm(elem.dir)
	if norm eq 0 then begin
	    elem.dim = -1
	    message, 'Warning, direction not defined!', /continue
	endif else elem.dir = elem.dir/norm
    endif

    case elem.dim of
	-1  :	elem = blank
	0   :	elem.dir = 0.
	1   :	elem.loc = Project_oper(elem.dir,/comp)#elem.loc
	2   :	elem.loc = Project_oper(elem.dir)#elem.loc
	else:	begin
		    elem = blank
		    message, 'Unrecognized element index', /continue
		end
    endcase

    return, elem
end
