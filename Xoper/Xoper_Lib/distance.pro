Function Distance, elef, eles

;+
; NAME:
;	DISTANCE
; PURPOSE:
;	Finds the distance between two geometrical elements of type GELEM (for
;	definition see routine MAKE_ELEM).
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = DISTANCE (ELEF, ELES)
; INPUTS:
;    ELEF
;	A GELEM structure
;    ELES
;	Ditto.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the distance between ELEF and ELES, if defined.  If one of the 
;	elements if undefined (.dim = -1) returns -1e38.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Both ELEF and ELES must be GELEM type structures.
; PROCEDURE:
;	Standard geometrical definitions.  Uses VNORM, VSCALP and PROJECT_OPER
;	FROM XOPER_LIB.  ALSO uses TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    on_error, 1
    if Type(elef) ne 8 or Type(eles) ne 8 then message, 'Improper elements!'

    if elef.dim le eles.dim then begin
	delf = elef
	dels = eles
    endif else begin
	delf = eles
	dels = elef
    endelse

    dr = delf.loc - dels.loc
    case delf.dim of
	-1: dist = -1e38
	0:  case dels.dim of
		0:  dist = Vnorm(dr)
		1:  dist = Vnorm(Project_oper(dels.dir,/comp)#dr)
		2:  dist = Vnorm(Project_oper(dels.dir)#dr)
		else:  message, 'Unrecognized element!'
	    endcase
	1:  case dels.dim of
		1:  begin
			nvec = crossp(delf.dir,dels.dir)
			if Vnorm(nvec) eq 0 then $
			dist = Vnorm(Project_oper(dels.dir,/comp)#dr) $
			else dist = Vnorm(Project_oper(nvec)#dr)
		    end
		2:  if Vscalp(delf.dir,dels.dir) ne 0 then dist = 0. $
		    else dist = Vnorm(Project_oper(dels.dir)#dr)
		else:  message, 'Unrecognized element!'
	    endcase
	2:  case dels.dim of
		2:  if Vnorm(crossp(delf.dir,dels.dir)) ne 0 then dist = 0. $
		    else dist = Vnorm(Project_oper(dels.dir)#dr)
		else:  message, 'Unrecognized element!'
	    endcase
	else:  message, 'Unrecognized element!'
    endcase 

    return, dist
end
