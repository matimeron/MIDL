Function Intercept, elef, eles, name = nam, idnum = id

;+
; NAME:
;	INTERCEPT
; PURPOSE:
;	Finds the intercept of the geometrical elements ELEF, ELES (both given
;	as GELEM structures, see routine MAKE_ELEM).
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = INTERCEPT ( ELEF, ELES, [, keywords])
; INPUTS:
;    ELEF
;	A GELEM structure
;    ELES
;	Ditto.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the result.
;	Default is the standard name ("Point", "Line" or "Plane")
;    IDNUM
;	Accepts a number that is used as the IDN field of the result.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the intersect of ELEF and ELES.
;	If no intersect exists, returns an element of type "Undefined".
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Standard geometrical definitions.  Uses VNORM, VSCALP and MAKE_ELEM
;	from XOPER_LIB.  Also uses TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
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

    rav = (dels.loc + delf.loc)/2
    der = (dels.loc - delf.loc)/2
    rloc = [0.,0.,0.]
    rdir = rloc

    case delf.dim of
	-1: rdim = -1
	0:  case dels.dim of
		0:  if Vnorm(der) eq 0 then begin
			rdim = 0
			rloc = delf.loc
		    endif else rdim = -1
		1:  if Vnorm(crossp(der,dels.dir)) eq 0 then begin
			rdim = 0
			rloc = delf.loc
		    endif else rdim = -1
		2:  if Vscalp(der,dels.dir) eq 0 then begin
			rdim = 0
			rloc = delf.loc
		    endif else rdim = -1
		else: begin
			message, 'Unrecognized element!', /continue
			rdim = -1
		    end
	    endcase
	1:  case dels.dim of
		1:  begin
			nvec = crossp(delf.dir,dels.dir)
			if Vnorm(nvec) eq 0 then begin
			    if Vnorm(crossp(der,dels.dir)) eq 0 then begin
				rdim = 1
				rloc = delf.loc
				rdir = delf.dir
			    endif else rdim = -1
			endif else begin
			    if Vscalp(der,nvec) eq 0 then begin
				rdim = 0
				scaf = 1./(1 - Vscalp(delf.dir,dels.dir)^2)
				a = scaf*Vscalp(der,crossp(dels.dir,nvec))
				b = scaf*Vscalp(der,crossp(delf.dir,nvec))
				rloc = rav + a*delf.dir + b*dels.dir
			    endif else rdim = -1
			endelse
		    end
		2:  begin
			if Vscalp(delf.dir,dels.dir) eq 0 then begin
			    if Vscalp(der,dels.dir) eq 0 then begin
				rdim = 1
				rloc = delf.loc
				rdir = delf.dir
			    endif else rdim = -1
			endif else begin
			    rdim = 0
			    a = Vscalp(der,dels.dir)/Vscalp(delf.dir,dels.dir)
			    rloc = delf.loc + 2*a*delf.dir
			endelse
		    end
		else: begin
			message, 'Unrecognized element!', /continue
			rdim = -1
		    end
	    endcase
	2:  case dels.dim of
		2:  begin
			nvec = crossp(delf.dir,dels.dir)
			if Vnorm(nvec) eq 0 then begin
			    if Vscalp(der,delf.dir) eq 0 then begin
				rdim = 2
				rloc = delf.loc
				rdir = delf.dir
			    endif else rdim = -1
			endif else begin
			    rdim = 1
			    a = Vscalp(delf.loc,delf.dir)
			    b = Vscalp(dels.loc,dels.dir)
			    c = Vscalp(delf.dir,dels.dir)
			    va = delf.dir - c*dels.dir
			    vb = dels.dir - c*delf.dir
			    rloc = (a*va + b*vb)/(1 - c^2)
			    rdir = crossp(delf.dir,dels.dir)
			endelse
		    end
		else: begin
			message, 'Unrecognized element!', /continue
			rdim = -1
		    end
	    endcase
	else: begin
		message, 'Unrecognized element!', /continue
		rdim = -1
	    end
    endcase 

    return, Make_elem(rdim, rloc, rdir, name = nam, idnum = id)
end
